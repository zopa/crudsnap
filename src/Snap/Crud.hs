{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Crud where

import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.Readable
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Trans.Class
import           Control.Monad.Writer hiding (Product, pass)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Functor.Identity
import           Data.Functor.Product
import qualified Data.Map.Lazy as M
import           Data.Monoid hiding (Product)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Groundhog as GH
import qualified Database.Groundhog.Core as GH
import           Database.Groundhog.Utils
import           Heist
import           Heist.Compiled
import           Text.XmlHtml

-- Parse one field and bind one splice. We'll also need:
--     - to render a list of items
--     - to wrap digestive-funtors (which may obviate the need
--       for the parsing side).
--     - update a field in a record.
--     - delete a record.

-- The idea is to use this like formlets/digestive-functors:
--     Constructor <$> field name1 lens1 <*> field name2 lens2, etc.
-- Then `handle`, below, should turn a Crud m r r into a MonadSnap m,
-- which knows about all the relevant paths, methods, and splices.

type Crud m r a = Product m (Retrieve m r) a

field :: (Readable a, Show a, MonadSnap m) =>
         ByteString -> Lens' r a -> Crud m r a
field name l = Pair create retrieve
  where
    -- We probably want error-handling for the no-such-parameter case
    create = getParam name >>= maybe pass fromBS
   
    --splice :: Snap r -> Splice Snap
    splice m = return . yieldRuntime $ lift m >>=
        return . textSplice (T.pack . show) . view l
    retrieve = do
       f   <- lift get
       key <- lift . lift $ fromBS . rqPathInfo =<< getRequest
       tell $ (T.decodeUtf8 name ## splice (f key))
       rec <- lift . lift $ f key
       lift . put . return $ const rec
       return $ view l rec

type Retrieve m r = WriterT (Splices (Splice m)) (StateT (Int -> m r) m)
   
handle :: (GH.PersistEntity r, GH.PersistBackend n, MonadSnap m) => 
          (n r -> m r) -> Crud m r r -> m ()
handle doGH (Pair c r) = create <|> listAll <|> retrieve
  where
    create   = method POST $ c >>= \x -> doGH (GH.insert_ x) >> return x
    listAll  = method GET . ifTop $ pass -- Not yet defined
    retrieve = method GET $ cRender "single"

makeSingleTemplate :: Crud m r r -> Template
makeSingleTemplate (Pair _ wr) = 
    [Element "apply" [("template","base")]
        [Element "bind"  [("tag","item-header")] [header]
        ,Element "bind"  [("tag","item-detail")] [detail]
        ,Element "apply" [("template","_table")] []
        ]
    ]
  where
    header = Element "tr" [] $
        foldl (appendElem "th" ) [] $ map ((:[]) . TextNode) columns
    detail = Element "tr" [] $
        foldl (\es t -> appendElem t es []) [] columns
    
    columns = M.keys . runSplices $ splices -- See Node [Column order]
    (splices,_) = runWriterT wr

appendElem :: T.Text -> [Node] -> [Node] -> [Node] 
appendElem tag els conts = els ++ [Element tag [] conts]

{- Note [Column Order]
~~~~~~~~~~~~~~~~~~~~~~
M.key gives us the columns in alphabetical order: not what we
want. The solution will be to pass along a list of headings in the Writer.
-}
