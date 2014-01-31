{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Crud where

import           Snap
import           Snap.Util.Readable
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Trans.Class
import           Control.Monad.Writer hiding (Product, pass)
import           Data.Monoid hiding (Product)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Groundhog as GH
import qualified Database.Groundhog.Core as GH
import           Heist
import           Heist.Compiled

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

field :: (Readable a, Show a, MonadSnap m) => ByteString -> Lens' r a
      -> Crud m r a
field name l = Pair create retrieve
  where
    -- We probably want error-handling for the no-such-parameter case
    create = getParam name >>= fromBS
   
    --splice :: Snap r -> Splice Snap
    splice m = return . yieldRuntime $ lift m >>=
        return . textSplice (T.pack . show) . view l
    retrieve = do
       m   <- lift get
       tell $ (T.decodeUtf8 name ## splice m)
       rec <- lift . lift $ m
       lift . put $ return rec
       return $ view l rec

type Retrieve m r = WriterT (Splices (Splice m)) (StateT (m r) m)
    
handle :: (GH.PersistEntity r, GH.PersistBackend m, MonadSnap m) => 
          Crud m r r -> (ByteString -> m r) -> m ()
handle (Pair c r) byKey = route create <|> retrieve
  where
    create   = method POST $ c >>= GH.insert_

    -- This should come after the to-be implemented list-all handler,
    -- so that that handler can use `ifTop`.
    -- An alternative might be to use `pathArg`, which does almost
    -- the right thing.
    retrieve = method GET $ do
        let access = withRequest (byKey . rqPathInfo)
        ((spl, rec), _) <- (runStateT . runWriterT) r access
        undefined
