{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Snap.Crud where

import           Snap
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
import           Heist
import           Heist.Compiled
import qualified Database.Groundhog as GH


-- Parse one field and bind one splice. We'll also need:
--     - to render a list of items
--     - to wrap digestive-funtors (which may obviate the need
--       for the parsing side).
--     - update a field in a record.
--     - delete a record.

type Crud r a = Product (Recieve) (Render r) a

field :: (Read a, Show a) => ByteString -> Lens' r a -> Crud r a
field name l = Pair recieve render
  where
    -- We really want a user-supplied parser, and error-handling
    -- for the no-such-parameter case
    recieve = getParam name >>= maybe pass (return . read . B.unpack)
   
    --splice :: Snap r -> Splice Snap
    splice m = return . yieldRuntime $
           lift m >>= return . textSplice (T.pack . show) . view l
    render = do
       m   <- lift get
       tell $ (T.decodeUtf8 name ## splice m)
       rec <- lift . lift $ m
       lift . put $ return rec
       return $ view l rec

type Recieve = Snap 

type Render r = WriterT (Splices (Splice Snap)) (StateT (Snap r) Snap)
    

    -- If we're rendering (a single record, not a list),
    -- the last segment of the path represents the record id
