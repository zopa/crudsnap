{-# LANGUAGE RankNTypes #-}
module Snap.Crud where

import           Snap
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Trans.Class
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

-- This looks an awful lot like a lens.:..
-- What we're trying to do is create a database-aware lens for a
-- datatype...
--
--

-- We use a Writer applicative to build our routing tree
-- The intention is that m should be an instance of MonadSnap, with
-- the monoid Writer requires provided by snap's Alternative instance.
--type Crud m a = Product (ContT () m a) Identity

-- First complication: We need to know that our final datatype is
-- an instance of PersistEntity. But we only have available the 
-- for said datatype. How can we use that to ensure the suitability of
-- the final type, especially given that we don't know how many arguments
-- we might get?
--
-- Maybe we need a typeclass for eventually-persitable continuations?
--
-- For now, make Snap return the persisted type.


--resource :: forall m v. (PersistEntity v, MonadSnap m, PersistBackend m)
         -- => Bytestring -> (b -> a) -> Crud (m v) (b -> a)
--resource name constructor = Product (Constant handlers) (pure constructor)
  --where
--    handlers = create <|> retrieve <|> update <|> destroy <|> list
   
--    create :: v -> m ()
  --  create v = path byteString . method POST $ insert_ v

  --  retrieve :: m ()
--    g

-- Parse one field and bind one splice. We'll also need:
--     - to render a list of items
--     - to wrap digestive-funtors (which may obviate the need
--       for the parsing side).
--     - update a field in a record.
--     - delete a record.
type Field r a = Product (Recieve) (Render r) a

field :: (Read a, Show a) => ByteString -> Lens' r a -> a -> Field r a
field name l v = Pair recieve render
  where
    -- We really want a user-supplied parser, and error-handling
    -- for the no-such-parameter case
    recieve = getParam name >>= maybe pass (return . read . B.unpack)
    
    splice = return . yieldRuntime . return
           . textSplice (T.pack . show) . view l
    render = const undefined <$> tell (\r -> T.decodeUtf8 name ## splice r)
    
    -- If we're rendering (a single record, not a list),
    -- the last segment of the path represents the record id
--     render = Const (T.decodeUtf8 name ## splice)
--     splice = return . yieldRuntime $ do
--         key <- lift rqPathInfo
--         val <- view l <$> lift (GH.get key)
--         return $ textSplice (T.pack . show) val
--     render = ContT $ \fromKey -> do
--         key <- lift $ withRequest (return . rqPathInfo)
--         rec <- fromKey key
--         let splice = return . yieldRuntime . return
--                    . textSplice (T.pack . show)
--                    $ view l rec
--         tell (T.decodeUtf8 name ## splice)
--         return rec


type Recieve = Snap 

--type Render r = ContT r (WriterT (Splices (Splice Snap))  Snap)
type Render r = Writer (r -> Splices (Splice Snap))

type Writer mo = Product (Const mo) Identity

tell :: Monoid mo => mo -> Writer mo ()
tell mo = Pair (Const mo) (pure ())
