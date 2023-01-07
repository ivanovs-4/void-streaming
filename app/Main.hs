module Main where

import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Resource
import Crypto.Hash (hashWith, Blake2b_256(..))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base58 qualified as Base58
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Int
import Streaming as S
import Streaming.ByteString qualified as SBS
import Streaming.ByteString.Internal (bracketByteString)
import Streaming.Prelude qualified as S
import System.Environment (getArgs)


main :: IO ()
main = do
    filename <- head <$> getArgs
    mapM_ (BS8.putStrLn . encodeHex) =<< readFileToListOfHashesOf (256 * 1024) filename

encodeHex :: ByteString -> ByteString
encodeHex = Base58.encodeBase58 Base58.bitcoinAlphabet

readFileToListOfHashesOf :: Int64 -> FilePath -> IO [ByteString]
readFileToListOfHashesOf size filepath = runResourceT $
    readFileInChunksOf size filepath
      & S.map hashWithBlake2b_256
      & S.toList_

hashWithBlake2b_256 :: (BA.ByteArrayAccess a, BA.ByteArray b) => a -> b
hashWithBlake2b_256 = BA.convert . hashWith Blake2b_256

readFileInChunksOf :: MonadResource m => Int64 -> FilePath -> Stream (Of ByteString) m ()
readFileInChunksOf n filepath = streamChunksOf n (SBS.readFile filepath)

streamChunksOf :: Monad m => Int64 -> SBS.ByteStream m r -> Stream (Of ByteString) m r
streamChunksOf size = fix $ \go sbs -> do
    (bs, sbs') <- fmap S.lazily . lift . SBS.toStrict $ SBS.splitAt size sbs
    unless (BS.null bs) $
        S.yield bs
    if fromIntegral (BS.length bs) < size
      then lift $ S.snd' <$> SBS.toStrict sbs'
      else go sbs'
