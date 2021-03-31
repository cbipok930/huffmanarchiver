{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Huffman

import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as PP
import qualified Pipes.Binary     as PBB
import Data.Binary hiding (encodeFile)
import Data.Bits (setBit, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<$>))
import Control.Monad.Trans.State.Strict (evalState)
import Data.Foldable (sum)
import Data.Map.Strict (Map, (!))
import Lens.Family2 (view)
import Prelude hiding (sum)
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  print "What do you want to do with file (zip / unzip)?"
  mode <- getLine
  case mode of "zip" -> print "Enter input file name/path to compress file"
               "unzip" -> print "Enter input file name/path to decode file"
               _ -> error "Wrong command"
  inp <- getLine
  metadata <- analyzeFile inp
  let (len, tree) = case metadata of
         Just (l, t) -> (l, t)
         Nothing -> error "Empty File"                         
  print "Enter output file name/path"
  out <- getLine
  print "Doing some magic..."
  case mode of "zip" -> encodeFile inp out len tree
               "unzip" -> Main.decodeFile inp out             
  print"Done!!!!!!!!!!!"




---------- ============================================= encode pipeline and file analyze
bsToBytes :: Monad m => Pipe ByteString Word8 m r
bsToBytes = PP.mapFoldable B.unpack

encodeByte :: (Ord a, Monad m)
           => Map a Encoding
           -> Pipe a Direction m r
encodeByte encTable = PP.mapFoldable (encTable !)

dirsBytes :: (MonadIO m, Functor m)
          => Producer Direction m r
          -> Producer Word8     m ()
dirsBytes p = do
    (result, leftovers) <- lift $ runStateT dirsBytesP p
    case result of
      Just byte -> do
        yield byte
        dirsBytes leftovers
      Nothing   -> return ()

dirsBytesP :: (Monad m, Functor m) => Parser Direction m (Maybe Word8)
dirsBytesP = do
    isEnd <- isEndOfInput
    if isEnd
      then return Nothing
      else Just <$> go 0 0
  where
    go :: Monad m => Word8 -> Int -> Parser Direction m Word8
    go b 8 = return b
    go b i = do
      dir <- draw
      case dir of
        Just DLeft  -> go     b            (i + 1)
        Just DRight -> go     (setBit b i) (i + 1)
        Nothing     -> return b

analyzeFile :: FilePath -> IO (Maybe (Int, HTree Word8))
analyzeFile fp = withFile fp ReadMode $ \hIn -> do
    let byteProducer = PB.fromHandle hIn >-> bsToBytes
    fqs <- freqs byteProducer
    let len  = sum fqs
        tree = evalState (listQueueStateTable fqs >> buildTree) emptyPQ
    return $ fmap (len,) tree
  where
    freqs :: (Monad m, Ord a) => Producer a m () -> m (M.Map a Int)
    freqs = PP.fold f M.empty id
      where
        f m x = M.insertWith (+) x 1 m

encodeFile :: FilePath -> FilePath -> Int -> HTree Word8 -> IO ()
encodeFile inp out len tree =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      BL.hPut hOut $ encode (len, tree)
      let dirsOut   = PB.fromHandle hIn
                  >-> bsToBytes
                  >-> encodeByte encTable
          bsOut     = view PB.pack . dirsBytes $ dirsOut
          pipeline  = bsOut
                  >-> PB.toHandle hOut

      runEffect pipeline
  where
    encTable  = htTable tree
--- ============================================================= decode pipeline
searchHT :: forall a m r. Monad m
         => HTree a
         -> Pipe Direction a m r
searchHT t = searchHT' t >~ cat
  where
    searchHT' :: HTree a -> Consumer' Direction m a
    searchHT' (HLeaf x)       =
        return x
    searchHT' (HNode ht1 ht2) = do
        dir <- await
        searchHT' $ case dir of
                      DLeft  -> ht1
                      DRight -> ht2

bytesToDirs :: Monad m => Pipe Word8 Direction m r
bytesToDirs = PP.mapFoldable byteToDirList
  where
    byteToDirList :: Word8 -> [Direction]
    byteToDirList b = map f [0..7]
      where
        f i | testBit b i = DRight
            | otherwise   = DLeft

decodeFile :: FilePath -> FilePath -> IO ()
decodeFile inp out =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      let metadataPipe = PB.fromHandle hIn

      -- consume metapipe to read in the tree/metadata
      (metadata, decodingPipe) <- runStateT PBB.decode metadataPipe

      case metadata of
        Left   _          ->
          error "Corrupt metadata."
        Right (len, tree) -> do
          -- do everything with the rest
          let bytesOut  = decodingPipe >-> bsToBytes
                      >-> bytesToDirs  >-> searchHT tree
                      >-> PP.take len
              bsOut     = (view PB.pack) bytesOut
              pipeline  = bsOut
                      >-> PB.toHandle hOut

          runEffect pipeline