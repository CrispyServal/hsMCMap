module Game.Minecraft.Map.Region(
          getChunkRaws
        , ChunkRaw(..)
        )where

import           Game.Minecraft.Map.NBT

import qualified Codec.Compression.GZip as G
import qualified Codec.Compression.Zlib as Z
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Either
import           Data.List
import           Data.Word

import           Control.Applicative
import           Control.Monad


type Location = Word32
type TimeStamp = Word32

data Header = Header [Location] [TimeStamp] deriving Show

headGet :: Get Header
headGet = do
    l <- replicateM 1024 location
    t <- replicateM 1024 timeStamp
    return $ Header l t

location = getWord32be
timeStamp = getWord32be -- will be ignore

--offset in byte from start of file
getLocation :: Header -> [(Int,Int)]
getLocation (Header ls _) = filter (\(x,_)->x/=0) $ sort $ map
        (\l -> mapTuple2 (fromIntegral.(4096*)) (l `shiftR` 8 , l .&. 0xff) )
        ls

mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x, f y)



-- raw chunk data: without parse its nbt
-- ChunkRaw length type_of_compression data(decompressed NBT in binary)
data ChunkRaw = ChunkRaw {
                      chunkRawLen    :: Word32
                    , chunkRawTypeC  :: Word8
                    , chunkRawRawNBT :: BL.ByteString
                    } deriving Show

-- getRawNBT :: ChunkRaw -> BL.ByteString
-- getRawNBT (ChunkRaw _ _ b) = b

chunkGet :: Get ChunkRaw
chunkGet = do
    len <- getWord32be
    t     <- getWord8
    bs     <- getLazyByteString $ fromIntegral (len-1)
    return $ ChunkRaw len t $ Z.decompress bs

-- from whole ByteString to [ChunkRaw]
getChunkRaws :: BL.ByteString -> [ChunkRaw]
getChunkRaws c =    let
                        head = runGet headGet c
                        offsets = map fst ( getLocation head)
                        crs = [c] <**> map (BL.drop . fromIntegral) offsets <**> [runGet chunkGet]
                    in     crs
