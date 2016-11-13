module Game.Minecraft.Map.Region where

import Game.Minecraft.Map.NBT

--import System.Directory
--import System.IO
--import System.Environment

--import Data.Attoparsec.Binary
--import Data.Attoparsec

import Data.Binary.Strict.Get

import Data.Word
import Data.List
import Data.Either
import Data.Bits
import qualified Data.ByteString as B
import qualified Codec.Compression.GZip as G
import qualified Data.ByteString.Char8 as BC

import Control.Applicative
import Control.Monad


type Location = Word32
type TimeStamp = Word32

data Header = Header [Location] [TimeStamp] deriving Show

--headGet :: Get Header
headGet :: Get Header
headGet = do
	l <- replicateM 1024 location
	t <- replicateM 1024 timeStamp
	return $ Header l t

location = getWord32be
timeStamp = getWord32be -- will be ignore

--offset in byte from start of file
getLocation :: Header -> [(Integer,Integer)]
getLocation (Header ls _) = filter (\(x,_)->x/=0) $ sort $ map  
		( (\l -> mapTuple2 (fromIntegral.(4096*)) (l `shiftR` 8 , l .&. 0xff) ) )
		ls

mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x, f y)



-- raw chunk data: without parse its nbt
-- ChunkRaw length type_of_compression data(compressed NBT)
data ChunkRaw = ChunkRaw Word32 Word8 B.ByteString deriving Show

getRawNBT :: ChunkRaw -> B.ByteString
getRawNBT (ChunkRaw _ _ b) = b

chunkGet :: Get ChunkRaw
chunkGet = do
	len <- getWord32be
	t 	<- getWord8
	bs 	<- B.pack <$> (replicateM . fromIntegral) (len-1) getWord8
	return $ ChunkRaw len t bs

-- from whole ByteString to [ChunkRaw]
getChunkRaws :: B.ByteString -> [ChunkRaw]
getChunkRaws c = case (runGet headGet c) of
			(Right res, _) -> let offsets = map (\(x,_) -> x) ( getLocation res ) in
									let rs = [c] <**> (map (B.drop . fromIntegral) offsets)  <**> [(runGet chunkGet)] in
                                        rights rs
			(Left err, _)	-> error err


