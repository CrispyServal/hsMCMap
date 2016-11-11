{-# LANGUAGE OverloadedStrings #-}
module Game.Minecraft.Map where

import Game.Minecraft.Map.NBT
import Game.Minecraft.Map.Block
import Game.Minecraft.Map.Region

import System.Directory
import System.IO
import System.Environment

import Data.Word
import Data.Int
import Data.List
import qualified Data.ByteString as B
import Data.Maybe
import Data.Bits
import System.Directory

import Control.Applicative

import Debug.Trace

data Chunk = Chunk XPos ZPos [[(Word8,Word8)]] deriving (Eq,Show)
type XPos = Int32 
type ZPos = Int32
data ChunkTop = ChunkTop
                {
                    getTopX ::  XPos,
                    getTopZ ::  ZPos,
                    getTopData ::  [(Word8,Word8)]
                } deriving (Eq, Show)

make2D :: Int -> [a] -> [[a]]
make2D _ [] = []
make2D y xs = (Data.List.take y xs) : make2D y (Data.List.drop y xs)

toTopView :: Chunk -> ChunkTop
toTopView (Chunk x z bs) = ChunkTop x z (foldr ref e bs) where
    e = replicate 256 (0x00,0x00)
    ref lows highs = zipWith refT lows highs
    refT low high
        | ifDraw high = high
        | otherwise = low

buildChunk :: NBT -> Chunk
buildChunk nbt = 
    let
        c       = contentNBT nbt
        secs    = openList $ fromJust $ navigate ["Level","Sections"] c
        ids     = (concat $ map ( byteArrayContent.fromJust.navigate ["Blocks"] ) secs )
        adds    = (concat $ map ( splitData.byteArrayContent.fromJust.navigate ["Data"] ) secs )
        x       = fromIntegral . intContent . fromJust $ navigate ["Level","xPos"] c
        z       = fromIntegral . intContent . fromJust $ navigate ["Level", "zPos"] c
    in
        Chunk x z (make2D 256 $ zipWith (,) ids adds)

-- 8 bits per block, but 4 bits per additional data, so split it
splitData :: [Word8] -> [Word8]
splitData s = concat $ map sp s where
    sp w = [w `shiftR` 4 , w .&. 0x0F]


loadRegions :: String -> IO [[ChunkTop]]
loadRegions path = do
	rFiles <- filter (isSuffixOf "mca") <$> listDirectory path
	contents <- sequence $ map (B.readFile .((path ++ "/") ++ )) rFiles
	let r = (pure $ map (toTopView . buildChunk . getNBT . getRawNBT)) <*> (map getChunkRaws contents)
	return r
	
-- {Y} rename this plz
-- {Y} yypFun :: [[ChunkTop]] -> [Word8]
-- ...

-- usage ./main "a region folder"
main :: IO ()
main = do 
	print "starting"
	[testArg] <- getArgs
	-- {Y} rs :: [[ChunkTop]]
	rs <- loadRegions testArg
	-- {Y} let yypRes = yypFunk rs
	-- {Y} print yypRes
	-- print rs
	print "done"

