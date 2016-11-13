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
import qualified Data.ByteString.Lazy as BL
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

allDraw :: [(Word8,Word8)] -> Bool
allDraw = all ifDraw

toTopView :: Chunk -> ChunkTop
toTopView (Chunk x z bs) = ChunkTop x z (myFoldl' ref e $ reverse bs) where
    e = replicate 256 (0x00,0x00)
    myFoldl' f e [] = e
    myFoldl' f e (x:xs)
        | allDraw e = e
        | otherwise =   let e' = e `f` x
                        in seq e' $ myFoldl' f e' xs
    ref highs lows = zipWith refT highs lows
    refT high low
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


loadRegions :: String -> IO [ChunkTop]
loadRegions path = do
	rFiles <- filter (isSuffixOf "mca") <$> listDirectory path
	contents <- sequence $ map (BL.readFile .((path ++ "/") ++ )) rFiles
	let r = map (toTopView . buildChunk . getNBT . getRawNBT) (concatMap getChunkRaws contents)
	return r
	
-- {Y} rename this plz
-- {Y} yypFun :: [[ChunkTop]] -> [Word8]
yypFun :: [ChunkTop] -> (Int32, Int32, [Word8])
yypFun chunkTopGroup =(height, width, unfoldMap (insertMap (getSize chunkTopGroup) chunkTopGroup))  
    where
        getSize :: [ChunkTop] -> (Int32, Int32, Int32, Int32)
        getSize chunkTopGroup =(maximum xlist, minimum xlist, maximum zlist, minimum zlist)
            where 
                xlist = map getTopX chunkTopGroup
                zlist = map getTopZ chunkTopGroup
        getHW :: (Int32, Int32, Int32, Int32) -> (Int32, Int32)
        getHW (xmax, xmin, zmax, zmin) =(xmax - xmin + 1, zmax - zmin + 1)
        (height, width) = getHW . getSize $ chunkTopGroup
        defaultChunk = replicate 16 $ replicate 16  $ replicate 4 (0::Word8)
        insertMap :: (Int32, Int32, Int32, Int32) -> [ChunkTop]->[[[[[Word8]]]]]
        insertMap (xmax, xmin, zmax, zmin) chunkTopGroup = [[insertOneChunk (xnow, znow) | xnow <- [xmin..xmax]] | znow <- [zmin..zmax]]
            where
                insertOneChunk :: (Int32, Int32) -> [[[Word8]]]
                insertOneChunk (x, z)
                    | length (findChunk x z) > 0 =make2D 16 $ map getBlockColor $ getTopData $ head $ findChunk x z
                    | otherwise =defaultChunk
                    where
                        findChunk x z = [chunk |chunk <- chunkTopGroup, getTopX chunk == x, getTopZ chunk == z]
        unfoldMap :: [[[[[Word8]]]]] -> [Word8]
        unfoldMap chunkGroup =concat $ map unfoldRow chunkGroup
            where
                unfoldRow :: [[[[Word8]]]] -> [Word8]
                unfoldRow chunkRow = concat $ unfold chunkRow 16 []
                    where
                        unfold :: [[[[Word8]]]] -> Int -> [[Word8]] -> [[Word8]]
                        unfold chunkRow 0 result =result
                        unfold chunkRow num result =unfold [tail chunk | chunk <- chunkRow] (num - 1) (result ++ concat [head chunk| chunk <- chunkRow])    

-- usage ./main "a region folder"
main :: IO ()
main = do 
	print "starting"
	[testArg] <- getArgs
	-- {Y} rs :: [[ChunkTop]]
	rs <- loadRegions testArg
	let yypRes = yypFun rs
	print yypRes
	--print rs
	print "done"

