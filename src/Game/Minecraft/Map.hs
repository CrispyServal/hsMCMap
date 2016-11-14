{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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
import Codec.Picture
import Codec.Picture.Png

--import qualified Data.Vector.Generic         as V
--import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Control.Monad
import Control.Monad.Primitive
import Control.Applicative

import Debug.Trace

data Chunk = Chunk XPos ZPos [[(Word8,Word8)]] deriving (Eq,Show)
type XPos = Int
type ZPos = Int
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
        x       = fromIntegral (fromIntegral . intContent . fromJust $ navigate ["Level","xPos"] c :: Int32) :: Int
        z       = fromIntegral (fromIntegral . intContent . fromJust $ navigate ["Level", "zPos"] c :: Int32 ) :: Int
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
--yypFun :: [ChunkTop] -> (Int, Int, [Word8])
{-
yypFun :: [ChunkTop] -> Image PixelRGBA8
yypFun chunkTopGroup = Image width height (V.fromList $ unfoldMap (insertMap (getSize chunkTopGroup) chunkTopGroup))
    where
        getSize :: [ChunkTop] -> (Int, Int, Int, Int)
        getSize chunkTopGroup =(maximum xlist, minimum xlist, maximum zlist, minimum zlist)
            where 
                xlist = map (fromIntegral . getTopX) chunkTopGroup :: [Int]
                zlist = map (fromIntegral . getTopZ) chunkTopGroup :: [Int]
        getHW :: (Int, Int, Int, Int) -> (Int, Int)
        getHW (xmax, xmin, zmax, zmin) = mapTuple2 (16*) (xmax - xmin + 1, zmax - zmin + 1)
        (height, width) = getHW . getSize $ chunkTopGroup
        defaultChunk = replicate 16 $ replicate 16  $ replicate 4 (0::Word8)
        insertMap :: (Int, Int, Int, Int) -> [ChunkTop]->[[[[[Word8]]]]]
        insertMap (xmax, xmin, zmax, zmin) chunkTopGroup = [[insertOneChunk (xnow, znow) | xnow <- [xmin..xmax]] | znow <- [zmin..zmax]]
            where
                insertOneChunk :: (Int, Int) -> [[[Word8]]]
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
                        -}

getRange :: [ChunkTop] -> (Int,Int,Int,Int)
getRange chs = foldl' ref e chs where
    e = (0,0,0,0)
    ref (oMinX,oMinZ,oMaxX,oMaxZ) (ChunkTop x z _) = (min oMinX x, min oMinZ z,max oMaxX x, max oMaxZ z)

--writeList :: (PrimMonad m, MV.MVector v a) => v (PrimState m) a -> Int -> [a] -> m ()
writeList :: (PrimMonad m, MV.Storable a) => MV.MVector (PrimState m) a -> Int -> [a] -> m ()
writeList v pos xs = do
    forM_ [0..(length xs - 1)] (\i -> MV.write v (pos+i) (xs !! i)) 
    --forM_ [0..(length xs - 1)] (\i -> MV.unsafeWrite v (pos+i) (xs !! i)) 

lqFun :: [ChunkTop] -> IO (Image PixelRGBA8)
lqFun chs = do
                v <- MV.replicate n (0::Word8)
                forM_ chs $ \ch -> fillChunk ch v
                fv <- V.unsafeFreeze v
                return $ Image (width*16) (height*16) fv
            where
                (minX, minZ, maxX, maxZ) = getRange chs
                width = fromIntegral (maxX - minX + 1) :: Int
                height = fromIntegral (maxZ - minZ + 1) :: Int
                n = width * height * 1024
                --fillChunk :: (PrimMonad m, MV.MVector v Word8) => ChunkTop -> v (PrimState m) Word8 -> m ()
                fillChunk :: (PrimMonad m) => ChunkTop -> MV.MVector (PrimState m) Word8 -> m ()
                fillChunk (ChunkTop x z chData) v = do
                    let xr = x - minX
                    let zr = z - minZ
                    let chColor = make2D (16*4) $ concatMap getBlockColor chData
                    forM_ [0..15] $ \row -> writeList v ((width*16*4)*(16*zr+row) + 16*4*xr) (chColor !! row)



-- usage ./main "a region folder"
    -- {Y} rs :: [[ChunkTop]]
main :: IO ()
main = do 
        print "starting"
        [testArg] <- getArgs
        rs <- loadRegions testArg
        --let yypRes = yypFun rs
        img <- lqFun rs
        writePng "lq.png" img 
        --print $ ("w = " ++ (show $ imageWidth yypRes) ++ "\nh = " ++ (show $ imageHeight yypRes) ++ "\n data = " ++ (show $ V.length $ imageData yypRes))
        print "done"

