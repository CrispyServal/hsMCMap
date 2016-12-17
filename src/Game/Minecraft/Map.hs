{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Game.Minecraft.Map (
          loadRegionsP
        , buildImage
        , buildAndWritePng 
        )where

import Game.Minecraft.Map.NBT
import Game.Minecraft.Map.Block
import Game.Minecraft.Map.Region

import System.Directory
import System.IO

import Data.Word
import Data.Int
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Bits
import Codec.Picture
import Codec.Picture.Png

import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV

import Control.Monad
import Control.Monad.Primitive
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan

import Debug.Trace

data Chunk = Chunk !XPos !ZPos ![[(Word8,Word8)]] deriving (Eq,Show)
type XPos = Int
type ZPos = Int
data ChunkTop = ChunkTop
                {
                    getTopX ::  !XPos,
                    getTopZ ::  !ZPos,
                    getTopData ::  ![(Word8,Word8)]
                } deriving (Eq, Show)

make2D :: Int -> [a] -> [[a]]
make2D _ [] = []
make2D y xs = Data.List.take y xs : make2D y (Data.List.drop y xs)

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
    ref = zipWith refT
    refT high low
        | ifDraw high = high
        | otherwise = low

buildChunk :: NBT -> Chunk
buildChunk nbt =
    let
        c       = contentNBT nbt
        secs    = openList $ fromJust $ navigate ["Level","Sections"] c
        ids     = concatMap ( byteArrayContent.fromJust.navigate ["Blocks"] ) secs
        adds    = concatMap ( splitData.byteArrayContent.fromJust.navigate ["Data"] ) secs
        x       = fromIntegral (fromIntegral . intContent . fromJust $ navigate ["Level","xPos"] c :: Int32) :: Int
        z       = fromIntegral (fromIntegral . intContent . fromJust $ navigate ["Level", "zPos"] c :: Int32 ) :: Int
    in
        Chunk x z (make2D 256 $ zip ids adds)

-- 8 bits per block, but 4 bits per additional data, so split it
splitData :: [Word8] -> [Word8]
splitData = concatMap sp where
    sp w = [w `shiftR` 4 , w .&. 0x0F]

loadRegions :: String -> IO [ChunkTop]
loadRegions path = do
    rFiles <- filter (isSuffixOf "mca") <$> listDirectory path
    contents <- mapM (BL.readFile .((path ++ "/") ++ )) rFiles
    --let r =  map (toTopView . buildChunk . getNBT . getRawNBT) (concatMap getChunkRaws contents)
    let r =  map (toTopView . buildChunk . getNBT . chunkRawRawNBT) (concatMap getChunkRaws contents)
    return r

loadRegionsP :: String -> IO [ChunkTop]
loadRegionsP path = do
    rFiles <- filter (isSuffixOf "mca") <$> listDirectory path
    contents <- mapM (BL.readFile .((path ++ "/") ++ )) rFiles
    let numCon = length contents
    chTsChan <- newChan
    forM_ [0..numCon-1] $ \i -> forkIO $ fromOneFile (contents !! i) chTsChan
    reduce chTsChan numCon []
        where
            fromOneFile content outChan = do
                let chTs = map (toTopView . buildChunk . getNBT . chunkRawRawNBT) (getChunkRaws content)
                writeChan outChan chTs
            reduce inChan n nowResult
                    | n == 0 = return nowResult
                    | otherwise = do
                        chTs <- readChan inChan
                        reduce inChan (n-1) (chTs `seq` chTs++nowResult)

getRange :: [ChunkTop] -> (Int,Int,Int,Int)
getRange = foldl' ref e where
    e = (0,0,0,0)
    ref (oMinX,oMinZ,oMaxX,oMaxZ) (ChunkTop x z _) = (min oMinX x, min oMinZ z,max oMaxX x, max oMaxZ z)

writeList :: (PrimMonad m, MV.MVector v a) => v (PrimState m) a -> Int -> [a] -> m ()
writeList v pos xs = forM_ [0..(length xs - 1)] (\i -> MV.unsafeWrite v (pos+i) (xs !! i))

buildAndWritePng :: [ChunkTop] -> FilePath -> Bool -> IO ()
buildAndWritePng chs path log = do
    when log $ print "building image ..."
    img <- buildImage chs
    when log $ print "saving ..."
    writePng path img

buildImage :: [ChunkTop] -> IO (Image PixelRGBA8)
buildImage chs = do
                v <- MV.new n
                forM_ chs $ \ch -> fillChunk ch v
                fv <- V.unsafeFreeze v
                return $ Image (width `shiftL` 4) (height `shiftL` 4) fv
            where
                (minX, minZ, maxX, maxZ) = getRange chs
                width = maxX - minX + 1
                height = maxZ - minZ + 1
                wordPerLine = width`shiftL`6
                n = width * height `shiftL` 10
                fillChunk :: (PrimMonad m, MV.MVector v Word8) => ChunkTop -> v (PrimState m) Word8 -> m ()
                fillChunk (ChunkTop x z chData) v = do
                    let xr = x - minX
                    let zr = z - minZ
                    let chColor = make2D 64 $ concatMap getBlockColor chData
                    forM_ [0..15] $ \row -> writeList v ( wordPerLine *( (zr`shiftL`4) + row) + (xr`shiftL`6) ) (chColor !! row)

-- func for NBT
openList :: Content -> [Content]
openList (TAGList cs) = cs
openList _             = []

byteArrayContent :: Content -> [Word8]
byteArrayContent (TAGByte_Array bs) = bs

intContent :: Content -> Word32
intContent (TAGInt n) = n
