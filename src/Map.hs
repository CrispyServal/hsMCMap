{-# LANGUAGE OverloadedStrings #-}
module Game.Minecraft.Map where

import Game.Minecraft.Map.NBT
import Game.Minecraft.Map.Block

import Data.Word
import Data.List
import Data.Maybe
import Data.Bits

data Chunk = Chunk XPos ZPos [[(Word8,Word8)]] deriving (Eq,Show)
type XPos = Int
type ZPos = Int
data ChunkTop = ChunkTop XPos ZPos [(Word8,Word8)] deriving (Eq, Show)

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


-- test for nbt
{--
test f = do
    --[a1] <- getArgs
    exist <- doesFileExist f
    if exist then do
        content <- B.readFile f
        let
            chunks = getChunkRaws content
            nbts = map (\(ChunkRaw _ _ c) -> getNBT c ) chunks
        do
            print $ length chunks
            --writeFile "nbts.debug" (show nbts)
            writeFile "test.out" (show $ ( map (toTopView.buildChunk) nbts) )
	else
	    print $ "empty"
--}