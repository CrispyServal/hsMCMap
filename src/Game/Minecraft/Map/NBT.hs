module Game.Minecraft.Map.NBT (
            NBT(..)
          , Content(..)
          , getNBT
          , navigate
        ) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.List
import           Data.Word

import           Control.Monad


type Name = BL.ByteString
data NBT = NBT { nameNBT    :: Name
               , contentNBT :: !Content
               } deriving (Eq,Show)

data Content = TAGEnd                      -- 0
    | TAGByte           Word8              -- 1
    | TAGShort          Int16              -- 2
    | TAGInt            Int32              -- 3
    | TAGLong           Int64              -- 4
    | TAGFloat          Float              -- 5
    | TAGDouble         Double             -- 6
    | TAGByteArray      [Word8]            -- 7
    | TAGString         BL.ByteString      -- 8
    | TAGList           [Content]          -- 9
    | TAGCompound       [NBT]              --10
    | TAGIntArray       [Int32]            --11
    deriving (Eq,Show)

-- ignore tag id at start(must be 10)
getNBT :: BL.ByteString -> NBT
getNBT = runGet (byte >> nbtGet 10)

-- NOTE: no id parsing here. will be in compound and in start(getNBT)
nbtGet :: Word8 -> Get NBT
nbtGet id = do
    lenName <- short
    name    <- BL.pack <$> (replicateM . fromIntegral) lenName byte
    content <- tag id
    return ( NBT name content )

tag :: Word8 -> Get Content
tag  0 = return TAGEnd
tag  1 = TAGByte            <$> byte
tag  2 = TAGShort           <$> short
tag  3 = TAGInt             <$> int
tag  4 = TAGLong            <$> long
tag  5 = TAGFloat           <$> float
tag  6 = TAGDouble          <$> double
tag  7 = do
    len <- int
    TAGByteArray            <$> (replicateM . fromIntegral) len byte
tag  8 = do
    len <- short
    TAGString . BL.pack     <$> (replicateM . fromIntegral) len byte
tag  9 = do
    id <- byte
    len <- int
    TAGList                 <$> (replicateM . fromIntegral)  len (tag id)
tag 10 = TAGCompound        <$> compound
tag 11 = do
    len <- int
    TAGIntArray             <$> (replicateM . fromIntegral) len int


byte    = getWord8
short   = getInt16be
int     = getInt32be
long    = getInt64be
float   = getFloatbe
double  = getDoublebe
compound :: Get [NBT]
compound = do
    id <- byte
    case id of
        0 -> return []
        _ -> (:) <$> nbtGet id <*> compound

navigate :: [BL.ByteString] -> Content -> Maybe Content
navigate xs c = foldl' ( \cc n -> contentNBT <$> ( (findOne n) =<< cc) ) (Just c) xs


findContent :: BL.ByteString -> Content -> [NBT]
findContent name (TAGCompound ns) = f name ns where
    f name ns = [n | n <- ns, nameNBT n == name]
findContent _ _ = []

findOne :: BL.ByteString -> Content -> Maybe NBT
findOne name c = let rs = findContent name c in
                    case length rs of
                        0 -> Nothing
                        _ -> Just ( head rs )
