module Game.Minecraft.Map.NBT where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zlib as Z
import Data.Binary.IEEE754
import Data.Word
import Data.List
import Data.Binary.Get

import Control.Applicative
import Control.Monad

---------------------------
-- NBT
---------------------------

type Name = BL.ByteString
data NBT = NBT Name Content
	deriving (Eq,Show)

data Content = TAG_End						-- 0
	| TAG_Byte			Word8 			-- 1
	| TAG_Short			Word16 			-- 2
	| TAG_Int			Word32 			-- 3
	| TAG_Long			Word64 			-- 4
	| TAG_Float 		Float 			-- 5
	| TAG_Double 		Double 			-- 6
	| TAG_Byte_Array	[Word8] 		-- 7
	| TAG_String		BL.ByteString 	-- 8
	| TAG_List			[Content]		-- 9
	| TAG_Compound		[NBT] 			--10
	| TAG_Int_Array		[Word32] 		--11
	deriving (Eq,Show)


-- ignore tag id at start(must be 10)
getNBT :: BL.ByteString -> NBT
getNBT bs = runGet (byte >> nbtGet 10) (Z.decompress bs)
				--case result of
					--(Right nbt, _)	-> nbt
					--(Left err, _)	-> error err


-- NOTE: no id parsing here. will be in compound and in start(getNBT)
nbtGet :: Word8 -> Get NBT
nbtGet id = do
	lenName <- short
	--traceM $ ("nbtGet: len: " ++ show lenName ++ "\n") 
	name 	<- BL.pack <$> (replicateM . fromIntegral) lenName byte
	--traceM $ ("nbtGet: name: " ++ show name ++ "\n") 
	content <- tag id
	return ( NBT name content )

tag :: Word8 -> Get Content
tag  0 = return TAG_End
tag  1 = TAG_Byte		<$> byte 
tag  2 = TAG_Short		<$> short
tag  3 = TAG_Int		<$> int
tag  4 = TAG_Long		<$> long
tag  5 = TAG_Float		<$> float
tag  6 = TAG_Double		<$> double
tag  7 = do
	len <- int
	TAG_Byte_Array		<$> (replicateM . fromIntegral) len byte
tag  8 = do
	len <- short
	TAG_String			<$> BL.pack <$> (replicateM . fromIntegral) len byte
tag  9 = do
	id <- byte
	len <- int
	TAG_List			<$> (replicateM . fromIntegral)  len (tag id)
tag 10 = TAG_Compound	<$> compound
tag 11 = do
	len <- int
	TAG_Int_Array 		<$> (replicateM . fromIntegral) len int


byte 	= getWord8
short 	= getWord16be
int 	= getWord32be
long 	= getWord64be
float 	= getFloat32be
--float 	= wordToFloat <$> getWord32be
double 	= getFloat64be
--double 	= wordToDouble <$> getWord64be
compound :: Get [NBT]
compound = do
	id <- byte
	--traceM $ ("com id: " ++ show id ++ "\n") 
	case id of
		0 -> return []
		_ -> (:) <$> nbtGet id <*> compound
	--if id == 0 then
		--return []
	--else do
		--(:) <$> nbtGet id <*> compound

-- useful functions for reading data NBT
--
nameNBT :: NBT -> BL.ByteString
nameNBT (NBT n _) = n

contentNBT :: NBT -> Content
contentNBT (NBT _ c) = c

navigate :: [BL.ByteString] -> Content -> Maybe Content
navigate xs c = foldl' (\cc n -> contentNBT <$> ( (findOne n) =<< cc) ) (Just c) xs
    

findContent :: BL.ByteString -> Content -> [NBT]
findContent name (TAG_Compound ns) = f name ns where
    f name ns = [n | n <- ns, nameNBT n == name]
findContent _ _ = []

findOne :: BL.ByteString -> Content -> Maybe NBT
findOne name c = let rs = findContent name c in
                    case ( length rs ) of
                        0   -> Nothing
                        _   -> Just ( head rs )

openList :: Content -> [Content]
openList (TAG_List cs) = cs
openList _ = []

byteArrayContent :: Content -> [Word8]
byteArrayContent (TAG_Byte_Array bs) = bs

intContent :: Content -> Word32
intContent (TAG_Int n) = n