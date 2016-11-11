module Game.Minecraft.Map.NBT where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.Zlib as Z
import Data.Attoparsec.Binary
import Data.Attoparsec
import Data.Binary.IEEE754
import Data.Word
import Data.List

import Control.Applicative
import Control.Monad

---------------------------
-- NBT
---------------------------

type Name = B.ByteString
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
	| TAG_String		B.ByteString 	-- 8
	| TAG_List			[Content]		-- 9
	| TAG_Compound		[NBT] 			--10
	| TAG_Int_Array		[Word32] 		--11
	deriving (Eq,Show)


-- ignore tag id at start(must be 10)
getNBT :: B.ByteString -> NBT
getNBT bs = let result = parse (byte >> nbtParser 10) (L.toStrict $ Z.decompress $ L.fromStrict bs) in
				case result of
					Done _ nbt 	-> nbt
					_			-> error "parse nbt error"


-- NOTE: no id parsing here. will be in compound and in start(getNBT)
nbtParser :: Word8 -> Parser NBT
nbtParser id = do
	lenName <- short
	--traceM $ ("nbtParser: len: " ++ show lenName ++ "\n") 
	name 	<- B.pack <$> (replicateM . fromIntegral) lenName byte
	--traceM $ ("nbtParser: name: " ++ show name ++ "\n") 
	content <- tag id
	return ( NBT name content )

tag :: Word8 -> Parser Content
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
	TAG_String			<$> B.pack <$> (replicateM . fromIntegral) len byte
tag  9 = do
	id <- byte
	len <- int
	TAG_List			<$> (replicateM . fromIntegral)  len (tag id)
tag 10 = TAG_Compound	<$> compound
tag 11 = do
	len <- int
	TAG_Int_Array 		<$> (replicateM . fromIntegral) len int


byte 	= anyWord8
short 	= anyWord16be
int 	= anyWord32be
long 	= anyWord64be
float 	= wordToFloat <$> anyWord32be
double 	= wordToDouble <$> anyWord64be
compound :: Parser [NBT]
compound = do
	id <- byte
	--traceM $ ("com id: " ++ show id ++ "\n") 
	case id of
		0 -> return []
		_ -> (:) <$> nbtParser id <*> compound
	--if id == 0 then
		--return []
	--else do
		--(:) <$> nbtParser id <*> compound

-- useful functions for reading data NBT
--
nameNBT :: NBT -> B.ByteString
nameNBT (NBT n _) = n

contentNBT :: NBT -> Content
contentNBT (NBT _ c) = c

navigate :: [B.ByteString] -> Content -> Maybe Content
navigate xs c = foldl' (\cc n -> contentNBT <$> ( (findOne n) =<< cc) ) (Just c) xs
    

findContent :: B.ByteString -> Content -> [NBT]
findContent name (TAG_Compound ns) = f name ns where
    f name ns = [n | n <- ns, nameNBT n == name]
findContent _ _ = []

findOne :: B.ByteString -> Content -> Maybe NBT
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