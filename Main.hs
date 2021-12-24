import Numeric (readHex)
import Text.Printf (printf)

main :: IO ()
main = do
  fileContent <- readFile "Input.txt"
  let bools = lineToBoolList $ head $ lines fileContent
  putStrLn $ (++ " bits read") $ show $ length bools
  putStrLn $ show $ bitsToNum [True, True, False]
  putStrLn "The end"

lineToBoolList :: String -> [Bool]
lineToBoolList = map (binToBool.hexToBin)

hexToBin :: Char -> Char
hexToBin c = case readHex [c] of (x,_):_ -> head $ printf "%04b" (x::Int)
      
binToBool :: Char -> Bool
binToBool = (== '1')

bitsToNum :: [Bool] -> Int
bitsToNum = foldl (\sum bit -> fromEnum bit + 2 * sum) 0


consumePacket :: [Bool] -> [Bool]
consumePacket bs = 

readVersionSum :: [Bool] -> Int
readVersionSum (v1:v2:v3:t1:t2:t3:otherBits) 
  = let vs = getVersion [v1, v2, v3]
        pt = getPacketType [t1, t2, t3]
    in 0

getVersion :: [Bool] -> Int
getVersion = bitsToNum

getPacketType :: [Bool] -> PackageType
getPacketType ts = 
  case bitsToNum ts of
    4 -> LiteralType
    _ -> OperatorType


data PackageType = LiteralType | OperatorType

data LengthType = LengthType0 | LengthType1



