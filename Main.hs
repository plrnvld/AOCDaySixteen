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

parsePacket :: Parser Packet
parsePacket bits 
  = let (version, rem1) = parseVersion bits
        (packetTypeNumber, rem2) = parseBitsToNum 3 rem1
    in case packetTypeNumber of
      4 -> parseLiteralPacket version rem2 
      _ -> parseOperatorPacket version packetTypeNumber rem2

parseLiteralPacket :: Version -> Parser Packet
parseLiteralPacket version bits = let (num, rem1) = parseLiteralNumber bits
                                  in (LiteralPacket version num, rem1)

parseLiteralNumber :: Parser LiteralNumber
parseLiteralNumber bits = let (content, rem1) = parseLiteralBlocks bits
                           in (bitsToNum content, rem1)

parseLiteralBlocks :: Parser Bits
parseLiteralBlocks (b:bs) = let (fourBits, others) = splitAt 4 bs
                            in case b of
                              False -> (fourBits, others)
                              True -> let (otherBits, rem1) = parseLiteralBlocks others
                                      in (fourBits ++ otherBits, rem1)

parseOperatorPacket :: Version -> Int -> Parser Packet
parseOperatorPacket version operatorType bits = (OperatorPacket version operatorType [], bits) -- Continue here ######################

parseVersion :: Parser Version
parseVersion = parseBitsToNum 3

parseBitsToNum :: Int -> Parser Int
parseBitsToNum bitsToParse bits = 
  let (part1, part2) = splitAt bitsToParse bits
  in (bitsToNum part1, part2)

data Packet = LiteralPacket Version LiteralNumber 
  | OperatorPacket Version OperatorType [Packet]

type Version = Int
type LiteralNumber = Int
type OperatorType = Int
type BitsParsed = Int
type Bits = [Bool]
type Parser a = Bits -> (a, Bits)

data PacketType = LiteralType | OperatorType

data LengthType = LengthType0 | LengthType1





