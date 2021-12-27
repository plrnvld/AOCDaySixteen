import Numeric (readHex)
import Text.Printf (printf)

main :: IO ()
main = do
  fileContent <- readFile "Input.txt"
  
  let hex = head $ lines fileContent
      bits = lineToBoolList hex
      parsed = fst $ parsePacket bits
      versionSum = sumVersions parsed
      totalValue = getValue parsed
      
  putStrLn $ "Bits: " ++ show (map (\c -> if c then '1' else '0') bits)
  putStrLn $ (++ " bits read") $ show $ length bits
  putStrLn $ "Version sum: " ++ show versionSum
  putStrLn $ "Value: " ++ show totalValue


lineToBoolList :: String -> [Bool]
lineToBoolList hex = map binToBool $ concat $ map hexToBin hex

hexToBin :: Char -> String
hexToBin c = case readHex [c] of (x,_):_ -> printf "%04b" (x::Int)
      
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
parseOperatorPacket version operatorType [] = (ErrorPacket "No bits left", []) 
parseOperatorPacket version operatorType (b:bs) = 
  case b of 
    False -> parseOperatorPacketWithNumBits version operatorType bs
    True -> parseOperatorPacketWithNumPackets version operatorType bs

parseOperatorPacketWithNumBits :: Version -> OperatorType -> Parser Packet
parseOperatorPacketWithNumBits version operatorType bits 
  = let (firstFifteen, rem1) = splitAt 15 bits
        numBits = bitsToNum firstFifteen
        (subPackets, rem2) = parsePacketsWithNumBits numBits rem1
    in (OperatorPacket version (getOperator operatorType) subPackets, rem2)

parsePacketsWithNumBits :: Int -> Parser [Packet]
parsePacketsWithNumBits x bits = 
  if x == 0
  then ([], bits)
  else let (packet, rem1) = parsePacket bits
           (packets, rem2) = parsePacketsWithNumBits (x - (length bits - length rem1)) rem1
           in (packet:packets, rem2)

parseOperatorPacketWithNumPackets :: Version -> OperatorType -> Parser Packet
parseOperatorPacketWithNumPackets version operatorType bits 
  = let (firstEleven, rem1) = splitAt 11 bits
        numPackets = bitsToNum firstEleven
    in (OperatorPacket version (getOperator operatorType) [], rem1)

getOperator :: Int -> [Int] -> Int
getOperator 0 ns = sum ns
getOperator 1 ns = product ns
getOperator 2 ns = minimum ns
getOperator 3 ns = maximum ns
getOperator 5 [item1, item2] = if item1 > item2 then 1 else 0
getOperator 6 [item1, item2] = if item1 < item2 then 1 else 0
getOperator 7 [item1, item2] = if item1 == item2 then 1 else 0
getOperator ot list = error $ "ot = " ++ show ot ++ " and list = " ++ show list



parsePacketsWithNumPackets :: Int -> Parser [Packet]
parsePacketsWithNumPackets num bits = 
  if num == 0 
  then ([], bits)
  else let (packet, rem1) = parsePacket bits
           (packets, rem2) = parsePacketsWithNumPackets (num - 1) rem1
       in (packet:packets, rem2)

parseVersion :: Parser Version
parseVersion = parseBitsToNum 3

parseBitsToNum :: Int -> Parser Int
parseBitsToNum bitsToParse bits = 
  let (part1, part2) = splitAt bitsToParse bits
  in (bitsToNum part1, part2)


sumVersions :: Packet -> Int
sumVersions p = case p of 
  LiteralPacket v n -> v
  OperatorPacket v o ps -> v + sum (map sumVersions ps)


getValue :: Packet -> Int
getValue p = case p of 
  LiteralPacket v n -> n
  OperatorPacket v o ps -> o $ map getValue ps

data Packet = LiteralPacket Version LiteralNumber 
  | OperatorPacket Version Operator [Packet] 
  | ErrorPacket String 

type Version = Int
type LiteralNumber = Int
type OperatorType = Int
type Bits = [Bool]
type Parser a = Bits -> (a, Bits)
type Operator = [Int] -> Int

data PacketType = LiteralType | OperatorType

data LengthType = LengthType0 | LengthType1




