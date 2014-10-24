import Data.Word
import Tokenizer
import Evaluation
import Expression
import Data.Bits
import Data.List
import Macros
import Numeric

data Label = Label String Int deriving(Show)

decomposeU16 :: Word16 -> (Word8, Word8)
decomposeU16 v = (fromIntegral (v .&. 0xFF), fromIntegral (shiftR (v .&. 0xFF00) 8))

implied :: String -> [Word8]
implied "asl" = [0x0A]
implied "brk" = [0x00]
implied "clc" = [0x18]
implied "cld" = [0xD8]
implied "cli" = [0x58]
implied "clv" = [0xB8]
implied "dex" = [0xCA]
implied "dey" = [0x88]
implied "inx" = [0xE8]
implied "iny" = [0xC8]
implied "nop" = [0xEA]
implied "pha" = [0x48]
implied "php" = [0x08]
implied "pla" = [0x68]
implied "plp" = [0x28]
implied "rol" = [0x2A]
implied "ror" = [0x6A]
implied "rti" = [0x40]
implied "rts" = [0x60]
implied "sec" = [0x38]
implied "sed" = [0xF8]
implied "sei" = [0x78]
implied "tax" = [0xAA]
implied "tay" = [0xA8]
implied "tsx" = [0xBA]
implied "txa" = [0x8A]
implied "txs" = [0x9A]
implied "tya" = [0x98]

immediate :: String -> Word8 -> [Word8]
immediate "adc" v = [0x69, v]
immediate "and" v = [0x29, v]
immediate "cmp" v = [0xC9, v]
immediate "cpx" v = [0xE0, v]
immediate "cpy" v = [0xC0, v]
immediate "eor" v = [0x49, v]
immediate "lda" v = [0xA9, v]
immediate "ldx" v = [0xA2, v]
immediate "ldy" v = [0xA0, v]
immediate "ora" v = [0x09, v]
immediate "sbc" v = [0xE9, v]

zeropage :: String -> Word8 -> [Word8]
zeropage "adc" v = [0x65, v]
zeropage "and" v = [0x25, v]
zeropage "asl" v = [0x06, v]
zeropage "bit" v = [0x24, v]
zeropage "cmp" v = [0xC5, v]
zeropage "cpx" v = [0xE4, v]
zeropage "cpy" v = [0xC4, v]
zeropage "dec" v = [0xC6, v]
zeropage "eor" v = [0x45, v]
zeropage "inc" v = [0xE6, v]
zeropage "lda" v = [0xA5, v]
zeropage "ldx" v = [0xA6, v]
zeropage "ldy" v = [0xA4, v]
zeropage "lsr" v = [0x46, v]
zeropage "ora" v = [0x05, v]
zeropage "rol" v = [0x26, v]
zeropage "ror" v = [0x66, v]
zeropage "sbc" v = [0xE5, v]
zeropage "sta" v = [0x85, v]
zeropage "stx" v = [0x86, v]
zeropage "sty" v = [0x84, v]

zeropageX :: String -> Word8 -> [Word8]
zeropageX "adc" v = [0x75, v]
zeropageX "and" v = [0x35, v]
zeropageX "asl" v = [0x16, v]
zeropageX "cmp" v = [0xD5, v]
zeropageX "dec" v = [0xD6, v]
zeropageX "eor" v = [0x55, v]
zeropageX "inc" v = [0xF6, v]
zeropageX "lda" v = [0xB5, v]
zeropageX "ldy" v = [0xB4, v]
zeropageX "lsr" v = [0x56, v]
zeropageX "ora" v = [0x15, v]
zeropageX "rol" v = [0x36, v]
zeropageX "ror" v = [0x76, v]
zeropageX "sbc" v = [0xF5, v]
zeropageX "sta" v = [0x95, v]
zeropageX "sty" v = [0x94, v]

zeropageY :: String -> Word8 -> [Word8]
zeropageY "ldx" v = [0xB6, v]
zeropageY "stx" v = [0x96, v]

absolute :: String -> Word16 -> [Word8]
absolute i v = absoluteV i
  where
    (l, h) = decomposeU16 v
    absoluteV "adc" = [0x6D, l, h]
    absoluteV "and" = [0x2D, l, h]
    absoluteV "asl" = [0x0E, l, h]
    absoluteV "bit" = [0x2C, l, h]
    absoluteV "cmp" = [0xCD, l, h]
    absoluteV "cpx" = [0xEC, l, h]
    absoluteV "cpy" = [0xCC, l, h]
    absoluteV "dec" = [0xCE, l, h]
    absoluteV "eor" = [0x4D, l, h]
    absoluteV "inc" = [0xEE, l, h]
    absoluteV "jmp" = [0x4C, l, h]
    absoluteV "jsr" = [0x20, l, h]
    absoluteV "lda" = [0xAD, l, h]
    absoluteV "ldx" = [0xAE, l, h]
    absoluteV "ldy" = [0xAC, l, h]
    absoluteV "ora" = [0x0D, l, h]
    absoluteV "rol" = [0x2E, l, h]
    absoluteV "ror" = [0x6E, l, h]
    absoluteV "sbc" = [0xED, l, h]
    absoluteV "sta" = [0x8D, l, h]
    absoluteV "stx" = [0x8E, l, h]
    absoluteV "sty" = [0x8C, l, h]

absoluteX :: String -> Word16 -> [Word8]
absoluteX i v = absoluteV i
  where
    (l, h) = decomposeU16 v
    absoluteV "adc" = [0x7D, l, h]
    absoluteV "and" = [0x3D, l, h]
    absoluteV "asl" = [0x1E, l, h]
    absoluteV "cmp" = [0xDD, l, h]
    absoluteV "dec" = [0xDE, l, h]
    absoluteV "eor" = [0x5D, l, h]
    absoluteV "inc" = [0xFE, l, h]
    absoluteV "lda" = [0xBD, l, h]
    absoluteV "ldy" = [0xBC, l, h]
    absoluteV "lsr" = [0x5E, l, h]
    absoluteV "ora" = [0x1D, l, h]
    absoluteV "rol" = [0x3E, l, h]
    absoluteV "ror" = [0x7E, l, h]
    absoluteV "sbc" = [0xFD, l, h]
    absoluteV "sta" = [0x9D, l, h]

absoluteY :: String -> Word16 -> [Word8]
absoluteY i v = absoluteV i
  where
    (l, h) = decomposeU16 v
    absoluteV "adc" = [0x79, l, h]
    absoluteV "and" = [0x39, l, h]
    absoluteV "cmp" = [0xD9, l, h]
    absoluteV "eor" = [0x59, l, h]
    absoluteV "lda" = [0xB9, l, h]
    absoluteV "ldx" = [0xBE, l, h]
    absoluteV "ora" = [0x19, l, h]
    absoluteV "sbc" = [0xF9, l, h]
    absoluteV "sta" = [0x99, l, h]

indexedIndirect :: String -> Word8 -> [Word8]
indexedIndirect "adc" v = [0x61, v]
indexedIndirect "and" v = [0x21, v]
indexedIndirect "cmp" v = [0xC1, v]
indexedIndirect "eor" v = [0x41, v]
indexedIndirect "lda" v = [0xA1, v]
indexedIndirect "ora" v = [0x01, v]
indexedIndirect "sbc" v = [0xE1, v]
indexedIndirect "sta" v = [0x81, v]

indirectIndexed :: String -> Word8 -> [Word8]
indirectIndexed "adc" v = [0x71, v]
indirectIndexed "and" v = [0x31, v]
indirectIndexed "cmp" v = [0xD1, v]
indirectIndexed "eor" v = [0x51, v]
indirectIndexed "lda" v = [0xB1, v]
indirectIndexed "ora" v = [0x11, v]
indirectIndexed "sbc" v = [0xF1, v]
indirectIndexed "sta" v = [0x91, v]

indirect :: String -> Word16 -> [Word8]
indirect i v = indirectV i
  where
    (l, h) = decomposeU16 v
    indirectV "jmp" = [0x6C, l, h]


relative :: String -> Word8 -> [Word8]
relative "bcc" v = [0x90, v]
relative "bcs" v = [0xB0, v]
relative "beq" v = [0xF0, v]
relative "bmi" v = [0x30, v]
relative "bne" v = [0xD0, v]
relative "bpl" v = [0x10, v]
relative "bvc" v = [0x50, v]
relative "bvs" v = [0x70, v]

instructionImplied i = (1, implied i)
instructionLiteral i v = (2, immediate i (fromIntegral v))

instructionAddress :: String -> Int -> (Int, [Word8])
instructionAddress i v
  | v <= 255 = (2, zeropage i (fromIntegral v))
  | v > 255  = (3, absolute i (fromIntegral v))
instructionAddressX i v
  | v <= 255 = (2, zeropageX i (fromIntegral v))
  | v > 255 = (3, absoluteX i (fromIntegral v))
instructionAddressY i v
  | v <= 255 = (2, zeropageY i (fromIntegral v))
  | v > 255 = (3, absoluteY i (fromIntegral v))

instructionLabel :: Int -> String -> Label -> (Int, [Word8])
instructionLabel addr i label
  | i == "bcc" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bcc")
  | i == "bcs" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bcs")
  | i == "beq" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from beq")
  | i == "bmi" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bmi")
  | i == "bne" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bne")
  | i == "bpl" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bpl")
  | i == "bvc" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bvc")
  | i == "bvs" = if (rel > -128 && rel < 127) then (2, relative i (fromIntegral rel)) else error ("Relative branch " ++ labelName ++ " is out of range from bvs")
  | otherwise = (3, absolute i (fromIntegral address))
  where
    (Label labelName address) = label
    rel = address - addr - 2

instructionIndirect i v = (3, indirect i (fromIntegral v))
instructionIndirectIndexed i v = (2, indirectIndexed i (fromIntegral v))
instructionIndexedIndirect i v = (2, indexedIndirect i (fromIntegral v))

findLabels :: Int -> [Token] -> [Label]
findLabels addr ((TokenLabel v):r) = Label v addr : findLabels addr r
findLabels addr ((TokenSymbol i):(TokenLiteral v):r) = findLabels (addr + increment) r where (increment, result) = instructionLiteral i v 
findLabels addr ((TokenSymbol i):(TokenSymbol v):r) = findLabels (addr + increment) r where (increment, result) = instructionLabel addr i (Label "__dummy__" addr)
findLabels addr ((TokenSymbol i):(TokenAddress v):r) = findLabels (addr + increment) r where (increment, result) = instructionAddress i v 
findLabels addr ((TokenSymbol i):(TokenAddressX v):r) = findLabels (addr + increment) r where (increment, result) = instructionAddressX i v
findLabels addr ((TokenSymbol i):(TokenAddressY v):r) = findLabels (addr + increment) r where (increment, result) = instructionAddressY i v 
findLabels addr ((TokenSymbol i):(TokenIndirect v):r) = findLabels (addr + increment) r where (increment, result) = instructionIndirect i v 
findLabels addr ((TokenSymbol i):(TokenIndirectIndexed v):r) = findLabels (addr + increment) r where (increment, result) = instructionIndirectIndexed i v 
findLabels addr ((TokenSymbol i):(TokenIndexedIndirect v):r) = findLabels (addr + increment) r where (increment, result) = instructionIndexedIndirect i v 
findLabels addr ((TokenSymbol i):r) = findLabels (addr + increment) r where (increment, result) = instructionImplied i
findLabels addr ((TokenPragma "org"):(TokenAddress x):r) = findLabels x r
findLabels addr (v:[]) = error ("Unexpected token: " ++ (show v))
findLabels addr [] = []
findLabels addr x = error ("Something went wrong" ++ (show x))

resolveLabel :: [Label] -> String -> Label
resolveLabel labels name = checkLabel (find (\(Label n _) -> n == name) labels)
  where
    checkLabel (Just x) = x
    checkLabel Nothing = error ("Cannot resolve label: " ++ name)

assembleTokens :: [Label] -> Int -> [Token] -> [Word8]
assembleTokens labels addr ((TokenLabel v):r) = assembleTokens labels addr r
assembleTokens labels addr ((TokenSymbol i):(TokenLiteral v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionLiteral i v 
assembleTokens labels addr ((TokenSymbol i):(TokenSymbol v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionLabel addr i (resolveLabel labels v)
assembleTokens labels addr ((TokenSymbol i):(TokenAddress v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionAddress i v 
assembleTokens labels addr ((TokenSymbol i):(TokenAddressX v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionAddressX i v
assembleTokens labels addr ((TokenSymbol i):(TokenAddressY v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionAddressY i v 
assembleTokens labels addr ((TokenSymbol i):(TokenIndirect v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionIndirect i v 
assembleTokens labels addr ((TokenSymbol i):(TokenIndirectIndexed v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionIndirectIndexed i v 
assembleTokens labels addr ((TokenSymbol i):(TokenIndexedIndirect v):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionIndexedIndirect i v 
assembleTokens labels addr ((TokenSymbol i):r) = result ++ assembleTokens labels (addr + increment) r where (increment, result) = instructionImplied i
assembleTokens labels addr ((TokenPragma "org"):(TokenAddress x):r) = assembleTokens labels x r
assembleTokens labels addr (v:[]) = error ("Unexpected token: " ++ (show v))
assembleTokens labels addr [] = []

assemble :: String -> [Word8]
assemble c = assembleTokens labels 0x0 expandedCode
  where
    code = tokenize c
    expandedCode = expandMacros code
    labels = findLabels 0x0 expandedCode

testcode = ".org $600 jerry: lda #32 bne jerry"
testexpanded = expandMacros $ tokenize testcode
test = map (\n -> showHex n "") $ assemble testcode