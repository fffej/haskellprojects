-- {-# LANGUAGE BangPatterns #-}
module Em6502 where

-- TODO use cabal!

-- ghci -hide-package monads-fd-0.1.0.1 -Wall Em6502.hs

-- Lots of useful infomration from
-- http://e-tradition.net/bytes/6502/6502cpu.js

import Data.Array
import Data.IORef
import Data.Word (Word8,Word16)
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

import Control.Monad

-- I'm pretty sure that I want to express this better in a state monad
-- import Control.Monad.ST
-- import Control.Monad.State

import Prelude hiding (break)

type Byte = Word8
type ByteVector = M.IOVector Byte

-- http://www.obelisk.demon.co.uk/6502/registers.html
data CPU = CPU {
      ram :: ByteVector
    , pc :: IORef Word16  -- ^ Program counter
    , yr :: IORef Byte    -- ^ Y Register
    , xr :: IORef Byte    -- ^ X Register
    , sr :: IORef Byte    -- ^ Status Register
    , sp :: IORef Byte    -- ^ Stack Pointer
    , ac :: IORef Byte    -- ^ Accumulator
    , cycles :: IORef Int -- ^ Processor cycles
}

data Flag = Negative
          | Overflow
          | Ignored
          | Break
          | Decimal
          | Interrupt
          | Zero
          | Carry

-- http://www.obelisk.demon.co.uk/6502/addressing.html explains addressing modes
data AddressMode = Accumulator      -- ^Operates directly on the accumulator
                 | Immediate Byte   -- ^specifies constant
                 | ZeroPage Byte    -- ^load from memory
                 | ZeroPageX Byte   -- ^load from memory + x register
                 | ZeroPageY Byte   -- ^load from memory + y register
                 | Relative Int     -- ^signed bit relative offset added to program counter
                 | Absolute Word16  -- ^full addressing
                 | AbsoluteX Word16 -- ^16 bit + x register
                 | AbsoluteY Word16 -- ^16 bit + y register
                 | Indirect Word16  -- ^LSB of another 16 bit memory address?
                 | IndexedIndirect Byte
                 | IndirectIndexed Byte
                 | Implicit         -- ^Irrelevant

data Instruction = ADC    -- ^  ADd with Carry
                 | AND    -- ^  AND (with accumulator)
                 | ASL    -- ^  Arithmetic Shift Left
                 | BCC    -- ^  Branch on Carry Clear
                 | BCS    -- ^  Branch on Carry Set
                 | BEQ    -- ^  Branch on EQual (zero set)
                 | BIT    -- ^  BIT test
                 | BMI    -- ^  Branch on MInus (negative set)
                 | BNE    -- ^  Branch on Not Equal (zero clear)
                 | BPL    -- ^  Branch on PLus (negative clear)
                 | BRK    -- ^  BReaK (interrupt)
                 | BVC    -- ^  Branch on oVerflow Clear
                 | BVS    -- ^  Branch on oVerflow  Set
                 | CLC    -- ^  CLear Carry
                 | CLD    -- ^  CLear Decimal
                 | CLI    -- ^  CLear Interrupt disable
                 | CLV    -- ^  CLear oVerflow
                 | CMP    -- ^  CoMPare (with accumulator)
                 | CPX    -- ^  ComPare with X
                 | CPY    -- ^  ComPare with Y
                 | DEC    -- ^  DECrement (accumulator)
                 | DEX    -- ^  DEcrement X
                 | DEY    -- ^  DEcrement Y
                 | EOR    -- ^  Exclusive OR (with accumulator)
                 | INC    -- ^  INCrement (accumulator)
                 | INX    -- ^  INcrement X
                 | INY    -- ^  INcrement Y
                 | JMP    -- ^  JuMP
                 | JSR    -- ^  Jump SubRoutine
                 | LDA    -- ^  LoaD Accumulator
                 | LDX    -- ^  LoaD X
                 | LDY    -- ^  LoaD Y
                 | LSR    -- ^  Logical Shift Right
                 | NOP    -- ^  No OPeration
                 | ORA    -- ^  OR with Accumulator
                 | PHA    -- ^  PusH Accumulator
                 | PHP    -- ^  PusH Processor status (SR)
                 | PLA    -- ^  PulL Accumulator
                 | PLP    -- ^  PulL Processor status (SR)
                 | ROL    -- ^  ROtate Left
                 | ROR    -- ^  ROtate Right
                 | RTI    -- ^  ReTurn from Interrupt
                 | RTS    -- ^  ReTurn from Subroutine
                 | SBC    -- ^  SuBtract with Carry
                 | SEC    -- ^  SEt Carry
                 | SED    -- ^  SEt Decimal
                 | SEI    -- ^  SEt Interrupt disable
                 | STA    -- ^  STore Accumulator
                 | STX    -- ^  STore X
                 | STY    -- ^  STore Y
                 | TAX    -- ^  Transfer Accumulator to X
                 | TAY    -- ^  Transfer Accumulator to Y
                 | TSX    -- ^  Transfer Stack pointer to X
                 | TXA    -- ^  Transfer X to Accumulator
                 | TXS    -- ^  Transfer X to Stack pointer
                 | TYA    -- ^  Transfer Y to Accumulator

-- |The maximum amount of RAM addressable by a 6502
maxAddress :: Word16
maxAddress = maxBound

flag :: Flag -> Word8
flag Negative  = 8
flag Overflow  = 7
flag Ignored   = 6
flag Break     = 5
flag Decimal   = 4
flag Interrupt = 3
flag Zero      = 2
flag Carry     = 1

setFlagValue :: CPU -> Flag -> Bool -> IO ()
setFlagValue c f True = setFlag c f 
setFlagValue c f False = clearFlag c f

setFlag :: CPU -> Flag -> IO ()
setFlag c f = modifyIORef (sr c) (`setBit` fromIntegral (flag f))

setFlags :: CPU -> [Flag] -> IO ()
setFlags c = mapM_ (setFlag c)

clearFlag :: CPU -> Flag -> IO ()
clearFlag c f = modifyIORef (sr c) (`clearBit` fromIntegral (flag f))

clearFlags :: CPU -> [Flag] -> IO ()
clearFlags c = mapM_ (clearFlag c) 

isSet :: CPU -> Flag -> IO Bool
isSet cpu f = do
  sr' <- readIORef (sr cpu)
  return (testBit sr' (fromIntegral $ flag f))

incPC :: CPU -> Word16 -> IO ()
incPC c i = modifyIORef (pc c) (+ i)

stepPC :: CPU -> IO ()
stepPC c = incPC c 1

step2PC :: CPU -> IO ()
step2PC c = incPC c 2

toByte :: Word16 -> Byte
toByte w = fromIntegral (255 .&. w)

readByte :: CPU -> Word16 -> IO Byte
readByte cpu addr = GM.read (ram cpu) (fromIntegral addr)

readWord :: CPU -> Word16 -> IO Word16
readWord cpu addr = do
  byte1 <- readByte cpu addr
  byte2 <- readByte cpu (0xFFFF .&. (addr + 1))
  return $ fromIntegral byte1 + (fromIntegral byte2 * 256)

writeByte :: CPU -> Word16 -> Byte -> IO ()
writeByte cpu addr = GM.write (ram cpu) (fromIntegral addr)

currentByte :: CPU -> IO Byte
currentByte cpu = do
  p <- readIORef (pc cpu)
  readByte cpu p

stackPushByte :: CPU -> Byte -> IO ()
stackPushByte cpu val = do 
  sp' <- readIORef (sp cpu)
  writeByte cpu (fromIntegral sp' + 256) (val .&. 255)
  modifyIORef (sp cpu) (\x -> (x - 1) .&. 255)

stackPopByte :: CPU -> IO Byte
stackPopByte cpu = do
  s <- readIORef (sp cpu)
  val <- readByte cpu (fromIntegral s+256)
  modifyIORef (sp cpu) (\x -> (x + 1) .&. 255)
  return val

stackPushWord :: CPU -> Word16 -> IO ()
stackPushWord cpu x = do
  stackPushByte cpu (fromIntegral (x `shiftR` 8) .&. 0xFF)
  stackPushByte cpu (fromIntegral x .&. 0xFF)

stackPopWord :: CPU -> IO Word16
stackPopWord cpu = do
  byte1 <- stackPopByte cpu
  byte2 <- stackPopByte cpu
  return $ (fromIntegral byte1 :: Word16) + (256 * fromIntegral byte2 :: Word16)

zeroPageAddr :: CPU -> IO Word16
zeroPageAddr cpu = do
  pc' <- readIORef (pc cpu)
  liftM fromIntegral $ readByte cpu pc'

zeroPageXAddr :: CPU -> IO Word16
zeroPageXAddr cpu = do
  pc' <- readIORef (pc cpu)
  b <- readByte cpu pc'
  xr' <- readIORef (xr cpu)
  return $ fromIntegral (255 .&. (xr' + b))

zeroPageYAddr :: CPU -> IO Word16
zeroPageYAddr cpu = do
  pc' <- readIORef (pc cpu)
  b <- readByte cpu pc'
  yr' <- readIORef (yr cpu)
  return $ fromIntegral (255 .&. (yr' + b))

indirectXAddr :: CPU -> IO Word16
indirectXAddr cpu = do
  pc' <- readIORef (pc cpu)
  b <- readByte cpu pc'
  xr' <- readIORef (xr cpu)
  readWord cpu (255 .&. (fromIntegral b + fromIntegral xr'))

indirectYAddr :: CPU -> IO Word16
indirectYAddr cpu = do
  pc' <- readIORef (pc cpu)
  b <- readByte cpu pc'
  yr' <- readIORef (yr cpu)
  readWord cpu ((fromIntegral b + fromIntegral yr') .&. 0xFFFF)

absoluteAddr :: CPU -> IO Word16
absoluteAddr cpu = do
  pc' <- readIORef (pc cpu)
  readWord cpu pc'

absoluteXAddr :: CPU -> IO Word16
absoluteXAddr cpu = do
  pc' <-readIORef (pc cpu)
  w <- readWord cpu pc'
  xr' <- readIORef (xr cpu)
  return (w + fromIntegral xr' .&. 0xFFFF)

absoluteYAddr :: CPU -> IO Word16
absoluteYAddr cpu = do
  pc' <- readIORef (pc cpu)
  w <- readWord cpu pc'
  yr' <- readIORef (yr cpu)
  return (w + fromIntegral yr' .&. 0xFFFF)

branchRelAddr :: CPU -> IO ()
branchRelAddr cpu = do
  address <- currentByte cpu
  pc' <- readIORef (pc cpu)
  let pcOff = if testBit addr 7 then -(1 + (address `xor` 255)) else address
      addr = pc' + fromIntegral pcOff
  writeIORef (pc cpu) (addr .&. 0xFFFF)

dereference :: CPU -> AddressMode -> IO Word16
dereference = undefined

-- |Create a brand new CPU initialized appropriately
init :: IO CPU
init = do
  mem <- GM.newWith (fromIntegral (maxBound :: Word16)) 0
  pc' <- newIORef 0
  yr' <- newIORef 0
  xr' <- newIORef 0
  sr' <- newIORef $ flag Ignored
  sp' <- newIORef 255
  ac' <- newIORef 0
  cycles' <- newIORef 0
  break' <- newIORef False
  return CPU { 
            ram = mem
          , pc = pc'
          , yr = yr'
          , xr = xr'
          , sr = sr'
          , sp = sp'
          , ac = ac'
          , cycles = cycles'
          }

execute :: CPU -> AddressMode -> Instruction -> IO ()
execute cpu addressMode ADC = adcOp cpu addressMode 
execute cpu addressMode AND = bitWiseOp cpu addressMode (.&.)
execute cpu addressMode ASL = shiftLeft cpu addressMode
execute cpu addressMode BCC = branchIf cpu Carry False
execute cpu addressMode BCS = branchIf cpu Carry True
execute cpu addressMode BEQ = branchIf cpu Zero True
execute cpu addressMode BIT = bitTest cpu addressMode
execute cpu addressMode BMI = branchIf cpu Negative True
execute cpu addressMode BNE = branchIf cpu Zero False
execute cpu addressMode BPL = branchIf cpu Negative False
execute cpu addressMode BRK = undefined
execute cpu addressMode BVC = branchIf cpu Overflow False
execute cpu addressMode BVS = branchIf cpu Overflow True
execute cpu addressMode CLC = clearFlag cpu Carry 
execute cpu addressMode CLD = clearFlag cpu Decimal
execute cpu addressMode CLI = clearFlag cpu Interrupt
execute cpu addressMode CLV = clearFlag cpu Overflow
execute cpu addressMode CMP = comp cpu addressMode (ac cpu)
execute cpu addressMode CPX = comp cpu addressMode (xr cpu)
execute cpu addressMode CPY = comp cpu addressMode (yr cpu)
execute cpu addressMode DEC = undefined
execute cpu addressMode DEX = undefined
execute cpu addressMode DEY = undefined
execute cpu addressMode EOR = bitWiseOp cpu addressMode xor
execute cpu addressMode INC = undefined
execute cpu addressMode INX = undefined
execute cpu addressMode INY = undefined
execute cpu addressMode JMP = undefined
execute cpu addressMode JSR = undefined
execute cpu addressMode LDA = load cpu (ac cpu) addressMode
execute cpu addressMode LDX = load cpu (xr cpu) addressMode
execute cpu addressMode LDY = load cpu (yr cpu) addressMode
execute cpu addressMode LSR = undefined
execute cpu addressMode NOP = undefined
execute cpu addressMode ORA = bitWiseOp cpu addressMode (.|.)
execute cpu addressMode PHA = pushRef cpu (ac cpu)
execute cpu addressMode PHP = pushRef cpu (sr cpu)
execute cpu addressMode PLA = pullRef cpu (ac cpu) True
execute cpu addressMode PLP = pullRef cpu (sr cpu) False
execute cpu addressMode ROL = undefined
execute cpu addressMode ROR = undefined
execute cpu addressMode RTI = undefined
execute cpu addressMode RTS = undefined
execute cpu addressMode SBC = sbcOp cpu addressMode
execute cpu addressMode SEC = setFlag cpu Carry
execute cpu addressMode SED = setFlag cpu Decimal
execute cpu addressMode SEI = setFlag cpu Interrupt
execute cpu addressMode STA = store cpu (ac cpu) addressMode
execute cpu addressMode STX = store cpu (xr cpu) addressMode
execute cpu addressMode STY = store cpu (yr cpu) addressMode
execute cpu addressMode TAX = transferToAccumulator cpu (xr cpu) 
execute cpu addressMode TAY = transferToAccumulator cpu (yr cpu)
execute cpu addressMode TSX = copyRegister cpu (sp cpu) (xr cpu) True
execute cpu addressMode TXA = copyRegister cpu (xr cpu) (ac cpu) True
execute cpu addressMode TXS = copyRegister cpu (xr cpu) (sp cpu) False
execute cpu addressMode TYA = copyRegister cpu (yr cpu) (ac cpu) True

shiftLeft :: CPU -> AddressMode -> IO ()
shiftLeft cpu address = do
  byte <- dereference cpu address
  clearFlags cpu [Carry,Negative,Zero]
  when (testBit (byte .&. 255) 7) (setFlag cpu Carry)
  let shf = shiftL (fromIntegral byte) 1
  if shf == 0 
     then setFlag cpu Zero
     else setFlagValue cpu Overflow (testBit (byte .&. 255) 7)
  writeByte cpu byte shf

adcOp :: CPU -> AddressMode -> IO ()
adcOp cpu address = do
  status <- readIORef (sr cpu)
  byte <- dereference cpu address
  acc <- readIORef (ac cpu)
  isDecimalMode <- isSet cpu Decimal
  isCarry <- isSet cpu Carry
  let carry = if isCarry then 0 else 1
  case isDecimalMode of
    True -> do
      let d = (bcd2dec ! (fromIntegral acc)) + (bcd2dec ! (fromIntegral byte)) + carry
      clearFlags cpu [Carry,Zero,Negative,Overflow]
      when (d>99) (setFlags cpu [Overflow,Carry])
      when (d==0) (setFlag cpu Zero)
      when (d <0) (setFlagValue cpu Zero $ testBit (d .&. 255) (fromIntegral $ flag Zero))
      writeIORef (ac cpu) ((fromIntegral d .&. 255) - if d > 99 then 100 else 0)
    False -> do
      let d = (fromIntegral acc) + byte + (if isCarry then 1 else 0)
      when (d > 255) (setFlags cpu [Carry,Overflow])
      when (d == 0 ) (setFlag cpu Zero)
      setFlagValue cpu Overflow $ testBit (d .&. 255) (fromIntegral $ flag Overflow)
      writeIORef (ac cpu) (fromIntegral d .&. 255)

sbcOp :: CPU -> AddressMode -> IO ()
sbcOp cpu address = do
  status <- readIORef (sr cpu)
  byte <- dereference cpu address
  acc <- readIORef (ac cpu)
  isDecimalMode <- isSet cpu Decimal
  isCarry <- isSet cpu Carry
  let carry = if isCarry then 0 else 1
  case isDecimalMode of
    True -> do
      let d = (bcd2dec ! (fromIntegral acc)) - (bcd2dec ! (fromIntegral byte)) - carry
      clearFlags cpu [Carry,Zero,Negative,Overflow]
      when (d==0) (setFlags cpu [Zero,Carry])
      when (d >0) (setFlag cpu Carry)
      when (d <0) (setFlag cpu Negative)
      writeIORef (ac cpu) ((fromIntegral d .&. 255) + if (d < 0) then 100 else 0)
    False -> do
      let d = (fromIntegral acc) - byte - carry
      clearFlags cpu [Carry,Zero,Negative,Overflow]
      when (d==0) (setFlags cpu [Zero,Carry])
      when (d >0) (setFlag cpu Carry)
      when (d <0) (setFlag cpu Overflow)
      setFlagValue cpu Overflow $ testBit (d .&. 255) (fromIntegral $ flag Overflow)
      writeIORef (ac cpu) (fromIntegral d .&. 255)

bitTest :: CPU -> AddressMode -> IO ()
bitTest cpu address = do
  this <- dereference cpu address
  clearFlags cpu [Carry,Zero,Negative]
  ac' <- readIORef (ac cpu)
  let res = ac' .&. (fromIntegral this .&. 255)
  when (res == 0) $ setFlag cpu Zero
  setFlagValue cpu Overflow $ testBit res (fromIntegral $ flag Overflow)
  setFlagValue cpu Negative $ testBit res (fromIntegral $ flag Negative)

comp :: CPU -> AddressMode -> IORef Byte -> IO ()
comp cpu address src = do
  that <- dereference cpu address
  this <- readIORef src
  clearFlags cpu [Carry,Zero,Negative]
  case (compare this (fromIntegral $ that .&. 255)) of
    EQ -> setFlags cpu [Carry,Zero]
    GT -> setFlag cpu Carry
    LT -> setFlag cpu Negative

branchIf :: CPU -> Flag -> Bool -> IO ()
branchIf cpu flag val = do
  f <- isSet cpu flag
  if f == val then branchRelAddr cpu else stepPC cpu

copyRegister :: CPU -> IORef Byte -> IORef Byte -> Bool -> IO ()
copyRegister cpu src dest updateFlags = do
  byte <- readIORef src
  writeIORef dest byte
  when updateFlags $ setZeroNegativeFlags cpu byte

store :: CPU -> IORef Byte -> AddressMode -> IO ()
store cpu source address = do
  src <- readIORef source
  addr <- dereference cpu address
  writeByte cpu addr src

load :: CPU -> IORef Byte -> AddressMode -> IO ()
load cpu destination address = do
  addr <- dereference cpu address
  byte <- readByte cpu addr
  writeIORef destination byte
  setZeroNegativeFlags cpu byte

bitWiseOp :: CPU -> AddressMode -> (Byte -> Byte -> Byte) -> IO ()
bitWiseOp cpu byte op = do
  b <- dereference cpu byte 
  modifyIORef (ac cpu) (\x -> fromIntegral $ op (fromIntegral b) x)
  result <- readIORef (ac cpu)
  setZeroNegativeFlags cpu result

pushRef :: CPU -> IORef Byte -> IO ()
pushRef cpu src = do
  val <- readIORef src
  stackPushByte cpu val

pullRef :: CPU -> IORef Byte -> Bool -> IO ()
pullRef cpu src setFlags = do
  val <- stackPopByte cpu
  writeIORef src val
  when setFlags (setZeroNegativeFlags cpu val)

transferToAccumulator :: CPU -> IORef Byte -> IO ()
transferToAccumulator cpu dest = do
  val <- readIORef dest
  writeIORef (ac cpu) val
  setZeroNegativeFlags cpu val

setZeroNegativeFlags :: CPU -> Byte -> IO ()
setZeroNegativeFlags cpu b = do
  clearFlags cpu [Zero,Negative]
  if b == 0 then setFlag cpu Zero else when (testBit b 7) (setFlag cpu Negative)
      
bcd2dec :: Array Byte Word16
bcd2dec= listArray (0,255) [0,  1,  2,  3,   4,  5, 6,  7, 8,    9, 10, 11, 12, 13, 14, 15  -- 0x00
	                   ,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25  -- 0x10
	                   ,20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35  -- 0x20
	                   ,30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45  -- 0x30
	                   ,40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55  -- 0x40
	                   ,50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65  -- 0x50
	                   ,60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75  -- 0x60
	                   ,70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85  -- 0x70
	                   ,80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95  -- 0x80
	                   ,90, 91, 92, 93, 94, 95, 96, 97, 98, 99,100,101,102,103,104,105  -- 0x90
	                   ,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115 -- 0xA0
	                   ,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125 -- 0xB0
	                   ,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135 -- 0xC0
	                   ,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145 -- 0xD0
	                   ,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155 -- 0xE0
	                   ,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165]-- 0xF0

dec2bcd :: Array Byte Byte
dec2bcd = listArray (0,255) [0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09
	                    ,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19
	                    ,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29
	                    ,0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39
	                    ,0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49
	                    ,0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59
	                    ,0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69
	                    ,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79
	                    ,0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89
	                    ,0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99]

