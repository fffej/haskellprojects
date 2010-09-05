-- {-# LANGUAGE BangPatterns #-}
module Em6502 where

-- TODO use cabal!

-- ghci -hide-package monads-fd-0.1.0.1 -Wall Em6502.hs

-- Lots of useful infomration from
-- http://e-tradition.net/bytes/6502/6502cpu.js

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

data CPU = CPU {
      ram :: ByteVector
    , pc :: IORef Word16  -- ^ Program counter
    , yr :: IORef Byte    -- ^ Y Register
    , xr :: IORef Byte    -- ^ X Register
    , sr :: IORef Byte    -- ^ Status Register
    , sp :: IORef Word16  -- ^ Stack Pointer
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
data Instruction = ADC    -- |^  ADd with Carry
                 | AND    -- |^  AND (with accumulator)
                 | ASL    -- |^  Arithmetic Shift Left
                 | BCC    -- |^  Branch on Carry Clear
                 | BCS    -- |^  Branch on Carry Set
                 | BEQ    -- |^  Branch on EQual (zero set)
                 | BIT    -- |^  BIT test
                 | BMI    -- |^  Branch on MInus (negative set)
                 | BNE    -- |^  Branch on Not Equal (zero clear)
                 | BPL    -- |^  Branch on PLus (negative clear)
                 | BRK    -- |^  BReaK (interrupt)
                 | BVC    -- |^  Branch on oVerflow Clear
                 | BVS    -- |^  Branch on oVerflow  Set
                 | CLC    -- |^  CLear Carry
                 | CLD    -- |^  CLear Decimal
                 | CLI    -- |^  CLear Interrupt disable
                 | CLV    -- |^  CLear oVerflow
                 | CMP    -- |^  CoMPare (with accumulator)
                 | CPX    -- |^  ComPare with X
                 | CPY    -- |^  ComPare with Y
                 | DEC    -- |^  DECrement (accumulator)
                 | DEX    -- |^  DEcrement X
                 | DEY    -- |^  DEcrement Y
                 | EOR    -- |^  Exclusive OR (with accumulator)
                 | INC    -- |^  INCrement (accumulator)
                 | INX    -- |^  INcrement X
                 | INY    -- |^  INcrement Y
                 | JMP    -- |^  JuMP
                 | JSR    -- |^  Jump SubRoutine
                 | LDA    -- |^  LoaD Accumulator
                 | LDX    -- |^  LoaD X
                 | LDY    -- |^  LoaD Y
                 | LSR    -- |^  Logical Shift Right
                 | NOP    -- |^  No OPeration
                 | ORA    -- |^  OR with Accumulator
                 | PHA    -- |^  PusH Accumulator
                 | PHP    -- |^  PusH Processor status (SR)
                 | PLA    -- |^  PulL Accumulator
                 | PLP    -- |^  PulL Processor status (SR)
                 | ROL    -- |^  ROtate Left
                 | ROR    -- |^  ROtate Right
                 | RTI    -- |^  ReTurn from Interrupt
                 | RTS    -- |^  ReTurn from Subroutine
                 | SBC    -- |^  SuBtract with Carry
                 | SEC    -- |^  SEt Carry
                 | SED    -- |^  SEt Decimal
                 | SEI    -- |^  SEt Interrupt disable
                 | STA    -- |^  STore Accumulator
                 | STX    -- |^  STore X
                 | STY    -- |^  STore Y
                 | TAX    -- |^  Transfer Accumulator to X
                 | TAY    -- |^  Transfer Accumulator to Y
                 | TSX    -- |^  Transfer Stack pointer to X
                 | TXA    -- |^  Transfer X to Accumulator
                 | TXS    -- |^  Transfer X to Stack pointer
                 | TYA    -- |^  Transfer Y to Accumulator

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

setFlag :: CPU -> Flag -> IO ()
setFlag c f = modifyIORef (sr c) (`setBit` fromIntegral (flag f))

clearFlag :: CPU -> Flag -> IO ()
clearFlag c f = modifyIORef (sr c) (`clearBit` fromIntegral (flag f))

incPC :: CPU -> Word16 -> IO ()
incPC c i = modifyIORef (pc c) (+ i)

stepPC :: CPU -> IO ()
stepPC c = incPC c 1

step2PC :: CPU -> IO ()
step2PC c = incPC c 2

-- TODO readByte and readWord should have an addressing mode
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
  writeByte cpu (sp' + 256) (val .&. 255)
  modifyIORef (sp cpu) (\x -> (x - 1) .&. 255)

stackPopByte :: CPU -> IO Byte
stackPopByte cpu = do
  s <- readIORef (sp cpu)
  val <- readByte cpu (s+256)
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

-- |An unimplemented function.  Should never be called if things are going
-- well!
ini :: CPU -> IO ()
ini _ = return ()
i00   = undefined
i01 c = execute c indirectXAddr ORA >> stepPC c
i05 c = execute c zeroPageAddr ORA >> stepPC c
i06 c = execute c zeroPageAddr ASL >> stepPC c
i08 c = undefined
i09   = undefined
i0a   = undefined
i0d c = execute c absoluteAddr ORA >> step2PC c
i0e c = execute c absoluteAddr ASL >> step2PC c
i10 c = undefined
i11 c = execute c indirectYAddr ORA >> stepPC c
i15 c = execute c zeroPageXAddr ORA >> stepPC c
i16 c = execute c zeroPageXAddr ASL >> stepPC c
i18 c = undefined
i19 c = execute c absoluteYAddr ORA >> step2PC c
i1d c = execute c absoluteXAddr ORA >> step2PC c
i1e c = execute c absoluteXAddr ASL >> step2PC c
i20 c = undefined
i21 c = execute c indirectXAddr AND >> stepPC c
i24 c = execute c zeroPageAddr BIT >> stepPC c
i25 c = execute c zeroPageAddr AND >> stepPC c
i26 c = execute c zeroPageAddr ROL >> stepPC c
i28 c = undefined
i29 c = undefined
i2a c = undefined
i2c c = execute c absoluteAddr BIT >> step2PC c
i2d c = execute c absoluteAddr AND >> step2PC c
i2e c = execute c absoluteAddr ROL >> step2PC c
i30 c = undefined
i31 c = execute c indirectYAddr AND >> stepPC c
i35 c = execute c zeroPageXAddr AND >> stepPC c
i36 c = execute c zeroPageXAddr ROL >> stepPC c
i38 c = undefined
i39 c = execute c absoluteYAddr AND >> step2PC c
i3d c = execute c absoluteXAddr AND >> step2PC c
i3e c = execute c absoluteXAddr ROL >> step2PC c
i40 c = undefined
i41 c = execute c indirectXAddr EOR >> stepPC c
i45 c = execute c zeroPageAddr EOR >> stepPC c
i46 c = execute c zeroPageAddr LSR >> stepPC c
i48 c = undefined
i49 c = undefined
i4a c = undefined
i4c c = undefined
i4d c = execute c absoluteAddr EOR >> step2PC c
i4e c = execute c absoluteAddr LSR >> step2PC c
i50 c = undefined
i51 c = execute c indirectYAddr EOR >> stepPC c
i55 c = execute c zeroPageXAddr EOR >> stepPC c
i56 c = execute c zeroPageXAddr LSR >> stepPC c
i58 c = undefined
i59 c = execute c absoluteYAddr EOR >> step2PC c
i5d c = execute c absoluteXAddr EOR >> step2PC c
i5e c = execute c absoluteXAddr LSR >> step2PC c
i60 c = undefined
i61 c = execute c indirectXAddr ADC >> stepPC c
i65 c = execute c zeroPageAddr ADC >> stepPC c
i66 c = execute c zeroPageAddr ROR >> stepPC c
i68 c = undefined
i69 c = undefined
i6a c = undefined
i6c c = undefined
i6d c = execute c absoluteAddr ADC >> step2PC c
i6e c = execute c absoluteAddr ROR >> step2PC c
i70 c = undefined
i71 c = execute c indirectYAddr ADC >> stepPC c
i75 c = execute c zeroPageXAddr ADC >> stepPC c
i76 c = execute c zeroPageXAddr ROR >> stepPC c
i78 c = undefined
i79 c = execute c absoluteYAddr ADC >> step2PC c
i7d c = execute c absoluteXAddr ADC >> step2PC c
i7e c = execute c absoluteXAddr ROR >> step2PC c
i81 c = execute c indirectXAddr STA >> stepPC c
i84 c = execute c zeroPageAddr STY >> stepPC c
i85 c = execute c zeroPageAddr STA >> stepPC c
i86 c = execute c zeroPageAddr STX >> stepPC c
i88 c = undefined
i8a c = undefined
i8c c = execute c absoluteAddr STY >> step2PC c
i8d c = execute c absoluteAddr STA >> step2PC c
i8e c = execute c absoluteAddr STX >> step2PC c
i90 c = undefined
i91 c = execute c indirectYAddr STA >> stepPC c
i94 c = execute c zeroPageXAddr STY >> stepPC c
i95 c = execute c zeroPageXAddr STA >> stepPC c
i96 c = execute c zeroPageYAddr STX >> stepPC c
i98 c = undefined
i99 c = execute c absoluteYAddr STA >> step2PC c
i9a c = undefined
i9d c = execute c absoluteXAddr STA >> step2PC c
ia0 c = undefined
ia1 c = execute c indirectXAddr LDA >> stepPC c
ia2 c = undefined
ia4 c = execute c zeroPageAddr LDY >> stepPC c
ia5 c = execute c zeroPageAddr LDA >> stepPC c
ia6 c = execute c zeroPageAddr LDX >> stepPC c
ia8 c = undefined
ia9 c = undefined
iaa c = undefined
iac c = execute c absoluteAddr LDY >> step2PC c
iad c = execute c absoluteAddr LDA >> step2PC c
iae c = execute c absoluteAddr LDX >> step2PC c
ib0 c = undefined
ib1 c = execute c indirectYAddr LDA >> stepPC c
ib4 c = execute c zeroPageXAddr LDY >> stepPC c
ib5 c = execute c zeroPageXAddr LDA >> stepPC c
ib6 c = execute c zeroPageYAddr LDX >> stepPC c
ib8 c = undefined
ib9 c = execute c absoluteYAddr LDA >> step2PC c
iba c = undefined
ibc c = execute c absoluteXAddr LDY >> step2PC c
ibd c = execute c absoluteXAddr LDA >> step2PC c
ibe c = execute c absoluteYAddr LDX >> step2PC c
ic0 c = undefined
ic1 c = execute c indirectXAddr CMP >> stepPC c
ic4 c = execute c zeroPageAddr CPY >> stepPC c
ic5 c = execute c zeroPageAddr CMP >> stepPC c
ic6 c = execute c zeroPageAddr DEC >> stepPC c
ic8 c = undefined
ic9 c = undefined
ica c = undefined
icc c = execute c absoluteAddr CPY >> step2PC c
icd c = execute c absoluteAddr CMP >> step2PC c
ice c = execute c absoluteAddr DEC >> step2PC c
id0 c = undefined
id1 c = execute c indirectYAddr CMP >> stepPC c
id5 c = execute c zeroPageXAddr CMP >> stepPC c
id6 c = execute c zeroPageXAddr DEC >> stepPC c
id8 c = undefined
id9 c = execute c absoluteYAddr CMP >> step2PC c
idd c = execute c absoluteXAddr CMP >> step2PC c
ide c = execute c absoluteXAddr DEC >> step2PC c
ie0   = undefined
ie1 c = execute c indirectXAddr SBC >> stepPC c
ie4 c = execute c zeroPageAddr CPX >> stepPC c
ie5 c = execute c zeroPageAddr SBC >> stepPC c
ie6 c = execute c zeroPageAddr INC >> stepPC c 
ie8   = undefined 
ie9   = undefined
iea c = return ()
iec c = execute c absoluteAddr CPX >> step2PC c
ied c = execute c absoluteAddr SBC >> step2PC c
iee c = execute c absoluteAddr INC >> step2PC c 
if0 c = undefined -- execute c Undefined BST -- TODO take a flag
if1 c = execute c indirectYAddr SBC >> stepPC c 
if5 c = execute c zeroPageXAddr SBC >> stepPC c
if6 c = execute c zeroPageXAddr INC >> stepPC c
if8 c = undefined -- execute c Undefined SET -- TODO take a flag
if9 c = execute c absoluteYAddr SBC >> step2PC c 
ifd c = execute c absoluteXAddr SBC >> step2PC c 
ife c = execute c absoluteXAddr INC >> step2PC c

instructionTable :: [CPU -> IO ()]
instructionTable = [i00, i01, ini, ini, ini, i05, i06, ini
                   ,i08, i09, i0a, ini, ini, i0d, i0e, ini
                   ,i10, i11, ini, ini, ini, i15, i16, ini
                   ,i18, i19, ini, ini, ini, i1d, i1e, ini
                   ,i20, i21, ini, ini, i24, i25, i26, ini
                   ,i28, i29, i2a, ini, i2c, i2d, i2e, ini
                   ,i30, i31, ini, ini, ini, i35, i36, ini
                   ,i38, i39, ini, ini, ini, i3d, i3e, ini
                   ,i40, i41, ini, ini, ini, i45, i46, ini
                   ,i48, i49, i4a, ini, i4c, i4d, i4e, ini
                   ,i50, i51, ini, ini, ini, i55, i56, ini
                   ,i58, i59, ini, ini, ini, i5d, i5e, ini
                   ,i60, i61, ini, ini, ini, i65, i66, ini
                   ,i68, i69, i6a, ini, i6c, i6d, i6e, ini
                   ,i70, i71, ini, ini, ini, i75, i76, ini
                   ,i78, i79, ini, ini, ini, i7d, i7e, ini
                   ,ini, i81, ini, ini, i84, i85, i86, ini
                   ,i88, ini, i8a, ini, i8c, i8d, i8e, ini
                   ,i90, i91, ini, ini, i94, i95, i96, ini
                   ,i98, i99, i9a, ini, ini, i9d, ini, ini
                   ,ia0, ia1, ia2, ini, ia4, ia5, ia6, ini
                   ,ia8, ia9, iaa, ini, iac, iad, iae, ini
                   ,ib0, ib1, ini, ini, ib4, ib5, ib6, ini
                   ,ib8, ib9, iba, ini, ibc, ibd, ibe, ini
                   ,ic0, ic1, ini, ini, ic4, ic5, ic6, ini
                   ,ic8, ic9, ica, ini, icc, icd, ice, ini
                   ,id0, id1, ini, ini, ini, id5, id6, ini
                   ,id8, id9, ini, ini, ini, idd, ide, ini
                   ,ie0, ie1, ini, ini, ie4, ie5, ie6, ini
                   ,ie8, ie9, iea, ini, iec, ied, iee, ini
                   ,if0, if1, ini, ini, ini, if5, if6, ini
                   ,if8, if9, ini, ini, ini, ifd, ife, ini]

execute :: CPU -> (CPU -> IO Word16) -> Instruction -> IO ()
execute cpu addressMode ADC = undefined
execute cpu addressMode AND = undefined 
execute cpu addressMode ASL = undefined
execute cpu addressMode BCC = undefined
execute cpu addressMode BCS = undefined
execute cpu addressMode BEQ = undefined
execute cpu addressMode BIT = undefined
execute cpu addressMode BMI = undefined
execute cpu addressMode BNE = undefined
execute cpu addressMode BPL = undefined
execute cpu addressMode BRK = undefined
execute cpu addressMode BVC = undefined
execute cpu addressMode BVS = undefined
execute cpu addressMode CLC = clearFlag cpu Carry 
execute cpu addressMode CLD = clearFlag cpu Decimal
execute cpu addressMode CLI = clearFlag cpu Interrupt
execute cpu addressMode CLV = clearFlag cpu Overflow
execute cpu addressMode CMP = undefined
execute cpu addressMode CPX = undefined
execute cpu addressMode CPY = undefined
execute cpu addressMode DEC = undefined
execute cpu addressMode DEX = undefined
execute cpu addressMode DEY = undefined
execute cpu addressMode EOR = undefined
execute cpu addressMode INC = undefined
execute cpu addressMode INX = undefined
execute cpu addressMode INY = undefined
execute cpu addressMode JMP = undefined
execute cpu addressMode JSR = undefined
execute cpu addressMode LDA = undefined
execute cpu addressMode LDX = undefined
execute cpu addressMode LDY = undefined
execute cpu addressMode LSR = undefined
execute cpu addressMode NOP = undefined
execute cpu addressMode ORA = undefined
execute cpu addressMode PHA = undefined
execute cpu addressMode PHP = undefined
execute cpu addressMode PLA = undefined
execute cpu addressMode PLP = undefined
execute cpu addressMode ROL = undefined
execute cpu addressMode ROR = undefined
execute cpu addressMode RTI = undefined
execute cpu addressMode RTS = undefined
execute cpu addressMode SBC = undefined
execute cpu addressMode SEC = setFlag cpu Carry
execute cpu addressMode SED = setFlag cpu Decimal
execute cpu addressMode SEI = setFlag cpu Interrupt
execute cpu addressMode STA = undefined
execute cpu addressMode STX = store cpu (xr cpu) addressMode
execute cpu addressMode STY = store cpu (yr cpu) addressMode
execute cpu addressMode TAX = undefined
execute cpu addressMode TAY = undefined
execute cpu addressMode TSX = undefined
execute cpu addressMode TXA = undefined
execute cpu addressMode TXS = undefined
execute cpu addressMode TYA = undefined

store :: CPU -> IORef -> (IO -> Word16) -> IO ()
store cpu source address = do
  src <- readIORef source
  addr <- address
  writeByte cpu addr src