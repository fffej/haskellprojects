module Em6502 where

import Data.Word (Word8,Word16)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic.Mutable as GM

type MemAddress = Word16
type Byte = Word8
type ByteVector = M.IOVector Byte

data CPU = CPU {
      ram :: ByteVector
}

readMem :: CPU -> MemAddress -> IO Byte
readMem cpu addr = GM.read (ram cpu) (fromIntegral addr)

writeMem :: CPU -> MemAddress -> Byte -> IO ()
writeMem cpu addr = GM.write (ram cpu) (fromIntegral addr)


data Register = PC -- Program Counter
              | AC -- ACcumulator
              | XR -- X Register
              | YR -- Y Reigster
              | SR -- Status Register
              | SP -- Stack Pointer
               
data StatusRegister = Negative
                    | Overflow
                    | Ignored
                    | Break
                    | Decimal
                    | Interrupt
                    | Zero
                    | Carry

data Instruction = ADC    ---    ADd with Carry
                 | AND    ---    AND (with accumulator)
                 | ASL    ---    Arithmetic Shift Left
                 | BCC    ---    Branch on Carry Clear
                 | BCS    ---    Branch on Carry Set
                 | BEQ    ---    Branch on EQual (zero set)
                 | BIT    ---    BIT test
                 | BMI    ---    Branch on MInus (negative set)
                 | BNE    ---    Branch on Not Equal (zero clear)
                 | BPL    ---    Branch on PLus (negative clear)
                 | BRK    ---    BReaK (interrupt)
                 | BVC    ---    Branch on oVerflow Clear
                 | BVS    ---    Branch on oVerflow  Set
                 | CLC    ---    CLear Carry
                 | CLD    ---    CLear Decimal
                 | CLI    ---    CLear Interrupt disable
                 | CLV    ---    CLear oVerflow
                 | CMP    ---    CoMPare (with accumulator)
                 | CPX    ---    ComPare with X
                 | CPY    ---    ComPare with Y
                 | DEC    ---    DECrement (accumulator)
                 | DEX    ---    DEcrement X
                 | DEY    ---    DEcrement Y
                 | EOR    ---    Exclusive OR (with accumulator)
                 | INC    ---    INCrement (accumulator)
                 | INX    ---    INcrement X
                 | INY    ---    INcrement Y
                 | JMP    ---    JuMP
                 | JSR    ---    Jump SubRoutine
                 | LDA    ---    LoaD Accumulator
                 | LDX    ---    LoaD X
                 | LDY    ---    LoaD Y
                 | LSR    ---    Logical Shift Right
                 | NOP    ---    No OPeration
                 | ORA    ---    OR with Accumulator
                 | PHA    ---    PusH Accumulator
                 | PHP    ---    PusH Processor status (SR)
                 | PLA    ---    PulL Accumulator
                 | PLP    ---    PulL Processor status (SR)
                 | ROL    ---    ROtate Left
                 | ROR    ---    ROtate Right
                 | RTI    ---    ReTurn from Interrupt
                 | RTS    ---    ReTurn from Subroutine
                 | SBC    ---    SuBtract with Carry
                 | SEC    ---    SEt Carry
                 | SED    ---    SEt Decimal
                 | SEI    ---    SEt Interrupt disable
                 | STA    ---    STore Accumulator
                 | STX    ---    STore X
                 | STY    ---    STore Y
                 | TAX    ---    Transfer Accumulator to X
                 | TAY    ---    Transfer Accumulator to Y
                 | TSX    ---    Transfer Stack pointer to X
                 | TXA    ---    Transfer X to Accumulator
                 | TXS    ---    Transfer X to Stack pointer
                 | TYA    ---    Transfer Y to Accumulator

-- |The maximum amount of RAM addressable by a 6502
maxAddress :: Word16
maxAddress = maxBound
