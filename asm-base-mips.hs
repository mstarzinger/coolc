-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Assembler.MIPS (
		MIPSAssembly,
		MIPSDirective,
		zero, v0, v1, a0, a1, a2, a3, gp, sp, fp, ra,
		add, addiu, div_, mul, neg, sub, and_, li, slt, sle, seq_, beq, bne, beqz, j, jal, jalr, jr, la, lb, lw, sb, sw, move, syscall
	) where
import Assembler
import Data.List (intersperse)

-- Specialized assembler types for the MIPS architecture.
type MIPSAssembly = Assembly MIPSInstruction
type MIPSDirective = Directive MIPSInstruction

-- The data type representing a MIPS instruction including the opcode
-- and a list of operand specific to the instruction.
data MIPSInstruction = MIPSInstruction MIPSOpcode [MIPSOperand]

-- The data type representing possible MIPS operands to an instruction.
data MIPSOperand =
	Reg Register				|
	Mem Register Integer		|
	Lbl String					|
	Imm Integer

-- The data type representing possible MIPS opcodes for an instruction.
data MIPSOpcode =
	O_add				|	-- Addition (with overflow)
	O_addiu				|	-- Addition immediate (without overflow)
	O_div				|	-- Divide (with overflow)
	O_mul				|	-- Multiply (without overflow)
	O_neg				|	-- Negate value (with overflow)
	O_sub				|	-- Subtract (with overflow)
	O_and				|	-- AND
	O_li				|	-- Load immediate
	O_slt				|	-- Set less than
	O_sle				|	-- Set less than equal
	O_seq				|	-- Set equal
	O_beq				|	-- Branch on equal
	O_bne				|	-- Branch on not equal
	O_beqz				|	-- Branch on equal zero
	O_j					|	-- Jump
	O_jal				|	-- Jump and link
	O_jalr				|	-- Jump and link register
	O_jr				|	-- Jump register
	O_la				|	-- Load address
	O_lb				|	-- Load byte
	O_lw				|	-- Load word
	O_sb				|	-- Store byte
	O_sw				|	-- Store word
	O_move				|	-- Move
	O_syscall				-- System call
		deriving Show

-- The instructions can be converted into strings to allow easy printing
-- for debugging purposes and to run the result through a third party
-- simulator (e.g. SPIM) or assembler.
instance Show MIPSInstruction where
	show (MIPSInstruction c os) = opa ++ " " ++ ops
		where
			opa = opc ++ take (5 - length opc) (repeat ' ')
			opc = drop 2 (show c)
			ops = concat $ intersperse ", " (map op os)
			op (Reg r) = "$" ++ reg r
			op (Mem r o) = show o ++ "($" ++ reg r ++ ")"
			op (Lbl l) = l
			op (Imm i) = show i
			reg (Register _ s) = s

-- Helper to generate a single MIPS instruction.
emit :: MIPSOpcode -> [MIPSOperand] -> MIPSDirective
emit c os = ins $ MIPSInstruction c os

-- Constants for available MIPS registers.
zero = Register 0 "zero"
at = Register 1 "at"
v0 = Register 2 "v0"
v1 = Register 3 "v1"
a0 = Register 4 "a0"
a1 = Register 5 "a1"
a2 = Register 6 "a2"
a3 = Register 7 "a3"
t0 = Register 8 "t0"
t1 = Register 9 "t1"
-- TODO: Some registers are still missing!
gp = Register 28 "gp"
sp = Register 29 "sp"
fp = Register 30 "fp"
ra = Register 31 "ra"

add :: Register -> Register -> Register -> MIPSDirective
add rd rs rt = emit O_add [Reg rd, Reg rs, Reg rt]

addiu :: Register -> Register -> Integer -> MIPSDirective
addiu rt rs imm = emit O_addiu [Reg rt, Reg rs, Imm imm]

div_ :: Register -> Register -> Register -> MIPSDirective
div_ rd rs rt = emit O_div [Reg rd, Reg rs, Reg rt]

mul :: Register -> Register -> Register -> MIPSDirective
mul rd rs rt = emit O_mul [Reg rd, Reg rs, Reg rt]

neg :: Register -> Register -> MIPSDirective
neg rd rs = emit O_neg [Reg rd, Reg rs]

sub :: Register -> Register -> Register -> MIPSDirective
sub rd rs rt = emit O_sub [Reg rd, Reg rs, Reg rt]

and_ :: Register -> Register -> Register -> MIPSDirective
and_ rd rs rt = emit O_and [Reg rd, Reg rs, Reg rt]

li :: Register -> Integer -> MIPSDirective
li rt imm = emit O_li [Reg rt, Imm imm]

slt :: Register -> Register -> Register -> MIPSDirective
slt rd rs1 rs2 = emit O_slt [Reg rd, Reg rs1, Reg rs2]

sle :: Register -> Register -> Register -> MIPSDirective
sle rd rs1 rs2 = emit O_sle [Reg rd, Reg rs1, Reg rs2]

seq_ :: Register -> Register -> Register -> MIPSDirective
seq_ rd rs1 rs2 = emit O_seq [Reg rd, Reg rs1, Reg rs2]

beq :: Register -> Register -> String -> MIPSDirective
beq rs rt lbl = emit O_beq [Reg rs, Reg rt, Lbl lbl]

bne :: Register -> Register -> String -> MIPSDirective
bne rs rt lbl = emit O_bne [Reg rs, Reg rt, Lbl lbl]

beqz :: Register -> String -> MIPSDirective
beqz rs lbl = emit O_beqz [Reg rs, Lbl lbl]

j :: String -> MIPSDirective
j lbl = emit O_j [Lbl lbl]

jal :: String -> MIPSDirective
jal lbl = emit O_jal [Lbl lbl]

jalr :: Register -> MIPSDirective
jalr rs = emit O_jalr [Reg rs]

jr :: Register -> MIPSDirective
jr rs = emit O_jr [Reg rs]

la :: Register -> String -> MIPSDirective
la rt lbl = emit O_la [Reg rt, Lbl lbl]

lb :: Register -> Register -> Integer -> MIPSDirective
lb rt rs off = emit O_lb [Reg rt, Mem rs off]

lw :: Register -> Register -> Integer -> MIPSDirective
lw rt rs off = emit O_lw [Reg rt, Mem rs off]

sb :: Register -> Register -> Integer -> MIPSDirective
sb rt rs off = emit O_sb [Reg rt, Mem rs off]

sw :: Register -> Register -> Integer -> MIPSDirective
sw rt rs off = emit O_sw [Reg rt, Mem rs off]

move :: Register -> Register -> MIPSDirective
move rd rs = emit O_move [Reg rd, Reg rs]

syscall :: MIPSDirective
syscall = emit O_syscall []
