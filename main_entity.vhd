use WORK.cpu_defs_pack.all;
use WORK.mem_defs_pack.all;
use WORK.arithm_defs_pack.all;
use std.textio.all;


entity MaxCPU is
end MaxCPU;

architecture functional of MaxCPU is
begin
	process
		file TraceFile : Text is out ?Trace?;
		file DumpFile : Text is out ?Dump?;
		file MemoryFile : Text is in ?Memory?;
		file IOInputFile : Text is in ?IOInput?;
		file IOOutputFile: Text is out ?IOOutput?;
		variable l : line;

		--variable Data  : data_type;
		variable Instr : data_type; -- bedeutet, wir werden Befehle benutzen
		variable OP    : data_type; -- und auch Verfahren
		variable Memory: mem_type := (
			0    => code_nop*(2**reg_addr_width)**3,
			1     => code_stop*(2**reg_addr_width)**3,
			others => 0
		);
		variable PC : addr_type := 0;
		variable X,Y,Z : reg_addr_type; -- die Variablen, auf denen Befehle gemacht werden

		-- In order to store particular results of some operations
		-- For details see pdf spec
		variable Zero, Carry, Negative, Overflow: Boolean := FALSE;

		-- further objects
		begin

		init_memory (MemoryFile, Memory);

		-- cmd fetch - Fetch the first instruction from the memory
		-- Format: OOOOOO XX YY ZZ
		-- Instr := O * 64 + X * 16 + Y * 4 + Z; 
		-- Obs: reg_addr_width = 2

		Instr := Memory(PC); 
		OP := Instr / (2**reg_addr_width)**3;
		X := (Instr / (2**reg_addr_width)**2) mod 2**reg_addr_width; -- zweiter Teil (2 st. E.)
		Y := (Instr / 2**reg_addr_width) mod 2**reg_addr_width; -- dritter Teil (2 st. E.)
		Z := Instr mod 2**reg_addr_width; -- vierter Teil (2 st. E.)

		-- Points to the memory address of the next instruction
		-- PC := PC + 1 would cause an overflow when the highest address is reached
		-- INC returns when when end position is reached
		PC := INC(PC); 

		-- cmd decode
		case OP is

			------------------------------------------------- MISCELLANEOUS

			when code_nop  => null;
			when code_stop => dump_memory( Memory, DumpFile ); wait;

			------------------------------------------------- ARITHMETIC
			
			-- Memory() and Reg() at the same line means 
			-- they will run simultaneously
			
			-- 000000 XX YY ZZ
		 	--    ADD  D S1 S2

			-- EXEC_ADD  -> D := S1 + S2
			-- EXEC_ADDC -> D := S1 + S2 + Carry
			-- EXEC_SUB  -> D := S1 - S2
			-- EXEC_SUBC -> D := S1 - S2 - Carry

			when code_add  => EXEC_ADDC(Reg(Y),Reg(Z),False,Reg(X),zero,Carry,Negative,Overflow);
			when code_addc => EXEC_ADDC(Reg(Y),Reg(Z),Carry,Reg(X),Zero,Carry,Negative,overflow);
			when code_sub  => EXEC_SUBC(Reg(Y),Reg(Z),False,Reg(X),zero,Carry,Negative,Overflow);
			when code_subc => EXEC_SUBC(Reg(Y),Reg(Z),Carry,Reg(X),Zero,Carry,Negative,overflow);

			------------------------------------------------- LOGICAL

			-- NOT ~ D := NOT S1
			-- AND ~ D := S1 AND S2
			-- OR  ~ D := S1 OR S2
			-- XOR ~ D := S1 XOR S2
			-- REA ~ LSB(D) := reduced_and(S1)
			-- REO ~ LSB(D) := reduced_or(S1)
			-- REX ~ LSB(D) := reduced_xor(S1)

			------------------------------------------------- SHIFT/ROTATE

			-- SLL  ~ Carry & D := S1 & '0'
			-- SRL  ~ D & Carry := '0' & S1
			-- SRA  ~ D & Carry := MSB(S1) & S1
			-- ROL  ~ D := rotate_left(S1)
			-- ROLC ~ Carry & D := rotate_left(Carry & S1)
			-- ROR  ~ D := rotate_right(S1)
			-- RORC ~ Carry & D := rotate_right(Carry & S1)
			
			when code_sll  => Carry := Reg(Y) / 2**(data_width - 1); Data := (Reg(Y) mod 2**(data_width - 1))*2;
 					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- 100101 => 1; 001010
			-- 011101 => 0; 111010
			when code_srl  => Carry := Reg(Y) mod 2; Data := Reg(Y) / 2;
 					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- 100101 => 010010; 1
			-- 011101 => 001110; 1
			when code_sra  => Carry := Reg(Y) mod 2; Data := Reg(Y) - (Reg(Y) mod 2**(data_width - 1)) + Reg(Y) / 2;
					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- 100101 => 110010; 1
			-- 011101 => 001110; 1
			when code_rol  => Data := (Reg(Y) mod 2**(data_width - 1))*2 + Reg(Y) / 2**(data_width - 1);
					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- 100101 => 001011
			-- 011101 => 111010
			when code_rolc => Data := (Reg(Y) mod 2**(data_width - 1))*2 + Carry; Carry := Reg(Y) / 2**(data_width - 1);
			-- 1;00101 => 0;01011
			-- 0;11101 => 1;11010
			when code_ror  => Data := Reg(Y) / 2 + (Reg(Y) mod 2) * 2**(data_width - 1);
					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- 100101 => 110010
			-- 011101 => 101110
			when code_rorc => Data := Reg(Y) / 2 + Carry * 2**(data_width - 1); Carry := Reg(Y) mod 2;
			-- 1;00101 => 1;10010
			-- 0;11101 => 1;01110

		        ------------------------------------------------- MEMORY ACCESS

			-- LDC ~ D := const
			-- LDD ~ D := mem(addr)
			-- LDR ~ D := mem(RA)
			-- STD ~ mem(addr) := S
			-- STR ~ mem(RA) := S

			-- was passiert zwischen Speicher und Register wird hier beschreibt

			when code_ldc  => Data := Memory(PC); Reg(X) := Data;
 					  Set_Flags_Load(Data, Zero, Negative, Overflow);
					  PC := INC(PC);
			-- man speichert das Datei aus Nächste Stelle in Stelle X vom Register
			when code_ldd  => Data := Memory(Memory(PC)); Reg(X) := Data;
 					  Set_Flags_Load(Data,Zero,Negative,Overflow);
					  PC := INC(PC);
			-- man kopiert die Stelle, deren Adresse in nächste Stelle liegt in Stelle X vom Register
			when code_ldr  => Data :=Memory(Reg(Y)); Reg(X) := Data;
 					  Set_Flags_Load(Data,Zero,Negative,Overflow);
			-- man kopiert die Stelle, deren Adresse in Stelle Y vom Register liegt in Stelle X vom Register
			when code_std  => Memory(Memory(PC)):=Reg(X);
 					  PC := INC(PC);
			-- Stelle X vom Register wird in die Stelle kopiert, deren Adresse in nächste Stelle liegt
			when code_str  => Memory(Reg(Y)):=Reg(X);
			-- man kopiert Stelle X vom Register in die Stellem dren Adresse in Stelle Y vom Register lieg

			------------------------------------------------- I/O
			-- IN  D _ _ ~ D := data_from_input_device
			-- OUT S _ _ ~ data_to_output_device := S

		        ------------------------------------------------- JUMP OPERATIONS 

			
			when code_jmp  => PC := Memory(PC); -- Unconditional Jump. Simply points to the memory address
			when code_jz   => if Zero then PC := Memory(PC); 	  -- Jump if Zero flag = '1'
 					  else PC := INC(PC); end if;
			when code_jc   => if Carry then PC := Memory(PC);	  -- Jump if Carry flag = '1'
					  else PC := INC(PC); end if;
			when code_jn   => if Negative then PC := Memory(PC); 	  -- Jump if Negative flag is = '1'
					  else PC := INC(PC); end if; 
			when code_jo   => if Overflow then PC := Memory(PC); 	  -- Jump if Overflow flag is = '1'
					  else PC := INC(PC); end if;
			when code_jnz   => if not Zero then PC := Memory(PC); 	  -- Jump if Zero flag = '0'
 					  else PC := INC(PC); end if;
			when code_jnc   => if not Carry then PC := Memory(PC); 	  -- Jump if Carry flag = '0'
					  else PC := INC(PC); end if;
			when code_jnn   => if not Negative then PC := Memory(PC); -- Jump if Negative flag is = '0'
					  else PC := INC(PC); end if; 
			when code_jno   => if not Overflow then PC := Memory(PC); -- Jump if Negative flag is = '0'
					  else PC := INC(PC); end if;

			-- **********************
			-- ** LOGIC OPERATIONS **
			-- **********************

			--when code_not  => Data := ?NOT?( Reg(Y) ); Reg(X) := Data;
			--		  Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
			--when code_and  => Data := Reg(Y) AND Reg(Z); Reg(X) := Data;
			--		  Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
			--when code_add  => EXEC_ADDC(Reg(Y), Reg(Z), FALSE, Reg(X),
				--	  Zero, Carry, Negative, Overflow);
			--when code_addc => EXEC_ADDC(Reg(Y), Reg(Z), Carry, Reg(X),
				--	  Zero, Carry, Negative, Overflow);

		        ------------------------------------------------- UNEXPECTED CODE

			when others    => -- Illegal or not yet implemented operations
				assert FALSE
				report "Illegal Operation"
				severity error;
		end case;
	end process;
end functional;