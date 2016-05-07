use WORK.cpu_defs_pack.all;
use WORK.mem_defs_pack.all;

entity MaxCPU is
end MaxCPU;

architecture functional of MaxCPU is
begin
	process
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

		-- further objects
		begin

		-- cmd fetch
		Instr := Memory(PC); 
		OP := Instr / (2**reg_addr_width)**3; -- erster Teil eines Befehles, der sagt, was man machen soll. Die Größe ist 3 standard Einheiten.

		X := (Instr / (2**reg_addr_width)**2) mod 2**reg_addr_width; -- zweiter Teil (2 st. E.)
		Y := (Instr / 2**reg_addr_width) mod 2**reg_addr_width; -- dritter Teil (2 st. E.)
		Z := Instr mod 2**reg_addr_width; -- vierter Teil (2 st. E.)

		PC := PC+1; -- nächstes Befehl

		-- cmd decode
		-- Übersetzung von OpCodes in echte Befehle
		case OP is

			-- *******************
			-- ** MISCELLANEOUS **
			-- *******************

			when code_nop  => null;
			when code_stop => wait;

		        -- *******************
			-- ** MEMORY ACCESS **
			-- *******************

			-- was passiert zwischen Speicher und Register wird hier beschreibt

			when code_ldc  => Data := Memory(PC); Reg(X) := Data;
 					  Set_Flags_Load(Data,Zero,Negative,Overflow);
					  PC := PC+1;
			-- man speichert das Datei aus Nächste Stelle in Stelle X vom Register
			when code_ldd  => Data := Memory(Memory(PC)); Reg(X) := Data;
 					  Set_Flags_Load(Data,Zero,Negative,Overflow);
					  PC := PC+1;
			-- man kopiert die Stelle, deren Adresse in nächste Stelle liegt in Stelle X vom Register
			when code_ldr  => Data :=Memory(Reg(Y)); Reg(X) := Data;
 					  Set_Flags_Load(Data,Zero,Negative,Overflow);
			-- man kopiert die Stelle, deren Adresse in Stelle Y vom Register liegt in Stelle X vom Register
			when code_std  => Memory(Memory(PC)):=Reg(X);
 					  PC := PC+1;
			-- Stelle X vom Register wird in die Stelle kopiert, deren Adresse in nächste Stelle liegt
			when code_str  => Memory(Reg(Y)):=Reg(X);
			-- man kopiert Stelle X vom Register in die Stellem dren Adresse in Stelle Y vom Register lieg

			-- *********************
			-- ** JUMP OPERATIONS **
			-- *********************

			-- Befehle, um die aktive Stelle zu ändern.

			when code_jmp  => PC := Memory( PC );
			-- die neue Adresse ist das Wert von der aktiver Stelle.
			when code_jz   => if zero then PC := Memory( PC );
 					  else PC := INC ( PC ); end if;
			-- selbe Sache aber nur wenn der Flag '0' ist aktiv
			when code_jnz  => if not zero then PC := Memory( PC );
					  else PC := INC ( PC ); end if;
			-- selbe Sache aber nur wenn '0# ist inaktiv
			when code_jc   => if carry then PC := Memory( PC );
					  else PC := INC ( PC ); end if;
			-- selbe Sache wenn 'carry' ist inaktiv
			
			-- TODO: JN, JO, JNZ, JNC, JNN, JND

			-- **********************
			-- ** LOGIC OPERATIONS **
			-- **********************

			when code_not  => Data := ?NOT?( Reg(Y) ); Reg(X) := Data;
					  Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
			when code_and  => Data := Reg(Y) AND Reg(Z); Reg(X) := Data;
					  Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
			when code_add  => EXEC_ADDC(Reg(Y), Reg(Z), FALSE, Reg(X),
					  Zero, Carry, Negative, Overflow);
			when code_addc => EXEC_ADDC(Reg(Y), Reg(Z), Carry, Reg(X),
					  Zero, Carry, Negative, Overflow);

			-- *********************
			-- ** UNEXPECTED CODE **
			-- *********************

			when others    => 
				assert FALSE
				report "Illegal Operation"
				severity error;
		end case;
	end process;
end functional;
