use WORK.cpu_defs_pack.all;
use WORK.mem_defs_pack.all;

entity MaxCPU is
end MaxCPU;

architecture functional of MaxCPU is
begin
	process
		--variable Data  : data_type;
		variable Instr : data_type;
		variable OP    : data_type;
		variable Memory: mem_type := (
			0    => code_nop*(2**reg_addr_width)**3,
			1     => code_stop*(2**reg_addr_width)**3,
			others => 0
		);
		variable PC : addr_type := 0;
		variable X,Y,Z : reg_addr_type;

		-- further objects
		begin

		-- cmd fetch
		Instr := Memory(PC); 
		OP := Instr / (2**reg_addr_width)**3;

		X := (Instr / (2**reg_addr_width)**2) mod 2**reg_addr_width;
		Y := (Instr / 2**reg_addr_width) mod 2**reg_addr_width;
		Z := Instr mod 2**reg_addr_width; 

		PC := PC+1;

		-- cmd decode
		case OP is
		-- example
			when code_nop  => null;
			when code_stop => wait;
			when others    => 
				assert FALSE
				report "Illegal Operation"
				severity error;
		end case;
	end process;
end functional;
