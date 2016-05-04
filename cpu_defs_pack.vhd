package cpu_defs_pack is

	constant bus_width  : natural := 12;
	constant data_width : natural := bus_width;
	constant addr_width : natural := bus_width;
	
	constant reg_addr_width : natural := 2;
	constant opcode_width   : natural := 6;

	subtype data_type is
		natural range 0 to 2**data_width-1;

	subtype addr_type is
		natural range 0 to 2**addr_width-1;

	subtype reg_addr_type is
		natural range 0 to 2**reg_addr_width-1;

	subtype opcode_type is
		natural range 0 to 2**opcode_width-1;

	type reg_type is array(reg_addr_type) of data_type;
	
	type mem_type is array(addr_type) of data_type;	
	
	constant code_nop : opcode_type:= 0;

	constant code_stop : opcode_type:= 1;

end cpu_defs_pack;