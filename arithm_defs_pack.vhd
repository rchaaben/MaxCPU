library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use WORK.cpu_defs_pack.all;
use WORK.mem_defs_pack.all;

package arithm_defs_pack is
	
	--- procedure ADDC declaration

	procedure EXEC_ADDC (	
	      constant A, B     : in data_type;				
	      variable CI       : in boolean;
	      variable R        : out data_type;
	      variable Z,CO,N,O : out boolean);
				
	--- procedure SUBC declaration
	
	procedure EXEC_SUBC (	
	      constant A, B     : in data_type;				
	      variable CI       : in boolean;
	      variable R        : out data_type;
	      variable Z,CO,N,O : out boolean);

end arithm_defs_pack;


package body arithm_defs_pack is
	
	---- procedure ADDC implementation
	
	procedure EXEC_ADDC (	
	  constant A, B     : in data_type; --- input variables
		variable CI       : in boolean; --- input carry 
		variable R        : out data_type;  --- result
		variable Z,CO,N,O : out boolean) is --- zero, output carry, Negative,   Overflow
				
          variable T: integer := A+B+Boolean'Pos( CI );
	  variable A_s, B_s, T_s :integer; -- signed interpretation
	  begin
	    --- determine the sign of A
	    if A >= 2**(data_width-1) then 
	      A_s := A-2**(data_width);
	    else 
	      A_s := A;
	    end if;
	  
	    --- determine the sign of B
	    if B >= 2**(data_width-1) then
	      B_s := B-2**(data_width);
	    else 
	      B_s := B;
	    end if;
		
	    T_s := A_s+B_s+Boolean'Pos( CI );
		
		  --- output carry 
		  if T >= 2**data_width then
	      R := T-2**(data_width); 
	      CO := True;
	    else 
	      R := T;
	      CO := False;
	    end if;	
	  
	    --- zero
	    if T mod 2**data_width = 0 then 
	      z := True;
	    else
	      z := False;
	    end if;
	  
	    --- Negative
	    if T_s < 0 then 
	      N := True;
	    else
	      N := False;
	    end if;
	  
	    --- overflow
	    if (T_s < -2**(data_width-1)) or (T_s >= 2**(data_width-1)) then 
	      O := True;
	    else 
	      O:= False;
	    end if;
	end EXEC_ADDC;  
	
	---- procedure SUBC implemantation
	
	procedure EXEC_SUBC (	
          constant A, B     : in data_type;  --- input variables
	  variable CI       : in boolean;  --- input carry 
          variable R        : out data_type;   --- result
	  variable Z,CO,N,O : out boolean )is --- zero, output carry, Negative,   Overflow
				
	variable T: integer := A-B-Boolean'Pos( CI );
	variable A_s, B_s, T_s :integer; -- signed interpretation
	begin
	  --- determine the sign of A
	  if A >= 2**(data_width-1) then 
	    A_s := A-2**(data_width);
	  else 
	    A_s := A;
	  end if;
	  --- determine the sign of B
	  if B >= 2**(data_width-1) then
	    B_s := B-2**(data_width);
	  else 
	    B_s := B;
	  end if;
		
		T_s := A_s-B_s-Boolean'Pos( CI );
		
		--- output carry
		if T < 0 then      
			R := T + 2**data_width;    
			CO := TRUE;   
		else   
   			R := T;     
			CO := FALSE;   
		end if;	
		
	  --- zero
	  
  		if T = 0 then
  		  Z := TRUE;  
 		else     
 			Z := FALSE;   
		end if;
	  
	  --- Negative
	  if T_s < 0 then
	  		N := TRUE;
  		else
     	N := FALSE;
   	end if;
	  
	  --- overflow
	  if (T_s < -2**(data_width-1)) or (T_s >= 2**(data_width-1)) then
 	  	 O := TRUE;
		else
		  O := FALSE;
		end if;
	end EXEC_SUBC;
	

end arithm_defs_pack;
