pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Command_Line; use Ada.Command_Line;

procedure Pi_Digits
is
	Acc : Big_Integer := 0;
	Den, Num : Big_Integer := 1;
	K : Natural := 0;

	function Extract_Digit (N : Integer) return Integer
	is (To_Integer (
		(Num * To_Big_Integer (N) + Acc) / Den));	
	
	pragma Inline (Extract_Digit);
	
	procedure Eliminate_Digit (N : Integer)
	is begin
		Acc := 10 * (@ - To_Big_Integer (N) * Den);
		Num := @ * 10;
	end Eliminate_Digit;

	pragma Inline (Eliminate_Digit);

	procedure Next_Term
	is
		K2 : constant Big_Integer := To_Big_Integer ((K + 1) * 2 + 1);
	begin 
		K := @ + 1;

		Acc := (@ + 2 * Num) * K2;
		Den := @ * K2;

		Num := @ * To_Big_Integer (K);
	end Next_Term;

	pragma Inline (Next_Term);

	D : Integer;
	I : Natural := 0;
begin
	while I < Integer'Value (Argument (1)) loop
		Next_Term;

		if Num <= Acc then
			D := Extract_Digit (3);	

			if D = Extract_Digit (4) then
				Put (Character'Val (Character'Pos ('0') + D));
				I := @ + 1;
				if I rem 10 = 0 then
					Put_Line (ASCII.HT & ":" & I'Image (I'Image'First + 1 .. I'Image'Last));
				end if;

				Eliminate_Digit (D);
			end if;
		end if;
	end loop;
end Pi_Digits;
