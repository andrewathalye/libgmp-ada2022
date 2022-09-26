pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Big_Integers; use Big_Integers;

procedure Pi_Digits_GMP_Big_Integer
is
	Acc : Big_Integer := 0;
	Den : Big_Integer := 1;
	Num : Big_Integer := 1;

	K : Integer := 0;

	function Extract_Digit (N : Integer) return Integer
	is (To_Integer (
		(Acc + Num * To_Big_Integer (N)) / Den));
	
	pragma Inline (Extract_Digit);
	
	procedure Eliminate_Digit (N : Integer)
	is begin
		Acc := (@ - Den * To_Big_Integer (N)) * 10;

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
end Pi_Digits_GMP_Big_Integer;
