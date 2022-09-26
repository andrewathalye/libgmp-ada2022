pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

with GMP; use GMP;

procedure Pi_Digits_GMP
is
	pragma Warnings (Off, "overlaps with actual for");

	Tmp1, Tmp2, Acc, Den, Num : MPZ_Type;
	K : unsigned := 0;

	function Extract_Digit (N : unsigned) return unsigned
	is begin
		Multiply (Tmp1, Num, N);
		Add (Tmp2, Tmp1, Acc);
		Divide (Tmp1, Tmp2, Den);

		return Convert (Tmp1);
	end Extract_Digit;
	
	pragma Inline (Extract_Digit);
	
	procedure Eliminate_Digit (N : unsigned)
	is begin
		Multiply (Tmp1, Den, N);
		Subtract (Acc, Acc, Tmp1);
		Multiply (Acc, Acc, 10);
		Multiply (Num, Num, 10);
	end Eliminate_Digit;

	pragma Inline (Eliminate_Digit);

	procedure Next_Term
	is begin 
		K := @ + 1;

		Multiply (Tmp1, Num, 2);
		Add (Acc, Acc, Tmp1);
		Multiply (Acc, Acc, K * 2 + 1);
		Multiply (Den, Den, K * 2 + 1);
		Multiply (Num, Num, K);
	end Next_Term;

	pragma Inline (Next_Term);

	D : unsigned;
	I : Natural := 0;
begin
	Init (Tmp1);
	Init (Tmp2);
	Init (Acc, 0);
	Init (Den, 1);
	Init (Num, 1);

	while I < Integer'Value (Argument (1)) loop
		Next_Term;

		if Compare (Num, Acc) <= 0 then
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

	Clear (Tmp1);
	Clear (Tmp2);
	Clear (Acc);
	Clear (Den);
	Clear (Num);
end Pi_Digits_GMP;
