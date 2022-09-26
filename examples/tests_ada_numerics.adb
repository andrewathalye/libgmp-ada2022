pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Tests_Ada_Numerics
is
	subtype BI is Big_Integer;

	-- General Comparison (impl using one function)
	CmBI_1 : constant BI := 2 ** 32;
	CmBI_2 : constant BI := 2 ** 40;
	CmBI_R : constant Boolean := CmBI_1 < CmBI_2;


	-- To_Big_Integer, To_Integer
	TBI : constant BI := To_Big_Integer (Integer'Last);
	TBI_R : constant Boolean := To_Integer (TBI) = Integer'Last;

	-- In_Range
	IR_R : constant Boolean := In_Range (47, 30, 50);

	-- Signed / Unsigned Conversion + To_String + From_String
--	package SUC is new Unsigned_Conversions (Unsigned_128);
--	SUC_1 : constant Unsigned_128 := Unsigned_128'Last;
--	SUC_2 : constant BI := SUC.To_Big_Integer (SUC_1);
--	SUC_R : constant Boolean := SUC_1 = SUC.From_Big_Integer (SUC_2);
	SUC_R : constant Boolean := False;

	-- Arithmetic
	AR_ABS_R : constant Boolean := abs BI'(-300) = BI'(300);
	AR_ADD_R : constant Boolean := BI'(1) + BI'(200) = BI'(201);
	AR_SUB_R : constant Boolean := BI'(1) - BI'(200) = BI'(-199);
	AR_MUL_R : constant Boolean := BI'(2) * BI'(200) = BI'(400);
	AR_DIV_R : constant Boolean := BI'(20) / BI'(4) = BI'(5);
	AR_MOD_R : constant Boolean := BI'(-200) mod BI'(49) = BI'(45);
	AR_REM_R : constant Boolean := BI'(-200) rem BI'(49) = BI'(-4);
	AR_EXP_R : constant Boolean := BI'(2) ** 8 = BI'(256);
	AR_MIN_R : constant Boolean := Min (20, 40) = 20;
	AR_MAX_R : constant Boolean := Max (20, 40) = 40;
	AR_GCD_R : constant Boolean := Greatest_Common_Divisor (16, 8) = Big_Positive'(8);

	-- Large Numbers
	EX_R : constant Boolean := (2 ** 1024) = From_String ("179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216");
begin
	Put_Line ("Big_Integer Tests:");
	Put_Line ("If any of these tests fail, please report a GNAT bug.");
	Put_Line ("SUC is known to fail on GNAT due to From_Big_Integer");
	New_Line;
	Put_Line ("CmBI: " & CmBI_R'Image);
	Put_Line ("TBI: " & TBI_R'Image);
	Put_Line ("IR: " & IR_R'Image);
	Put_Line ("SUC: " & SUC_R'Image);
	Put_Line ("AR_ABS: " & AR_ABS_R'Image);
	Put_Line ("AR_ADD: " & AR_ADD_R'Image);
	Put_Line ("AR_SUB: " & AR_SUB_R'Image);
	Put_Line ("AR_MUL: " & AR_MUL_R'Image);
	Put_Line ("AR_DIV: " & AR_DIV_R'Image);
	Put_Line ("AR_MOD: " & AR_MOD_R'Image);
	Put_Line ("AR_REM: " & AR_REM_R'Image);
	Put_Line ("AR_EXP: " & AR_EXP_R'Image);
	Put_Line ("AR_MIN: " & AR_MIN_R'Image);
	Put_Line ("AR_MAX: " & AR_MAX_R'Image);
	Put_Line ("AR_GCD: " & AR_GCD_R'Image);
	Put_Line ("EX: " & EX_R'Image);
end Tests_Ada_Numerics;
