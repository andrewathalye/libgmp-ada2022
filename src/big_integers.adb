pragma Ada_2022;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Unchecked_Deallocation;

with GMP; use GMP;

package body Big_Integers is
	-- Controlled Operations
	procedure Initialize (BI : in out Controlled_Big_Integer)
	is begin
		BI.MPZ := new MPZ_Type;
		Init (BI.MPZ.all);
	end Initialize;

	-- Used to copy a Big Integer upon assignment
	procedure Adjust (BI : in out Controlled_Big_Integer)
	is
		Original : constant MPZ_Access := BI.MPZ;
	begin
		if Original /= null then
			BI.MPZ := new MPZ_Type;
			Init (BI.MPZ.all, Original.all);
		end if;
	end Adjust;

	-- Free memory used by Big Integer on scope exit
	procedure Finalize (BI : in out Controlled_Big_Integer) is
		procedure Free is new Unchecked_Deallocation (
			Object => MPZ_Type,
			Name => MPZ_Access);
	begin
		if BI.MPZ /= null then
			Clear (BI.MPZ.all);
			Free (BI.MPZ);
		end if;
	end Finalize;

	-- Is_Valid
	function Is_Valid (Arg : Big_Integer) return Boolean
	is (Arg.C.MPZ /= null);

	-- Comparison Subprograms
	function "=" (L, R : Valid_Big_Integer) return Boolean
	is (Compare (L.C.MPZ.all, R.C.MPZ.all) = 0);

	function "<" (L, R : Valid_Big_Integer) return Boolean
	is (Compare (L.C.MPZ.all, R.C.MPZ.all) < 0);

	function "<=" (L, R : Valid_Big_Integer) return Boolean
	is (Compare (L.C.MPZ.all, R.C.MPZ.all) <= 0);

	function ">" (L, R : Valid_Big_Integer) return Boolean
	is (Compare (L.C.MPZ.all, R.C.MPZ.all) > 0);

	function ">=" (L, R : Valid_Big_Integer) return Boolean
	is (Compare (L.C.MPZ.all, R.C.MPZ.all) >= 0);

	-- Conversions
	function To_Big_Integer (Arg : Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Set (Result.C.MPZ.all, Arg);
		return Result;
	end To_Big_Integer;

	function To_Integer (Arg : Valid_Big_Integer) return Integer
	is (Convert (Arg.C.MPZ.all));

	package body Signed_Conversions is
		function To_Big_Integer (Arg : Int) return Valid_Big_Integer
		is (From_String (Arg'Image));

		function From_Big_Integer (Arg : Valid_Big_Integer) return Int
		is (Int'Value (To_String (Arg)));
	end Signed_Conversions;

	package body Unsigned_Conversions is
		function To_Big_Integer (Arg : Int) return Valid_Big_Integer
		is (From_String (Arg'Image));

		function From_Big_Integer (Arg : Valid_Big_Integer) return Int
		is (Int'Value (To_String (Arg)));
	end Unsigned_Conversions;


	function To_String (
		Arg : Valid_Big_Integer;
		Width : Field := 0;
		Base : Number_Base := 10) return String
	is
		Str : chars_ptr := Convert (
			Dest => Null_Ptr,
			Base => Base,
			Source => Arg.C.MPZ.all);
	
		Result : constant String := (
			if Arg >=0 then " " & Value (Str)
			else Value (Str));

		Necessary_Width : constant Natural := Width - Result'Length;
	begin
		Free (Str);

		return [for I in 1 .. Necessary_Width => ' '] & Result;
	end To_String;

	function From_String (Arg : String) return Valid_Big_Integer
	is
		Str : chars_ptr := New_String (Arg);
		Result : Big_Integer;
	begin
		if Set (Result.C.MPZ.all, Str) /= 0 then
			raise Constraint_Error;
		end if;

		Free (Str);
		return Result;
	end From_String;

	procedure Put_Image (
		Buffer : in out Root_Buffer_Type'Class;
		Arg : in Valid_Big_Integer)
	is
	begin
		Put (Buffer, To_String (Arg));
	end Put_Image;

	-- Arithmetic
	function "+" (L : Valid_Big_Integer) return Valid_Big_Integer
	is (L);

	function "-" (L : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Negate (Result.C.MPZ.all, L.C.MPZ.all);
		return Result;
	end "-";

	function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Absolute_Value (Result.C.MPZ.all, L.C.MPZ.all);
		return Result;
	end "abs";

	function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Add (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "+";
		
	function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Subtract (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "-";

	function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Multiply (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "*";

	function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Divide (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "/";

	function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		procedure Modulo (
			Dest : out MPZ_Type;
			Source_1 : MPZ_Type;
			Source_2 : MPZ_Type)
		with
			Import => True,
			Convention => C,
			External_Name => "__gmpz_mod";

		Result : Big_Integer;
	begin
		Modulo (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "mod";

	function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Remainder (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end "rem";

	function "**" (
		L : Valid_Big_Integer;
		R : Natural) return Valid_Big_Integer
	is
		Result : Big_Integer;
	begin
		Exponent (Result.C.MPZ.all, L.C.MPZ.all, unsigned (R));
		return Result;
	end "**";

	function Min (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		(case L > R is
			when True => R,
			when False => L);

	function Max (L, R : Valid_Big_Integer) return Valid_Big_Integer
	is
		(case L > R is
			when True => L,
			when False => R);
	
	function Greatest_Common_Divisor
		(L, R : Valid_Big_Integer) return Big_Positive
	is
		Result : Big_Integer;
	begin
		GCD (Result.C.MPZ.all, L.C.MPZ.all, R.C.MPZ.all);
		return Result;
	end Greatest_Common_Divisor;
end Big_Integers;
