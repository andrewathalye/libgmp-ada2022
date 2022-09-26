pragma Ada_2022;

with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;
with Ada.Numerics.Big_Numbers; use Ada.Numerics.Big_Numbers;

private with Ada.Finalization;

private with GMP;

package Big_Integers
	with Preelaborate
is
	-- High-level interface
	-- Compatible with Ada.Numerics.Big_Numbers.Big_Integers
	type Big_Integer is private
		with Integer_Literal => From_Universal_Image,
		Put_Image => Put_Image;
	
	function Is_Valid (Arg : Big_Integer) return Boolean
		with Convention => Intrinsic;
	
	subtype Valid_Big_Integer is Big_Integer
		with Dynamic_Predicate => Is_Valid (Valid_Big_Integer),
			Predicate_Failure => (raise Program_Error);

	function "=" (L, R : Valid_Big_Integer) return Boolean;
	
	function "<" (L, R : Valid_Big_Integer) return Boolean;
	
	function "<=" (L, R : Valid_Big_Integer) return Boolean;

	function ">" (L, R : Valid_Big_Integer) return Boolean;
	
	function ">=" (L, R : Valid_Big_Integer) return Boolean;

	function To_Big_Integer (Arg : Integer) return Valid_Big_Integer;
	
	subtype Big_Positive is Big_Integer
		with Dynamic_Predicate => (if Is_Valid (Big_Positive)
			then Big_Positive > 0),
			Predicate_Failure => (raise Constraint_Error);

	subtype Big_Natural is Big_Integer
		with Dynamic_Predicate => (if Is_Valid (Big_Natural)
			then Big_Natural >= 0),
			Predicate_Failure => (raise Constraint_Error);

	function In_Range (Arg, Low, High : Valid_Big_Integer) return Boolean is
		(Low <= Arg and Arg <= High);

	function To_Integer (Arg : Valid_Big_Integer) return Integer
	with Pre => In_Range (Arg,
		Low => To_Big_Integer (Integer'First),
		High => To_Big_Integer (Integer'Last))
		or else raise Constraint_Error;

	generic
		type Int is range <>;
	package Signed_Conversions is
		function To_Big_Integer (Arg : Int) return Valid_Big_Integer;
		function From_Big_Integer (Arg : Valid_Big_Integer) return Int
		with Pre => In_Range (Arg,
			Low => To_Big_Integer (Int'First),
			High => To_Big_Integer (Int'Last))
		or else raise Constraint_Error;
	end Signed_Conversions;

	generic
		type Int is mod <>;
	package Unsigned_Conversions is
		function To_Big_Integer (Arg : Int) return Valid_Big_Integer;
		function From_Big_Integer (Arg : Valid_Big_Integer) return Int
		with Pre => In_Range (Arg,
		Low => To_Big_Integer (Int'First),
		High => To_Big_Integer (Int'Last))
		or else raise Constraint_Error;
	end Unsigned_Conversions;

	function To_String (
		Arg : Valid_Big_Integer;
		Width : Field := 0;
		Base : Number_Base := 10) return String
	with Post => To_String'Result'First = 1;

	function From_String (Arg : String) return Valid_Big_Integer;

	function From_Universal_Image (Arg : String) return Valid_Big_Integer
		renames From_String;

	procedure Put_Image (
		Buffer : in out Root_Buffer_Type'Class;
		Arg : in Valid_Big_Integer);

	function "+" (L : Valid_Big_Integer) return Valid_Big_Integer;
	function "-" (L : Valid_Big_Integer) return Valid_Big_Integer;
	function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer;
	function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function "**" (L : Valid_Big_Integer; R : Natural) return Valid_Big_Integer;
	function Min (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function Max (L, R : Valid_Big_Integer) return Valid_Big_Integer;
	function Greatest_Common_Divisor
		(L, R : Valid_Big_Integer) return Big_Positive
	with Pre => (L /= 0 and R /= 0) or else raise Constraint_Error;
	
private
	type MPZ_Access is access GMP.MPZ_Type;

	type Controlled_Big_Integer is new Ada.Finalization.Controlled with
		record
			MPZ : MPZ_Access := null;
		end record;
	
	type Big_Integer is record
		C : Controlled_Big_Integer;
	end record;

	procedure Initialize (BI : in out Controlled_Big_Integer);
	procedure Adjust (BI : in out Controlled_Big_Integer);
	procedure Finalize (BI : in out Controlled_Big_Integer);
end Big_Integers;
