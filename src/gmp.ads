with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

private with System;

package GMP
	with Preelaborate
is
	pragma Linker_Options ("-lgmp");

	-- Low-level subprograms
	type MPZ_Type is limited private;

	procedure Multiply (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Source_2 : unsigned)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_mul_ui";
	
	procedure Multiply (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Soruce_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_mul";
	
	procedure Add (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Source_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_add";
	
	procedure Subtract (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Source_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_sub";
	
	procedure Divide (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Source_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_tdiv_q";
	
	function Compare (
		Source_1 : MPZ_Type;
		Source_2 : MPZ_Type) return Integer
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_cmp";
	
	function Convert (Source : MPZ_Type) return unsigned
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_ui";

	function Convert (
			Dest : chars_ptr;
			Base : Integer;
			Source : MPZ_Type) return chars_ptr
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_str";
	
	function Convert (Source : MPZ_Type) return Integer
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_si";
	
	procedure Set (
		Destination : out MPZ_Type;
		Source : unsigned)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set_ui";
	
	procedure Set (
		Destination : out MPZ_Type;
		Source : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set";

	procedure Set (
			Dest : out MPZ_Type;
			Source : Integer)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set_si";

	function Set (
		Dest : out MPZ_Type;
		Source : chars_ptr;
		Base : Integer := 10) return Integer
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set_str";
	
	procedure Init (Destination : out MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init";
	
	procedure Init (
		Destination : out MPZ_Type;
		Value : unsigned)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init_set_ui";
	
	procedure Init (
		Destination : out MPZ_Type;
		Value : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init_set";
	
	procedure Clear (Destination : out MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_clear";

	procedure Absolute_Value (
		Dest : out MPZ_Type;
		Source : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_abs";

	procedure Exponent (
			Dest : out MPZ_Type;
			Base : MPZ_Type;
			Exponent : unsigned)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_pow_ui";

	procedure Exponent (
			Dest : out MPZ_Type;
			Base : MPZ_Type;
			Exponent : MPZ_Type;
			Modulo : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_powm";

	procedure Remainder (
		Dest : out MPZ_Type;
		Source_1 : MPZ_Type;
		Source_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_tdiv_r";

	procedure GCD (
		Dest : out MPZ_Type;
		Source_1, Source_2 : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_gcd";

	procedure Negate (
		Dest : out MPZ_Type;
		Source : MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_neg";
private
	type MPZ_Type is record
		mp_alloc : Integer;
		mp_size : Integer;
		mp_d : System.Address := System.Null_Address;
	end record;
end GMP;
