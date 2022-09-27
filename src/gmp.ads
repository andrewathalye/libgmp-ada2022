with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

private with System;

package GMP
	with Preelaborate
is
	pragma Linker_Options ("-lgmp");

	-- Low-level subprograms
	type MPZ_Type is limited private;

	-- Prevent use-after-free / use without initialisation
	subtype Valid_MPZ_Type is MPZ_Type
	with
		Dynamic_Predicate => Is_Valid (Valid_MPZ_Type),
		Predicate_Failure =>
			"MPZ_Type must be initialised before use";
	
	function Is_Valid (Source : MPZ_Type) return Boolean;

	procedure Multiply (
		Dest : out MPZ_Type;
		Source_1 : Valid_MPZ_Type;
		Source_2 : unsigned_long)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_mul_ui";
	
	procedure Multiply (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_mul";
	
	procedure Add (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_add";
	
	procedure Subtract (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_sub";
	
	procedure Divide (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_tdiv_q";
	
	function Compare (Source_1, Source_2 : Valid_MPZ_Type) return Integer
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_cmp";
	
	function Convert (Source : Valid_MPZ_Type) return unsigned_long
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_ui";

	function Convert (
			Dest : chars_ptr;
			Base : Integer;
			Source : Valid_MPZ_Type) return chars_ptr
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_str";
	
	function Convert (Source : Valid_MPZ_Type) return Long_Integer
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_get_si";
	
	procedure Set (
		Destination : out MPZ_Type;
		Source : unsigned_long)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set_ui";
	
	procedure Set (
		Destination : out MPZ_Type;
		Source : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_set";

	procedure Set (
		Dest : out MPZ_Type;
		Source : Long_Integer)
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
	
	-- Initialise and set to zero
	procedure Init (Destination : out MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init";
	
	procedure Init (
		Destination : out MPZ_Type;
		Value : unsigned_long)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init_set_ui";
	
	procedure Init (
		Destination : out MPZ_Type;
		Value : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_init_set";
	
	-- Free memory allocated for MPZ_Type
	procedure Clear (Destination : in out Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_clear";

	procedure Absolute_Value (
		Dest : out MPZ_Type;
		Source : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_abs";

	procedure Exponent (
			Dest : out MPZ_Type;
			Base : Valid_MPZ_Type;
			Exponent : unsigned_long)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_pow_ui";

	procedure Exponent (
			Dest : out MPZ_Type;
			Base, Exponent, Modulo : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_powm";

	procedure Remainder (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_tdiv_r";

	procedure GCD (
		Dest : out MPZ_Type;
		Source_1, Source_2 : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_gcd";

	procedure Negate (
		Dest : out MPZ_Type;
		Source : Valid_MPZ_Type)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_neg";

	procedure Factorial (
		Dest : out MPZ_Type;
		Limit : unsigned_long)
	with
		Import => True,
		Convention => C,
		External_Name => "__gmpz_fac_ui";

	procedure Modulo (
			Dest : out MPZ_Type;
			Source_1 : Valid_MPZ_Type;
			Source_2 : Valid_MPZ_Type)
		with
			Import => True,
			Convention => C,
			External_Name => "__gmpz_mod";
private
	type MPZ_Type is record
		mp_alloc : Integer;
		mp_size : Integer;
		mp_d : System.Address := System.Null_Address;
	end record
	with
		Convention => C;
end GMP;
