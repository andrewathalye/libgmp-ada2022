with System; use System;

package body GMP is
	function Is_Valid (Source : MPZ_Type) return Boolean
	is (Source.mp_d /= System.Null_Address);
end GMP;
