GMP-Ada : Ada 2022-compatible Big Numbers library
=================================================

Ada 2022 introduces support for Big Numbers into the 
standard library, however the best available implementation
of this standard (GNAT >= 12.1.0) suffers from slow performance
as numbers grow larger.

In its current implementation, GNAT enforces a restriction on the length
of Big\_Integers, preventing them from exceeding 200 32-bit digits. If this
restriction is removed, however, it becomes apparent that Big\_Integers is
not currently optimised for the case of exceedingly large numbers, and as a result
tests (like the enclosed pi\_digits benchmark) run very slowly, if at all.

To remedy this problem, this repository contains a small (hopefully) standards-compliant
wrapper for Ada.Numerics.Big\_Numbers.Big\_Integers that uses libgmp behind the scenes to
drastically improve performance. A direct mapping to a subset of GMP's API is also included
in `gmp.ads`, however this is primarily an implementation detail.

I haven't given it much thought yet, but if there is interest, I would be willing to write a 
compliant implementation of Big\_Reals as well that uses GMP and then submit it to GNAT upstream
for consideration. At the very least, I hope this proves useful for people using Ada for mathematics.

Warning
-------

This library is still in development, and there may be bugs. I have done my best to ensure it functions as
required by the Ada Reference Manual (A.5.5), however since Ada 2022 is still a draft things may change, or
I may have simply made a mistake.

License
-------

This library is licensed under the LGPL Version 3 or Higher.
