use "ebs.sml";


(*
    rsaux (0,n) = (r,s) such that s is odd and 2^r * s = n.
*)
fun rsaux (r,s) = if (s mod 2 = 0) then rsaux (r + 1,s div 2) else (r,s);


(*
    rs n = (r,s) such that s is odd and 2^r * s = n.
*)
fun rs n = rsaux (0,n);


(*
    testrest n ($,...,$,n-1,1,%,...,%) = true where $ is not 1 or n-1
                                                and % are arbitrary
    testrest n other = false
*)
fun testrest n []         = false
 |  testrest n [x]        = false
 |  testrest n (x::y::xs) = (x=(n-1)) andalso (y=1) orelse testrest n (y::xs);


(*
    testseq n (1,...,1) = true
    testseq n ($,...,$,n-1,1,%,...,%) = true where $ is not 1 or n-1
                                               and % are arbitrary
    testseq n other = false
*)
fun testseq n []      = false
 |  testseq n (x::xs) = (x = 1) orelse testrest n (x::xs);


(*
    mkseqaux n b s r 0 creates a list
     [ b^s mod n, b^(2s) mod n, ..., b^(n-1) mod n]
     with 2^r * s = n.
*)
fun mkseqaux n b s r r' = if (r < r') then []
    else
        let
            val rs = (ebs 2 r') * s
        in
            (ebsmod b rs n) :: mkseqaux n b s r (r' + 1)
        end;


(*
    mkseq n b creates a list
     [ b^s mod n, b^(2s) mod n, ..., b^(n-1) mod n]
     with 2^r * s = n.
*)
fun mkseq n b =
        let
            val (r,s) = rs (n - 1)
        in
            mkseqaux n b s r 0
        end;


(*
    greatest common divisor
*)
fun gcd a 0 = a
 |  gcd a b = gcd b (a mod b);


(*
    This is a single Rabin-Miller primality test of n with a given b
    such that 1 < b < n.
    if rmprime n b = true
    then n is prime or n is not prime
         with a probability of < 1/2
    else n is not prime.
*)
fun rmprime n b = if not ((gcd n b) = 1) then false
    else
        let
            val seq = mkseq n b
        in
            testseq n seq
        end;
