use "ebs.sml";


structure RMPRIME : sig
    val prime1: int -> int -> bool
    val ndprime1: Random.rand -> int -> bool
    val ndprime: int -> int -> bool
    val prime: int -> bool
end = struct

    (*
        rs n = (r,s) such that s is odd and 2^r * s = n.
    *)
    fun rs n =
        let
            fun rsaux (r,s) = if (s mod 2 = 0) then rsaux (r + 1,s div 2) else (r,s)
        in
            rsaux (0,n)
        end


    (*
        testrest n ($,...,$,n-1,1,%,...,%) = true where $ is not 1 or n-1
                                                    and % are arbitrary
        testrest n other = false
    *)
    fun testrest n []         = false
     |  testrest n [x]        = false
     |  testrest n (x::y::xs) = (x=(n-1)) andalso (y=1) orelse testrest n (y::xs)


    (*
        testseq n (1,%,...,%) = true
        testseq n ($,...,$,n-1,1,%,...,%) = true
        with $ is not 1 or n-1
            and % are arbitrary
        testseq n other = false
    *)
    fun testseq n []      = false
     |  testseq n (1::xs) = true
     |  testseq n xs      = testrest n xs


    (*
        mkseqaux n b s r creates the list
         [ b^(n-1) mod n, ... , b^(2s) mod n, b^s mod n ]
         with 2^r * s = n.
    *)
    fun mkseqaux n b s ~1 = []
     |  mkseqaux n b s r = (ebsmod b ((ebs 2 r) * s) n) :: mkseqaux n b s (r-1)


    (*
        mkseq n b creates a list
         [ b^s mod n, b^(2s) mod n, ... , b^(n-1) mod n]
         with 2^r * s = n.
    *)
    fun mkseq n b =
        let
            val (r,s) = rs (n - 1)
        in
            rev (mkseqaux n b s r)
        end


    (*
        greatest common divisor
    *)
    fun gcd a 0 = a
     |  gcd a b = gcd b (a mod b)


    (*
        This is a single Rabin-Miller primality test of n with a given b
        such that 1 < b < n.
        if prime n b = true
        then n is prime or n is not prime
             with a probability of < 1/2
        else n is not prime.
    *)
    fun prime1 n b =
        if (gcd n b) = 1
        then
            testseq n (mkseq n b)
        else
            false



    (*
        ndprime1 n r tests the number n with a random b.
        r is a Random.rand structure.
    *)
    fun ndprime1 r n =
        let
            val b = if (n > 3) then (Random.randInt r mod (n-3)) + 2 else 2
        in
            (n = 2) orelse (n = 3) orelse (prime1 n b)
        end


    (*
        allTrue l = true if l contains only elements true.
    *)
    val allTrue = foldl (fn (x,y) => x andalso y) true


    (*
        for loop
    *)
    fun for 0 f x = []
     |  for n f x = (f x)::(for (n-1) f x)


    (*
        ndprime tests n with k random bs.
        r is a Random.rand structure.
    *)
    fun ndprime k n =
        let
            val r = Random.rand (IntInf.toInt ((Time.toSeconds (Time.now ())) mod 1073741823),0)
        in
            allTrue (for k (ndprime1 r) n)
        end


    (*
        prime tests n with 20 random bs.
    *)
    fun prime n = ndprime 20 n

end
