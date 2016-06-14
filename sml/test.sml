use "rmprime.sml";

fun genbsaux n b = if (n = b) then [] else b :: genbsaux n (b + 1)

fun genbs n = genbsaux n 2

fun gcd a 0 = a
 |  gcd a b = gcd b (a mod b)

val allTrue = foldl (fn (x,y) => x andalso y) true


fun isPrime x = allTrue (map (fn y => gcd x y = 1) (genbs x));

if allTrue (map (fn x => (isPrime x) = (RMPRIME.prime x)) (genbs 10000))
    then
        print "PASSED!\n"
    else
        print "FAILED!\n"
