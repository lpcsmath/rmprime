use "rmprime.sml";

fun genbsaux n b = if (n = b) then [] else b :: genbsaux n (b + 1);

fun genbs n = genbsaux n 2;

val allTrue = foldl (fn (x,y) => x andalso y) true;

fun testaux bs x = allTrue (map (rmprime x) bs);

fun test x = testaux (genbs x) x;

fun isPrime x = allTrue (map (fn y => gcd x y = 1) (genbs x));

if allTrue (map (fn x => (isPrime x) = (test x)) (genbs 1000))
then
    print "PASSED!\n"
else
    print "FAILED!\n"
