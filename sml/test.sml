use "rmprime.sml";

fun genbsaux n b = if (n = b) then [] else b :: genbsaux n (b + 1);

fun genbs n = genbsaux n 4;


fun isPrime x = allTrue (map (fn y => gcd x y = 1) (2::3::(genbs x)));

let
val r = Random.rand (IntInf.toInt ((Time.toSeconds (Time.now ())) mod 1073741823),0);

in
if allTrue (map (fn x => (isPrime x) = (ndrmprime r 20 x)) (genbs 10000))
then
    print "PASSED!\n"
else
    print "FAILED!\n"
end;
