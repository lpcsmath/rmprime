-module(rmprime).
-export([prime/1,ndprime/2,ndprime1/1,prime1/2]).
-import(ebs,[ebs/2,ebsmod/3]).

%
%    rs(N) = (R,S) such that S is odd and 2^R * S = N.
%
rs(N) -> rsaux(0,N).

rsaux(R,S) ->
    case (S rem 2) of
        0 -> rsaux(R + 1,S div 2);
        1 -> {R,S}
    end.

%
%   testrest(N,[$,...,$,N-1,1,%,...,%]) = true where $ is not 1 or N-1
%                                               and % are arbitrary
%   testrest(N,_) = false
%
testrest(_,[])  -> false;
testrest(_,[_]) -> false;
testrest(N,[X|[Y|Xs]]) ->
    (X =:= (N-1)) and (Y =:= 1) or testrest(N,[Y|Xs]).


%
%    testseq(N,[1,%,...,%]) = true
%    testseq(N,[$,...,$,N-1,1,%,...,%]) = true
%    with $ is not 1 or N-1
%        and % are arbitrary
%    testseq(N,_) = false
%
testseq(_,[])    -> false;
testseq(_,[1|_]) -> true;
testseq(N,Xs)    -> testrest(N,Xs).


%
%    mkseqaux(N,B,S,R) creates the list
%    [ B^(N-1) mod N, ... , B^(2S) mod N, B^S mod N ]
%    with 2^R * S = N.
%
mkseqaux(_,_,_,-1) -> [];
mkseqaux(N,B,S,R)  ->
    [ebsmod(B,ebs(2,R) * S,N) | mkseqaux(N,B,S,R-1)].


%
%   mkseq(N,B) creates a list
%   [ B^S mod N, B^(2S) mod N, ... , B^(N-1) mod N]
%   with 2^R * S = N.
%
mkseq(N,B) ->
    {R,S} = rs(N - 1),
    lists:reverse(mkseqaux(N,B,S,R)).


%
%   greatest common divisor
%
gcd(A,0) -> A;
gcd(A,B) -> gcd(B,A rem B).


%
%    This is a single Rabin-Miller primality test of n with a given b
%    such that 1 < b < n.
%    if prime n b = true
%    then n is prime or n is not prime
%         with a probability of < 1/2
%    else n is not prime.
%
prime1(N,B) ->
    case gcd(N,B) of
        1 -> Seq = mkseq(N,B),
             testseq(N,Seq);
        _ -> false
    end.


%
%   ndprime1(N) tests the number N with a random b.
%
ndprime1(N) ->
    B = if
            (N > 2) -> random:uniform(N-2) + 1;
            true    -> 2
        end,
    (N =:= 2) or prime1(N,B).


%
%   allTrue L = true if L contains only elements true.
%
allTrue(L) -> lists:all(fun (X) -> X end,L).


%
%   ndprime tests N with K random bs.
%
ndprime(K,N) ->
    L = lists:map(fun (_) -> ndprime1(N) end, lists:seq(1,K)),
    allTrue(L).


%
%   prime tests N with 20 random bs.
%
prime(N) -> ndprime(20,N).
