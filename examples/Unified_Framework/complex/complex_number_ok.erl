-module(complex_number_ok).
-compile([export_all]).
 
-record(complex, {real, img}).
 
calculate() ->
    A = #complex{real=1, img=3},
    B = #complex{real=5, img=2},
    Sum = add (A, B),
    Product = multiply (A, B),
    Negation = negation (A),
    Inversion = inverse (A),
    Conjugate = conjugate (A),  
    {Sum, Product, Negation, Inversion, Conjugate}.
 
add (A, B) ->
    RealPart = A#complex.real + B#complex.real,
    ImgPart = A#complex.img + B#complex.img,
    #complex{real=RealPart, img=ImgPart}.
 
multiply (A, B) ->
    RealPart = (A#complex.real * B#complex.real) - (A#complex.img * B#complex.img),
    ImgPart = (A#complex.real * B#complex.img) + (B#complex.real * A#complex.img),
    #complex{real=RealPart, img=ImgPart}.
 
negation (A) ->
    #complex{real=-A#complex.real, img=-A#complex.img}.
 
inverse (A) ->
    C = conjugate (A),
    Mod = (A#complex.real * A#complex.real) + (A#complex.img * A#complex.img),
    RealPart = C#complex.real / Mod,
    ImgPart = C#complex.img / Mod,
    #complex{real=RealPart, img=ImgPart}.
 
conjugate (A) ->
    RealPart = A#complex.real,
    ImgPart = -A#complex.img,
    #complex{real=RealPart, img=ImgPart}.
