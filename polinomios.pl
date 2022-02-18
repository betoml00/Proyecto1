%SUMA
%Si B es vacio TODO comentar
suma_pol(A,[],A) :- A = [_|_].
%Si A es vacio la suma es igual a B.
suma_pol([],B,B).
%Para sumar dos polinomios se suman sus cabezas y llama recursivamente
suma_pol([Ca|A], [Cb|B], [Cc|C]) :-
   Cc is Ca+Cb,
   suma_pol(A, B, C).

%RESTA
%Si B es vacio TODO
resta_pol(A,[],A) :- A = [_|_].
%Si A es vacio la suma es igual a B.
resta_pol([],B,B).
%Para sumar dos polinomios se suman sus cabezas y llama recursivamente
resta_pol([Ca|A], [Cb|B], [Cc|C]) :-
   Cc is Ca-Cb,
   resta_pol(A, B, C).

%PRODUCTO ESCALAR
%Si el polinomio es vacio su producto Esc tambien.
producto_Esc_pol([],_,[]).
%Si es no vacio, se multiplica su cabeza con el Esc y llama rec.
producto_Esc_pol([Ca|A], Esc, [Cc|C]) :-
   Cc is Ca*Esc,
   producto_Esc_pol(A, Esc, C).

%PRODUCTO
%Si B es vacio el producto es vacio.
producto_pol(_,[],[]).
%Si son no vacios
producto_pol(A,[Cb|B], C) :-
   producto_pol(A,B, Rec), %quitamos cabeza de B y llamamos recursivamente.
   producto_Esc_pol(A, Cb, Esc), %calculamos el prod. Esc con la cabeza de B.
   suma_pol(Esc, [0.0|Rec], C). %sumamos ambos resultados anteriores en C.

%GRADO
%Basicamente la longitud de la lista
grado_pol([],0).
grado_pol([_|A],Grad):-
    grado_pol(A,Temp),
    Grad is Temp+1.

%EVALUAR
eval_pol([],_,0).
eval_pol([Ca|A],X,Res):-
    eval_pol(A,X,Temp),
    Res is (Temp*X)+Ca.

%DIFERENCIAR
dif_pol([],[]).
dif_pol([Ca|A],)
