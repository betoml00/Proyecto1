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
producto_pol(_,[],[]):-!.
%Si son no vacios
producto_pol(A,[Cb|B], C) :-
   producto_pol(A,B, Rec), %quitamos cabeza de B y llamamos recursivamente.
   producto_Esc_pol(A, Cb, Esc), %calculamos el prod. Esc con la cabeza de B.
   suma_pol(Esc, [0.0|Rec], C). %sumamos ambos resultados anteriores en C.

%GRADO
%Basicamente la longitud de la lista
grado_pol([],-1). %suponiendo que grado indefinido = -1.
grado_pol([_|A],Grad):-
    grado_pol(A,Temp),
    Grad is Temp+1.

%EVALUAR
eval_pol([],_,0).
eval_pol([Ca|A],X,Res):-
    eval_pol(A,X,Temp),
    Res is (Temp*X)+Ca.

%ONE HOT - para usar para combinar? 
%Para hacer polinomios [0,0,0,3,0] podriamos
%hacer prod_escalar([0,0,0,1,0],3).

%combina(i,i,o), combina(i,i,i)
combina([],Lista,Lista) :-!. %caso base
combina([X|Lista1],Lista2,[X|Lista3]):-
    combina(Lista1,Lista2,Lista3).

ceros(A,0,A):-
    !.
ceros(A,1,[0|A]):-
    !.
ceros(A,N,[0|Resto]):-
    Ntemp is N-1,
    ceros(A,Ntemp,Resto).

one_hot(N,Target,Res):-
    ceros([],Target-1,Primera),
    combina(Primera,[1],Temp),
    ceros([],N-Target,Segunda),
    combina(Temp,Segunda,Res).




%COMPOSICION - TODAVIA NO SIRVE
%Tenemos que hacer un polinomio con Ca para sumar
comp_pol([],_,[]).
comp_pol([Ca|A],B,C):-
    comp_pol(A,B,Temp),
    write(Ca),write(Temp),
    producto_pol(B,Temp,Producto),
    write(Producto),
    suma_pol(Ca,Producto,C).

%DIFERENCIAR
%La idea es implementar algo parecido al de java.
%cada coeficiente nuevo es indice*coefsViejos[indice]
%donde 1<=indice<=coeficientesViejos.length

%Funciones "wrappers"
%Si el polinomio es vacio su derivada tambien.
dif_pol([],[]).
%Si es no vacio descartamos el primer elemento, "inicializamos" el indice en 1,
% y llamamos a la funcion "helper".
dif_pol([_|A],Res):-
    dif_pol(A,1,Res).

%Si el polinomio es vacio su derivada tambien, sin importar el indice.
dif_pol([],_,[]).
%Agregamos al polinomio resultado indice*coefsViejos[indice] y llamamos recursivamente.
dif_pol([Ca|A],Indice,[Cc|C]):-
    Cc is (Ca*Indice),
    dif_pol(A,Indice+1, C).

%dif_pol([1,2,3,4],C).