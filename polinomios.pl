
%SUMA
%Si B es vacio la suma es igual a A
suma_pol(A,[],A) :- A = [_|_].
%Si A es vacio la suma es igual a B.
suma_pol([],B,B):- !.
%Para sumar dos polinomios se suman sus cabezas y llama recursivamente
suma_pol([Ca|A], [Cb|B], [Cc|C]) :-
   Cc is Ca+Cb,
   suma_pol(A, B, C).

%RESTA
%Negamos B usando producto escalar y despues sumamos
resta_pol(A,B,C):-
    producto_Esc_pol(B,-1,Bneg),
    suma_pol(A,Bneg,C).


%PRODUCTO ESCALAR
%Si el polinomio es vacio su producto Esc tambien.
producto_Esc_pol([],_,[]):-!.
%Si es no vacio, se multiplica su cabeza con el Esc y llama recursivamente.
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
   suma_pol(Esc, [0.0|Rec], C), %sumamos ambos resultados anteriores en C.
   !.

%GRADO
%La posicion del ultimo coeficiente no cero
grado(Pol,Grado):- %wrapper (funcion publica)
    grado(Pol,0,0,Grado),
    !.
%Caso base: Si el pol. es vacio el grado es el indice del ultimo coef. no cero
grado([],_,Ultimo,Grado):-
    Grado is Ultimo,
    !.
%Si la cabeza es cero solo incrementamos el indice y recursamos.
grado([0|Pol],Index,Ultimo,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Ultimo,Grado),
    !.
%Si no es cero el ultimo ahora es el indice actual y recursamos.
grado([_|Pol],Index,_,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Index,Grado),
    !.

%EVALUAR
%Caso base:
eval_pol([],_,0).
eval_pol([Ca|A],X,Res):-
    eval_pol(A,X,Temp),
    Res is (Temp*X)+Ca.


%COMPOSICION
comp_pol([],_,[]):-!.
%Usamos la definicion recursiva de Horner
comp_pol([Ca|A],B,C):-
    comp_pol(A,B,Temp),
    producto_pol(B,Temp,Producto),
    suma_pol([Ca],Producto,C),
    !.

%DIFERENCIAR
%dif_pol(i,i), dif_pol(i,o)
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


% TO STRING
% toString(i)
%funcion wrapper (publica)

%Predicados a utilizar
%Determina si agregamos 'x^{Index}' al str dependiedo del coeficiente e indice del termino.
%Si el coeficiente es cero nuestra representacion es ''.
terminoActual(0,_,''):-!.
%Si nuestro indice o potencia es 0 no incluimos 'x^'
terminoActual(Coef,0,Coef):-!.
%Si los anteriores no se cumplen entonces incluimos 'x^{Indice}'
terminoActual(Coef,Index,Res):-
    atom_concat(Coef,'x^',Sb1),
    atom_concat(Sb1,Index,Res),
    !.
%Determina si agregamos ' + ' al str dependiendo del str armado recursivamente y del termino actual.
mas('',_,''):-!.
mas(_,'',''):-!.
mas(_,_,' + '):-!.

%Funcion wrapper que le asigna a Res la representacion
toString(Pol,Res):-
    toString(Pol,0,Res),
    !.
%Funcion wrapper que imprime directamente
toString(Pol):-
    toString(Pol,Res),
    write(Res),
    !.
%Caso base: si lista vacia nuestra representacion es ''.
toString([],_,''):-!.
%Llamamos recursivamente y concatenamos el termino actual (y el mas).
toString([Cabeza|Pol],Index,Sb):-
    Index2 is Index+1,
    toString(Pol,Index2,Rec),
    terminoActual(Cabeza,Index,TerminoActual),
    mas(Rec,TerminoActual,M),
    atom_concat(M,TerminoActual,Sb3),
    atom_concat(Rec,Sb3,Sb),
    !.


%MAIN
p([1,2,3,4]).
q([5,0,3]).
main:-
    p(P),
    write("p(x) = "),toString(P),nl,
    q(Q),
    write("q(x) = "),toString(Q),nl,
    suma_pol(P,Q,R), %p+q
    write("q(x) + p(x) = "),toString(R),nl,
    producto_pol(P,Q,S), %p*q
    write("q(x) * p(x) = "),toString(S),nl,
    comp_pol(P,Q,T),
    write("p(q(x)) = "), toString(T),nl,
    resta_pol([0],P,Z), %0-p
    write("0 - p(x) = "),toString(Z),nl,
    eval_pol(P,3,E), %p(3)
    write("p(3) = "),write(E),nl,
    dif_pol(P,D), %p'
    write("p'(x) = "),toString(D),nl,
    dif_pol(D,D2), %p''
    write("p''(x) = "),toString(D2),nl,
    !.
main.
