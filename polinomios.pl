% -------- SUMA
% Si B es vacío, la suma es igual a A.
suma_pol(A,[],A) :- A = [_|_].
% Si A es vacío, la suma es igual a B.
suma_pol([],B,B):- !.
% Para sumar dos polinomios, se suman sus cabezas y llama recursivamente.
suma_pol([Ca|A], [Cb|B], [Cc|C]) :-
   Cc is Ca+Cb,
   suma_pol(A, B, C).

% -------- RESTA
% Negamos B usando producto escalar y después sumamos
resta_pol(A,B,C):-
    producto_Esc_pol(B,-1,Bneg),
    suma_pol(A,Bneg,C).


% -------- PRODUCTO ESCALAR
% Si el polinomio es vacío, su producto escalara también.
producto_Esc_pol([],_,[]):-!.
% Si es no vacío, se multiplica su cabeza con el escalar y llama recursivamente.
producto_Esc_pol([Ca|A], Esc, [Cc|C]) :-
   Cc is Ca*Esc,
   producto_Esc_pol(A, Esc, C).

% -------- PRODUCTO
% Si B es vacío, el producto es vacío.
producto_pol(_,[],[]):-!.
% Si son no vacíos
producto_pol(A,[Cb|B], C) :-
   producto_pol(A,B, Rec), %quitamos cabeza de B y llamamos recursivamente.
   producto_Esc_pol(A, Cb, Esc), %calculamos el prod. Esc con la cabeza de B.
   suma_pol(Esc, [0.0|Rec], C), %sumamos ambos resultados anteriores en C.
   !.

% -------- GRADO
% La posición del último coeficiente no cero
grado(Pol,Grado):- % wrapper (función pública)
    grado(Pol,0,0,Grado),
    !.
% Caso base: Si el pol. es vacío, el grado es el índice del último coef. no cero
grado([],_,Ultimo,Grado):-
    Grado is Ultimo,
    !.
% Si la cabeza es cero, sólo incrementamos el índice y recursamos.
grado([0|Pol],Index,Ultimo,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Ultimo,Grado),
    !.
% Si no es cero, el último ahora es el índice actual y recursamos.
grado([_|Pol],Index,_,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Index,Grado),
    !.

% -------- EVALUAR
% Caso base:
eval_pol([],_,0).
eval_pol([Ca|A],X,Res):-
    eval_pol(A,X,Temp),
    Res is (Temp*X)+Ca.


% -------- COMPOSICIÓN
comp_pol([],_,[]):-!.
% Usamos la definición recursiva de Horner
comp_pol([Ca|A],B,C):-
    comp_pol(A,B,Temp),
    producto_pol(B,Temp,Producto),
    suma_pol([Ca],Producto,C),
    !.

% -------- DIFERENCIAR
% dif_pol(i,i), dif_pol(i,o)
% La idea es implementar algo parecido al de java.
% cada coeficiente nuevo es indice*coefsViejos[indice]
% donde 1<=indice<=coeficientesViejos.length

% Funciones "wrappers"
% Si el polinomio es vacío, su derivada también.
dif_pol([],[]).
% Si es no vacío descartamos el primer elemento, "inicializamos" el índice en 1
% y llamamos a la funcion "helper".
dif_pol([_|A],Res):-
    dif_pol(A,1,Res).

% Si el polinomio es vacío, su derivada también, sin importar el índice.
dif_pol([],_,[]).
% Agregamos al polinomio resultado indice*coefsViejos[indice] y llamamos recursivamente.
dif_pol([Ca|A],Indice,[Cc|C]):-
    Cc is (Ca*Indice),
    dif_pol(A,Indice+1, C).


% -------- TO STRING
% toString(i)
% función wrapper (publica)

% Predicados a utilizar
% Determina si agregamos 'x^{Index}' al str dependiedo del coeficiente e índice del término.
% Si el coeficiente es cero, nuestra representación es ''.
terminoActual(0,_,''):-!.
% Si nuestro índice o potencia es 0, no incluimos 'x^'
terminoActual(Coef,0,Coef):-!.
% Si los anteriores no se cumplen, entonces incluimos 'x^{Indice}'
terminoActual(Coef,Index,Res):-
    atom_concat(Coef,'x^',Sb1),
    atom_concat(Sb1,Index,Res),
    !.
% Determina si agregamos ' + ' al str dependiendo del str armado recursivamente y del término actual.
mas('',_,''):-!.
mas(_,'',''):-!.
mas(_,_,' + '):-!.

% Función wrapper que le asigna a Res la representación
toString(Pol,Res):-
    toString(Pol,0,Res),
    !.
% Función wrapper que imprime directamente
toString(Pol):-
    toString(Pol,Res),
    write(Res),
    !.
% Caso base: si lista vacía, nuestra representación es ''.
toString([],_,''):-!.
% Llamamos recursivamente y concatenamos el término actual (y el más).
toString([Cabeza|Pol],Index,Sb):-
    Index2 is Index+1,
    toString(Pol,Index2,Rec),
    terminoActual(Cabeza,Index,TerminoActual),
    mas(Rec,TerminoActual,M),
    atom_concat(M,TerminoActual,Sb3),
    atom_concat(Rec,Sb3,Sb),
    !.


% -------- MAIN
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
