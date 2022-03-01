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
   suma_pol(Esc, [0.0|Rec], C), %sumamos ambos resultados anteriores en C.
   !. 

%GRADO
%La posicion del ultimo coeficiente no cero

grado(Pol,Grado):- %wrapper (funcion publica)
    grado(Pol,0,0,Grado),
    !.
%si pol. vacio el grado es el indice del ultimo coef. no cero
grado([],_,Ultimo,Grado):- 
    Grado is Ultimo,
    !.  
%si la cabeza es cero solo incrementamos el indice
grado([0|Pol],Index,Ultimo,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Ultimo,Grado),
    !.
%si no es cero setteamos ultimo al indice actual
grado([_|Pol],Index,_,Grado):-
    Index2 is Index+1,
    grado(Pol,Index2,Index,Grado),
    !.



%EVALUAR
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
%comp_pol([1,2,3,4],[5,0,3],C).

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
toString(Pol):- %pregunta prof: tiene que regresar o solo imprimir?
    toString(Pol,0,''),
    !.
%Caso base: recorrimos toda la lista construyendo el string en Res.
%Entonces solo imprimimos
toString([], _, Res) :-
    write(Res),
    !.

%Si la cabeza (coeficiente actual) es cero nos saltamos la concatenacion y avanzamos.
toString([0|T], Counter, Res):-
    Counter2 is Counter+1,
    toString(T,Counter2,Res),
    !.

%Para el coeficiente de grado cero no incluimos "x^"
toString([H|T], 0, Res):-
    atom_concat(Res, H, Este),
    toString(T, 1, Este),
    !.

toString([H|T], Counter, Res):-
    mas(Res,M),
    atom_concat(Res, M, Sb),
    atom_concat(Sb, H, Sb1),
    atom_concat(Sb1, 'x', Sb2),
    atom_concat(Sb2, '^', Sb3),
    atom_concat(Sb3, Counter, Este),
    Counter2 is Counter + 1,
    toString(T, Counter2, Este),
    !.

mas('',''):-!.
mas(_,' + '):-!.

%MAIN
p([1,2,3,4]).
q([5,0,3]).
main:-
    p(P),
    write("p(x) = "),toString(P),nl,
    q(Q),
    write("q(x) = "),toString(Q),
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

%PREGUNTAS PROF:
% el toString tiene que imprimir o 'regresar' la cadena
% el toString va de mayor a menor coeficiente o como lo tenemos nosotros?


