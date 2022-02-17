poly_sum(Ms,[],Ms) :- Ms = [|].
poly_sum([],Ns,Ns).
poly_sum([M|Ms], [N|Ns], [S|Ss]) :-
   S is M+N,
   poly_sum(Ms, Ns, Ss).

scal_prod([],_Sc,[]).
scal_prod([M|Ms], Sc, [P|Ps]) :-
   P is M*Sc,
   scal_prod(Ms, Sc, Ps).

poly_prod(_,[],[]).
poly_prod(Ms,[N|Ns], Xs2) :-
   poly_prod(Ms,Ns, Xs1),
   scal_prod(Ms, N, Ps),
    poly_sum(Ps, [0.0|Xs1], Xs2).

/*
?- poly_prod([1,2,3,4],[5,6,7],Xs).
Xs = [5.0, 16.0, 34.0, 52, 45, 28] ;
false.
*/