% ============================================================================================================ %
%                                           1. Predicados varios                                               %
% ============================================================================================================ %

% pertenece(?X,?L) ← El elemento X pertenece a la lista L.
pertenece(X, [X|_]).
pertenece(X,[_| R]) :- pertenece(X, R).


% unico(+X,+L) ← El elemento X tiene una única ocurrencia en la lista L.
unico(X, L) :- 
    pertenece(X, L),
    not(repetido(X, L)).

    % Forma alternativa:
        % unico(X, L) :- contar_ocurrencias(X, L, 1).

    % Predicado auxiliar para contar ocurrencias
        % contar_ocurrencias(_, [], 0).
        % contar_ocurrencias(X, [X|Resto], N) :-
        %     contar_ocurrencias(X, Resto, N1),
        %     N is N1 + 1.
        % contar_ocurrencias(X, [Y|Resto], N) :-
        %     X \= Y,
        %     contar_ocurrencias(X, Resto, N).

% elegir_primero(+X,+L1,?L2) ← La lista L2 contiene los elementos de L1 sin la primera ocurrencia de X, 
%                              si X pertenece a L2.
elegir_primero(_,[],[]).
elegir_primero(X,[X|T],T).
elegir_primero(X,[Y|T],[Y|L]):-
        elegir_primero(X,T,L).

% repetido(+X,?L) ← El elemento X tiene más de una ocurrencia en la lista L.

% pertenece_veces(+X,+L,?N) ← El elemento X ocurre N veces en la lista L
pertenece_veces(_, [], 0).
pertenece_veces(X, [X|R], N) :- pertenece_veces(X, R, N1), N1 is N - 1.
pertenece_veces(X, [Y|R], N) :- X \= Y, pertenece_veces(X, R, N).

% pares(+L1,?L2) ← L2 es la lista que contiene los elementos pares de L1.
pares([],[]).
pares([X|T], [X|P]):-
    0 is X mod 2,
    pares(T, P).
pares([X|T], P):-
    1 is X mod 2,
    pares(T, P).    


% pares_impares(+L1,?L2,?L3) ← L2 es una lista con los valores pares de la lista L1,
%                              L3 es una lista con los valores impares de la lista L1.
pares_impares([], [], []).
pares_impares([X|Resto], [X|Pares], Impares) :-
    0 is X mod 2,
    pares_impares(Resto, Pares, Impares).
pares_impares([X|Resto], Pares, [X|Impares]) :-
    1 is X mod 2,
    pares_impares(Resto, Pares, Impares).

% ordenada(+L1,?L2) ← L2 contiene los elementos de L1 ordenados de menor a mayor,
%                     utilizando el algoritmo de ordenación por selección. Las listas contienen 
%                     valores enteros y no hay elementos repetidos.

% ============================================================================================================ %
%                                           2. Palabras cruzadas                                               %
% ============================================================================================================ %
palabra([a,l]).
palabra([l,o]).
palabra([a,s]).
palabra([s,i]).
palabra([l,a]).
palabra([l,e]).
palabra([a,s]).
palabra([s,e]).
palabra([a,l,a]).
palabra([c,a,n]).
palabra([a,s,a]).
palabra([a,c,a]).
palabra([a,m,a]).
palabra([c,a,l]).
palabra([m,a,l]).
palabra([m,a,s]).
palabra([l,a,s]).
palabra([a,n,a]).
palabra([m,e,s,a]).
palabra([o,s,a,s]).
palabra([r,a,n,a]).
palabra([a,s,e,s]).
palabra([m,e,r,a]).
palabra([e,s,a,s]).
palabra([s,a,n,e]).
palabra([a,s,a,s]).
palabra([c,a,s,a]).
palabra([m,o,r,a]).

% Predicados para ambas soluciones: 

% matrizN(+N,-M) ← M es una matriz de tamaño N X N que en sus celdas contiene variables,
% de modo que representa un tablero vacío. La matriz está representada como lista de listas.
% ?- matriz(4,M).
% M = [[_,_,_,_], [_,_,_,_], [_,_,_,_], [_,_,_,_]]



% traspuesta(?M,?MT) ← MT es la traspuesta de la matriz M.
% ?- traspuesta([[A,B],[C,D],MT).
% MT = [[A,C],[B,D]]

% Parte 2.1:

% Parte 2.2:

% Parte 2.3:








