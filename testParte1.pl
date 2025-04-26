% tests.pl

% Cargar los predicados (copiar/pegar el contenido original aquí o usar consult/1 si están en otro archivo).

% Casos de prueba para los predicados

% ----------------------------
% pertenece/2
% ----------------------------
test_pertenece :-
    pertenece(3, [1,2,3,4]),
    \+ pertenece(5, [1,2,3,4]),
    pertenece(a, [b,a,c]),
    write('pertenece/2 OK'), nl.

% ----------------------------
% unico/2
% ----------------------------
test_unico :-
    unico(3, [1,2,3,4]),
    \+ unico(2, [1,2,2,3]),
    write('unico/2 OK'), nl.

% ----------------------------
% elegir_primero/3
% ----------------------------
test_elegir_primero :-
    elegir_primero(3, [1,2,3,4,3], [1,2,4,3]),
    elegir_primero(a, [a,b,a], [b,a]),
    elegir_primero(x, [], []),
    write('elegir_primero/3 OK'), nl.

% ----------------------------
% repetido/2
% ----------------------------
test_repetido :-
    repetido(2, [1,2,3,2,4]),
    \+ repetido(3, [1,2,3,4]),
    write('repetido/2 OK'), nl.

% ----------------------------
% pertenece_veces/3
% ----------------------------
test_pertenece_veces :-
    pertenece_veces(2, [1,2,3,2,4], 2),
    pertenece_veces(5, [1,2,3,4], 0),
    pertenece_veces(a, [a,b,a,a], 3),
    write('pertenece_veces/3 OK'), nl.

% ----------------------------
% pares/2
% ----------------------------
test_pares :-
    pares([1,2,3,4,5,6], [2,4,6]),
    pares([1,3,5], []),
    pares([], []),
    write('pares/2 OK'), nl.

% ----------------------------
% pares_impares/3
% ----------------------------
test_pares_impares :-
    pares_impares([1,2,3,4,5,6], [2,4,6], [1,3,5]),
    pares_impares([], [], []),
    write('pares_impares/3 OK'), nl.

% ----------------------------
% ordenada/2
% ----------------------------
test_ordenada :-
    ordenada([4,1,3,2], [1,2,3,4]),
    ordenada([5], [5]),
    ordenada([], []),
    write('ordenada/2 OK'), nl.

% Ejecutar todos los tests
run_tests :-
    test_pertenece,
    test_unico,
    test_elegir_primero,
    test_repetido,
    test_pertenece_veces,
    test_pares,
    test_pares_impares,
    test_ordenada.
