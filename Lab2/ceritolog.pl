:- module(ceritolog,
[
tablero/2, 
% tablero(+N,?Tablero)
% Devuelve un tablero de tamaño N vacío, o sea una matriz que representa un
% tablero vacío de juego como la descrita en la letra del laboratorio.
% Como dice la letra, el tablero debe ser de tamaño NxN que señalan los vertices
% pero las filas y columnas marcan las aristas, por lo que el tablero es de tamño (N-1)x(N-1)

fin_del_juego/4, 
% fin_del_juego(+Tablero,?P1,?P2,?Ganador)
% Dado un tablero, el predicado es verdadero si el tablero representa un juego
% finalizado, y devuelve % la cantidad de puntos del jugador 1 en P1, la
% cantidad de puntos del jugador 2 en P2, y un string % que indica si alguno
% ganó, en el formato: "Gana el jugador 1", "Gana el jugador 2", o "Empate".
% En caso de que no sea el fin del juego, el predicado falla.

jugada_humano/8, 
% jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y la línea elegida por el
% jugador humano con las variables F-C-D, y devuelve: el tablero modificado con
% la línea marcada (y celdas marcadas en caso de que sea necesario), de quién es
% el siguiente turno (Turno2), y una lista de celdas que se capturaron con esta
% acción en formato [Fila,Columna]. Por ejemplo: [[1,2],[1,3]]

jugada_maquina/9, 
% jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
% Se le envía un tablero, de quién es el turno (1 o 2) y el Nivel de minimax,
% debe elegir una jugada a realizar por el jugador controlado por la computadora.
% El predicado devuelve: el tablero modificado luego de la jugada, de quién es
% el siguiente turno (Turno2), y una lista de celdas que se cerraron con esta
% acción en formato [Fila,Columna], de la misma forma que en el predicado anterior.

sugerencia_jugada/6 % sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
% Utiliza la estrategia de minimax para calcular una buena jugada para sugerirle
% a un jugador humano.
]).

% ======================================================================================================= %
% ======================================================================================================= %

% tablero(+N, -Tablero)
tablero(N, Tablero) :-
    N > 1,
    N1 is N - 1,
    fila_vacia(N1, Fila),
    construir_tablero(N1, Fila, Tablero).

% fila_vacia(+N, -Fila)
fila_vacia(0, []).
fila_vacia(N, [c(0, 0, 0) | Resto]) :-
    N > 0,
    N1 is N - 1,
    fila_vacia(N1, Resto).

% construir_tablero(+CantFilas, +Fila, -Tablero)
construir_tablero(0, _, []).
construir_tablero(N, Fila, [Fila | Resto]) :-
    N > 0,
    N1 is N - 1,
    construir_tablero(N1, Fila, Resto).

% ======================================================================================================= %
% ======================================================================================================= %

% fin_del_juego(+Tablero, ?P1, ?P2, ?Ganador)
fin_del_juego(Tablero, P1, P2, Ganador) :-
    todas_lineas_marcadas(Tablero),
    contar_puntos(Tablero, P1, P2),
    determinar_ganador(P1, P2, Ganador).

% todas_lineas_marcadas(+Tablero)
todas_lineas_marcadas(Tablero) :-
    forall(member(Fila, Tablero),
           forall(member(c(_, _, Propietario), Fila),
                  Propietario \== 0)).

% contar_puntos(+Tablero, -P1, -P2)
contar_puntos(Tablero, P1, P2) :-
    contar_puntos_aux(Tablero, 0, 0, P1, P2).

% contar_puntos_aux(+Tablero, +Acc1, +Acc2, -P1, -P2)
contar_puntos_aux([], P1, P2, P1, P2).
contar_puntos_aux([Fila|Resto], Acc1, Acc2, P1, P2) :-
    contar_puntos_fila(Fila, 0, 0, P1Fila, P2Fila),
    NuevoAcc1 is Acc1 + P1Fila,
    NuevoAcc2 is Acc2 + P2Fila,
    contar_puntos_aux(Resto, NuevoAcc1, NuevoAcc2, P1, P2).

% contar_puntos_fila(+Fila, +Acc1, +Acc2, -P1, -P2)
contar_puntos_fila([], P1, P2, P1, P2).
contar_puntos_fila([c(_, _, 1)|Resto], Acc1, Acc2, P1, P2) :-
    NuevoAcc1 is Acc1 + 1,
    contar_puntos_fila(Resto, NuevoAcc1, Acc2, P1, P2).
contar_puntos_fila([c(_, _, 2)|Resto], Acc1, Acc2, P1, P2) :-
    NuevoAcc2 is Acc2 + 1,
    contar_puntos_fila(Resto, Acc1, NuevoAcc2, P1, P2).
contar_puntos_fila([c(_, _, 0)|Resto], Acc1, Acc2, P1, P2) :-
    contar_puntos_fila(Resto, Acc1, Acc2, P1, P2).

% determinar_ganador(+P1, +P2, -Ganador)
determinar_ganador(P1, P2, "Gana el jugador 1") :-
    P1 > P2, !.
determinar_ganador(P1, P2, "Gana el jugador 2") :-
    P2 > P1, !.
determinar_ganador(P1, P2, "Empate") :-
    P1 =:= P2.


% ======================================================================================================= %
% ======================================================================================================= %

