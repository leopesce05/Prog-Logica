% Predicado para crear un tablero vacío de tamaño NxN
tablero(0, []).
tablero(N, [Fila|Resto]) :-
    N > 0,
    crearListaN(N, Fila),
    N1 is N - 1,
    crearMatrizN(N, Resto, N1).

crearListaN(0, []).
crearListaN(N, [0|Resto]) :-
    N > 0,
    N1 is N - 1,
    crearListaN(N1, Resto).

crearMatrizN(_,[],0).
crearMatrizN(N, [F|R], Ni):-
    Ni > 0,
    Nj is Ni-1,
    crearListaN(N, F), 
    crearMatrizN(N, R, Nj).

% Predicado para verificar si el juego ha terminado
fin_del_juego(Tablero, P1, P2, Ganador) :-
    contar_puntos(Tablero, P1, P2),
    (P1 > P2 -> Ganador = "Gana el jugador 1"
    ; P2 > P1 -> Ganador = "Gana el jugador 2"
    ; Ganador = "Empate").

% Predicado auxiliar para contar puntos
contar_puntos(Tablero, P1, P2) :-
    contar_celdas(Tablero, 1, P1),
    contar_celdas(Tablero, 2, P2).

% Predicado para contar celdas de un jugador
contar_celdas(Tablero, Jugador, Total) :-
    findall(1, (member(Fila, Tablero), member(Jugador, Fila)), Lista),
    length(Lista, Total).

% Predicado para realizar una jugada humana
jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas) :-
    validar_jugada(Tablero, F, C, D),
    realizar_jugada(Tablero, Turno, F, C, D, Tablero2, Celdas),
    (Celdas = [] -> Turno2 is 3 - Turno ; Turno2 = Turno).

% Predicado para validar una jugada
validar_jugada(Tablero, F, C, D) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, Valor),
    Valor = 0.

% Predicado para realizar una jugada
realizar_jugada(Tablero, Turno, F, C, D, Tablero2, Celdas) :-
    copiar_tablero(Tablero, Tablero2),
    marcar_linea(Tablero2, F, C, D, Turno),
    verificar_celdas(Tablero2, F, C, D, Celdas).

% Predicado para copiar un tablero
copiar_tablero([], []).
copiar_tablero([F|R], [F2|R2]) :-
    copiar_fila(F, F2),
    copiar_tablero(R, R2).

copiar_fila([], []).
copiar_fila([X|R], [X|R2]) :-
    copiar_fila(R, R2).

% Predicado para marcar una línea
marcar_linea(Tablero, F, C, D, Turno) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, _, Fila2),
    nth1(C, Fila2, Turno, _).

% Predicado para verificar celdas completadas
verificar_celdas(Tablero, F, C, D, Celdas) :-
    findall([F2,C2], celda_completada(Tablero, F2, C2), Celdas).

% Predicado para verificar si una celda está completa
celda_completada(Tablero, F, C) :-
    F1 is F - 1,
    C1 is C - 1,
    celda_valida(Tablero, F1, C1),
    celda_rodeada(Tablero, F1, C1).

% Predicado para verificar si una celda está rodeada
celda_rodeada(Tablero, F, C) :-
    F1 is F + 1,
    C1 is C + 1,
    obtener_linea(Tablero, F, C, 1, L1),
    obtener_linea(Tablero, F, C1, 2, L2),
    obtener_linea(Tablero, F1, C, 3, L3),
    obtener_linea(Tablero, F1, C1, 4, L4),
    L1 \= 0, L2 \= 0, L3 \= 0, L4 \= 0.

% Predicado para obtener una línea
obtener_linea(Tablero, F, C, D, Valor) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, Valor).

% Predicado para jugada de la máquina usando minimax
jugada_maquina(Tablero, Turno, Nivel, F, C, D, Tablero2, Turno2, Celdas) :-
    minimax(Tablero, Turno, Nivel, F, C, D, _),
    jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas).

% Predicado para sugerir una jugada
sugerencia_jugada(Tablero, Turno, Nivel, F, C, D) :-
    minimax(Tablero, Turno, Nivel, F, C, D, _).

% Implementación básica de minimax
minimax(Tablero, Turno, Nivel, F, C, D, Valor) :-
    Nivel > 0,
    encontrar_jugadas_validas(Tablero, Jugadas),
    evaluar_jugadas(Jugadas, Tablero, Turno, Nivel, F, C, D, Valor).

% Predicado para encontrar jugadas válidas
encontrar_jugadas_validas(Tablero, Jugadas) :-
    findall([F,C,D], validar_jugada(Tablero, F, C, D), Jugadas).

% Predicado para evaluar jugadas
evaluar_jugadas([], _, _, _, _, _, _, 0).
evaluar_jugadas([[F,C,D]|R], Tablero, Turno, Nivel, MejorF, MejorC, MejorD, MejorValor) :-
    jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas),
    length(Celdas, Puntos),
    Nivel1 is Nivel - 1,
    minimax(Tablero2, Turno2, Nivel1, _, _, _, Valor),
    Valor2 is Valor + Puntos,
    evaluar_jugadas(R, Tablero, Turno, Nivel, F2, C2, D2, Valor3),
    (Valor2 > Valor3 -> 
        MejorF = F, MejorC = C, MejorD = D, MejorValor = Valor2
    ; 
        MejorF = F2, MejorC = C2, MejorD = D2, MejorValor = Valor3).