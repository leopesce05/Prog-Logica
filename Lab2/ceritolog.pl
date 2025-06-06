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

% jugada_humano(+Tablero,+Turno,+F,+C,+D,?Tablero2,?Turno2,?Celdas)
jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas) :-
    jugada_valida(Tablero, F, C, D),
    marcar_linea(Tablero, F, C, D, TableroConLinea),
    verificar_capturas(TableroConLinea, F, C, D, Turno, Tablero2, Celdas),
    determinar_siguiente_turno(Turno, Celdas, Turno2).

% jugada_valida(+Tablero, +F, +C, +D)
% Verifica que la jugada sea válida (la línea no esté ya marcada)
jugada_valida(Tablero, F, C, h) :-
    length(Tablero, N),
    F >= 1, F =< N,
    C >= 1, C =< N-1,
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(H, _, _)),
    H =:= 0.

jugada_valida(Tablero, F, C, v) :-
    length(Tablero, N),
    F >= 1, F =< N-1,
    C >= 1, C =< N,
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(_, V, _)),
    V =:= 0.

% marcar_linea(+Tablero, +F, +C, +D, -TableroNuevo)
% Marca una línea en el tablero
marcar_linea(Tablero, F, C, h, TableroNuevo) :-
    marcar_linea_horizontal(Tablero, F, C, TableroNuevo).

marcar_linea(Tablero, F, C, v, TableroNuevo) :-
    marcar_linea_vertical(Tablero, F, C, TableroNuevo).

% marcar_linea_horizontal(+Tablero, +F, +C, -TableroNuevo)
marcar_linea_horizontal(Tablero, F, C, TableroNuevo) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(_, V, J)),
    replace_nth1(C, Fila, c(1, V, J), FilaNueva),
    replace_nth1(F, Tablero, FilaNueva, TableroNuevo).

% marcar_linea_vertical(+Tablero, +F, +C, -TableroNuevo)
marcar_linea_vertical(Tablero, F, C, TableroNuevo) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(H, _, J)),
    replace_nth1(C, Fila, c(H, 1, J), FilaNueva),
    replace_nth1(F, Tablero, FilaNueva, TableroNuevo).

% replace_nth1(+Index, +List, +Element, -NewList)
% Reemplaza el elemento en la posición Index de List con Element
replace_nth1(1, [_|T], Element, [Element|T]).
replace_nth1(N, [H|T], Element, [H|NewT]) :-
    N > 1,
    N1 is N - 1,
    replace_nth1(N1, T, Element, NewT).

% verificar_capturas(+TableroConLinea, +F, +C, +D, +Turno, -Tablero2, -Celdas)
% Verifica qué celdas se capturaron y las asigna al jugador
verificar_capturas(TableroConLinea, F, C, D, Turno, Tablero2, Celdas) :-
    celdas_afectadas(F, C, D, CeldasPosibles),
    verificar_celdas_capturadas(TableroConLinea, CeldasPosibles, Turno, Tablero2, Celdas).

% celdas_afectadas(+F, +C, +D, -CeldasPosibles)
% Determina qué celdas podrían haberse capturado con esta línea
celdas_afectadas(F, C, h, CeldasPosibles) :-
    % Una línea horizontal F-C-h puede afectar:
    % - La celda (F-1, C) si F > 1 (celda de arriba)
    % - La celda (F, C) si F <= N-1 (celda de abajo)
    F1 is F - 1,
    findall([Fila, C], 
            (member(Fila, [F1, F]), Fila >= 1), 
            CeldasTemp),
    % Filtrar celdas válidas (dentro del rango capturable)
    include(celda_capturable, CeldasTemp, CeldasPosibles).

celdas_afectadas(F, C, v, CeldasPosibles) :-
    % Una línea vertical F-C-v puede afectar:
    % - La celda (F, C-1) si C > 1 (celda de la izquierda)
    % - La celda (F, C) si C <= N-1 (celda de la derecha)
    C1 is C - 1,
    findall([F, Col], 
            (member(Col, [C1, C]), Col >= 1), 
            CeldasTemp),
    % Filtrar celdas válidas (dentro del rango capturable)
    include(celda_capturable, CeldasTemp, CeldasPosibles).

% celda_capturable(+[F,C])
% Verifica si una celda está en el rango capturable (no es borde)
celda_capturable([F, C]) :-
    F >= 1, C >= 1.

% verificar_celdas_capturadas(+Tablero, +CeldasPosibles, +Turno, -TableroFinal, -CeldasCapturadas)
verificar_celdas_capturadas(Tablero, [], _, Tablero, []).
verificar_celdas_capturadas(Tablero, [[F,C]|Resto], Turno, TableroFinal, CeldasCapturadas) :-
    (celda_esta_cerrada(Tablero, F, C) ->
        capturar_celda(Tablero, F, C, Turno, TableroTemp),
        CeldasCapturadas = [[F,C]|RestoCeldas]
    ;
        TableroTemp = Tablero,
        CeldasCapturadas = RestoCeldas
    ),
    verificar_celdas_capturadas(TableroTemp, Resto, Turno, TableroFinal, RestoCeldas).

% celda_esta_cerrada(+Tablero, +F, +C)
% Verifica si una celda está completamente cerrada (4 líneas marcadas)
celda_esta_cerrada(Tablero, F, C) :-
    length(Tablero, N),
    F >= 1, F =< N-1,
    C >= 1, C =< N-1,
    % Línea superior: F-C-h
    nth1(F, Tablero, FilaF),
    nth1(C, FilaF, c(H1, _, _)),
    H1 =:= 1,
    % Línea izquierda: F-C-v
    nth1(C, FilaF, c(_, V1, _)),
    V1 =:= 1,
    % Línea inferior: (F+1)-C-h
    F1 is F + 1,
    nth1(F1, Tablero, FilaF1),
    nth1(C, FilaF1, c(H2, _, _)),
    H2 =:= 1,
    % Línea derecha: F-(C+1)-v
    C1 is C + 1,
    nth1(C1, FilaF, c(_, V2, _)),
    V2 =:= 1.

% capturar_celda(+Tablero, +F, +C, +Turno, -TableroNuevo)
capturar_celda(Tablero, F, C, Turno, TableroNuevo) :-
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(H, V, _)),
    replace_nth1(C, Fila, c(H, V, Turno), FilaNueva),
    replace_nth1(F, Tablero, FilaNueva, TableroNuevo).

% determinar_siguiente_turno(+TurnoActual, +CeldasCapturadas, -SiguienteTurno)
determinar_siguiente_turno(Turno, [], SiguienteTurno) :-
    (Turno =:= 1 -> SiguienteTurno = 2; SiguienteTurno = 1).
determinar_siguiente_turno(Turno, [_|_], Turno).

% ======================================================================================================= %
% ======================================================================================================= %

% jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
jugada_maquina(Tablero, Turno, Nivel, F, C, D, Tablero2, Turno2, Celdas) :-