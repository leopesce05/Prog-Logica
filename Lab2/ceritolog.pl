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
    fila_vacia(N, Fila),
    construir_tablero(N, Fila, Tablero).

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
    length(Tablero, N),
    N1 is N - 1,
    % Verificar celdas internas
    forall(between(1, N1, F),
           forall(between(1, N1, C),
                  (nth1(F, Tablero, Fila),
                   nth1(C, Fila, c(H, V, _)),
                   H =:= 1,
                   V =:= 1))),
    % Verificar última fila (H=1, V=0)
    forall(between(1, N1, C),
           (nth1(N, Tablero, Fila),
            nth1(C, Fila, c(H, V, _)),
            H =:= 1,
            V =:= 0)),
    % Verificar última columna (H=0, V=1)
    forall(between(1, N1, F),
           (nth1(F, Tablero, Fila),
            nth1(N, Fila, c(H, V, _)),
            H =:= 0,
            V =:= 1)).

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
    F >= 1, F =< N, % No se puede marcar la linea de fuera
    C >= 1, C =< N-1, % No se puede marcar la linea de fuera
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(0, _, _)).

jugada_valida(Tablero, F, C, v) :-
    length(Tablero, N),
    F >= 1, F =< N-1, % No se puede marcar la linea de fuera
    C >= 1, C =< N, % No se puede marcar la linea de fuera
    nth1(F, Tablero, Fila),
    nth1(C, Fila, c(_, 0, _)).

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
    length(TableroConLinea, N),
    celdas_afectadas(F, C, D, N, CeldasPosibles),
    verificar_celdas_capturadas(TableroConLinea, CeldasPosibles, Turno, Tablero2, Celdas).

% celdas_afectadas(+F, +C, +D, +N, -CeldasPosibles)
% Determina qué celdas podrían haberse capturado con esta línea
% N es el tamaño del tablero (matriz NxN con celdas (N-1)x(N-1))
celdas_afectadas(F, C, h, N, CeldasPosibles) :-
    % Una línea horizontal F-C-h puede afectar:
    % - La celda (F-1, C) si F > 1 (celda de arriba)
    % - La celda (F, C) si F <= N-1 (celda de abajo)
    F1 is F - 1,
    findall([Fila, C], 
            (member(Fila, [F1, F]), 
             celda_capturable([Fila, C], N)), 
            CeldasPosibles).

celdas_afectadas(F, C, v, N, CeldasPosibles) :-
    % Una línea vertical F-C-v puede afectar:
    % - La celda (F, C-1) si C > 1 (celda de la izquierda)
    % - La celda (F, C) si C <= N-1 (celda de la derecha)
    C1 is C - 1,
    findall([F, Col], 
            (member(Col, [C1, C]), 
             celda_capturable([F, Col], N)), 
            CeldasPosibles).

% celda_capturable(+[F,C], +N)
% Verifica si una celda está en el rango capturable (1 <= F,C <= N-1)
% donde N es el tamaño del tablero
celda_capturable([F, C], N) :-
    F >= 1, F =< N-1,
    C >= 1, C =< N-1.

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

% % jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
% jugada_maquina(Tablero, Turno, Nivel, F, C, D, Tablero2, Turno2, Celdas) :-
%     minimax(Tablero, Turno, Nivel, _, F, C, D),
%     jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas).

% % sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
% sugerencia_jugada(Tablero, Turno, Nivel, F, C, D) :-
%     minimax(Tablero, Turno, Nivel, _, F, C, D).

% % minimax(+Tablero, +Jugador, +Profundidad, -Valor, -MejorF, -MejorC, -MejorD)
% minimax(Tablero, Jugador, Profundidad, Valor, MejorF, MejorC, MejorD) :-
%     (fin_del_juego(Tablero, P1, P2, _) ->
%         (Jugador = 1 -> Valor is P1 - P2 ; Valor is P2 - P1),
%         MejorF = -1, MejorC = -1, MejorD = h
%     ; Profundidad =< 0 ->
%         evaluar_tablero(Tablero, Jugador, Valor),
%         MejorF = -1, MejorC = -1, MejorD = h
%     ;
%         movimientos_posibles(Tablero, Movimientos),
%         (Movimientos = [] ->
%             evaluar_tablero(Tablero, Jugador, Valor),
%             MejorF = -1, MejorC = -1, MejorD = h
%         ;
%             evaluar_movimientos(Tablero, Jugador, Profundidad, Movimientos, 
%                               Valor, MejorF, MejorC, MejorD)
%         )
%     ).

% % movimientos_posibles(+Tablero, -Movimientos)
% movimientos_posibles(Tablero, Movimientos) :-
%     length(Tablero, N),
%     findall([F,C,D], 
%             (between(1, N, F), between(1, N, C), member(D, [h,v]),
%              jugada_valida(Tablero, F, C, D)), 
%             Movimientos).

% % evaluar_movimientos(+Tablero, +Jugador, +Profundidad, +Movimientos, -MejorValor, -MejorF, -MejorC, -MejorD)
% evaluar_movimientos(Tablero, Jugador, Profundidad, [[F,C,D]], MejorValor, F, C, D) :-
%     !,
%     simular_movimiento(Tablero, Jugador, F, C, D, NuevoTablero, SiguienteJugador),
%     Profundidad1 is Profundidad - 1,
%     minimax(NuevoTablero, SiguienteJugador, Profundidad1, ValorOponente, _, _, _),
%     % Si el siguiente jugador es diferente, negamos el valor (minimax)
%     (SiguienteJugador = Jugador ->
%         MejorValor = ValorOponente
%     ;
%         MejorValor is -ValorOponente
%     ).

% evaluar_movimientos(Tablero, Jugador, Profundidad, [[F,C,D]|RestoMovimientos], MejorValor, MejorF, MejorC, MejorD) :-
%     simular_movimiento(Tablero, Jugador, F, C, D, NuevoTablero, SiguienteJugador),
%     Profundidad1 is Profundidad - 1,
%     minimax(NuevoTablero, SiguienteJugador, Profundidad1, ValorOponente, _, _, _),
%     % Si el siguiente jugador es diferente, negamos el valor (minimax)
%     (SiguienteJugador = Jugador ->
%         ValorActual = ValorOponente
%     ;
%         ValorActual is -ValorOponente
%     ),
%     evaluar_movimientos(Tablero, Jugador, Profundidad, RestoMovimientos, ValorResto, FR, CR, DR),
%     (ValorActual > ValorResto ->
%         MejorValor = ValorActual, MejorF = F, MejorC = C, MejorD = D
%     ;
%         MejorValor = ValorResto, MejorF = FR, MejorC = CR, MejorD = DR
%     ).

% % simular_movimiento(+Tablero, +Jugador, +F, +C, +D, -NuevoTablero, -SiguienteJugador)
% simular_movimiento(Tablero, Jugador, F, C, D, NuevoTablero, SiguienteJugador) :-
%     jugada_humano(Tablero, Jugador, F, C, D, NuevoTablero, SiguienteJugador, _).

% % evaluar_tablero(+Tablero, +Jugador, -Valor)
% % Función de evaluación heurística
% evaluar_tablero(Tablero, Jugador, Valor) :-
%     contar_puntos(Tablero, P1, P2),
%     contar_casilleros_casi_completos(Tablero, Jugador, CasiCompletos),
%     OtroJugador is 3 - Jugador,
%     contar_casilleros_casi_completos(Tablero, OtroJugador, CasiOpuesto),
%     (Jugador = 1 ->
%         Valor is (P1 - P2) * 10 + CasiCompletos - CasiOpuesto
%     ;
%         Valor is (P2 - P1) * 10 + CasiCompletos - CasiOpuesto
%     ).

% % contar_casilleros_casi_completos(+Tablero, +Jugador, -Cantidad)
% contar_casilleros_casi_completos(Tablero, Jugador, Cantidad) :-
%     length(Tablero, N),
%     N1 is N - 1,
%     findall(1, (between(1, N1, F), between(1, N1, C),
%                 casillero_casi_completo(Tablero, F, C)), Lista),
%     length(Lista, Cantidad).

% % casillero_casi_completo(+Tablero, +F, +C)
% % Un casillero está casi completo si tiene 3 de sus 4 líneas marcadas
% casillero_casi_completo(Tablero, F, C) :-
%     obtener_celda(Tablero, F, C, c(H1, V1, 0)), % No capturado
%     F1 is F + 1,
%     C1 is C + 1,
%     obtener_celda(Tablero, F1, C, c(H2, _, _)),
%     obtener_celda(Tablero, F, C1, c(_, V2, _)),
%     Lineas = [H1, V1, H2, V2],
%     contar_ocurrencias(Lineas, 1, 3).

% % obtener_celda(+Tablero, +F, +C, -Celda)
% obtener_celda(Tablero, F, C, Celda) :-
%     nth1(F, Tablero, Fila),
%     nth1(C, Fila, Celda).

% % contar_ocurrencias(+Lista, +Elemento, -Cantidad)
% contar_ocurrencias([], _, 0).
% contar_ocurrencias([X|Resto], X, N) :-
%     contar_ocurrencias(Resto, X, N1),
%     N is N1 + 1.
% contar_ocurrencias([Y|Resto], X, N) :-
%     X \= Y,
%     contar_ocurrencias(Resto, X, N).



% ======================================================================================================= %
% ======================================================================================================= %


 % jugada_maquina(+Tablero,+Turno,+Nivel,?F,?C,?D,?Tablero2,?Turno2,?Celdas)
 jugada_maquina(Tablero, Turno, Nivel, F, C, D, Tablero2, Turno2, Celdas) :-
     sugerencia_jugada(Tablero, Turno, Nivel, F, C, D),
     jugada_humano(Tablero, Turno, F, C, D, Tablero2, Turno2, Celdas).

 % sugerencia_jugada(+Tablero,+Turno,+Nivel,?F,?C,?D)
 sugerencia_jugada(Tablero, Turno, Nivel, F, C, D) :-
     length(Tablero, N),
     N1 is N - 1,

     findall([F1, C1, D1], (
         member(D1, [h, v]),
         between(0, N1, F1),
         between(0, N1, C1),
         jugada_valida(Tablero, F1, C1, D1)
     ), Jugadas),
     evaluar_jugadas(Jugadas, Tablero, Turno, Nivel, [], [MejorF, MejorC, MejorD]),
     F = MejorF, C = MejorC, D = MejorD.

% evaluar_jugadas(+Jugadas,+Tablero,+Turno,+Nivel,+MejorHastaAhora,?MejorJugada)
evaluar_jugadas([], _, _, _, [Jugada, _], Jugada).
evaluar_jugadas([[F,C,D]|Resto], Tablero, Turno, Nivel, MejorActual, MejorJugada) :-
    jugada_humano(Tablero, Turno, F, C, D, Tab1, Turno1, _),
    N1 is Nivel - 1,
    valor_minimax(Tab1, Turno1, N1, Valor),
    mejor_entre([[F,C,D], Valor], MejorActual, NuevoMejor),
    evaluar_jugadas(Resto, Tablero, Turno, Nivel, NuevoMejor, MejorJugada).

% valor_minimax(+Tablero,+Turno,+Nivel,?Valor)
valor_minimax(Tablero, Turno, 0, Valor) :-
    puntaje(Tablero, Turno, MiPuntaje),
    otro_turno(Turno, Otro),
    puntaje(Tablero, Otro, PuntajeOponente),
    Valor is MiPuntaje - PuntajeOponente.

valor_minimax(Tablero, Turno, Nivel, Valor) :-
    Nivel > 0,
    N1 is Nivel - 1,
    length(Tablero, N),
    N2 is N - 1,

    findall(V, (
        member(D, [h, v]),
        between(0, N2, F),
        between(0, N2, C),
        jugada_valida(Tablero, F, C, D),
        jugada_humano(Tablero, Turno, F, C, D, Tab1, Turno1, Celdas),
        (Celdas \= [] -> valor_minimax(Tab1, Turno, N1, V) ; 
                         otro_turno(Turno, Turno1),
                         valor_minimax(Tab1, Turno1, N1, V))
    ), Valores),
    (Valores == [] -> 
        puntaje(Tablero, Turno, MiPuntaje),
        otro_turno(Turno, Otro),
        puntaje(Tablero, Otro, PuntajeOponente),
        Valor is MiPuntaje - PuntajeOponente
    ; mejor_valor(Turno, Valores, Valor)).

% otro_turno(+TurnoActual, -TurnoSiguiente)
otro_turno(1, 2).
otro_turno(2, 1).

% puntaje(+Tablero, +Turno, -Puntos)
puntaje(Tablero, Turno, Puntos) :-
    flatten(Tablero, Lista),
    include(es_casilla_del(Turno), Lista, Casillas),
    length(Casillas, Puntos).

% es_casilla_del(+Turno, +Elemento)
% Verdadero si el elemento es una celda reclamada por el jugador "Turno"
es_casilla_del(Turno, c(_,_,Turno)).


% mejor_valor(+Turno,+ListaValores,?Valor)
mejor_valor(1, Valores, Valor) :- max_list(Valores, Valor).
mejor_valor(2, Valores, Valor) :- min_list(Valores, Valor).

% mejor_entre(+[Jugada,Valor1], +[JugadaMejor,Valor2], -Mejor)
mejor_entre([J1, V1], [], [J1, V1]).
mejor_entre([J1, V1], [_, V2], [J1, V1]) :- V1 > V2.
mejor_entre([_, V1], [J2, V2], [J2, V2]) :- V1 =< V2.
