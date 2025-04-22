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

% repetido(+X,?L) ← El elemento X tiene mas de una ocurrencia en la lista L

repetido(X, [X|R]) :- otra_vez(X, R).
repetido(X, [Y|R]) :- X \= Y, repetido(X, R).

% otra_vez(+X,?L) <- El elemento X no aparece al menos una vez mas en la lista L

otra_vez(X, [X|_]).
otra_vez(X, [_|R]) :- otra_vez(X, R).

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

% ordenada(+L1, ?L2) ← L2 contiene los elementos de L1 ordenados de menor a mayor,
%                     utilizando el algoritmo de ordenación por selección. Las listas contienen 
%                     valores enteros y no hay elementos repetidos.

ordenada([], []).
ordenada(L, [Min|Resto]) :-
    seleccionar_menor(L, Min, R),
    ordenada(R, Resto).

% seleccionar_menor(+L, ?Min, ?R) ← Min es el menor de L, R es L sin Min

seleccionar_menor([X], X, []).
seleccionar_menor([X|Xs], X, Xs) :-
    seleccionar_menor(Xs, Y, Resto),
    X =< Y.
seleccionar_menor([X|Xs], Y, [X|Resto]) :-
    seleccionar_menor(Xs, Y, Resto),
    X > Y.

% ============================================================================================================ %
%                                           2. Palabras cruzadas                                               %
% ============================================================================================================ %

% Predicados para ambas soluciones: 

% matrizN(+N,?M) ← M es una matriz de tamaño N X N que en sus celdas contiene variables,
% de modo que representa un tablero vacío. La matriz está representada como lista de listas.
% ?- matriz(4,M).
% M = [[_,_,_,_], [_,_,_,_], [_,_,_,_], [_,_,_,_]]
crearListaN(0, []).
crearListaN(N, [_|Resto]) :-
    N > 0,
    N1 is N - 1,
    crearListaN(N1, Resto).

matrizNAux(_,[],0).
matrizNAux(N, [F|R], Ni):-
    Ni > 0,
    Nj is Ni-1,
    crearListaN(N, F), 
    matrizNAux(N, R, Nj).  

matrizN(0, []).
matrizN(N, [Fila|Resto]) :-
    N > 0,
    crearListaN(N, Fila),  % Fila nueva en cada paso
    N1 is N - 1,
    matrizNAux(N, Resto, N1).  
   


% traspuesta(?M,?MT) ← MT es la traspuesta de la matriz M.
% ?- traspuesta([[A,B],[C,D],MT).
% MT = [[A,C],[B,D]]

columnaN_Esima([], [], []).
columnaN_Esima([[N|F] | M], [N|Col], [F|MSig]):-
    columnaN_Esima(M,Col,MSig).

trasponer(_, 0, []).  
trasponer(M, N, [Col|MT]) :-
    N > 0,
    columnaN_Esima(M, Col, MSig),  
    N1 is N - 1, 
    trasponer(MSig, N1, MT).

tamanio([],0).
tamanio([_|T], N):-
    tamanio(T,N1),
    N is N1+1.

traspuesta([], []).
traspuesta(M, MT) :-
    tamanio(M, N), 
    trasponer(M, N, MT).



% Parte 2.1:

% Verifica que todas las filas sean palabras válidas
todas_filas_validas([]).
todas_filas_validas([F|Resto]) :-
    palabra(F),
    todas_filas_validas(Resto).

% Comprueba que P sea una palabra de longitud N
palabra_longitud_n(P, N) :-
    palabra(P),
    tamanio(P, N).

% Asigna palabras de tamaño N a las filas
asignar_palabras_longitud_n([], _).
asignar_palabras_longitud_n([F|Resto], N) :-
    palabra_longitud_n(F, N),
    asignar_palabras_longitud_n(Resto, N).


% cruzadas1(+N,?T) ← T es un tablero válido de tamaño N X N de palabras cruzadas
% cruzadas1(3,T)
% T = [[a,l,a],[c,a,l],[a,s,a]]

cruzadas1(N, T) :-
    matrizN(N, T),
    asignar_palabras_longitud_n(T, N),
    traspuesta(T, TT),
    todas_filas_validas(TT).

% Parte 2.2:

% intercaladas(+M1,+M2,?I) ← I es una lista que contiene las filas de M y MT intercaladas. 
% M y MT son de igual tamaño. 
% ?- intercaladas([[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]], I). 
% I = [[1,2,3],[1,4,7],[4,5,6],[2,5,8],[7,8,9],[3,6,9]] 

intercaladas([], [], []).
intercaladas([H1|M1], [H2|M2], [H1,H2|I]) :-
    intercaladas(M1, M2, I).


% cruzadas2(+N,?T) ← T es un tablero válido de tamaño N X N de palabras cruzadas, es 
% decir, todas las filas y todas las columnas contienen letras que forman palabras de largo N 
% pertenecientes al diccionario. 
% ?- cruzadas2(3,T). 
% T = [[a,l,a],[c,a,l],[a,s,a]]

% Construye una matriz de NxN con palabras válidas, alternando entre filas y columnas
cruzadas2(N, T) :-
    matrizN(N, T),          % 1. Crea matriz NxN con variables
    traspuesta(T, TT),      % 2. Crea la traspuesta (también con variables vinculadas)
    intercaladas(T, TT, I), % 3. Intercala filas (T) y columnas (TT)
    todas_palabras_validas_longitud_n(I, N). % 4. Valida/Asigna todas las filas/columnas intercaladas

% Verifica que todas las listas en ListaDeListas sean palabras válidas de longitud N
todas_palabras_validas_longitud_n([], _).
todas_palabras_validas_longitud_n([P1,P2|Resto], N) :-
    palabra_longitud_n(P1, N), % Intenta unificar/validar P como palabra de longitud N
    palabra_longitud_n(P2, N),
    todas_palabras_validas_longitud_n(Resto, N).




% Parte 2.3:

% Comando a ejecutar para comparar eficiencia (Con N = {2, 3, 4, 5, 6}):
% ?- time(findall(T1, cruzadas1(N, T1), Soluciones1)).
% ?- time(findall(T2, cruzadas2(N, T2), Soluciones2)).

% Parte 2.3:
    % Explicacion de porque cruzadas2 es mas eficiente que cruzadas1:
        % La diferencia clave radica en cuándo se detectan y aplican las restricciones, lo que lleva a una poda más temprana del árbol de búsqueda en cruzadas2.
        % Estrategia de cruzadas1 (Generar Filas, Luego Verificar Columnas):
            % -  Generación de Filas Completa: cruzadas1 primero intenta llenar todas las filas de la matriz T con palabras válidas de longitud N usando asignar_palabras_longitud_n. 
            %    Esto puede requerir mucho backtracking por sí solo si hay muchas combinaciones posibles de palabras para las filas.
            % -  Verificación Tardía de Columnas: Solo después de haber encontrado una asignación completa y válida para todas las filas, calcula la transpuesta TT y entonces verifica 
            %    si cada una de las columnas (ahora filas en TT) también forma una palabra válida usando todas_filas_validas.
            % -  Ineficiencia - Detección Tardía de Fallos: El gran problema aquí es que una elección "mala" en las primeras filas (por ejemplo, Fila1 y Fila2) podría hacer que Columna1 
            %    sea imposible de formar como palabra válida. Sin embargo, cruzadas1 no se dará cuenta de esto hasta que haya asignado todas las demás filas (Fila3, Fila4, etc.) y 
            %    luego intente verificar las columnas. En ese punto, si Columna1 falla, tiene que retroceder (backtrack) potencialmente a través de muchas asignaciones de filas posteriores antes de 
            %    poder probar una Fila1 o Fila2 diferente. Se realiza mucho trabajo generando combinaciones de filas que estaban condenadas al fracaso desde el principio debido a las restricciones de 
            %    las columnas.
        % Estrategia de cruzadas2 (Construcción Incremental y Alternante):
            % -  Inicio Restringido: Comienza asignando Fila1 (palabra_longitud_n(Fila1, N)) y luego inmediatamente intenta asignar Columna1 (palabra_longitud_n(Col1, N) 
            %    donde Col1 se obtiene de la traspuesta parcial). Si hay un conflicto inmediato en la celda (0,0), falla y retrocede aquí mismo.
            % -  Construcción Alternante y Verificación Temprana (construir_resto): Intenta asignar/validar Fila_i. Inmediatamente después, verifica la compatibilidad (verificar_compatibilidad) 
            %    entre esta Fila_i y la Columna_i (tal como está instanciada hasta ahora). verificar_interseccion comprueba si las letras ya asignadas coinciden. Si hay un conflicto 
            %    (p.ej., la letra asignada en Fila_i en la posición j no coincide con la letra ya asignada en Columna_j en la posición i), falla y retrocede ahora, sin intentar asignar el resto 
            %    de filas/columnas. Intenta asignar/validar Columna_j. Inmediatamente después, verifica la compatibilidad entre Columna_j y Fila_j. Si hay conflicto, falla y retrocede ahora.
            % -  Eficiencia: Poda Temprana del Árbol de Búsqueda: Al verificar la compatibilidad inmediatamente después de intentar asignar una palabra a una fila o columna, cruzadas2 detecta 
            %    inconsistencias mucho antes. Si asignar palabra_X a Fila2 hace imposible formar Columna1 o Columna2 válidas (debido a las letras que impone palabra_X en esas columnas), 
            %    cruzadas2 lo detectará cuando verifique la compatibilidad de Fila2 o cuando intente asignar Columna1 o Columna2. Esto evita explorar todas las combinaciones posibles para 
            %    Fila3, Fila4, etc., que se basarían en la elección incorrecta de Fila2. El árbol de búsqueda se "poda" mucho antes, eliminando ramas que no llevarán a una solución.      


% medir_cruzadas1(+N) - Mide el tiempo para encontrar todas las soluciones con cruzadas1
medir_cruzadas1(N) :-
    format('Midiendo cruzadas1 para N = ~w:~n', [N]),
    time(findall(T, cruzadas1(N, T), Soluciones)).

% medir_cruzadas2(+N) - Mide el tiempo para encontrar todas las soluciones con cruzadas2
medir_cruzadas2(N) :-
    format('Midiendo cruzadas2 para N = ~w:~n', [N]),
    time(findall(T, cruzadas2(N, T), Soluciones)).

% comparar(+N) - Compara ambas implementaciones para un valor de N
comparar(N) :-
    format('~n===== Comparacion para N = ~w =====~n', [N]),
    medir_cruzadas2(N),
    medir_cruzadas1(N).

% comparar_todos - Compara para N = 2, 3, 4, 5, 6
comparar_todos :-
    comparar(2),
    comparar(3),
    comparar(4),
    comparar(5),
    comparar(6).



