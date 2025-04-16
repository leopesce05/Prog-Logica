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


% matrizN(+N,-M) ← M es una matriz de tamaño N X N que en sus celdas contiene variables,
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

% Parte 2.2:

% intercaladas(+M1,+M2,?I) ← I es una lista que contiene las filas de M y MT intercaladas. 
% M y MT son de igual tamaño. 
% ?- intercaladas([[1,2,3],[4,5,6],[7,8,9]], [[1,4,7],[2,5,8],[3,6,9]], I). 
% I = [[1,2,3],[1,4,7],[4,5,6],[2,5,8],[7,8,9],[3,6,9]] 

intercaladas([], [], []).
intercaladas([H1|M1], [H2|M2], [H1,H2|I]) :-
    intercaladas(M1, M2, I).

% --- Auxiliares necesarios (Tamaño y Traspuesta) ---

tamanio([],0).
tamanio([_|T], N):-
    tamanio(T,N1),
    N is N1+1.

concatenar([],L,L).
concatenar([H|T],L,[H|R]):-
    concatenar(T,L,R).

elemN_Esimo(1,[E|_],E).
elemN_Esimo(N,[_|F],E):-
    N>0,
    NSig is N-1,
    elemN_Esimo(NSig,F,E).

columnaN_Esima(_, [], []).
columnaN_Esima(N, [F | M], [X|Col]):-
    elemN_Esimo(N,F,X),
    columnaN_Esima(N,M,Col).

trasponer(_, 0, []).
trasponer(M, N, MT) :-
    N > 0,
    columnaN_Esima(N, M, Col),
    N1 is N - 1,
    trasponer(M, N1, R),
    concatenar(R, [Col], MT). % Construye en orden correcto

traspuesta([], []).
traspuesta([Fila1|RestoFilas], MT) :-
    tamanio(Fila1, N), % Asume matriz no vacía y cuadrada
    trasponer([Fila1|RestoFilas], N, MT).

% --- Predicados específicos de cruzadas2 (Versión con intercaladas) ---

% Comprueba que P sea una palabra de longitud N
palabra_longitud_n(P, N) :-
    palabra(P),
    tamanio(P, N).

% Verifica que todas las listas en ListaDeListas sean palabras válidas de longitud N
todas_palabras_validas_longitud_n([], _).
todas_palabras_validas_longitud_n([P|Resto], N) :-
    palabra_longitud_n(P, N), % Intenta unificar/validar P como palabra de longitud N
    todas_palabras_validas_longitud_n(Resto, N).

% Predicado principal Parte 2.2 (Versión con intercaladas)
cruzadas2(N, T) :-
    matrizN(N, T),          % 1. Crea matriz NxN con variables
    traspuesta(T, TT),      % 2. Crea la traspuesta (también con variables vinculadas)
    intercaladas(T, TT, I), % 3. Intercala filas (T) y columnas (TT)
    todas_palabras_validas_longitud_n(I, N). % 4. Valida/Asigna todas las filas/columnas intercaladas

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