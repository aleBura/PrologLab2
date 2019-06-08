% --------------------------------------
% Laboratorio 1 Programacion logica 2019
% --------------------------------------
% Predicados sobre listas
% --------------------------------------
% largo(+L,?N) ← N es el largo de la lista L.
largo([],0).
largo([_|Xs],N):- largo(Xs,M),
                  N is M+1.

% todos_iguales(?L) ← Todos los elementos de la lista L son iguales entre sí
todos_iguales([]).
todos_iguales([_]).
todos_iguales([X,X|Xs]) :- todos_iguales([X|Xs]).

% concatenacion(?L1,?L2,?L) ← La lista L es la concatenación de L1 con L2.
concatenacion([],X,X).
concatenacion(X,[],X).
concatenacion([X|Xs],L2,[X|L]) :- concatenacion(Xs,L2,L).

% contenida(?L1,+L2) ← todos los elementos de la lista L1 pertenecen a L2
contenida([],_).
contenida([X|Xs],[Y|Ys]) :- pertenece(X,[Y|Ys]),
                            contenida(Xs,[Y|Ys]).

% pertenece(?X,?L) ← X pertenece a la lista L
pertenece(X,[X|_]).
pertenece(X,[_|Ys]) :- pertenece(X,Ys).

% ww(?L,+V) ← La lista L es la concatenacion consigo misma de una lista W, cuyos elementos pertenecen al conjunto representado por la lista V, largo(L) >= 2
ww(L,V):- splitMedio(L1,L2,L),
          iguales(L1,L2),
          contenida(L1,V).

% splitMedio(?L1, ?L2, ?L) ← La lista L1 es la primera mitad de la lista L, la lista L2 es la segunda mitad de la lista L, largo(L) >= 2 y par
splitMedio(L1, L2, L) :- concatenacion(L1,L2,L),
                         largo(L1,N),
                         largo(L2,N).

% iguales(?X, ?Y) ← La lista X tiene los mismos elementos y en el mismo orden que la lista Y
iguales([],[]).
iguales([X|Xs],[X|Ys]) :- iguales(Xs,Ys).

% wwR(?L,+V) ← La lista L es la concatenacion de una lista W y su reverso, con elementos pertenecientes al conjunto representado por la lista V, largo(L) >=2
wwR(L,V):- splitMedio(L1,L2,L),
           reversoConAccu(L1,[],L2),
           contenida(L1,V).

% reversoConAccu(?L,+Ac,?Rev) ← La lista Rev es la lista L invertida. Ac es el acumulador e inicialmente debe ser [] 
reversoConAccu([],Ac,Ac).
reversoConAccu([H|T],Ac,Rev) :- reversoConAccu(T,[H|Ac],Rev).

% sin_elem(+L,?E,?LSinE) ← LSinE es la lista L sin ninguna ocurrencia del elemento E.
sin_elem(L,E,LSinE) :- sin_elem_accu(L,E,[],LSinE).

% sin_elem_accu(?L,?E,+Ac,?SinE) ← La lista SinE es la lista L sin E. Ac es el acumulador e inicialmente debe ser [] 
sin_elem_accu([],_,Ac,Ac).
sin_elem_accu([E|T],E,Ac,SinE) :- sin_elem_accu(T,E,Ac,SinE),!.
sin_elem_accu([X|T],E,Ac,SinE) :- concatenacion(Ac,[X],C),
                                  sin_elem_accu(T,E,C,SinE).

% sublista(?L,?Sub) ← Sub contiene un subconjunto de elementos contiguos de L en el mismo orden que aparecen en L.                                
sublista(_,[]).
sublista([X|L1],[X|L2]) :- prefijo(L1,L2),!.
sublista([_|L1],[Y|L2]) :- sublista(L1,[Y|L2]).

% prefijo(?L,?S) ← La lista S es un prefijo de la lista L. Un prefijo es un subconjunto de elementos contiguos comenzando por el primero de la lista.
prefijo(_,[]).
prefijo([X|L],[X|S]) :- prefijo(L,S).

% enesimo(?L,+N,?E) ← El elemento E esta en la N-sima posición en la lista L
enesimo([X|_],1,X).
enesimo([_|Xs],N,Y) :- enesimo(Xs,M,Y),
                       N is M+1.

% sublista(?L,?Sub,?I,?J) ← Sub contiene un subconjunto de elementos contiguos de L en el mismo orden que aparecen en L, 
% empezando en la posición I-ésima de L y terminado en la J-ésima.
sublista(L1,L2,I,J) :- sublista(L1,L2),
                       primero(L2,P),
                       ultimo(L2,U),
                       enesimo(L1,I,P),
                       enesimo(L1,J,U).

% primero(?L,?X) ← X es el primer elemento de la lista L 
primero([X|_],X).

% ultimo(?L,?X) ← X es el último elemento de la lista L 
ultimo([X],X).
ultimo([_|Xs],Y) :- ultimo(Xs,Y).

% Predicados sobre matrices
% --------------------------------------
% matriz(?M,?N,+A) ← A es una matriz de M filas y N columnas. La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
matriz(0, _, []).
matriz(0, 0, [[]]).
matriz(M, N, [X|Xs]) :- largo(X, N), matriz(O, N, Xs), M is O+1.

% valor_celda(+I,+J,+A,?E) ← E es el contenido de la celda (I,J) de la matriz A.
valor_celda(I,J,A,E) :- fila(A, I, F), enesimo(F, J, E).

% fila(+M,?N,?F) ← F es la fila N-ésima de la matriz
fila(M, N, F) :- enesimo(M, N, F).

% col(+M,?N,?C) ← C es la columna N-ésima de la matriz
col([],_,[]).
col([X|Xs],N,[Cn|C]):- enesimo(X,N,Cn), col(Xs,N,C).

% diagonalD(+M,coord(?I,?J),?Dir) ← Dir es una diagonal de la matriz M, con índices de fila y de columna consecutivos crecientes. 
% El 1er elemento de Dir tiene coordenadas I,J. Los elementos de la fila 1 y los de la columna 1 son los posibles 1eros elementos de Dir
diagonalD(M, coord(1, J), Dir) :- diagonalDAux(M, coord(1, J), Dir).
diagonalD(M, coord(I, 1), Dir) :- diagonalDAux(M, coord(I, 1), Dir).

% diagonalDAux(+M,coord(?I,?J),?X) X es una diagonal de la matriz M, con índices de fila de fila y de columna consecutivos crecientes. 
% El primer elemento de X tiene coordernadas I, J.
diagonalDAux(M, coord(I, J), [X]) :- fila(M, I, F),
                                     enesimo(F, J, X),
                                     (matriz(I, _, M) ; matriz(_, J, M)).
diagonalDAux(M, coord(I, J), [X|Xs]) :- fila(M, I, F),
                                        enesimo(F, J, X),
                                        K is I+1,
                                        L is J+1,
                                        diagonalDAux(M, coord(K, L), Xs).

% diagonalI(+M,coord((?I,?J),?Inv) ← Inv es una una diagonal inversa de la matriz M, con índices de fila consecutivos decrecientes y de columna consecutivos crecientes. 
% El 1er elemento de Inv tiene coordenadas I,J. Los elementos de la columna 1 y los de la última fila son los posibles 1eros elementos de Inv
diagonalI(M, coord(I, 1), Inv) :- diagonalIAux(M, coord(I, 1), Inv).
diagonalI(M, coord(U, J), Inv) :- largo(M,U),
                                  diagonalIAux(M, coord(U, J), Inv).


% diagonalIAux(+M,coord((?I,?J),?X) ← X es una una diagonal inversa de la matriz M, con índices de fila consecutivos decrecientes y de columna consecutivos crecientes. 
% El 1er elemento de Inv tiene coordenadas I,J.
diagonalIAux(M, coord(I, J), [X]) :- fila(M, I, F),
                                     enesimo(F, J, X),
                                     largo(F,L),
                                     (I is 1; J is L).
diagonalIAux(M, coord(I, J), [X|Xs]) :- fila(M, I, F),
                                        enesimo(F, J, X),
                                        K is I-1,
                                        L is J+1,
                                        diagonalIAux(M, coord(K, L), Xs).


% Sopa de letras
% --------------------------------------
% sopa(+M,+Pals,?Xs) ← Xs es una lista de palabras con su coordenada de inicio y fin en la matriz M. 
% Todas las palabras en Xs están en Pals y todas las palabras en Pals están en Xs.
sopa(_, [], []).
sopa(M, Pals, [p(Pal, [PosIni, PosFin]) | Xs]) :- pertenece(Pal, Pals),
                                             (
                                                estaEnFila(M, Pal, [PosIni, PosFin]);
                                                estaEnColumna(M, Pal, [PosIni, PosFin]);
                                                estaEnDiagonalD(M, Pal, [PosIni, PosFin]);
                                                estaEnDiagonalI(M, Pal, [PosIni, PosFin])
                                             ),
                                             sin_elem(Pals, Pal, PalsRestantes),
                                             sopa(M, PalsRestantes, Xs).

% estaEnFila(+M,?Pal,[(?I,?J1), (?I,?J2)]) ← Pal es la sublista de la fila I de la matriz M entre las columnas J1 y J2.
estaEnFila(M, Pal, [(I, J1), (I, J2)] ) :- fila(M, I, F),
                                          ((J2 >= J1, sublista(F, Pal, J1, J2));
                                           (J1 > J2, reversoConAccu(Pal, [], PalRevertida), sublista(F, PalRevertida, J2, J1))
                                          ).

% estaEnColumna(+M,?Pal,[(?I1,?J), (?I2,?J)]) ← Pal es la sublista de la columna J de la matriz M entre las filas I1 e I2.
estaEnColumna(M, Pal, [(I1, J), (I2, J)] ) :- col(M, J, C),
                                             ((I2 >= I1, sublista(C, Pal, I1, I2));
                                              (I1 > I2, reversoConAccu(Pal, [], PalRevertida), sublista(C, PalRevertida, I2, I1))
                                             ).

% estaEnDiagonalD(+M,?Pal,[(?I1,?J1), (?I2,?J1)]) ← Pal es la sublista de la diagonal de la matriz M 
% (con índices de fila y de columna consecutivos crecientes) entre las coordenadas (I1, J1) y (I2, J2).
estaEnDiagonalD(M, Pal, [(I1, J1), (I2, J2)] ) :- ((I2 >= I1, J2 >= J1,
                                                   diagonalDAux(M, coord(I1,J1), Diagonal),
                                                   L is J2-J1+1,
                                                   sublista(Diagonal, Pal, 1, L)
                                                   );

                                                  (I1 >= I2, J1 >= J2,
                                                   diagonalDAux(M, coord(I2, J2), Diagonal),
                                                   L is J1-J2+1,
                                                   reversoConAccu(Pal,[],PalRevertida),
                                                   sublista(Diagonal, PalRevertida, 1, L))
                                                 ).

% estaEnDiagonalI(+M,?Pal,[(?I1,?J1), (?I2,?J1)]) ← Pal es la sublista de la diagonal de la matriz M 
% (con índices de fila consecutivos decrecientes y de columna consecutivos crecientes) entre las coordenadas (I1, J1) y (I2, J2).
estaEnDiagonalI(M, Pal, [(I1, J1), (I2, J2)] ) :- ((I2 =< I1, J2 >= J1,
                                                   diagonalIAux(M, coord(I1,J1), Diagonal),
                                                   L is J2-J1+1,
                                                   sublista(Diagonal, Pal, 1, L)
                                                   );

                                                   (I1 < I2, J1 > J2,
                                                   diagonalIAux(M, coord(I2,J2), Diagonal),
                                                   L is J1-J2+1,
                                                   reversoConAccu(Pal,[],PalRevertida),
                                                   sublista(Diagonal, PalRevertida, 1, L))
                                                   ).
