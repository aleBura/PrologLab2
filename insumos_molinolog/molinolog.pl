:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(3).

% molinolog(+JugadorNegro,+JugadorBlanco,+T)
% JugadorNegtro y JugadorBlanco pueden ser los átomos humano o maquina.
% T es el tamaño del tablero.
molinolog(JugadorNegro,JugadorBlanco,T) :-
        pce_image_directory(icons),
        gr_crear(Visual, T, [
                     boton('Reiniciar',reiniciar),
                     boton('Salir',salir)] % salir puede ser por el boton o por el click en la ventana
                 ),
    iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T),
    !,
    gr_destruir(Visual).

iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T):-
    gr_dibujar_tablero(Visual,T,[]),
    loop(Visual,negro,JugadorNegro,JugadorBlanco,T,colocar,[]).

contrincante(negro,blanco).
contrincante(blanco,negro).

% --------------------------
% Loop principal
% --------------------------

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas) :-
    sformat(Msg, 'Jugador ~w', [Turno]),
    gr_estado(Visual, Msg),
    gr_evento(Visual,E),
    evento(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas).
    
% TODO fase MOVER
% por ahora solo considero que es  humano vs humano

hayFicha(Dir,Dist,PosicionesConFichas) :- pertenece((Dir, Dist, _), PosicionesConFichas).

% pertenece(?X,?L) ← X pertenece a la lista L
pertenece((X,Y,Z),[(X,Y,Z)|_]).
pertenece((X,Y,Z),[_|Ys]) :- pertenece((X,Y,Z),Ys).

% largo(+L,?N) ← N es el largo de la lista L.
largo([],0).
largo([_|Xs],N):- largo(Xs,M),
                  N is M+1.

%%%%%% Predicados de navegacion por el tablero %%%%%%%

%La segunda poscicion esta a la izquierda de la primera
izquierda(s,sw).
izquierda(se,s).
izquierda(n,nw).
izquierda(ne,n).

dobleIzquierda(ne,nw).
dobleIzquierda(se,sw).

%La segunda posicion esta a la derecha de la primera
derecha(n,ne).
derecha(nw,n).
derecha(s,se).
derecha(sw,n).

dobleDerecha(nw,ne).
dobleDerecha(sw,se).

%La segunda posicion esta arriba de la primera
arriba(sw,w).
arriba(w,nw).
arriba(se,e).
arriba(e,ne).

dobleArriba(sw,nw).
dobleArriba(se,ne).

%La segunda posicion esta abajo de la primera
abajo(nw,w).
abajo(w,sw).
abajo(ne,e).
abajo(e,se).

dobleAbajo(nw,sw).
dobleAbajo(ne,se).

medio(s).
medio(n).
medio(w).
medio(e).

% El concepto de arriba, abajo, izquierda y derecha
% en las filas y columnas del medio es por la distancia
% ademas pueden ser de largo variable

masUno(Dist,M,T) :- M is Dist+1, M =< T.
masDos(Dist,M,T) :- M is Dist+2, M =< T.
menosUno(Dist,M) :- M is Dist-1, 1 =< M.
menosDos(Dist,M) :- M is Dist-2, 1 =< M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Predicados para ver si hay molino %%%%

hayMolino(Dir,Dist,Turno,PosicionesConFichas,T,Ventana) :- hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana);
                                                           hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana);
                                                           hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana).
                                                   
hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                                    %La posicion que agruegé puede estar al medio, a la izquierda o a la derecha.
                                                                    %Primer caso al medio
                                                                    ((
                                                                     izquierda(Dir,X),
                                                                     derecha(Dir,Y),
                                                                     pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                                     pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                                    );
                                                                    %Segundo caso a la izquierda
                                                                    (
                                                                     derecha(Dir,X),
                                                                     dobleDerecha(Dir,Y),
                                                                     pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                                     pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                                    );
                                                                    %Tercer caso a la derecha
                                                                    (
                                                                     izquierda(Dir,X),
                                                                     dobleIzquierda(Dir,Y),
                                                                     pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                                     pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                                    )),
                                                                    mismoJugador(Turno, Turno2, Turno3),
                                                                    marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]).
                                                           
hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                          %La posicion que agruegé puede estar al medio, arriba o abajo.
                                                          %Primer caso al medio
                                                          ((
                                                           arriba(Dir,X),
                                                           abajo(Dir,Y),
                                                           pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                           pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                          );
                                                          %Segundo caso arriba
                                                          (
                                                           abajo(Dir,X),
                                                           dobleAbajo(Dir,Y),
                                                           pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                           pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                          );
                                                          %Tercer caso abajo
                                                          (
                                                           arriba(Dir,X),
                                                           dobleArriba(Dir,Y),
                                                           pertenece((X,Dist,Turno2),PosicionesConFichas),
                                                           pertenece((Y,Dist,Turno3),PosicionesConFichas)
                                                          )),
                                                          mismoJugador(Turno, Turno2, Turno3),
                                                          marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]).


hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                        %La posicion que agruegé puede estar al medio, arriba o abajo.
                                                        %Primer caso al medio
                                                        ((
                                                         masUno(Dist,X,T),
                                                         menosUno(Dist,Y),
                                                         pertenece((Dir,X,Turno2),PosicionesConFichas),
                                                         pertenece((Dir,Y,Turno3),PosicionesConFichas)
                                                        );
                                                        %Segundo caso arriba
                                                        (
                                                         menosUno(Dist,X),
                                                         menosDos(Dist,Y),
                                                         pertenece((Dir,X,Turno2),PosicionesConFichas),
                                                         pertenece((Dir,Y,Turno3),PosicionesConFichas)
                                                        );
                                                        %Tercer caso abajo
                                                        (
                                                         masUno(Dist,X,T),
                                                         masDos(Dist,Y,T),
                                                         pertenece((Dir,X,Turno2),PosicionesConFichas),
                                                         pertenece((Dir,Y,Turno3),PosicionesConFichas)
                                                        )),
                                                        mismoJugador(Turno, Turno2, Turno3),
                                                        marcarMolino(Ventana,T,[(Dir,Dist),(Dir,X),(Dir,Y)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mismoJugador(blanco,blanco,blanco).
mismoJugador(negro,negro,negro).

marcarMolino(_, _, []).
marcarMolino(Ventana, T, [(Dir,Dist)|Xs]) :- gr_ficha(Ventana, T, Dir, Dist, "seleccion"),
                                             marcarMolino(Ventana, T, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evento(click(Dir,Dist), Visual, Turno, JugadorNegro, JugadorBlanco, T, colocar, PosicionesConFichas) :-
    hayFicha(Dir,Dist,PosicionesConFichas) ->
        ( gr_mensaje(Visual,'La posición no está vacía, no puede colocar una ficha ahí.'),
          loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas)
        );
        ( largo(PosicionesConFichas,L),
          L < 3 * (T + 1)
        ) ->
        (
          gr_ficha(Visual,T,Dir,Dist,Turno),

          % En este punto ya puso la ficha, ahora hay que chequear si hay molino o no
          % Le paso la posicion nueva porque solo considero molinos que incluyan esa posicion
          % Si se generan 2 molinos a la vez solo se saca una ficha.

          (hayMolino(Dir,Dist,Turno,PosicionesConFichas,T,Visual) ->
             % Click en la ficha que quiere sacar
             gr_mensaje(Visual,'Hay molino'));
             
             %eliminarFicha(click(Dir,Dist),Turno,Visual,PosicionesConFichas));
          
             (contrincante(Turno,SiguienteTurno),
             loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Dir,Dist,Turno)|PosicionesConFichas]))
        );
          gr_mensaje(Visual,'Finalizo la fase colocar, se alcanzó el máximo de fichas para este tablero.'),
          true.
    
evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->
            true;
            loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas)
    ).
        
evento(reiniciar,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->  % reiniciar el juego
           iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
           loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas)
    ).
