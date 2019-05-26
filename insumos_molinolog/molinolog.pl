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

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,Fase,PosicionesConFichas) :-
    Fase = colocar ->
    (( getMyFichas(PosicionesConFichas,Turno,MyFichas),
      largo(MyFichas,L),
      L < 3 * (T + 1)
    ) -> (
      actualizarMensajeInferior(Turno,Fase,T,Visual,PosicionesConFichas),
      gr_evento(Visual,E),
      evento(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,Fase,PosicionesConFichas)
    );
      sformat(Msg, 'Finalizó la fase colocar, el jugador ~w alcanzó el máximo de fichas para este tablero. Comenzará la fase mover, iniciando el jugador negro.', [Turno]),
      gr_mensaje(Visual,Msg),
      loop(Visual,negro,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas));
      
      %Fase mover
      %Lo primero que se debe hacer es chequear que haya movimientos válidos.
      hayMovimientosValidos(Turno,T,PosicionesConFichas) -> (

        sformat(Msg, 'Jugador ~w fase colocar mueva alguna de sus fichas a una posición adyacente.', [Turno]),
        gr_mensaje(Visual,Msg),
        gr_evento(Visual,E),
        primerClick(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,Fase,PosicionesConFichas)
        %Pueden mover solo a posiciones adyacentes
        %Se controla que la posicion esté vacia y sea adyacente
        %Se controla si se forma molino
        %Cuando alguno de los 2 queda con 2 fichas  termina el juego

      );
        gr_mensaje(Visual,'No tiene movimientos disponibles, pasará el turno al siguiente jugador'),
        contrincante(Turno,SiguienteTurno),
        loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas).

% TODO acá hay que controlar que para alguna de mis fichas haya una posicion
% adyacente vacía
hayMovimientosValidos(Turno, T, PosicionesConFichas).

adyacentes(Dir, Dist, DirSel, DistSel) :-
%Son adyacentes si tienen una diferencia de distancia 1 y la misma direcccion
%O diferencia de distancia 0 y direcciones pegadas (arriba, abajo, izq, der)
    Resta is Dist - DistSel,
    abs(Resta,Abs),
    ((Abs = 1, Dir = DirSel, medio(Dir));
     (Abs = 0 ,(izquierda(Dir,DirSel); derecha(Dir,DirSel); arriba(Dir,DirSel); abajo(Dir,DirSel)))
    ).

primerClick(click(DirSel,DistSel), Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas) :-
%Primer click selecciona la pieza, segundo click la mueve.
%Chequea que haya ficha en esa posición y que sea  de  ese jugador.
    pertenece((Turno,DirSel, DistSel), PosicionesConFichas) -> (
       gr_ficha(Visual, T, DirSel, DistSel, "seleccion"),
       gr_evento(Visual,E),
       evento(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas, DirSel, DistSel));
    %Marco mal
       gr_mensaje(Visual,'Seleccionó  una posición incorrecta, vuelva a intentarlo.'),
       gr_evento(Visual,E),
       primerClick(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas).
    
evento(click(Dir,Dist), Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas, DirSel, DistSel) :-
%Segundo click seleccionó el lugar a donde mover
%Hay que ver si la posición seleccionada para mover es válida, en caso de que no lo sea vuelve al primer click.
    pertenece((_,Dir,Dist), PosicionesConFichas) -> (
       gr_mensaje(Visual,'Seleccionó  una posición incorrecta, vuelva a intentarlo.'),
       convertirFormato(PosicionesConFichas,Fichas),
       gr_dibujar_tablero(Visual,T,Fichas),
       gr_evento(Visual,E),
       primerClick(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas)
    );
       adyacentes(Dir, Dist, DirSel, DistSel) -> (

         removerFicha(PosicionesConFichas,(Turno,DirSel,DistSel),PosicionesSinEsaFicha),
         convertirFormato(PosicionesSinEsaFicha,Fichas),
         gr_dibujar_tablero(Visual,T,Fichas),
         gr_ficha(Visual,T,Dir,Dist,Turno),
         
         hayMolino(Dir,Dist,Turno,PosicionesSinEsaFicha,T,Visual) -> (
         % Click en la ficha que quiere sacar
           gr_mensaje(Visual,'Seleccione una ficha de su rival para eliminar.'),
           gr_evento(Visual, E), %Espero por el click
           eliminarFicha(E,Turno,Visual,PosicionesSinEsaFicha,T,PosicionesSinEsaFichaMolino),
           %La vuelvo a dibujar porque no estaba agregada cuando se redibujo el tablero.
           gr_ficha(Visual,T,Dir,Dist,Turno),
           contrincante(Turno,SiguienteTurno),
           loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,[(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino])
         );
         % Else no hay molino
         (
           contrincante(Turno,SiguienteTurno),
           removerFicha(PosicionesConFichas,(Turno,DirSel,DistSel),PosicionesSinEsaFicha),
           loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,[(Turno,Dir,Dist)|PosicionesSinEsaFicha])
         )
         
       );
         gr_mensaje(Visual,'Seleccionó  una posición incorrecta, vuelva a intentarlo.'),
         gr_evento(Visual,E),
         primerClick(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas).

evento(click(Dir,Dist), Visual, Turno, JugadorNegro, JugadorBlanco, T, colocar, PosicionesConFichas) :-
    hayFicha(Dir,Dist,PosicionesConFichas) ->
        ( gr_mensaje(Visual,'La posición no está vacía, no puede colocar una ficha ahí.'),
          loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas)
        );
        gr_ficha(Visual,T,Dir,Dist,Turno),

        % En este punto ya puso la ficha, ahora hay que chequear si hay molino o no
        % Le paso la posicion nueva porque solo considero molinos que incluyan esa posicion
        % Si se generan 2 molinos a la vez solo se saca una ficha.

        hayMolino(Dir,Dist,Turno,PosicionesConFichas,T,Visual) -> (
           % Click en la ficha que quiere sacar
             gr_mensaje(Visual,'Seleccione una ficha de su rival para eliminar.'),
             gr_evento(Visual, E), %Espero por el click
             eliminarFicha(E,Turno,Visual,PosicionesConFichas,T,PosicionesSinEsaFicha),
             %La vuelvo a dibujar porque no estaba agregada cuando se redibujo el tablero.
             gr_ficha(Visual,T,Dir,Dist,Turno),
             contrincante(Turno,SiguienteTurno),
             loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesSinEsaFicha])
           );
           % Else no hay molino
           (
             contrincante(Turno,SiguienteTurno),
             loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesConFichas])
           ).

    
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


hayFicha(Dir,Dist,PosicionesConFichas) :- pertenece((_,Dir, Dist), PosicionesConFichas).

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

hayMolino(Dir,Dist,Turno,PosicionesConFichas,T,Ventana) :- (hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana);
                                                           hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana);
                                                           hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana)),
                                                           sformat(Msg, 'Jugador ~w, obtuvo un molino debe eliminar ficha del contrincante', [Turno]),
                                                           gr_estado(Ventana, Msg).

hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                                    %La posicion que agruegé puede estar al medio, a la izquierda o a la derecha.
                                                                    %Primer caso al medio
                                                                    ((
                                                                     izquierda(Dir,X),
                                                                     derecha(Dir,Y),
                                                                     pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                                     pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                                    );
                                                                    %Segundo caso a la izquierda
                                                                    (
                                                                     derecha(Dir,X),
                                                                     dobleDerecha(Dir,Y),
                                                                     pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                                     pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                                    );
                                                                    %Tercer caso a la derecha
                                                                    (
                                                                     izquierda(Dir,X),
                                                                     dobleIzquierda(Dir,Y),
                                                                     pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                                     pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                                    )),
                                                                    mismoJugador(Turno, Turno2, Turno3),
                                                                    marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]).

hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                          %La posicion que agruegé puede estar al medio, arriba o abajo.
                                                          %Primer caso al medio
                                                          ((
                                                           arriba(Dir,X),
                                                           abajo(Dir,Y),
                                                           pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                           pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                          );
                                                          %Segundo caso arriba
                                                          (
                                                           abajo(Dir,X),
                                                           dobleAbajo(Dir,Y),
                                                           pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                           pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                          );
                                                          %Tercer caso abajo
                                                          (
                                                           arriba(Dir,X),
                                                           dobleArriba(Dir,Y),
                                                           pertenece((Turno2,X,Dist),PosicionesConFichas),
                                                           pertenece((Turno3,Y,Dist),PosicionesConFichas)
                                                          )),
                                                          mismoJugador(Turno, Turno2, Turno3),
                                                          marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]).


hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana):-
                                                        %La posicion que agruegé puede estar al medio, arriba o abajo.
                                                        %Primer caso al medio
                                                        ((
                                                         masUno(Dist,X,T),
                                                         menosUno(Dist,Y),
                                                         pertenece((Turno2,Dir,X),PosicionesConFichas),
                                                         pertenece((Turno3,Dir,Y),PosicionesConFichas)
                                                        );
                                                        %Segundo caso arriba
                                                        (
                                                         menosUno(Dist,X),
                                                         menosDos(Dist,Y),
                                                         pertenece((Turno2,Dir,X),PosicionesConFichas),
                                                         pertenece((Turno3,Dir,Y),PosicionesConFichas)
                                                        );
                                                        %Tercer caso abajo
                                                        (
                                                         masUno(Dist,X,T),
                                                         masDos(Dist,Y,T),
                                                         pertenece((Turno2,Dir,X),PosicionesConFichas),
                                                         pertenece((Turno3,Dir,Y),PosicionesConFichas)
                                                        )),
                                                        (Dir = n; Dir = s; Dir = e; Dir = s), %Para descartar molinos diagonales
                                                        mismoJugador(Turno, Turno2, Turno3),
                                                        marcarMolino(Ventana,T,[(Dir,Dist),(Dir,X),(Dir,Y)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mismoJugador(blanco,blanco,blanco).
mismoJugador(negro,negro,negro).

marcarMolino(_, _, []).
marcarMolino(Ventana, T, [(Dir,Dist)|Xs]) :- gr_ficha(Ventana, T, Dir, Dist, "seleccion"),
                                             marcarMolino(Ventana, T, Xs).
                                             
actualizarMensajeInferior(Turno,Fase,T,Visual,PosicionesConFichas) :- getMyFichas(PosicionesConFichas,Turno,MyFichas),
                                                                      largo(MyFichas,L),
                                                                      Cant is (3*(T+1)-L),
                                                                      sformat(Msg, 'Jugador ~w, fase ~w (restan colocar ~w fichas)', [Turno,Fase,Cant]),
                                                                      gr_estado(Visual, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eliminarFicha(click(Dir,Dist),Turno,Visual,PosicionesConFichas,T,PosicionesSinEsaFicha) :-
               posicionOtroJugador(Dir,Dist,Turno,PosicionesConFichas) ->
               (
                 contrincante(Turno,OtroJugador),
                 removerFicha(PosicionesConFichas,(OtroJugador,Dir,Dist),PosicionesSinEsaFicha),
                 convertirFormato(PosicionesSinEsaFicha,Fichas),
                 gr_dibujar_tablero(Visual,T,Fichas)
               );
                gr_mensaje(Visual,'La posición seleccionada no contiene una ficha de su rival.'),
                gr_evento(Visual, E), %Espero por el siguiente click
                eliminarFicha(E,Turno,Visual,PosicionesConFichas,T,PosicionesSinEsaFicha).
                
posicionOtroJugador(Dir,Dist,Turno,PosicionesConFichas) :-
               contrincante(Turno,OtroJugador),
               pertenece((OtroJugador,Dir,Dist),PosicionesConFichas).
               
% removerFicha(+L,?E,?LSinE) ← LSinE es la lista L sin ninguna ocurrencia del elemento E.
removerFicha(L,E,LSinE) :- sin_elem_accu(L,E,[],LSinE).

% sin_elem_accu(?L,?E,+Ac,?SinE) ← La lista SinE es la lista L sin E. Ac es el acumulador e inicialmente debe ser []
sin_elem_accu([],_,Ac,Ac).
sin_elem_accu([(Tipo,Dir,Dist)|T],(Tipo,Dir,Dist),Ac,SinE) :- sin_elem_accu(T,(Tipo,Dir,Dist),Ac,SinE),!.
sin_elem_accu([(J,X,Y)|T],(Tipo,Dir,Dist),Ac,SinE) :- concatenacion(Ac,[(J,X,Y)],C),
                                                   sin_elem_accu(T,(Tipo,Dir,Dist),C,SinE).
                                  
% concatenacion(?L1,?L2,?L) ← La lista L es la concatenación de L1 con L2.
concatenacion([],X,X).
concatenacion(X,[],X).
concatenacion([X|Xs],L2,[X|L]) :- concatenacion(Xs,L2,L).

convertirFormato([],[]).
convertirFormato([(Tipo,Dir,Dist)|Xs],[ficha(Tipo,Dir,Dist)|Xs2]) :- convertirFormato(Xs,Xs2).

getMyFichas([],_,[]).
getMyFichas([(Turno,Dir,Dist)|Xs],Turno,[(Turno,Dir,Dist)|Ys]) :- getMyFichas(Xs,Turno,Ys),!.
getMyFichas([(_,_,_)|Xs],Turno,L) :- getMyFichas(Xs,Turno,L).
                                           