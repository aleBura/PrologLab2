:- use_module(graficos).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREDICADOS AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%contrincante(+Turno,?OtroTurno) <- OtroTurno es el color del adversario
contrincante(negro,blanco).
contrincante(blanco,negro).


%direccion(+Direccion) <- Direccion es una direccón válida
direccion(e).
direccion(w).
direccion(n).
direccion(s).
direccion(ne).
direccion(se).
direccion(nw).
direccion(se).


%dibujarJugada(+PosicionesConFichas, +Visual, +T) <- Dibuja todas las posiciones
%(Turno, Dir, Dist) pertenecientes a la lista PosicionesConFichas en el tablero Visual de tamaño T
dibujarJugada(PosicionesConFichas,Visual,T) :-
   convertirFormato(PosicionesConFichas,Fichas),
   gr_dibujar_tablero(Visual,T,Fichas).


%generarTodasLasPosiciones(+DistMax, ?TodasLasPosiciones, +Ac) <- Genera todas las posiciones de un
% tablero cuya máxima distancia es DistMax, Ac  es el acumulador y debe invocarse como []
generarTodasLasPosiciones(0,Ac,Ac):- true.
generarTodasLasPosiciones(DistMax,TodasLasPosiciones,Ac) :-
  DistMax > 0,
  concatenacion([(nw,DistMax),(n,DistMax),(ne,DistMax),(e,DistMax),(se,DistMax),(s,DistMax),(sw,DistMax),(w,DistMax)], Ac, NuevasPos),
  !,
  Siguiente is DistMax - 1,
  generarTodasLasPosiciones(Siguiente,TodasLasPosiciones,NuevasPos).

%hayMovimientosValidos(+Turno, +IteradorPosiciones, +PosicionesConFichas, +PosicionesDelTablero) <- Para alguna de las posiciones
%con fichas mías hay una posicion adyacente vacía en el tablero.
hayMovimientosValidos(Turno, [(Turno,Dir, Dist)|_], PosicionesConFichas, PosicionesDelTablero) :-
                                                                         pertenece2((X,Y),PosicionesDelTablero),
                                                                         adyacentes(Dir, Dist, X, Y),
                                                                         (pertenece((_,X,Y),PosicionesConFichas) -> false;
                                                                                                                   true).
hayMovimientosValidos(Turno, [(_,_,_)|PosicionesConFichasIter], PosicionesConFichas, PosicionesDelTablero) :-
   hayMovimientosValidos(Turno,PosicionesConFichasIter, PosicionesConFichas, PosicionesDelTablero).


%esSalirOReiniciar(+Evento) <- El evento es del tipo salir o reiniciar
esSalirOReiniciar(salir).
esSalirOReiniciar(reiniciar).


%adyacentes(+DirA, +DistA, +DirB, +DisB) <- Las  posiciones A y B son adyacentes
%Son adyacentes si tienen una diferencia de distancia 1 y la misma direcccion
%O diferencia de distancia 0 y direcciones pegadas (arriba, abajo, izq, der)
adyacentes(Dir, Dist, DirSel, DistSel) :-
    Resta is Dist - DistSel,
    abs(Resta,Abs),
    ((Abs = 1, Dir = DirSel, medio(Dir));
     (Abs = 0 ,(izquierda(Dir,DirSel); derecha(Dir,DirSel); arriba(Dir,DirSel); abajo(Dir,DirSel)))
    ).


%finDelJuego(+Turno,+ListaPosiciones,?Cant,+Ac) <- De vuelve la cantidad  de posiciones del jugador Turno en la lista
finDelJuego(_,[],Ac,Ac).
finDelJuego(Turno,[(Turno,_,_)|Xs],Cant,Ac) :- Sum is Ac + 1,
                                               finDelJuego(Turno,Xs,Cant,Sum),
                                               !.
finDelJuego(Turno,[(_,_,_)|Xs],Cant,Ac) :- finDelJuego(Turno,Xs,Cant,Ac).


%esJugadorNegro(+Turno) <- Es jugador negro
esJugadorNegro(negro).

%esJugadorBlanco(+Turno) <- Es jugador blanco
esJugadorBlanco(blanco).


%hayFicha(+Dir,+Dist,+ListaPosicionesConFichas) <- En la lista hay una posicion con Dir y Dist
hayFicha(Dir,Dist,PosicionesConFichas) :- pertenece((_,Dir, Dist), PosicionesConFichas).


% pertenece(?X,?L) ← X pertenece a la lista L
pertenece((X,Y,Z),[(X,Y,Z)|_]).
pertenece((X,Y,Z),[_|Ys]) :- pertenece((X,Y,Z),Ys).


% pertenece2(?X,?L) ← X pertenece a la lista L
pertenece2((X,Y),[(X,Y)|_]).
pertenece2((X,Y),[_|Ys]) :- pertenece2((X,Y),Ys).



% largo(+L,?N) ← N es el largo de la lista L.
largo([],0).
largo([_|Xs],N):- largo(Xs,M),
                  N is M+1.


% tres jugadores distintos son el mismo.
mismoJugador(blanco,blanco,blanco).
mismoJugador(negro,negro,negro).


% marca un molino en el tablero, el molino viene dado en la lista de fichas.
marcarMolino(_, _, []).
marcarMolino(Ventana, T, [(Dir,Dist)|Xs]) :- gr_ficha(Ventana, T, Dir, Dist, "seleccion"),
                                             marcarMolino(Ventana, T, Xs).


% actualiza el mensaje en la parte inferior del tablero
actualizarMensajeInferior(Turno,colocar,T,Visual,TurnosPasados) :- Cant is (3*(T+1)-TurnosPasados),
                                                                  sformat(Msg, 'Jugador ~w, fase colocar (restan colocar ~w fichas)', [Turno,Cant]),
                                                                  gr_estado(Visual, Msg).
actualizarMensajeInferior(Turno,mover,capturar,Visual) :- sformat(Msg, 'Jugador ~w, capturar', [Turno]), gr_estado(Visual, Msg).
actualizarMensajeInferior(Turno,mover,Visual) :- sformat(Msg, 'Jugador ~w, mover', [Turno]), gr_estado(Visual, Msg).


%Elimina una ficha del rival seleccionada mediante un click. PosicionesSinEsaFicha es la  lista sin
%la ficha removida.
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


%La posicion Dir,Dist es de mi adversario.
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


%Convierte el orden de los parametros que determinan una poscion para que se puedan usar los
%predicados de gráficos.
convertirFormato([],[]).
convertirFormato([(Tipo,Dir,Dist)|Xs],[ficha(Tipo,Dir,Dist)|Xs2]) :- convertirFormato(Xs,Xs2).


%Dada una lista de fichas obtiene las del Turno
getMyFichas([],_,[]).
getMyFichas([(Turno,Dir,Dist)|Xs],Turno,[(Turno,Dir,Dist)|Ys]) :- getMyFichas(Xs,Turno,Ys),!.
getMyFichas([(_,_,_)|Xs],Turno,L) :- getMyFichas(Xs,Turno,L).


% pertenece(?X,?L) ← X pertenece a la lista L
perteneceLista(X,[X|_]).
perteneceLista(X,[_|Ys]) :- perteneceLista(X,Ys).


% remover_pos(+L,?E,?LSinE) ← LSinE es la lista L sin ninguna ocurrencia del elemento E.
remover_pos(L,E,LSinE) :- remover_pos_accu(L,E,[],LSinE).


% remover_pos_accu(?L,?E,+Ac,?SinE) ← La lista SinE es la lista L sin E. Ac es el acumulador e inicialmente debe ser []
remover_pos_accu([],_,Ac,Ac).
remover_pos_accu([E|T],E,Ac,SinE) :- remover_pos_accu(T,E,Ac,SinE),!.
remover_pos_accu([X|T],E,Ac,SinE) :- concatenacion(Ac,[X],C),
                                     remover_pos_accu(T,E,C,SinE).


% primero(?L,?X) ← X es el primer elemento de la lista L
primero([X|_],X).


% La cantidad de fichas de ese turno en la lista
cantFichas([],_,Ac,Ac).
cantFichas([(Turno,_,_)|Xs],Turno,Cant,Ac) :-
   Sig is Ac + 1,
   cantFichas(Xs,Turno,Cant,Sig),
   !.
   
cantFichas([(_,_,_)|Xs],Turno,Cant,Ac) :- cantFichas(Xs,Turno,Cant,Ac),!.



% reversoConAccu(?L,+Ac,?Rev) ← La lista Rev es la lista L invertida. Ac es el acumulador e inicialmente debe ser []
reversoConAccu([],Ac,Ac).
reversoConAccu([H|T],Ac,Rev) :- reversoConAccu(T,[H|Ac],Rev).


% genera la lista con listas de posiciones adyacentes para cada posicion del tablero
generarAdyacentes([],_,Ac,Ac).

generarAdyacentes([(Dir,Dist)|RestoTablero], Tablero, ListaAdyacentes, Ac) :-
  generarAdyacentesPosicion((Dir,Dist), Tablero, AdyacentesPos, []),
  generarAdyacentes(RestoTablero, Tablero, ListaAdyacentes, [AdyacentesPos|Ac]).


% genera la lista de  posiciones adyacentes para una posicion del tablero
generarAdyacentesPosicion((_,_),[],Ac,Ac).

generarAdyacentesPosicion((Dir,Dist), Tablero, ListaAdyacentes, Ac) :-
   primero(Tablero,(X,Y)),
   remover_pos(Tablero,(X,Y),NuevoTablero),
   (adyacentes(Dir, Dist, X, Y) ->
      generarAdyacentesPosicion((Dir,Dist), NuevoTablero, ListaAdyacentes, [(X,Y)|Ac]);
      generarAdyacentesPosicion((Dir,Dist), NuevoTablero, ListaAdyacentes, Ac)
   ).


% lista no vacia
noEsVacia([_|_]).


% ordena de forma random una lista
randomOrder([],Ac,Ac).

randomOrder(L,ListaRandom,Ac) :-
   random_select(X,L,ListaSinJugada),
   randomOrder(ListaSinJugada,ListaRandom,[X|Ac]).

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
derecha(sw,s).

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

masUno(Dist,M,Max) :- M is Dist+1, M =< Max.
masDos(Dist,M,Max) :- M is Dist+2, M =< Max.
menosUno(Dist,M) :- M is Dist-1, 1 =< M.
menosDos(Dist,M) :- M is Dist-2, 1 =< M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    loop(Visual,negro,JugadorNegro,JugadorBlanco,T,colocar,[],0).

% --------------------------
% Loop principal
% --------------------------

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados) :-
  contrincante(Turno,OtroTurno),
  (TurnosPasados < 3 * (T + 1)) -> (
    actualizarMensajeInferior(Turno,colocar,T,Visual,TurnosPasados),
    (
     ((Turno = negro, JugadorNegro = humano); (Turno = blanco, JugadorBlanco = humano)) ->
      (gr_evento(Visual,E),
        evento(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados)
      );
      (
        %Solo para que se vea mejor el juego
        sleep(1),
        minimax((PosicionesConFichas,T),MejorJugada,Turno,colocar,TurnosPasados),
        dibujarJugada(MejorJugada,Visual,T),
        (
          (esJugadorBlanco(Turno)) -> (
            TurnosPasadosMasUno is TurnosPasados + 1,
            loop(Visual,OtroTurno,JugadorNegro,JugadorBlanco,T,colocar,MejorJugada,TurnosPasadosMasUno)
          );
          (
            loop(Visual,OtroTurno,JugadorNegro,JugadorBlanco,T,colocar,MejorJugada,TurnosPasados)
          )
        )
      )
    )
  );

  (sformat(Msg, 'Finalizó la fase colocar, el jugador ~w alcanzó el máximo de fichas para este tablero. Comenzará la fase mover, iniciando el jugador negro.', [Turno]),
  gr_mensaje(Visual,Msg),
  loop(Visual,negro,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas)).

loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas) :-
  %Lo primero que se debe hacer es chequear que haya movimientos válidos.
  DistMax is T + 1,
  generarTodasLasPosiciones(DistMax,TodasLasPosiciones,[]),
  hayMovimientosValidos(Turno,PosicionesConFichas,PosicionesConFichas,TodasLasPosiciones) -> (
    actualizarMensajeInferior(Turno,mover,Visual),
    (
    ((Turno = negro, JugadorNegro = humano); (Turno = blanco, JugadorBlanco = humano)) ->
    (
        gr_evento(Visual,E),
        (
          (esSalirOReiniciar(E), evento(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas));
          primerClick(E,Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas)
        )
        %Pueden mover solo a posiciones adyacentes
        %Se controla que la posicion esté vacia y sea adyacente
        %Se controla si se forma molino
        %Cuando alguno de los 2 queda con 2 fichas  termina el juego
    );
    (
      %Solo para que se vea mejor el juego
      sleep(1),
      minimax((PosicionesConFichas,T),MejorJugada,Turno,mover),
      dibujarJugada(MejorJugada,Visual,T),
      contrincante(Turno,SiguienteTurno),
      (finDelJuego(negro,MejorJugada,CantNegro,0), CantNegro > 2 ->
        (finDelJuego(blanco,MejorJugada,CantBlanco,0), CantBlanco > 2 ->
          loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,MejorJugada);
          %perdio el blanco
          gr_opciones(Visual, 'Fin del juego, jugador negro ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
              iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
              true
        );
        %perdio el negro
        gr_opciones(Visual, 'Fin del juego, jugador blanco ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
            iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
            true
        )
    ))
  );
    contrincante(Turno,SiguienteTurno),
    gr_mensaje(Visual,'No tiene movimientos disponibles, pasará el turno al siguiente jugador'),
    loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas).

%Obitene el primer click en la fase mover, este click selecciona una ficha la cual se moverá de posicion.
primerClick(click(DirSel,DistSel), Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas) :-
%Primer click selecciona la pieza, segundo click la mueve.
%Chequea que haya ficha en esa posición y que sea  de  ese jugador.
    pertenece((Turno,DirSel, DistSel), PosicionesConFichas) -> (
       gr_ficha(Visual, T, DirSel, DistSel, "seleccion"),
       gr_evento(Visual,E),
       evento(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas, DirSel, DistSel));
    %Marcó mal
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
           actualizarMensajeInferior(Turno,mover,capturar,Visual),
           gr_evento(Visual, E), %Espero por el click
           eliminarFicha(E,Turno,Visual,PosicionesSinEsaFicha,T,PosicionesSinEsaFichaMolino),
           %La vuelvo a dibujar porque no estaba agregada cuando se redibujo el tablero.
           gr_ficha(Visual,T,Dir,Dist,Turno),
           contrincante(Turno,SiguienteTurno),
           %Se chequea que haya finalizado el juego
           (finDelJuego(negro,[(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino],CantNegro,0), CantNegro > 2 ->
              (finDelJuego(blanco,[(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino],CantBlanco,0), CantBlanco > 2 ->
                loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,[(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino]);
                %perdio el blanco
                gr_opciones(Visual, 'Fin del juego, jugador negro ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
                   iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
                   true
              );
              %perdio el negro
              gr_opciones(Visual, 'Fin del juego, jugador blanco ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
                 iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
                 true
           )
         );
         % Else no hay molino
         (
           contrincante(Turno,SiguienteTurno),
           removerFicha(PosicionesConFichas,(Turno,DirSel,DistSel),PosicionesSinEsaFicha),
           %Se chequea que haya finalizado el juego
           (finDelJuego(negro,[(Turno,Dir,Dist)|PosicionesSinEsaFicha],CantNegro,0), CantNegro > 2 ->
              (finDelJuego(blanco,[(Turno,Dir,Dist)|PosicionesSinEsaFicha],CantBlanco,0), CantBlanco > 2 ->
                loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,mover,[(Turno,Dir,Dist)|PosicionesSinEsaFicha]);
                %perdio el blanco
                gr_opciones(Visual, 'Fin del juego, jugador negro ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
                   iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
                   true
              );
              %perdio el negro
              gr_opciones(Visual, 'Fin del juego, jugador negro ganador ¿Desea volver a jugar?', ['Sí', 'No'], 'Sí') ->
                 iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
                 true
           )
         )

       );
         gr_mensaje(Visual,'Seleccionó  una posición incorrecta, vuelva a intentarlo.'),
         gr_evento(Visual,E),
         primerClick(E, Visual, Turno, JugadorNegro, JugadorBlanco, T, mover, PosicionesConFichas).

evento(click(Dir,Dist), Visual, Turno, JugadorNegro, JugadorBlanco, T, colocar, PosicionesConFichas,TurnosPasados) :-
    hayFicha(Dir,Dist,PosicionesConFichas) ->
        ( gr_mensaje(Visual,'La posición no está vacía, no puede colocar una ficha ahí.'),
          loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados)
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
            (esJugadorBlanco(Turno) -> (
              TurnosPasadosMasUno is TurnosPasados + 1,
              loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesSinEsaFicha],TurnosPasadosMasUno)
            );
            (
              loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesSinEsaFicha],TurnosPasados))
            )

           );
           % Else no hay molino
           (
            contrincante(Turno,SiguienteTurno),
            (esJugadorBlanco(Turno) -> (
              TurnosPasadosMasUno is TurnosPasados + 1,
              loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesConFichas],TurnosPasadosMasUno)
            );
            (
              loop(Visual,SiguienteTurno,JugadorNegro,JugadorBlanco,T,colocar,[(Turno,Dir,Dist)|PosicionesConFichas],TurnosPasados))
            )
           ).

evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->
            true;
            loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados)
    ).

evento(reiniciar,Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->  % reiniciar el juego
            iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
            loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,colocar,PosicionesConFichas,TurnosPasados)
    ).

evento(salir,Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->
            true;
            loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas)
    ).

evento(reiniciar,Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas) :-
  (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') ->  % reiniciar el juego
          iniciar_juego(Visual,JugadorNegro,JugadorBlanco,T);
          loop(Visual,Turno,JugadorNegro,JugadorBlanco,T,mover,PosicionesConFichas)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicados para ver si hay molino %%%%

%Lo usa el minimax tiene el parametro bool en false para no dibujar el molino
hayMolino(Turno,Dir,Dist,PosicionesConFichas,T) :-  hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,_,false);
                                                hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,_,false);
                                                hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,_,false).

hayMolino(Dir,Dist,Turno,PosicionesConFichas,T,Ventana) :- (hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana,true);
                                                           hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana,true);
                                                           hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana,true)),
                                                           sformat(Msg, 'Jugador ~w, obtuvo un molino debe eliminar ficha del contrincante', [Turno]),
                                                           gr_estado(Ventana, Msg).

hayMolinoHorizontal(Dir,Dist,Turno,PosicionesConFichas,T,Ventana,HayQueMarcar):-
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
                                                                    (HayQueMarcar ->
                                                                       marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]);
                                                                       true).

hayMolinoVertical(Dir,Dist,Turno,PosicionesConFichas,T,Ventana, HayQueMarcar):-
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
                                                          (HayQueMarcar ->
                                                             marcarMolino(Ventana,T,[(Dir,Dist),(X,Dist),(Y,Dist)]);
                                                             true).


hayMolinoMedio(Dir,Dist,Turno,PosicionesConFichas,T,Ventana, HayQueMarcar):-
                                                        %La posicion que agruegé puede estar al medio, arriba o abajo.
                                                        %Primer caso al medio
                                                        ((
                                                         masUno(Dist,X,T+1),
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
                                                         masUno(Dist,X,T+1),
                                                         masDos(Dist,Y,T+1),
                                                         pertenece((Turno2,Dir,X),PosicionesConFichas),
                                                         pertenece((Turno3,Dir,Y),PosicionesConFichas)
                                                        )),
                                                        (Dir = n; Dir = s; Dir = e; Dir = w), %Para descartar molinos diagonales
                                                        mismoJugador(Turno, Turno2, Turno3),
                                                        (HayQueMarcar ->
                                                           marcarMolino(Ventana,T,[(Dir,Dist),(Dir,X),(Dir,Y)]);
                                                           true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MINIMAX %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(3).
alpha_inicial(-9999).
beta_inicial(9999).

minimax((ListaPosConFichas,T), MejorJugada, Turno, mover) :-
   minimax_depth(MaxDepth),
   alpha_inicial(Alpha),
   beta_inicial(Beta),
   minimax_step(max, (ListaPosConFichas,T), Turno, Alpha, Beta, MejorJugada, _, MaxDepth, mover, _),
   !.

minimax((ListaPosConFichas,T), MejorJugada, Turno, colocar, TurnosPasados) :-
   minimax_depth(MaxDepth),
   alpha_inicial(Alpha),
   beta_inicial(Beta),
   minimax_step(max, (ListaPosConFichas,T), Turno, Alpha, Beta, MejorJugada, _, MaxDepth, colocar, TurnosPasados),
   !.

minimax_step(MinMax, (ListaPosConFichas,T), Turno, Alpha, Beta, MejorJugada, MejorValor, Depth, mover, _) :-
   DistMax is T + 1,

   %Si es un tablero final o si alcancé la máxima profundidad
   %cantFichas(ListaPosConFichas,negro,CantNegras,0),
   %cantFichas(ListaPosConFichas,blanco,CantBlancas,0),
   %((Depth >= 0, CantNegras > 2, CantBlancas > 2) -> (
   ((Depth >= 0) -> (
     generarTodasLasPosiciones(DistMax,Tablero, []),
     contrincante(Turno,OtroTurno),
     getMyFichas(ListaPosConFichas,OtroTurno,FichasDelOtro),

     %Devuelve una lista con las posiciones adyacentes para cada posicion del tablero
     generarAdyacentes(Tablero, Tablero, ListaAdyacentes, []),
     reversoConAccu(ListaAdyacentes,[],RevListaAdyacentes),

     posibles_jugadas(Turno, (ListaPosConFichas,T), Jugadas, Tablero, [], FichasDelOtro, mover, RevListaAdyacentes),
     %Para que no eliga siempre la misma si hay varias del mismo valor
     randomOrder(Jugadas, RandomOrderJugadas, []),
     mejor_jugada(MinMax, RandomOrderJugadas, MejorJugada, MejorValor, Turno, Alpha, Beta, [], mover, T, Depth, 0)
   );
     false
   ).

minimax_step(MinMax, (ListaPosConFichas,T), Turno, Alpha, Beta, MejorJugada, MejorValor, Depth, colocar, TurnosPasados) :-
  %Si es un tablero final o si alcancé la máxima profundidad
  ((DistMax is T + 1,MaxFichas is 3*(T+1),(TurnosPasados \= MaxFichas; esJugadorBlanco(Turno)), Depth >= 0) -> (
    generarTodasLasPosiciones(DistMax,Tablero, []),
    contrincante(Turno,OtroTurno),
    getMyFichas(ListaPosConFichas,OtroTurno,FichasDelOtro),
    posibles_jugadas(Turno, (ListaPosConFichas,T), Jugadas, Tablero, [], FichasDelOtro, colocar),
    %Para que no eliga siempre la misma si hay varias del mismo valor
    randomOrder(Jugadas, RandomOrderJugadas, []),
    mejor_jugada(MinMax, RandomOrderJugadas, MejorJugada, MejorValor, Turno, Alpha, Beta, [], colocar, T, Depth, TurnosPasados)
  );
    false
  ).

mejor_jugada(max, [], [], -9999, _, _, _, _, _, _, _, _).
mejor_jugada(min, [], [], 9999, _, _, _, _, _, _, _, _).

mejor_jugada(min, [Jugada|OtrasJugadas], MejorJugada, MejorValor, Turno, Alpha, Beta, MejorJugadaPrevia, Fase, T, 0, TurnosPasados) :-
  heuristica(Jugada, Turno, Fase, Valor, T),
  comparar_jugadas(min, MejorJugadaPrevia, Beta, Jugada, Valor, MejorJugadaActual, BetaActualizado),
  (BetaActualizado > Alpha ->
    % No puedo podar
    B is -BetaActualizado,
    A is -Alpha,
    mejor_jugada(min, OtrasJugadas, MejorJugadaDemas, MejorValorDemas, Turno, B, A, MejorJugadaPrevia, Fase, T, 0, TurnosPasados),
    comparar_jugadas(min,Jugada,Valor,MejorJugadaDemas,MejorValorDemas,MejorJugada,MejorValor)
  ;
    % Puedo podar
    MejorJugada = MejorJugadaActual,
    MejorValor = BetaActualizado
  ).

mejor_jugada(max, [Jugada|OtrasJugadas], MejorJugada, MejorValor, Turno, Alpha, Beta, MejorJugadaPrevia, Fase, T, 0, TurnosPasados) :-
  heuristica(Jugada, Turno, Fase, Valor, T),
  comparar_jugadas(max, MejorJugadaPrevia, Alpha, Jugada, Valor, MejorJugadaActual, AlphaActualizado),
  (Beta > AlphaActualizado ->
    % No puedo podar
    B is -Beta,
    A is -AlphaActualizado,
    mejor_jugada(max, OtrasJugadas, MejorJugadaDemas, MejorValorDemas, Turno, B, A, MejorJugadaPrevia, Fase, T, 0, TurnosPasados),
    comparar_jugadas(max,Jugada,Valor,MejorJugadaDemas,MejorValorDemas,MejorJugada,MejorValor)
  ;
    % Puedo podar
    MejorJugada = MejorJugadaActual,
    MejorValor = AlphaActualizado
  ).

mejor_jugada(min, [Jugada|OtrasJugadas], MejorJugada, MejorValor, Turno, Alpha, Beta, MejorJugadaPrevia, Fase, T, Depth, TurnosPasados) :-
  contrincante(Turno, Otro),
  SigDepth is Depth-1,
  minimax_step(max, (Jugada,T), Otro, Alpha, Beta, _, BottomBestV, SigDepth, Fase, TurnosPasados),
  comparar_jugadas(min, MejorJugadaPrevia, Beta, Jugada, BottomBestV, MejorJugadaActual, BetaActualizado),
  (BetaActualizado > Alpha ->
    % No puedo podar
    B is -BetaActualizado,
    A is -Alpha,
    mejor_jugada(min, OtrasJugadas, MejorJugadaDemas, MejorValorDemas, Turno, B, A, MejorJugadaPrevia, Fase, T, 0, TurnosPasados),
    comparar_jugadas(min,Jugada,BottomBestV,MejorJugadaDemas,MejorValorDemas,MejorJugada,MejorValor)
  ;
    % Puedo podar
    MejorJugada is MejorJugadaActual,
    MejorValor is BetaActualizado
  ).

mejor_jugada(max, [Jugada|OtrasJugadas], MejorJugada, MejorValor, Turno, Alpha, Beta, MejorJugadaPrevia, Fase, T, Depth, TurnosPasados) :-
  contrincante(Turno, Otro),
  SigDepth is Depth-1,
  minimax_step(min, (Jugada,T), Otro, Alpha, Beta, _, BottomBestV, SigDepth, Fase, TurnosPasados),
  comparar_jugadas(max, MejorJugadaPrevia, Alpha, Jugada, BottomBestV, MejorJugadaActual, AlphaActualizado),
  (Beta > AlphaActualizado ->
    % No puedo podar
    B is -Beta,
    A is -AlphaActualizado,
    mejor_jugada(max, OtrasJugadas, MejorJugadaDemas, MejorValorDemas, Turno, B, A, MejorJugadaPrevia, Fase, T, 0, TurnosPasados),
    comparar_jugadas(max,Jugada,BottomBestV,MejorJugadaDemas,MejorValorDemas,MejorJugada,MejorValor)
  ;
    % Puedo podar
    MejorJugada is MejorJugadaActual,
    MejorValor is AlphaActualizado
  ).

cambiar_max_min(max,min).
cambiar_max_min(min,max).

comparar_jugadas(max, MovA, ValA, _, ValB, MovA, ValA) :- ValA >= ValB.
comparar_jugadas(max, _, ValA, MovB, ValB, MovB, ValB) :- ValA <  ValB.
comparar_jugadas(min, MovA, ValA, _, ValB, MovA, ValA) :- ValA =< ValB.
comparar_jugadas(min, _, ValA, MovB, ValB, MovB, ValB) :- ValA >  ValB.

posibles_jugadas(_,_,Jugadas,[],Jugadas,_,_).

%Fichas del otro se usa para llevar la cuenta de las fichas que le quedan al otro por si hay molino
posibles_jugadas(Turno, (ListaPosConFichas,T), Jugadas, Tablero, AcJugadas, FichasDelOtro, colocar) :-

  %Se expande para todos los posibles lugares donde puede colocar una ficha
  %Es una posicion del tablero
  primero(Tablero,(Dir,Dist)),
  remover_pos(Tablero,(Dir,Dist),NuevoTablero),
  %Es una posicion libre
  (pertenece((_,Dir,Dist),ListaPosConFichas) ->
     posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,NuevoTablero,AcJugadas,FichasDelOtro, colocar);

     %Si agregar la nueva posicion genera molino también hay que expandir
     %en función de las posibilidades de sacar la ficha del otro.
     (hayMolino(Turno,Dir,Dist,ListaPosConFichas,T), noEsVacia(FichasDelOtro) ->
        (
          contrincante(Turno,OtroTurno),
          primero(FichasDelOtro,(OtroTurno,DirSel,DistSel)),
          removerFicha(FichasDelOtro, (OtroTurno,DirSel,DistSel), OtroSinEsaFicha),
          removerFicha(ListaPosConFichas,(OtroTurno,DirSel,DistSel),PosicionesSinEsaFicha),

          %Para considerar efectivamente todas las jugadas (por ejemplo todas las posibles fichas del oponente que puedo sacar).
          %Hay que volver a agregar esa posición al tablero para pasar de nuevo y elegir otra ficha del oponente.
          %Por eso le paso "Tablero"

          (perteneceLista([(Turno,Dir,Dist)|PosicionesSinEsaFicha], AcJugadas) ->
             posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,AcJugadas,OtroSinEsaFicha, colocar);
             posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,[[(Turno,Dir,Dist)|PosicionesSinEsaFicha]|AcJugadas],OtroSinEsaFicha,colocar))
        );
        
        %Si no hay molino significa que terminé de procesar ese molino y tengo que restaurar las fichas del otro
        %Por si aparece un molino más adelante

         contrincante(Turno,OtroTurno),
         getMyFichas(ListaPosConFichas,OtroTurno,FichasDelOtroRecargado),
         
         %Si hay  molino es porque falló la segunda condicion del if
        (perteneceLista([(Turno,Dir,Dist)|ListaPosConFichas], AcJugadas); hayMolino(Turno,Dir,Dist,ListaPosConFichas,T) ->
             posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,NuevoTablero,AcJugadas,FichasDelOtroRecargado, colocar);
             posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,NuevoTablero,[[(Turno,Dir,Dist)|ListaPosConFichas]|AcJugadas],FichasDelOtroRecargado,colocar)
        )
     )
  ).
  
posibles_jugadas(_,_,Jugadas,[],Jugadas,_,_,_).
  
%Fichas del otro se usa para llevar la cuenta de las fichas que le quedan al otro por si hay molino
posibles_jugadas(Turno, (ListaPosConFichas,T), Jugadas, Tablero, AcJugadas, FichasDelOtro, mover, [ListaAdyacentes|Xs]) :-

  %Se expande para todos los posibles lugares donde puede colocar una ficha
  %Es una posicion del tablero
  primero(Tablero,(Dir,Dist)),
  remover_pos(Tablero,(Dir,Dist),NuevoTablero),
  %Es una posicion libre
  (pertenece((_,Dir,Dist),ListaPosConFichas) ->
     posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,NuevoTablero,AcJugadas,FichasDelOtro, mover, Xs);

     (noEsVacia(ListaAdyacentes) -> (
     
         primero(ListaAdyacentes, (DirAdy, DistAdy)),
         remover_pos(ListaAdyacentes, (DirAdy, DistAdy), TailListaAdyacentes),
         %Si está libre me fijo que la primer pos ady tenga una ficha mía.
         ( pertenece((Turno,DirAdy,DistAdy),ListaPosConFichas) -> (

           %Caso positivo, tengo una posición libre adyacente a una ficha mía, entonces hago el cambio.

           %Remuevo la adyacente
           removerFicha(ListaPosConFichas,(Turno,DirAdy,DistAdy),PosicionesSinEsaFicha),
           %concatenacion(PosicionesSinEsaFicha,[(Turno,Dir,Dist)],ListPosConCambio),

           %Si agregar la nueva posicion genera molino también hay que expandir
           %en función de las posibilidades de sacar la ficha del otro.
           (hayMolino(Turno,Dir,Dist,PosicionesSinEsaFicha,T), noEsVacia(FichasDelOtro) ->
              (
                contrincante(Turno,OtroTurno),
                primero(FichasDelOtro,(OtroTurno,DirSel,DistSel)),
                removerFicha(FichasDelOtro, (OtroTurno,DirSel,DistSel), OtroSinEsaFicha),
                removerFicha(PosicionesSinEsaFicha,(OtroTurno,DirSel,DistSel),PosicionesSinEsaFichaMolino),

                %Para considerar efectivamente todas las jugadas (por ejemplo todas las posibles fichas del oponente que puedo sacar).
                %Hay que volver a agregar esa posición al tablero para pasar de nuevo y elegir otra ficha del oponente.
                %Por eso le paso "Tablero"

                (perteneceLista([(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino], AcJugadas) ->
                   posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,AcJugadas,OtroSinEsaFicha, mover, [ListaAdyacentes|Xs]);
                   posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,[[(Turno,Dir,Dist)|PosicionesSinEsaFichaMolino]|AcJugadas],OtroSinEsaFicha,mover, [ListaAdyacentes|Xs]))
              );

              %Si no hay molino significa que terminé de procesar ese molino y tengo que restaurar las fichas del otro
              %Por si aparece un molino más adelante

               contrincante(Turno,OtroTurno),
               getMyFichas(PosicionesSinEsaFicha,OtroTurno,FichasDelOtroRecargado),

               %Si hay  molino es porque falló la segunda condicion del if
              (perteneceLista([(Turno,Dir,Dist)|PosicionesSinEsaFicha], AcJugadas); hayMolino(Turno,Dir,Dist,PosicionesSinEsaFicha,T) ->
                   posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,AcJugadas,FichasDelOtroRecargado, mover,[TailListaAdyacentes|Xs]);
                   posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,[[(Turno,Dir,Dist)|PosicionesSinEsaFicha]|AcJugadas],FichasDelOtroRecargado,mover, [TailListaAdyacentes|Xs])
              )
           ));
             posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,Tablero,AcJugadas,FichasDelOtro, mover, [TailListaAdyacentes|Xs])
         )
     );
       posibles_jugadas(Turno, (ListaPosConFichas,T),Jugadas,NuevoTablero,AcJugadas,FichasDelOtro, mover, Xs)
     )
  ).

%Diferencia entre mis fichas y las de mi oponente.
%Importan los casi molinos (2 fichas que están a una  jugada de ser molino)
%Importan más los de mi oponente, porque en su turno va a completar ese molino.
%Acá no importan los molinos, porque los conté antes. En la diferencia de la cantidad de piezas.
heuristica(ListaPosConFichas, Turno, _, Valor, T) :-
  contrincante(Turno,OtroTurno),
  DistMax is T+1,
  generarTodasLasPosiciones(DistMax,Tablero, []),

  casiMolinos(Tablero,T,ListaPosConFichas,Turno,CasiMolinosMios,0),
  casiMolinos(Tablero,T,ListaPosConFichas,OtroTurno,CasiMolinosOtro,0),
  %cantMolinos(T,ListaPosConFichas,ListaPosConFichas,Turno,MolinosMios,0),
  cantFichas(ListaPosConFichas,Turno,Mias,0),
  cantFichas(ListaPosConFichas,OtroTurno,DelOtro,0),
  %Se ponderan las distintas situaciones
  Valor is 4*(DelOtro - Mias) + 2*CasiMolinosMios - 3*CasiMolinosOtro.

%Se generan todas las posiciones del tablero, luego para cada posición se simula
%que exista una ficha y se ve si hay molinos (en ese caso tengo un "casi molino")
casiMolinos([],_,_,_,Ac,Ac).

casiMolinos(Tablero,T,ListaPosConFichas,Turno,CasiMolinosMios,Ac) :-

  primero(Tablero,(Dir,Dist)),
  remover_pos(Tablero,(Dir,Dist),NuevoTablero),
  %Es una posicion libre
  (pertenece((_,Dir,Dist),ListaPosConFichas) ->
     casiMolinos(NuevoTablero,T,ListaPosConFichas,Turno,CasiMolinosMios,Ac);
     (hayMolino(Turno,Dir,Dist,ListaPosConFichas,T) ->
         Sig is Ac+1;
         Sig is Ac),
     casiMolinos(NuevoTablero,T,ListaPosConFichas,Turno,CasiMolinosMios,Sig)
  ).
