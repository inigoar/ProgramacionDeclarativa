% Practica 1, Programacion declarativa

alumno_prode('Redondo', 'Aranguren', 'Inigo').
alumno_prode('Martinez', 'de las Alas Pumarino', 'Ignacio').


nat(0).          % El cero es un numero natural
nat(s(N)) :-     % El sucesor de un numero natural es un numero natural
	nat(N).


equal(0,0).             
equal(s(X),s(Y)) :-    % Functor que evalua que dos numeros son iguales
    equal(X,Y).


diff(s(X),s(Y)) :-     % Functor que evalua si dos numeros son distintos
    not(equal(X,Y)).   % En funcion del resultado del predicado equal


evenNumber(0).                              % El cero es un numero par
evenNumber(s(s(X))) :-                      % Functor que evalua si un numero es par
    evenNumber(X).          


isPieza(pieza(s(X),s(Y),s(Z),C)) :-          % Functor que evalua si la pieza introducida esta correctamente introducida
    nat(X),                                  % Valor anchura de la pieza
    nat(Y),                                  % Valor altura de la pieza
    nat(Z),                                  % Valor profundidad de la pieza
    colour(C).                               % Valor color de la pieza


colour(r).      % El rojo es un color correcto
colour(a).      % El azul es un color correcto
colour(v).      % El verde es un color correcto
colour(am).     % El amarillo es un color correcto


lessOrEqual(0,X) :- nat(X).     
lessOrEqual(s(X),s(Y)) :-        % Functor que evalua si un numero es menor o igual que otro
    lessOrEqual(X,Y).


compare(pieza(s(X1),_,s(Z1),C1),pieza(s(X2),_,s(Z2),C2)):-              % Functor que comprueba que la pieza1 es menor que la pieza 2
	colour(C1),                                                         % Comprueba que el color C1 es valido
	colour(C2),                                                         % Comprueba que el color C2 es valido 
	lessOrEqual(s(X1),s(X2)),                                         % El ancho de la pieza1 es menor que el de la pieza2
	lessOrEqual(s(Z1),s(Z2)).                                         % La profundidad de la pieza1 es menor que el de la pieza2


add(0,X,X) :- nat(X).                   
add(s(X),Y,s(Z)) :-	                         % Functor que realiza la operacion suma
    add(X, Y, Z).


addHeight(pieza(_,s(X),_,_) ,H,H0) :-       % Functor que realiza la suma de las alturas de las piezas
    add(s(X),H0,H).                         % Mediante la suma de las alturas de todas las piezas introducidas


makeColourList([],[]).                      % Functor para sacar la lista de colores de las construcciones
makeColourList([pieza(_,_,_,C)|P],[C|Z]) :- 
    makeColourList(P,Z).


isIncluded(X,[X|_]).            % Functor que compruba que el elemento X esta incluido en la lista 
isIncluded(X,[_|Tail]) :-
    isIncluded(X,Tail).



sublist([],_).                  % Functor que comprueba que la lista del primer argumento es subconjunto de la lista del segundo argumento
sublist([L1|L2],[L3|L4]) :- 
    isIncluded(L1,[L3|L4]),
    sublist(L2,[L3|L4]).
    
countClavosN(Nivel,X1) :-       % Functor que cuenta el numero de clavos
    countClavos(Nivel,0,X2),
    equal(X1,X2).
    
    
countClavos([],X,X).
countClavos([b|Nivel],X1,X2):-
   countClavos(Nivel,X1,X2).
countClavos([C|Nivel],X1,X2):-
    colour(C),
    countClavos(Nivel,s(X1),X2).


/* PRIMERA PARTE 
   Predicados para las construcciones tipo torre
*/


esTorre(Construccion) :- isTower(Construccion).                 % esTorre/1, predicado que recibe una construccion y comprueba que es una torre
isTower([]).
isTower([Construccion]) :- 
    isPieza(Construccion).                                      % Comprueba que la pieza esta correctamente definida
isTower([Construccion1,Construccion2|ConstruccionN]) :- 
    compare(Construccion1,Construccion2),                       % Compara el tamaño, condicion necesaria para que sea torre, cada pieza sólo puede ensamblarse encima de una pieza igual o más grande que ella
    isTower([Construccion2|ConstruccionN]).


alturaTorre(Construccion,H):-                                   % alturaTorre/2, predicado que recibe una construccion y la altura, y comprueba que se corresponde
    towerHeight(Construccion,H).     
towerHeight([],H0) :- H0 = 0.
towerHeight([Construccion|ConstruccionN],H) :-
	esTorre([Construccion|ConstruccionN]),                      % Comprobamos que la construccion es una torre
	addHeight(Construccion,H,H0),                               % Sumamos la altura
	towerHeight(ConstruccionN,H0).



coloresTorre(Construccion,Colores) :-                           % coloresTorre/2, predicado que recibe una construccion y una lista de colores, comprueba que se corresponda en orden los colores
    esTorre(Construccion),                                      % Comprobamos que la construccion es una torre
    checkColour(Construccion,Colores).
    
checkColour([],[]).                
checkColour([pieza(_,_,_,C)|P],[C1|C2]) :-                      % Functor que comprueba que el color de la pieza se corresponde con el color correspondiente en la lista
    C=C1,                                                       % Comprobacion del color
    checkColour(P,C2).


coloresIncluidos(Construccion1,Construccion2) :-                % coloresIncluidos/2, predicado que recibe dos construcciones y compara que los colores de la construccion1 estan en la construccion2
	esTorre(Construccion1),                                     % Comprueba que construccion1 es una torre
	esTorre(Construccion2),                                     % Comprueba que construccion2 es una torre
	makeColourList(Construccion1,Colores1),                     % Functor para sacar la lista de colores de la construccion1
	makeColourList(Construccion2,Colores2),                     % Functor para sacar la lista de colores de la construccion2 
	sublist(Colores1,Colores2).                                 % la lista de colores de la construccion1 tiene que ser subconjunto de la lista de la construccion2


/* SEGUNDA PARTE
   Predicados para las construcciones tipo edificio
*/


esEdificioPar(Construccion) :- isEven(Construccion).

isEven([]). 
isEven([Nivel|Construccion]) :-                                    % esEdificioPar/1, predicado que recibe un edificio y comprueba que cada nivel tiene un numero par de clavos
    countClavosN(Nivel,X),
    evenNumber(X),
    isEven(Construccion).

esEdificioPiramide([Nivel|Construccion]) :-		 % esEdificioPiramide/1, predicado que recibe un edificio y comprueba que es una piramide
    countClavosN(Nivel,X),
    isPyramyd(Construccion, X).

isPyramyd([], _).
isPyramyd([Nivel|Construccion], X1) :-
    countClavosN(Nivel,X2),
    lessOrEqual(X1,X2),
    diff(X1,X2),
    isPyramyd(Construccion,X2).
