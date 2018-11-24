:- dynamic popularidad/2. % Permitir cambiar popularidad
:- dynamic generoAnime/2. % Permitir cambiar generos de un anime
% Revisar si nos importa hacer cleanup http://www.swi-prolog.org/howto/database.html

anime("Naruto").
anime("Dragon Ball").
anime("Bleach").
anime("HunterXHunter").
anime("Hamtaro").
anime("Full Metal Alchemist").

genero("Aventura").
genero("Shoujo").
genero("Shounen").
genero("Kodomo").
genero("Seinen").
genero("Josei").
genero("Ficción").
genero("Fantasía").
genero("Mecha").
genero("Sobrenatural").
genero("Magia").
genero("Gore").

generoAnime("Naruto",["Shounen","Aventura"]).
generoAnime("Dragon Ball",["Shounen"]).
generoAnime("Bleach",["Shounen", "Sobrenatural"]).
generoAnime("HunterXHunter",["Seinen", "Aventura"]).
generoAnime("Hamtaro",["Kodomo"]).
generoAnime("Full Metal Alchemist",["Shounen", "Magia"]).

rating("Dragon Ball",3).
rating("Naruto",1).
rating("Bleach",4).
rating("HunterXHunter",5).
rating("Hamtaro",2).
rating("Full Metal Alchemist",4).

popularidad("Dragon Ball",7).
popularidad("Naruto",5).
popularidad("Bleach",8).
popularidad("HunterXHunter",3).
popularidad("Hamtaro",10).
popularidad("Full Metal Alchemist",1).

% Reglas de Bot

% Popularidad a String (La Popularidad puede ser un numero en un string)
popularidad_string(PString, Resp) :- 
    string(PString), !, atom_number(PString, P), !, popularidad_string(P, Resp).
popularidad_string(P, "Muy poco conocido") :- 0 < P, P < 3, !.
popularidad_string(P, "Poco conocido") :- 2 < P, P < 6, !.
popularidad_string(P, "Conocido") :- 5 < P, P < 8, !.
popularidad_string(P, "Muy conocido") :- 7 < P, P < 10, !.
popularidad_string(10, "Bastante conocido") :- !.

% Subir Rating si usuarios preguntan 5 veces.
preguntar_popularidad(A, P) :- 
    \+(current_predicate(pregunta_popularidad/2)), !, % Necesario si no existe otro predicado parecido
    assertz(pregunta_popularidad(A, 1)), !,   
    popularidad(A, P).

preguntar_popularidad(A, P) :- 
    \+(pregunta_popularidad(A, _)), !, % No se ha preguntado para este anime
    assertz(pregunta_popularidad(A, 1)), !,   
    popularidad(A, P).

preguntar_popularidad(A, P) :- 
    pregunta_popularidad(A, 4), !,
    retract(pregunta_popularidad(A, 4)),
    popularidad(A, P),
    P1 is min(P+1, 10), !,
    retract(popularidad(A, P)),
    assertz(popularidad(A, P1)).

preguntar_popularidad(A, P) :- 
    pregunta_popularidad(A, N), !,
    N1 is N+1,
    retract(pregunta_popularidad(A, N)),
    assertz(pregunta_popularidad(A, N1)),
    popularidad(A, P).


% Obtener los primeros N elementos de una lista
trim(_, 0, []).
trim([H|T1], N, [H|T2]) :- trim(T1, N1, T2), N is N1+1.

% Anadir Un Genero nuevo, si no existia antes
addGenero(G) :- \+(genero(G)), !, assertz(genero(G)).
% Anadir Lista de Generos
addGeneros([]) :- !.
addGeneros([N|T]) :- addGenero(N), !, addGeneros(T).
addGeneros([_|T]) :- addGeneros(T).

% Anadir Anime nuevo, si no existia antes, o actualizar su lista de generos. 
% Generos tiene que ser no vacio 
% Manejo de Rating
% Si no se da popularidad, por default se coloca como 1. 
% Si no se lee string de Popularidad, se coloca como 1.
% Si ya el anime tiene popularidad, no se coloca nueva popularidad.
addAnime(A, GS, RString, PString) :- 
    \+(popularidad(A, _)),
    atom_number(PString, P), !, 
    addAnime(A, GS, RString), !, 
    P1 is min(P, 10), P2 is max(P1, 1),
    retract(popularidad(A, 1)), assertz(popularidad(A, P2)), !.

addAnime(A, GS, RString, _) :- addAnime(A, GS, RString), !.

addAnime(A, GS, RString) :- 
    \+(rating(A, _)),
    atom_number(RString, R), !, 
    addAnime(A, GS), !, 
    R1 is min(R, 5), R2 is max(R1, 1),
    retract(rating(A, 1)), assertz(rating(A, R2)), !.

addAnime(A, GS, _) :- addAnime(A, GS), !.

addAnime(A, GS) :- 
    \+(anime(A)), !,
    length(GS, L), 0 < L, !, 
    assertz(anime(A)), addGeneros(GS), 
    Tam is min(L, 5),
    trim(GS, Tam, FirstGenres), 
    assertz(generoAnime(A, FirstGenres)), assertz(popularidad(A, 1)), assertz(rating(A, 1)), !.

addAnime(A, GS) :- 
    length(GS, L), 0 < L, !,
    generoAnime(A, GSPrev),
    addGeneros(GS),
    append(GSPrev, GS, GSNew),
    list_to_set(GSNew, GSNoDup),
    length(GSNoDup, L2), 
    Tam is min(L2, 5),
    trim(GSNoDup, Tam, FirstGenres), 
    retract(generoAnime(A, GSPrev)), assertz(generoAnime(A, FirstGenres)), !.

    


    
%Palabras a reconocer
Peticion(["dame", "dime", "muestrame", "recomiendame", "Quiero"]).
Requerimiento(["rating", "genero", "popularidad"]).


% Lectura de Bot
% Esto podria servirnos para leer
readTokens(Tokens) :- 
    current_input(Stream),
    read_line_to_string(Stream, String), write(String),nl, split_string(String, " \t", "\n\r\t", Tokens),
    write(Tokens), nl,
    procesar_tok(Tokens).

%Si para salir del ciclo
procesar_tok(["quit"]).

procesar_tok([]):- write("Fin"),nl,main.

%Para reconocer peticiones
procesar_tok([Tok|Tokens],[Tok]):-
    write(Tok), nl, procesar_tok(Tokens).

%Para procesar requerimientos
procesar_tok([Tok|Tokens]):- 
    write("A").

%Para procesar palabras no reconocidas.
procesar_tok([_|Tokens]):- 
    write("A"). 
    




%Aqui se hace el loop infinito, Se separa 
main :- write("Bienvenido a AniBot."), nl, readTokens(Z).


