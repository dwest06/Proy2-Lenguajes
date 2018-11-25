:- dynamic popularidad/2. % Permitir cambiar popularidad
:- dynamic generoAnime/2. % Permitir cambiar generos de un anime
% Revisar si nos importa hacer cleanup http://www.swi-prolog.org/howto/database.html

anime("Naruto").
anime("Dragon Ball").
anime("Bleach").
anime("HunterXHunter").
anime("Hamtaro").
anime("Full Metal Alchemist").
anime("Neon Genesis Evangelion").

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
generoAnime("Neon Genesis Evangelion", ["Shounen", "Mecha", "Aventura"]).

rating("Dragon Ball",3).
rating("Naruto",1).
rating("Bleach",4).
rating("HunterXHunter",5).
rating("Hamtaro",2).
rating("Full Metal Alchemist",4).
rating("Neon Genesis Evangelion", 5).

popularidad("Dragon Ball",7).
popularidad("Naruto",5).
popularidad("Bleach",8).
popularidad("HunterXHunter",3).
popularidad("Hamtaro",10).
popularidad("Full Metal Alchemist",1).
popularidad("Neon Genesis Evangelion", 2).

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

preguntar_popularidad(A, P1) :- 
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
% Comparing
% Compara dos strings de anime por rating
ord_rat(>, A1, A2) :- rating(A1, R1), rating(A2, R2), R1 > R2.
ord_rat(<, A1, A2) :- rating(A1, R1), rating(A2, R2), R1 =< R2.

% Compara dos strings de anime por popularidad
ord_pop(>, A1, A2) :- popularidad(A1, P1), popularidad(A2, P2), P1 > P2.
ord_pop(<, A1, A2) :- popularidad(A1, P1), popularidad(A2, P2), P1 =< P2.

% Compara dos strings de anime por rating y popularidad
ord_rat_pop(>, A1, A2) :- 
    rating(A1, R1), rating(A2, R2),
    popularidad(A1, P1), popularidad(A2, P2),
     P1 + R1 > P2 + R2.
ord_rat_pop(<, A1, A2) :- 
    rating(A1, R1), rating(A2, R2), 
    popularidad(A1, P1), popularidad(A2, P2),
    P1 + R1 =< P2 + R2.

% Sorting
% Ordena una lista de strings de anime por rating de mayor a menor
sort_rat(L1, L3) :- predsort(ord_rat, L1 , L2), reverse(L2, L3).

% Ordena una lista de strings de anime por popularidad de mayor a menor
sort_pop(L1, L3) :- predsort(ord_pop, L1 , L2), reverse(L2, L3).

% Ordena una lista de strings de anime por rating y popularidad de mayor a menor
sort_rat_pop(L1, L3) :- predsort(ord_rat_pop, L1 , L2), reverse(L2, L3).

% Animes por Genero
% Encuentra todos los Animes de un genero G
find_gen(G, As) :- findall(A, (generoAnime(A, Gs),member(G, Gs)) , As).

% Encuentra todos los Animes de un genero G ordenados por rating
find_gen_rat(G, As) :- find_gen(G, A1s), !, sort_rat(A1s, As).

% Encuentra todos los Animes de un genero G ordenados por popularidad
find_gen_pop(G, As) :- find_gen(G, A1s), !, sort_pop(A1s, As).

% Encuentra todos los Animes de un genero G ordenados por rating y popularidad
find_gen_rat_pop(G, As) :- find_gen(G, A1s), !, sort_rat_pop(A1s, As).


% Filtar una lista de animes por Rating
filter_rat(_, [], []) :- !.
filter_rat(R, [A|T1], [A|T2]) :- rating(A, R), !, filter_rat(R, T1, T2).
filter_rat(R, [_|T1], T2) :- !, filter_rat(R, T1, T2).

% Animes que tenga rating R del genero G
find_rat_gen(R, G, As) :- find_gen_rat(G, A1s), filter_rat(R, A1s, As).

% Encontrar Animes Buenos (rating de 4 y 5) pero Poco Conocidos (popularidad <= 5)
ani_buenos(As) :- 
    findall(A, 
    (
        rating(A, R), R > 3, 
        popularidad(A, P), P =< 5
    ), 
    A1s), sort_rat(A1s, As).


%Palabras a reconocer

requerimiento(["rating", "genero", "popularidad", "populares", "buenos","poco", "conocido", "ordenados", "menor", "mayor"]).
res_genericas(["Que tengas un buen dia", "Quiere saber sobre animes?","Te gusta el helado?", "Que bueno!"]).
anadir(["añademe", "añade", "incluye", "añadir"]).

% Lectura de Bot
readTokens:- 
    %Lee desde el stdin
    current_input(Stream), read_line_to_string(Stream, String), split_string(String, " \t", "\n\r\t,", Tokens), 
    % retorna los Tokens necesarios para el parseo
    !, procesar_tok(Tokens,[]), !.

%Procesamos los Tokens para filtrar las palabras claves.
%Para salir del ciclo
procesar_tok(["quit"],_) :- !.

% Fin del primer procesamiento
procesar_tok([],Z):- write(Z), nl, parser_tok(Z), main2.

%Para reconocer numeros
procesar_tok([Tok|Tokens],Tokneed):-
    atom_number(Tok, _),
    append(Tokneed, [Tok], R),
    procesar_tok(Tokens, R), !.

%Para reconocer requerimientos
procesar_tok([Tok|Tokens],Tokneed):-
    requerimiento(Q),
    string_lower(Tok, Tok1),
    member(Tok1, Q),
    append(Tokneed, [Tok1], R),
    procesar_tok(Tokens, R), !.

%Para reconocer cuando añadir
procesar_tok([Tok|Tokens],Tokneed):-
    requerimiento(Q),
    string_lower(Tok, Tok1),
    member(Tok1, Q),
    append(Tokneed, [Tok1], R),
    procesar_tok(Tokens, R), !.
    
%Para reconocer animes
procesar_tok(Tokens,Tokneed):-
    recAnime(Tokens, Tok, NextTokens),
    append(Tokneed, [Tok], R),
    procesar_tok(NextTokens, R), !.

%Para reconocer genero
procesar_tok(Tokens,Tokneed):-
    recGenero(Tokens, Tok, NextTokens),
    append(Tokneed, [Tok], R),
    procesar_tok(NextTokens, R), !.

%Para procesar palabras no reconocidas.
procesar_tok([_|Tokens], Tokneed):- 
    procesar_tok(Tokens, Tokneed), !.    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Los Tokens filtrados pasan por "estados" para llegar a la queries indicada.
%Respuestas Genericas cuando el usuario haga un consulta no valida.
parser_tok([]):-
    respuesta_generica, !.

%Para cuando es genero
parser_tok([Tok|Tokens]):-
    genero(Tok),
    parser_tok2(Tokens, Tok), !.

%Para cuando es un requerimiento
parser_tok([Tok|Tokens]):-
    requerimiento(T),
    member(Tok, T),
    parser_tok2(Tokens, Tok), !.

parser_tok([Tok| _ ]):-
    anime(Tok),
    preguntar_popularidad(Tok, _), !,
    prettyAniFull(Tok, Z),
    write(Z), nl.

%%%%%%%%%%%%%%%%%%%%%%
%Animes buenos segun su popularidad
parser_tok2(_, Tok2):-
    (Tok2 == "buenos"; Tok2 == "poco"),
    ani_buenos(As),
    write("Estos son los animes buenos pero poco conocidos: "), nl, prettyWriteAnis(As),nl, !.

%Mostrar animes segun su genero
parser_tok2([], Genero):-
    genero(Genero),
    find_gen(Genero,As),
    write("Del genero "), write(Genero),write(": "), nl, prettyWriteAnis(As), nl.

%lista de animes de genero Genero segun el rating
parser_tok2([Tok|Tokens], Genero):-
    Tok == "rating",
    nth0(0, Tokens, Num),
    atom_number(Num,R),
    find_rat_gen(R, Genero, As),
    write("Del genero "), write(Genero), write(" :"), nl, prettyWriteAnis(As),nl, !.

%lista de animes de genero Genero ordenados
parser_tok2([Tok|Tokens], Genero):-
    Tok == "ordenados",
    parser_tok3(Tokens, Genero), !.

parser_tok2([ _ | _ ], _):-
    write("Token no reconocido"), nl, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ordenados por rating
parser_tok3([Tok], Genero):-
    Tok == "rating",
    find_gen_rat(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por rating:"), 
    nl, prettyWriteAnis(As),nl, !.

%Ordenados por popularidad
parser_tok3([Tok], Genero):-
    Tok == "popularidad",
    find_gen_pop(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por popularidad:"), 
    nl, prettyWriteAnis(As),nl, !.

% Ordenados por popularidad y rating
parser_tok3([Tok|[Tok2]], Genero):-
    Tokens = [Tok, Tok2],
    member("rating", Tokens),
    member("popularidad", Tokens),
    find_gen_rat_pop(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por rating y popularidad:"), 
    nl, prettyWriteAnis(As),nl, !.

% ordenados por rating de menor a mayor
parser_tok3([Tok | ["menor" | ["mayor"]]], Genero):-
    Tok == "rating",
    find_gen_rat(Genero,As1),
    reverse(As1, As),
    write("Del genero "), write(Genero), write(" ordenados por rating de menor a mayor:"), 
    nl, prettyWriteAnis(As),nl, !.

% ordenados por rating de mayor a menor
parser_tok3([Tok | ["mayor" | ["menor"]]], Genero):-
    Tok == "rating",
    find_gen_rat(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por rating de mayor a menor:"), 
    nl, prettyWriteAnis(As),nl, !.

% ordenados por popularidad de menor a mayor
parser_tok3([Tok | ["menor" | ["mayor"]]], Genero):-
    Tok == "popularidad",
    find_gen_rat(Genero,As1),
    reverse(As1, As),
    write("Del genero "), write(Genero), write(" ordenados por rating de menor a mayor:"), 
    nl, prettyWriteAnis(As),nl, !.

% ordenados por popularidad de mayor a menor
parser_tok3([Tok | ["mayor" | ["menor"]]], Genero):-
    Tok == "popularidad",
    find_gen_rat(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por rating de mayor a menor:"), 
    nl, prettyWriteAnis(As),nl, !.

% Ordenados por popularidad y rating de menor a mayor
parser_tok3([Tok|[Tok2 | ["menor" | ["mayor"]]]], Genero):-
    Tokens = [Tok, Tok2],
    member("rating", Tokens),
    member("popularidad", Tokens),
    find_gen_rat_pop(Genero,As1),
    reverse(As1, As),
    write("Del genero "), write(Genero), write(" ordenados por rating y popularidad de menor a mayor:"), 
    nl, prettyWriteAnis(As),nl, !.

% Ordenados por popularidad y rating de mayor a menor
parser_tok3([Tok|[Tok2 | ["mayor" | ["menor"]]]], Genero):-
    Tokens = [Tok, Tok2],
    member("rating", Tokens),
    member("popularidad", Tokens),
    find_gen_rat_pop(Genero,As),
    write("Del genero "), write(Genero), write(" ordenados por rating y popularidad de mayor a menor:"), 
    nl, prettyWriteAnis(As),nl, !.

%
%parser_tok([Tok|Tokens]):-
%    .

%parser_tok([Tok|_]):-
%    write("Token no reconocido"),nl.

%parser_tok2([Tok|Tokens]):-
%    .


respuesta_generica:- random_between(0, 3, R), res_genericas(L), nth0(R, L, E), write(E), nl, !.


%Aqui se hace el loop infinito, Se separa 
main2:- readTokens, !.
main :- write("Bienvenido a AniBot."), nl, !, main2,!.

% Concatenar una lista de strings con un separador
% De https://stackoverflow.com/questions/4708235/concatting-a-list-of-strings-in-prolog
strSepCat([ ],_,Empty) :-
    string_to_list(Empty,[ ]).
strSepCat([H|T],Separator,StrCat) :-
    strSepCat(T,Separator,H,StrCat).

% Concatenar una lista de strings con un separador, mantiene un acumulador
strSepCat([ ],_,StrCat,StrCat).
strSepCat([H|T],Sep,Str,Cat) :-
    string_concat(Sep,H,SepH),
    string_concat(Str,SepH,StrSepH),
    strSepCat(T,Sep,StrSepH,Cat).

% Reconoce un anime del prefijo de una lista de tokens y lo devuelve. 
% Tambien devuelve los siguientes tokens al anime
recAnime(Tokens, A, NextTokens) :- 
    append(Part, NextTokens, Tokens), strSepCat(Part, " ", A), anime(A), !.

% Reconoce un genero del prefijo de una lista de tokens y lo devuelve. 
% Tambien devuelve los siguientes tokens al genero
recGenero(Tokens, G, NextTokens) :- 
    append(Part, NextTokens, Tokens), strSepCat(Part, " ", G), genero(G), !.

% Pretty String de una lista de Generos
prettyGens(Gs, S) :- 
    strSepCat(Gs, ", ", S1), string_concat("[", S1, S2), string_concat(S2, "]", S), !.

% Pretty String de toda la info de un anime
prettyAniFull(A, S) :- 
    generoAnime(A, Gs), prettyGens(Gs, GString), 
    rating(A, R), atom_string(R, RString),
    popularidad(A, P), popularidad_string(P, PString),
    string_concat(A, " con Rating de ", S1),
    string_concat(S1, RString, S2),
    string_concat(S2, ", el cual es ", S3),
    string_concat(S3, PString, S4),
    string_concat(S4, " y de los Generos ", S5),
    string_concat(S5, GString, S), !.


% Pretty String de toda la info de una lista de animes
prettyAnisFull(As, S) :- 
    prettyAnisStringList(As, PrettyAs), 
    strSepCat(PrettyAs, ".\n  * ", S1), 
    string_concat("  * ", S1, S2), string_concat(S2, ".", S), !.

% Lista de Pretty Strings con toda la info de una lista de animes
prettyAnisStringList([], []) :- !.
prettyAnisStringList([A|As], [S1|T]) :- 
    prettyAniFull(A, S1), prettyAnisStringList(As, T).

% Imprime en consola una lista de animes con toda su informacion
prettyWriteAnis(As) :- prettyAnisFull(As, S), write(S), !.