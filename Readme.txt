# Universidad Simón Bolívar

## Proyecto 1 de Lenguajes

De Christian Oliveros 13-11000 y David Rodriguez 14-10930.

### Ejecucion

* Tener previamente instalado swipl
* Ejecutar swipl -s AniBot.pl
* Ejecutar el predicado main.

### Diseño

El Bot provee de 9 predicacion principales

* Preguntar_populridad: 
	Se usa implicitamente para aumentar la popularidad si el usuario pregunta 5 veces.

* addAnime:
	Existen dos versiones, Cuando se da la popularidad y cuando no. Agrega a la base de datos un anime con su nombre, su rating, su popularidad (Si es dada por el usuario) y sus generos. Internamente hace uso de otros predicados como addGenero, el cualq verifica si uno de los generos del anime a agregar existe, en caso de que no exista, se agrega a la base de datos de generos.

* find_gen:
	Encuentra todos los animes dado un Genero G. 

* find_gen_rat:
	Encuentra todos los animes dado un Genero G, ordenados por rating, el usuario puede pedir como quiere que se ordene, si de mayor a menor, o viceversa. 
	
* find_gen_pop:
	Encuentra todos los animes dado un Genero G, ordenados por popularidad, el usuario puede pedir como quiere que se ordene, si de mayor a menor, o viceversa. 

* find_gen_rat_pop:
	Encuentra todos los animes dado un Genero G, ordenados por rating + popularidad, el usuario puede pedir como quiere que se ordene, si de mayor a menor, o viceversa. 

* find_rat_gen:
	Encuentra animes que tenga rating R del Genero G. Internamente hace uso del predicado find_gen_rat.

* ani_buenos:
	Encuentra todos los animes que son buenos (con rating mayor a 4) pero que son poco conocidos (Popularidad menor a 5)

* ani_rat:
	Encuentra todos los animes dado un rating R


Finalmente, se implemento un lexer, el cual filtra los tokens que son claves para realizar la consulta. Por otra parte se implemento un parser, el cual sigue un camino segun los tokens filtrados por el lexer. Con esta estrategia se logró tener conversaciones lo mas natural posible con el usuario.

Como ultimo punto, si el lexer no detecta palabras claves, simplemente arroja una respuesta generica, esta puede tener sentido al texto dado por el usuario o no.