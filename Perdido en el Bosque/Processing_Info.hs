
module Processing_Info
(processing_info,
matching,
search_direction,
sub_search_direction,
search_object_use,
search_verb,
transform_verb,
prepositions,
conjuntions,
articles,
pronouns,
directions,
other_words) where

import Funtions



--processing_info:
--Devuelve un lista de String donde cada posición es:
--[acción (verbo), objetivo de la acción (sustantivo),
-- objeto con que se realiza (sustantivo(s)), lugar (adverbio)]
--ejemplo: ["mirar", "closet", "" "debajo"]
processing_info :: String -> [String]
processing_info info = let new_info = split info
                       in matching new_info


--Permite transformar la lista de palabras que necesita
--processing info
matching :: [String] -> [String]
matching list = let list_0 = to_lower_all list
                    list_1 = clean_list list_0 prepositions
                    list_2 = clean_list list_1 conjuntions
                    list_3 = clean_list list_2 pronouns
                    list_4 = clean_list list_3 articles
                    list_5 = clean_list list_4 other_words
                    (verb, form_verb) = search_verb list_5
                    list_6 = remove form_verb list_5 
                    direction = search_direction list_6
                    list_7 = clean_list list_6 directions
                    object_use = search_object_use list_7
                    list_8 = clean_list_after list_7 "con"
                    sustantive = union list_8 " "
                    in [verb, sustantive, object_use, direction]
 

--search_direction
--Verifica si en la lista existe alguna palabra
--que exprese dirección y la devuelve
search_direction :: [String] -> String
search_direction [] = ""
search_direction (x:xs) = let dir = sub_search_direction x
                          in if dir == "" then search_direction xs
                             else dir

--matchea la dirección para ver cuál entregar
sub_search_direction :: String -> String
sub_search_direction x | (member x ["arriba", "norte", "encima", "sobre"]) = "encima"
                       | (member x ["abajo", "sur", "debajo", "bajo"]) = "debajo"
                       | (member x ["izquierda", "oeste"]) = "izquierda"
                       | (member x ["derecha", "este"]) = "derecha"
                       | (member x ["detras", "atras"]) = "detras"
                       | (member x ["adentro", "dentro"]) = "dentro"
                       | (member x ["frente"]) = "frente"
                       | otherwise = ""

--search_object_use
--Busca si existe la palabra "con", y une el resto de palabras
--siguientes
search_object_use :: [String] -> String
search_object_use [] = ""
search_object_use (x:xs) = if x == "con" then union xs " "
                           else search_object_use xs


--search_verb
--Entrega el verbo correspondiente a una lista de verbos
--y formas verbales, y la palabra mediante la que fue encontrado
search_verb :: [String] -> (String, String)
search_verb [] = ("", "")
search_verb (x:xs) = let new_verb = transform_verb x
                     in if new_verb == "" then search_verb xs
                        else (new_verb, x)


--Función que, dado una forma verbal, devuelve 
--devuelve el verbo raíz, o, el verbo raíz sinónimo.
transform_verb :: String -> String
transform_verb x | (member x ["mirar", "miro", "mira", "miras",
                              "ver", "veo", "ves",
                              "observar", "observo", "observa", "obervas",
                              "contemplar", "contemplo", "contempla",
                              "revisar", "reviso", "revisa", "revisas"]) = "mirar"
                 | (member x ["coger", "cojo", "coge", "coges",
                              "agarrar", "agarro", "agarra", "agarras",
                              "tomar", "tomo", "toma", "tomas",
                              "recoger", "recojo", "recoge", "recoges",
                              "adquirir", "adquiero", "adquiere", "adquieres"]) = "coger"
                 | (member x ["abrir", "abro", "abre", "abres"]) = "abrir"
                 | (member x ["ser", "soy", "es"]) = "ser"
                 | (member x ["llamar", "llamo", "llama", "llamas"]) = "llamar"
                 | (member x ["buscar", "busco", "busca"]) = "buscar"
                 | (member x ["salir", "salgo", "sale", "salirme", "sales"]) = "salir"
                 | (member x ["entrar", "entro", "entra", "entras"]) = "entrar"
                 | (member x ["ir", "voy", "ve", "irme",
                              "caminar", "camino", "camina", "caminas",
                              "correr", "corro", "corre", "corres"]) = "ir"
                 | (member x ["comer", "como", "come", "comes",
                              "consumir", "consumo", "consume", "consumes"]) = "comer"
                 | (member x ["beber", "bebo", "bebe", "bebes"]) = "beber"
                 | (member x ["escribir", "escribo", "escribe", "escribes"]) = "escribir"
                 | (member x ["leer", "leo", "lee", "lees"]) = "leer"
                 | (member x ["hablar", "hablo", "habla", "hablas",
                              "conversar", "converso", "conversar", "conversas"]) = "hablar"
                 | (member x ["quitar", "quito", "quita", "quitas",
                              "eliminar", "elimino", "elimina", "eliminas"]) = "quitar"
                 | (member x ["nadar", "nado", "nada", "nadas"]) = "nadar"
                 | (member x ["bailar", "bailo", "baila", "bailas"]) = "bailar"
                 | (member x ["escuchar", "escucho", "escucha", "escuchas",
                              "oir", "oigo", "oye", "oyes"]) = "escuchar"
                 | (member x ["cocinar", "cocino", "cocina", "cocinas"]) = "cocinar"
                 | (member x ["limpiar", "limpio", "limpia", "limpias"]) = "limpiar"
                 | (member x ["alimentar", "alimento", "alimenta", "alimentas"]) = "alimentar"
                 | (member x ["ayudar", "ayudo", "ayuda", "ayudas"]) = "ayudar"
                 | (member x ["vestir", "visto", "viste", "vestirme", "vistes"]) = "vestir"
                 | (member x ["cambiar", "cambio", "cambia", "cambias"]) = "cambiar"
                 | (member x ["acostar", "acuesto", "acortarme", "acuestate", "acuestas"]) = "acostarse"
                 | (member x ["subir", "subo", "sube", "subirme", "subes",
                              "trepar", "trepo", "trepa", "treparme", "trepas"]) = "subir"
                 | (member x ["bajar", "baja", "bajas"]) = "bajar"
                 | (member x ["tirar", "tiro", "tira", "tiras", 
                              "lanzar", "lanzo", "lanza", "lanzas"]) = "lanzar"
                 | (member x ["colocar", "coloco", "coloca", "colocas", 
                              "poner", "pongo", "pon", "pones"]) = "colocar"
                 | (member x ["encender", "encender", "encender"]) = "encender"
                 | (member x ["curar", "curo", "cura", "curas"]) = "curar"
                 | (member x ["destupir", "destupo", "destupe", "destupes"]) = "destupir"
                 | otherwise = ""

--Lista de preposiciones (no incluye "con", "bajo", "sobre")
prepositions :: [String]
prepositions = ["a", "antes", "de", "desde", "contra",
                "en", "entre", "para", "por", "segun",
                "sin", "tras", "hacia", "hasta"]

--Lista de conjunciones
conjuntions ::  [String]
conjuntions = ["y", "e", "ni", "que", "o",
                "u", "pero", "aunque", "mas",
                "embargo", "sino"]

--Lista de artículos
articles :: [String]
articles = ["la", "el", "las", "los", "un", "una", 
             "del", "lo"]

--Lista de pronombres personales, indefinidos, demostrativos
--y posesivos (no incluye "este")
pronouns :: [String]
pronouns = ["yo", "me", "mí", "conmigo", "tú",
            "te", "ti", "contigo", "usted", "vos",
            "él", "lo", "le", "se", "sí", "consigo",
            "ella", "la", "ello", "lo",
            "un", "uno", "unos", "una", "unas",
            "algún", "alguno", "algunos", "alguna", "algunas",
            "otro", "otros", "otra", "otras",
            "mucho", "muchos", "mucha", "muchas",
            "esto", "estos", "esta", "estas",
            "ese", "eso", "esos", "esa", "esas",
            "aquel", "aquello", "aquellos", "aquella", "aquellas",
            "mi", "mío", "mía", "mis", "mías", "míos",
            "tu", "tuyo", "tuyos", "tuya", "tuyas",
            "nuestro", "nuestros", "nuestra", "nuestras",
            "su", "sus", "suyo", "suya",
            "vuestro", "vuestros", "vuestra", "vuestras"]

--Lista de direcciones las que realizar una acción
directions :: [String]
directions = ["arriba", "abajo", "izquierda", "derecha",
              "norte", "sur", "este", "oeste",
              "bajo", "debajo", "encima", "frente", "sobre",
              "atrás", "detras", "dentro", "adentro"]

--Lista de palabras que no pueden ser utilizadas:
other_words :: [String]
other_words = ["se", "al", "quiero", "querer",
              "quisiera", "quieres", "hay", "haber",
              "habria", "hubiera", "habia", "hubo"]
