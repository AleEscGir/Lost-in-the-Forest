
module Place_7
(place_7,
place_7_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_7_transform_sustantive
--Agrupa series de sustantivos de forma común para el séptimo lugar
place_7_transform_sustantive :: String -> String
place_7_transform_sustantive x | (member x ["sala", "puerta sala"]) = "sala"
                               | (member x ["analgesico", "pomo analgesico"]) = "analgesico"
                               | (member x ["gaveta", "gaveta comoda"]) = "gaveta"
                               | (member x ["objetos", "objetos comoda"]) = "objetos"
                               | (member x ["llave", "llave desvan",
                                            "llave comoda", "llave desvan comoda",
                                            "llave gaveta", "llave desvan gaveta",
                                            "llave gaveta comoda", "llave desvan gaveta comoda"]) = "llave"
                               | otherwise = x

--Séptimo lugar de la historia, el cuarto del primer piso
place_7 :: Player -> [String] -> (Player, String)

place_7 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas el cuarto, es bastante pequeño")

place_7 player ["mirar", "sala", "", ""] = (player, "Observas la puerta de la sala. \n" ++
                                                    "Tiene varios agujeros verticales por lo que se cuela la luz")

place_7 player ["mirar", "cama", "", ""] = (player, "No parece haber sido usada en un buen tiempo")

place_7 player ["mirar", "comoda", "", ""] = (player, "Es una cómoda bastante elegante, tiene varios objetos encima.")

place_7 player ["mirar", "comoda", "", "encima"] = if (member "Analgésico" (inventory player) ||
                                                       member "Analgésico" (object_used player)) then
                                                     (player, "Solo encuentras varios trastos, no parece servir ninguno") else
                                                     (player, "Revisas todos los trastos, entre ellos \n " ++ 
                                                              "solo parece servir un pomo de analgésico")

place_7 player ["mirar", "objetos", "", "encima"] = if (member "Analgésico" (inventory player) ||
                                                       member "Analgésico" (object_used player)) then
                                                     (player, "Solo encuentras varios trastos, no parece servir ninguno") else
                                                     (player, "Revisas todos los trastos, entre ellos \n " ++ 
                                                              "solo parece servir un pomo de analgésico")

place_7 player ["mirar", "gaveta", "", ""] = if ((decision_array player) !! 7) == 1 then
                                                    (player, "La gaveta está entreabierta, con las marcas de " ++
                                                             "los clavos extraídos") else
                                                    (player, "La gaveta está sellada por varios clavos, debería \n" ++
                                                             "ser posible sacarlos con la herramienta adecuada")

place_7 player ["mirar", "gaveta", "", "dentro"] | ((decision_array player) !! 7) == 1
                                                          = if (member "Llave del Desvan" (inventory player) || 
                                                                member "Llave del Desvan" (inventory player)) then
                                                            (player, "La gaveta está vacía") else
                                                            (player, "Miras dentro de la gaveta, en el fondo se encuentra " ++
                                                            "la llave del desván")
                                                        | not (((decision_array player) !! 7) == 1)
                                                          = (player, "No puedes mirar dentro hasta que la hayas abierto")
                                                        | otherwise = (player, "No puedes mirar dentro hasta que la hayas abierto")

place_7 player ["mirar", "comoda", "", "dentro"] | ((decision_array player) !! 7) == 1
                                                    = if (member "Llave del Desvan" (inventory player) || 
                                                        member "Llave del Desvan" (inventory player)) then
                                                        (player, "La gaveta está vacía") else
                                                    (player, "Miras dentro de la gaveta, en el fondo se encuentra " ++
                                                        "la llave del desván")
                                                    | not (((decision_array player) !! 7) == 1)
                                                    = (player, "No puedes mirar dentro hasta que la hayas abierto")
                                                    | otherwise = (player, "No puedes mirar dentro hasta que la hayas abierto")

place_7 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_7 player ["coger", "llave", "", ""]   | ((decision_array player) !! 7) == 1
                                                = if (member "Llave del Desván" (inventory player) || 
                                                      member "Llave del Desván" (inventory player))  then
                                                      (player, "Ya has cogido la Llave del Desván") else
                                                (add_player_inventory player "Llave del Desván",
                                                  "Coges la Llave del Desván dentro de la gaveta de la cómoda")
                                            | not (((decision_array player) !! 7) == 1)
                                            = (player, "No puedes coger nada dentro de la gaveta hasta que la hayas abierto")
                                            | otherwise = (player, "No puedes coger nada dentro de la gaveta hasta que la hayas abierto")

place_7 player ["coger", "llave", "", "dentro"]   | ((decision_array player) !! 7) == 1
                                                    = if (member "Llave del Desván" (inventory player) || 
                                                        member "Llave del Desván" (inventory player)) then
                                                        (player, "Ya has cogido la Llave del Desván") else
                                                    (add_player_inventory player "Llave del Desván",
                                                        "Coges la Llave del Desván dentro de la gaveta de la cómoda")
                                                    | not (((decision_array player) !! 7) == 1)
                                                    = (player, "No puedes coger nada dentro de la gaveta hasta que la hayas abierto")
                                                    | otherwise = (player, "No puedes coger nada dentro de la gaveta hasta que la hayas abierto")

place_7 player ["coger", "analgesico", "", ""] = if (member "Analgésico" (inventory player) ||
                                                     member "Analgésico" (object_used player)) then
                                                     (player, "Ya has cogido el Analgésico") else
                                                     ((add_player_inventory player "Analgésico"), "Coges el Analgésico")

place_7 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_7 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_7 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_7 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_7 player ["abrir", "gaveta", "", ""] 
                            | ((decision_array player) !! 7) == 1
                            = (player, "La gaveta ya fue abierta")
                            | not (((decision_array player) !! 7) == 1)
                            = if member "Martillo" (inventory player) then
                                (change_player_decision_array (player) 1 7,
                                "Quitas cuidadosamente los clavos de la gaveta con el martillo, logrando así abrirla") else
                                (player, "Necesitas alguna herramienta sacar los clavos que sellan la gaveta")
                            | otherwise = (player, "Necesitas alguna herramienta sacar los clavos que sellan la gaveta")

place_7 player ["abrir", "gaveta", "martillo", ""] 
                            | ((decision_array player) !! 7) == 1
                            = (player, "La gaveta ya fue abierta")
                            | not (((decision_array player) !! 7) == 1)
                            = if member "Martillo" (inventory player) then
                                (change_player_decision_array (player) 1 7,
                                "Quitas cuidadosamente los clavos de la gaveta con el martillo, logrando así abrirla") else
                                (player, "Necesitas alguna herramienta sacar los clavos que sellan la gaveta")
                            | otherwise = (player, "Necesitas alguna herramienta sacar los clavos que sellan la gaveta")

place_7 player ["abrir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_7 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")

place_7 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_7 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_7 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_7 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_7 player ["salir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_7 player ["salir", "cuarto", "", ""]
                = ((change_player_location player 4), "Abres la puerta y sales del cuarto. \n" ++ place_4_info)

place_7 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_7 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_7 player ["entrar", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_7 player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")

place_7 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_7 player ["ir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_7 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_7 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_7 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_7 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_7 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_7 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_7 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_7 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_7 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_7 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_7 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_7 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_7 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_7 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_7 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_7 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_7 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_7 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_7 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_7 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_7 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_7 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_7 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo")

place_7 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo")

place_7 player _ = (player, "Intenta hacer otra cosa")