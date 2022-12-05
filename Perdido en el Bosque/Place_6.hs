
module Place_6
(place_6,
place_6_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_6_transform_sustantive
--Agrupa series de sustantivos de forma común para el sexto lugar
place_6_transform_sustantive :: String -> String
place_6_transform_sustantive x | (member x ["sala", "puerta sala"]) = "sala"
                               | (member x ["taburete", "taburetes"]) = "taburete"
                               | (member x ["martillo", "martillo horno"]) = "martillo"
                               | (member x ["palanca", "mecanismo horno",
                                            "palanca horno"]) = "palanca"
                               | (member x ["destupidor", "destupidor meseta"]) = "destupidor"
                               | (member x ["mechero", "encendedor", "fosforera"]) = "mechero"
                               | otherwise = x

--Sexto lugar de la historia, la cocina
place_6 :: Player -> [String] -> (Player, String)

place_6 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "La cocina parece que hubiera sido usada recientemente")

place_6 player ["mirar", "refrigerador" , "", ""] = (player, "Es un refrigerador algo antiguo")

place_6 player ["mirar", "refrigerador" , "", "adentro"] = (player, "Revisas dentro del refrigerador \n" ++
                                                                     "no encuentras nada importante")

place_6 player ["mirar", "meseta" , "", ""] = (player, "Está llena de cazuelas, entre polvo y telarañas. \n" ++
                                                        "No parece que puedas hacer algo con ellas. Quizás \n" ++
                                                        "encima o debajo haya algo")

place_6 player ["mirar", "meseta" , "", "arriba"] = (player, "Está llena de cazuelas, entre polvo y telarañas. \n" ++
                                                             "No parece que puedas hacer algo con ellas")

place_6 player ["mirar", "meseta" , "", "debajo"] =
                               if (member "Destupidor" (inventory player) || member "Destupidor" (object_used player)) then
                               (player, "Está llena de trastos. No parece que alguno sirva") else
                                (player, "Está llena de trastos, el único aparentemente " ++
                                        "útil es un viejo destupidor.")


place_6 player ["mirar", "taburete" , "", ""] = if (member "Mechero" (inventory player)) then
                          (player, "Son unos taburetes comunes y corrientes") else
                          (player, "Uno de los taburetes tiene un mechero encima")

place_6 player ["mirar", "taburete" , "", "arriba"] = if (member "Mechero" (inventory player)) then
                          (player, "Son unos taburetes comunes y corrientes") else
                          (player, "Uno de los taburetes tiene un mechero encima")

place_6 player ["mirar", "mechero" , "", ""] = if (member "Mechero" (inventory player)) then
                          (player, "Ya has cogido el mechero") else
                          (player, "El mechero no tiene polvo, a diferencia del taburete. \n" ++
                                    "Lo habrán colocado recientemente?")

place_6 player ["mirar", "horno" , "", ""] | member "Palanca" (object_used player)
                                            = if ((decision_array player) !! 6) == 1 then
                                              (if (member "Martillo" (inventory player) ||
                                                   member "Martillo" (object_used player)) then
                                                   (player, "El horno está dejando de calentar poco a poco") else
                                                   (player, "Hay un objeto dentro del horno"))
                                             else (player, "El horno está encendido, es muy peligroso, no lo abras.")
                                             | otherwise = (player, "El horno está encendido, es muy peligroso, no lo abras. \n" ++
                                             "A su lado hay un mecanismo con el que se puede apagar, pero falta algo que ayude " ++
                                             "a activarlo.")

place_6 player ["mirar", "horno" , "", "dentro"] | member "Palanca" (object_used player)
                                                = if ((decision_array player) !! 6) == 1 then
                                                (if (member "Martillo" (inventory player) ||
                                                    member "Martillo" (object_used player)) then
                                                    (player, "No queda nada dentro del horno") else
                                                    (player, "Hay un martillo dentro del horno"))
                                                else (player, "El horno está encendido, es muy peligroso, no lo abras.")
                                                | otherwise = (player, "El horno está encendido, es muy peligroso, no lo abras. \n" ++
                                                "A su lado hay un mecanismo con el que se puede apagar, pero le falta una pieza que " ++
                                                "a activarlo.")

place_6 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_6 player ["coger", "destupidor", "", ""] =
                               if (member "Destupidor" (inventory player) || member "Destupidor" (object_used player)) then
                               (player, "Ya has cogido el destupidor") else
                                ((add_player_inventory player "Destupidor"),  
                                 "Coges el Destupidor, no parece que pueda ser usado muchas veces")

place_6 player ["coger", "martillo" , "", "dentro"] | member "Palanca" (object_used player)
                                                = if ((decision_array player) !! 6) == 1 then
                                                (if (member "Martillo" (inventory player) ||
                                                    member "Martillo" (object_used player)) then
                                                    (player, "Ya has cogido el martillo") else
                                                    ((add_player_inventory player "Martillo"), 
                                                    "Coges el martillo dentro del horno"))
                                                else (player, "No puedes coger nada mientras el horno esté encendido.")
                                                | otherwise = (player, "No puedes coger nada mientras el horno esté encendido.")

place_6 player ["coger", "martillo" , "", ""] | member "Palanca" (object_used player)
                                                = if ((decision_array player) !! 6) == 1 then
                                                (if (member "Martillo" (inventory player) ||
                                                    member "Martillo" (object_used player)) then
                                                    (player, "Ya has cogido el martillo") else
                                                    ((add_player_inventory player "Martillo"), 
                                                    "Coges el martillo dentro del horno"))
                                                else (player, "No puedes coger nada mientras el horno esté encendido.")
                                                | otherwise = (player, "No puedes coger nada mientras el horno esté encendido.")

place_6 player ["coger", "mechero" , "", ""] = if (member "Mechero" (inventory player)) then
                          (player, "Ya has cogido el Mechero") else
                          ((add_player_inventory player "Mechero"), "Coges el Mechero")

place_6 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_6 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_6 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_6 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_6 player ["abrir", "horno" , "", ""] | member "Palanca" (object_used player)
                                            = if ((decision_array player) !! 6) == 1 then
                                              (player, "El horno ya está abierto")
                                             else ((change_player_decision_array player 1 6),
                                             "Abres el horno, el calor comienza a disminuir")
                                             | not (member "Palanca" (object_used player))
                                             = ((change_player_location player (-1)),
                                             "Intentas abrir el horno, este explota repentinamente. " ++
                                             "El calor y el impacto te hacen desmayarte. Fin del juego...")
                                             | otherwise = ((change_player_location player (-1)),
                                             "Intentas abrir el horno, este explota repentinamente. " ++
                                             "El calor y el impacto te hacen desmayarte. Fin del juego...")

place_6 player ["abrir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_6 player ["abrir", "refrigerador", "", ""]
                = (player, "Abres el refrigerador, no encuentras nada interesante")

place_6 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")

place_6 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_6 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_6 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_6 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_6 player ["salir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_6 player ["salir", "cocina", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_6 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_6 player ["entras", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_6 player ["entras", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_6 player ["entras", _, "", _] = (player, "Debes querer entrar a algún lado no?")


place_6 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_6 player ["ir", "sala", "", ""]
                = ((change_player_location player 4), "Abres la puerta y entras a la sala. \n" ++ place_4_info)

place_6 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_6 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_6 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_6 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_6 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_6 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_6 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_6 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_6 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_6 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_6 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_6 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_6 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_6 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_6 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_6 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_6 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_6 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_6 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_6 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_6 player ["colocar", "palanca" , "", ""]  | member "Palanca" (object_used player)
                                                  = (player, "Ya has usado la Palanca como mecanismo para el horno")
                                                | not (member "Palanca" (object_used player))
                                                  = if member "Palanca" (inventory player) then
                                                  ((remove_player_inventory (add_player_object_used
                                                  (change_player_decision_array player 1 6) "Palanca") "Palanca"),
                                                  "Has colocado la Palanca en el mecanismo del horno y lo has apagado. \n" ++
                                                  "Ahora puedes abrirlo") else
                                                  (player, "No puedes hacer eso ahora")
                                                | otherwise = (player, "No puedes hacer eso ahora")

place_6 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_6 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_6 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo")

place_6 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo")

place_6 player _ = (player, "Intenta hacer otra cosa")