
module Place_5
(place_5,
place_5_darkness,
place_5_revision,
place_5_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_5_transform_sustantive
--Agrupa series de sustantivos de forma común para el quinto lugar
place_5_transform_sustantive :: String -> String
place_5_transform_sustantive x | (member x ["escaleras", "escaleras pasillo", "pasillo"]) = "pasillo"
                               | (member x ["vela", "vela lampara", "lampara vela"]) = "vela"
                               | (member x ["encendedor", "mechero", "fosforera"]) = "mechero"
                               | (member x ["nota", "nota pared", "nota pegada pared",
                                            "papel", "papel pared", "papel pegado pared"]) = "nota"
                               | otherwise = x

--Quinto lugar de la historia, el desván de la casa
--El desván necesita tener luz para ser revisado

--Revisión para ver si el cuarto ya fue encendido, o sigue apagado

place_5_revision :: Player -> [String] -> (Player, String)

place_5_revision player text = if ((decision_array player) !! 5) == 1 then
                                 place_5 player text else
                                 place_5_darkness player text


--Acciones que puedes realizar con el desván apagado
place_5_darkness :: Player -> [String] -> (Player, String)

place_5_darkness player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas todas las cosas tiradas, deben llevar ahí mucho tiempo")

place_5_darkness player ["mirar", "lampara", "", ""] | member "Vela" (object_used player)
                                                      = (player, "La vela se encuentra dentro de la lámpara, " ++
                                                         "necesitas encencerla")
                                                     | member "Vela" (inventory player)
                                                      = (player, "La lámpara está vacía, intenta colocar algo")
                                                     | otherwise = (player, "La lámpara está vacía, no la puedes encender")

place_5_darkness player ["mirar", _, _, _] = (player, "Está totalmente oscuro")

place_5_darkness player ["colocar", "vela", "", ""] | member "Vela" (object_used player)
                                                      = (player, "La vela ya se encuentra dentro de la lámpara.")
                                                    | member "Vela" (inventory player)
                                                      = ((remove_player_inventory
                                                      (add_player_object_used player "Vela") "Vela"),
                                                       "Colocas la Vela apagada dentro de la lámpara")
                                                    | otherwise = (player, "No tienes ninguna Vela")

place_5_darkness player ["encender", "vela", "mechero", ""]
                                   | (member "Vela" (object_used player) && member "Mechero" (inventory player))
                                    = let new_player = change_player_decision_array player 1 5
                                    in (new_player,
                                    "Enciendes la vela de la lámpara. Ya puedes ver dentro de la habitación \n" ++ 
                                    (place_5_info new_player))
                                    | not (member "Mechero" (inventory player))
                                    = (player, "No tienes con qué encender algo")
                                    | otherwise = (player, "No tienes nada que encender")

place_5_darkness player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_5_darkness player ["salir", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5_darkness player ["salir", "desvan", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5_darkness player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_5_darkness player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_5_darkness player ["entrar", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Entras al pasillo. \n" ++ place_2_info)

place_5_darkness player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")

place_5_darkness player ["ir", sustantive, object_use, direction]
                            | (object_use /= "")
                            = (player, "No necesitas nada en este momento")
                            | (sustantive == "")
                            = (player, "A dónde piensas ir?")

place_5_darkness player ["ir", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5_darkness player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_5_darkness player ["bajar", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5_darkness player ["bajar", _, "", ""]
                            = (player, "No puedes bajar de esa forma")

place_5_darkness player _ = (player, "No puedes hacer eso porque no ves casi nada")

--Acciones que puedes realizar con el desván encendido

place_5 :: Player -> [String] -> (Player, String)

place_5 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas todas las cosas tiradas, deben llevar ahí mucho tiempo")

place_5 player ["mirar", "nota" , "", ""] = (player, "Lees la nota, por la cantidad de polvo \n" ++
                                                     "que tiene debe llevar ahí mucho tiempo. Dice: \n" ++
                                                     "Hijo, salí al bosque, cerré la puerta con llave, \n" ++
                                                     "si no he regresado antes del mediodía, toma la llave \n " ++
                                                     "detrás del armario para salir.")

place_5 player ["mirar", "escaparate" , "", ""] = (player, "El escaparate tiene ambas puertas cerradas por un " ++
                                                           "candado oxidado")

place_5 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_5 player ["coger", "nota", "", ""] = (player, "Está pegada en la pared, no puedes cogerla")

place_5 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_5 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_5 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_5 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_5 player ["abrir", "escaparate" , _, ""] = (player, "No parece que puedas abrirlo con eso")

place_5 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")

place_5 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_5 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_5 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_5 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_5 player ["salir", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5 player ["salir", "desvan", "", ""]
                = ((change_player_location player 2),
                "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_5 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_5 player ["entrar", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Entras al pasillo. \n" ++ place_2_info)

place_5 player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")

place_5 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_5 player ["ir", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_5 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_5 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_5 player ["leer", "nota" , "", ""] = (player, "Lees la nota, por la cantidad de polvo \n" ++
                                                     "que tiene debe llevar ahí mucho tiempo. Dice: \n" ++
                                                     "Hijo, salí al bosque, cerré la puerta con llave," ++
                                                     "si no he regresado antes del mediodía, toma la llave " ++
                                                     "detrás del armario para salir.")

place_5 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_5 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_5 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_5 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_5 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_5 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_5 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_5 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_5 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_5 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_5 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_5 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_5 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_5 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_5 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_5 player ["bajar", "pasillo", "", ""]
                            = ((change_player_location player 2),
                            "Bajas las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_5 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_5 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_5 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_5 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_5 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo")

place_5 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo")

place_5 player _ = (player, "Intenta hacer otra cosa")