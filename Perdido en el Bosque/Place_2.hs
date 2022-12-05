
module Place_2
(place_2,
place_2_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info


--place_2_transform_sustantive
--Agrupa series de sustantivos de forma común para el segundo lugar
place_2_transform_sustantive :: String -> String
place_2_transform_sustantive x | (member x ["aseo", "puerta aseo",
                                            "puerta cuarto aseo"]) = "aseo"
                               | (member x ["cuarto", "puerta cuarto",
                                            "cuarto segundo piso",
                                            "puerta cuarto segundo piso"]) = "cuarto"
                               | (member x ["sala", "primer piso",
                                            "escaleras", "escaleras sala",
                                            "escalera", "escalera sala",
                                            "escaleras primer piso",
                                            "escalera primer piso"]) = "sala"
                               | (member x ["desvan", "puerta desvan"]) = "desvan"
                               | (member x ["llave", "llave desvan",
                                            "llave desvan casa"]) = "llave"
                               | otherwise = x

--Segundo lugar de la historia, el pasillo del 2do piso

place_2 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas el pasillo, hay algunas telarañas en el techo")

place_2 player ["mirar", "puerta", "", ""] = (player, "Hay muchas puertas, cuál quieres ver?")

place_2 player ["mirar", "desvan", "", ""] = 
               if (member "Llave del Desván" (inventory player) || member "Llave del Desván" (object_used player)) then
               (player, "Observas la puerta del desván, ya puedes abrirla") else
               (player, "Observas la puerta del desván, al parecer está cerrada")

place_2 player ["mirar", "cuarto", "", ""] = (player, "Miras con detenimiento la puerta del cuarto. " ++
                                                    "Está algo desgastada")

place_2 player ["mirar", "aseo", "", ""] = (player, "Miras con detenimiento la puerta del aseo. " ++
                                                    "Está entreabierta")

place_2 player ["mirar", "escaleras", "", ""] = (player, "Observas las escaleras, dan un poco de miedo")

place_2 player ["mirar", "ventana", "", ""] = (player, "Miras por la ventana, a lo lejos ves unos árboles")

place_2 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_2 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_2 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_2 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_2 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_2 player ["abrir", "aseo", "", ""]
                = ((change_player_location player 3), "Abres la puerta y entras al aseo. \n" ++ place_3_info)

place_2 player ["abrir", "cuarto", "", ""]
                = ((change_player_location player 1), "Abres la puerta y entras al cuarto. \n" ++ place_1_info)

place_2 player ["abrir", "desvan", "", ""] = if ((decision_array player !! 2) == 1) then
                ((change_player_location player 5),
                "Abres la puerta y subes por unas escaleras al desván. \n" ++ (place_5_info player)) else
                (player, "No puedes entrar, está cerrado con llave")
    
place_2 player ["abrir", "desvan", "llave", ""] = if ((decision_array player !! 2) == 1) then
                (player, "La puerta ya está abierta \n" ++ place_5_info player) else
                if member "Llave del Desván" (inventory player) then
                ((remove_player_inventory
                (add_player_object_used 
                (change_player_decision_array player 1 2)
                "Llave del Desván")
                "Llave del Desván"),
                "Has abierto la Puerta del Desván con la Llave, ya puedes entrar al desván") else
                (player, "No tienes esa llave")

place_2 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")


place_2 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_2 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_2 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_2 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_2 player ["salir", "aseo", "", ""]
                = ((change_player_location player 3), "Abres la puerta y entras al aseo. \n" ++ place_3_info)

place_2 player ["salir", "cuarto", "", ""]
                = ((change_player_location player 1), "Abres la puerta y entras al cuarto. \n" ++ place_1_info)

place_2 player ["salir", "desvan", "", ""] = if ((decision_array player !! 2) == 1) then
                ((change_player_location player 5),
                "Abres la puerta y subes por unas escaleras al desván. \n" ++ (place_5_info player)) else
                (player, "No puedes entrar, está cerrado con llave")

place_2 player ["salir", "sala", "", ""]
                = ((change_player_location player 4), "Bajas las escaleras al Primer Piso. \n" ++ place_4_info)

place_2 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_2 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_2 player ["entrar", "aseo", "", ""]
                = ((change_player_location player 3), "Abres la puerta y entras al aseo. \n" ++ place_3_info)

place_2 player ["entrar", "cuarto", "", ""]
                = ((change_player_location player 1), "Abres la puerta y entras al cuarto. \n" ++ place_1_info)

place_2 player ["entrar", "desvan", "", ""] = if ((decision_array player !! 2) == 1) then
                ((change_player_location player 5),
                "Abres la puerta y subes por unas escaleras al desván. \n" ++ (place_5_info player)) else
                (player, "No puedes entrar, está cerrado con llave")

place_2 player ["entrar", "sala", "", ""]
                = ((change_player_location player 4), "Bajas las escaleras al Primer Piso. \n" ++ place_4_info)

place_2 player ["entras", _, "", _] = (player, "Debes querer salir a algún lado no?")


place_2 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_2 player ["ir", "aseo", "", ""]
                = ((change_player_location player 3), "Abres la puerta y entras al aseo. \n" ++ place_3_info)

place_2 player ["ir", "cuarto", "", ""]
                = ((change_player_location player 1), "Abres la puerta y entras al cuarto. \n" ++ place_1_info)

place_2 player ["ir", "desvan", "", ""] = if ((decision_array player !! 2) == 1) then
                ((change_player_location player 5), "Abres la puerta y subes por unas escaleras al desván. \n"
                                                    ++ (place_5_info player)) else
                (player, "No puedes entrar, está cerrado con llave")

place_2 player ["ir", "sala", "", ""]
                = ((change_player_location player 4), "Bajas las escaleras al Primer Piso. \n" ++ place_4_info)

place_2 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_2 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_2 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_2 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_2 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_2 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_2 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_2 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_2 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_2 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_2 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_2 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_2 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_2 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_2 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_2 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_2 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_2 player ["subir", "desvan", "", ""] = if ((decision_array player !! 2) == 1) then
                ((change_player_location player 5), "Abres la puerta y subes por unas escaleras al desván. \n"
                                                    ++ (place_5_info player)) else
                (player, "No puedes entrar, está cerrado con llave")

place_2 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_2 player ["bajar", "sala", "", ""]
                = ((change_player_location player 4), "Bajas las escaleras al Primer Piso. \n" ++ place_4_info)

place_2 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_2 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_2 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_2 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_2 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo")

place_2 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo")

place_2 player _ = (player, "Intenta hacer otra cosa")