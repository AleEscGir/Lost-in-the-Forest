
module Place_3
(place_3,
place_3_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_3_transform_sustantive
--Agrupa series de sustantivos de forma común para el tercer lugar
place_3_transform_sustantive :: String -> String
place_3_transform_sustantive x | (member x ["ducha", "tina", "bañadera"]) = "ducha"
                               | (member x ["lavamanos", "lavado"]) = "lavamanos"
                               | (member x ["taza", "retrete"]) = "retrete"
                               | (member x ["puerta", "puerta pasillo", "pasillo"]) = "puerta"
                               | (member x ["grifo", "llave", "pila", "ducha"]) = "grifo"
                               | (member x ["espejo", "espejo botiquin"]) = "espejo"
                               | (member x ["vela", "velas",
                                            "vela botiquin", "velas botiquin"]) = "vela"
                               | (member x ["venda", "vendas",
                                            "venda botiquin", "vendas botiquin"]) = "vendas"
                               | (member x ["herida", "herida cabeza"]) = "herida"
                               | otherwise = x

--Tercer lugar de la historia, el aseo del 2do piso
place_3 :: Player -> [String] -> (Player, String)

place_3 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas a tu alrededor, el baño está muy sucio")

place_3 player ["mirar", "puerta", "", ""] = (player, "Observas la puerta del pasillo, está entreabierta")

place_3 player ["mirar", "lavamanos", "", ""] = (player, "El lavamanos está sucio, no sale agua del grifo")

place_3 player ["mirar", "retrete", "", ""] = (player, "El retrete está tupido con un líquido negro. \n" ++ 
                                                        "Parece que en el fondo hay un objeto, pero ahora " ++
                                                        "no puedes cogerlo")

place_3 player ["mirar", "ducha", "", ""] = (player, "El ducha tienes restos de... algo negro, no parece que haya agua")

place_3 player ["mirar", "botiquin", "", ""] = if ((decision_array player !! 4) == 1) then
                (player, "Miras el botiquin, está entreabierto \n") else
                if member "Destornillador" (inventory player) then
                (player, "Miras el botiquín, parece que ya puedes abrirlo") else
                (player, "El botiquín está sellado con varios tornillos")

place_3 player ["mirar", "botiquin", "", "dentro"] = if ((decision_array player !! 4) == 1) then
                (player, "Dentro del botiquín hay una vela y unas vendas enredadas \n") else
                (player, "El botiquín está bloqueado por varios tornillos, no puedes abrir con tus manos")

place_3 player ["mirar", "espejo", "", ""] = if ((decision_array player !! 1) == 1) then
                (player, "Te miras en el espejo, la herida no se ve tan mal. \n") else
                if member "Analgésico" (object_used player) then
                     (player, "Te miras en el espejo, tu herida ha dejado de sangrar, pero aún " ++
                              "debes colocarte algo que la tape") else
                (player, "Te miras en el espejo. Tienes una herida abierta en la cabeza. \n " ++
                         "Estás sangrando mucho. Debes intentar curarla con algo.")

place_3 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_3 player ["coger", "vela", "", ""] = if ((decision_array player !! 4) == 1) then
                (if (member "Vendas" (inventory player) || member "Vendas" (object_used player)) then
                (player, "Ya has cogido las cosas dentro del botiquín \n") else
                 ((add_player_inventory (add_player_inventory player "Vendas") "Vela"),
                 "Has desenredado las vendas de la vela, y has cogido ambos objetos")) else
                (player, "El botiquín está bloqueado por varios tornillos, no tienes con qué abrirlo")

place_3 player ["coger", "vendas", "", ""] = if ((decision_array player !! 4) == 1) then
                (if (member "Vendas" (inventory player) || member "Vendas" (object_used player)) then
                (player, "Ya has cogido las cosas dentro del botiquín \n") else
                 ((add_player_inventory (add_player_inventory player "Vendas") "Vela"),
                 "Has desenredado las vendas de la vela, y has cogido ambos objetos")) else
                (player, "El botiquín está bloqueado por varios tornillos, no tienes con qué abrirlo")

place_3 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_3 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_3 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_3 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_3 player ["abrir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales. \n" ++ place_2_info)

place_3 player ["abrir", "botiquin", "", ""] = if ((decision_array player !! 4) == 1) then
                (player, "El botiquin ya está abierto \n") else
                (player, "El botiquín está bloqueado por varios tornillos, no tienes con qué abrirlo")

place_3 player ["abrir", "botiquin", "destornillador", ""] = if ((decision_array player !! 4) == 1) then
                (player, "El botiquin ya está abierto \n") else
                if member "Destornillador" (inventory player) then
                ((change_player_decision_array player 1 4), 
               "Has quitado todos los tornillos y has abierto el botiquín") else
                (player, "No tienes ningún destornillador")

place_3 player ["abrir", "botiquin", object_use, ""] = if ((decision_array player !! 4) == 1) then
                (player, "El botiquin ya está abierto \n") else
                (player, "El botiquín está bloqueado por varios tornillos, no puedes abrirlo con " ++
                          object_use)

place_3 player ["abrir", "grifo", "ducha", ""] = (player, "El grifo de la ducha no gira, al parecer" ++
                                                          "está bloqueado, pero ya estaba abierto")

place_3 player ["abrir", "lavamanos", "ducha", ""] = (player, "El grifo del lavamanos no gira, al parecer" ++
                                                          "está bloqueado, pero ya estaba abierto")

place_3 player ["abrir", "grifo", "", ""] = (player, "Cuál grifo quieres abrir?")

place_3 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")

place_3 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_3 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_3 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_3 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_3 player ["salir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales al pasillo. \n" ++ place_2_info)

place_3 player ["salir", "aseo", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales del aseo. \n" ++ place_2_info)

place_3 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_3 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado no?")

place_3 player ["entrar", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y entras al pasillo. \n" ++ place_2_info)

place_3 player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")


place_3 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_3 player ["ir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales al pasillo. \n" ++ place_2_info)

place_3 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_3 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_3 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_3 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_3 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_3 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_3 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_3 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_3 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_3 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_3 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_3 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_3 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_3 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_3 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_3 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_3 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_3 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_3 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_3 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_3 player ["colocar", "analgesico herida", "", ""] = if ((decision_array player !! 1) == 1) then
                (player, "Esa herida ya fue curada. \n") else
                if member "Analgésico" (inventory player) then
                ((add_player_object_used (remove_player_inventory player "Analgésico") "Analgésico"),
                 "Echas cuidadosamente el analgésico en la herida, frente al espejo. \n " ++
                          "Tu cabeza te ha dejado de doler un poco, pero aún debes taparte la herida") else
                (player, "No tienes dicho objeto.")

place_3 player ["colocar", "vendas herida", "", ""] = if ((decision_array player !! 1) == 1) then
                (player, "Esa herida ya fue curada. \n") else
                if member "Analgésico" (object_used player) then
                (if member "Vendas" (inventory player) then
                     ((add_player_object_used (remove_player_inventory
                    (change_player_decision_array player 1 1)
                    "Vendas") "Vendas"),
                    "Envuelves la herida cuidadosamente frente al espejo. \n " ++
                          "Te sientes mucho mejor, ya no te duele la cabeza") else
                    (player, "No tienes dicho objeto")) else
                if member "vendas" (inventory player) then
                        (player, "Primero debes echar algo en la herida antes de vendarla") else 
                        (player, "No tienes dicho objeto.")

place_3 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_3 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_3 player ["curar", "herida", "analgesico", ""] = if ((decision_array player !! 1) == 1) then
                (player, "Esa herida ya fue curada. \n") else
                if member "Analgésico" (inventory player) then
                ((add_player_object_used (remove_player_inventory player "Analgésico") "Analgésico"),
                 "Echas cuidadosamente el analgésico en la herida, frente al espejo. \n " ++
                          "Tu cabeza te ha dejado de doler un poco, pero aún debes taparte la herida") else
                (player, "No tienes dicho objeto.")

place_3 player ["curar", "herida", "vendas", ""] = if ((decision_array player !! 1) == 1) then
                (player, "Esa herida ya fue curada. \n") else
                if member "Analgésico" (object_used player) then
                (if member "Vendas" (inventory player) then
                     ((add_player_object_used (remove_player_inventory
                    (change_player_decision_array player 1 1)
                    "Vendas") "Vendas"),
                    "Envuelves la herida cuidadosamente frente al espejo. \n " ++
                          "Te sientes mucho mejor, ya no te duele la cabeza") else
                    (player, "No tienes dicho objeto")) else
                          if member "vendas" (inventory player) then
                        (player, "Primero debes echar algo en la herida antes de vendarla") else 
                        (player, "No tienes dicho objeto.")

place_3 player ["curar", _, _, _] = if ((decision_array player !! 1) == 1) then
                (player, "La herida de tu cabeza ya fue curada. \n") else
                if member "Analgésico" (object_used player) then
                (player, "Necesitas algo con lo que envolver la herida \n ") else
                (player, "Necesitas algo con qué curar la herida de la cabeza")

place_3 player ["destupir", "retrete", "destupidor", ""] =
                                  if member "Destupidor" (inventory player) then
                                  ((remove_player_inventory (add_player_object_used 
                                  (add_player_inventory player "Destornillador")
                                   "Destupidor") "Destupidor"),
                                   "Destupes el retrete del aseo, en el fondo, el objeto que la obstruía aparece. \n" ++
                                   "Consigues un destornillador. El Destupidor, ya viejo, se rompe y lo desechas") else
                                   (player, "No tienes con qué destupir el retrete")

place_3 player ["destupir", "retrete", "", ""] =
                                  if member "Destupidor" (inventory player) then
                                  ((remove_player_inventory (add_player_object_used 
                                  (add_player_inventory player "Destornillador")
                                   "Destupidor") "Destupidor" ),
                                   "Destupes el retrete del aseo, en el fondo, el objeto que la obstruía aparece. \n" ++
                                   "Consigues un destornillador. El Destupidor, ya viejo, se rompe y lo desechas") else
                                   (player, "No tienes con qué destupir el retrete")

place_3 player ["destupir", _, _, _] = (player, "Necesitas decir qué quieres destupir y con qué objeto")

place_3 player _ = (player, "Intenta hacer otra cosa")