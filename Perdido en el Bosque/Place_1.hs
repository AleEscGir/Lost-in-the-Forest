
module Place_1
(place_1,
place_1_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_1_transform_sustantive
--Agrupa series de sustantivos de forma común para el primer cuarto
place_1_transform_sustantive :: String -> String
place_1_transform_sustantive x | (member x ["mesa", "mesita", "mesita noche", "mesa noche"]) = "mesa"
                               | (member x ["gaveta, gaveta mesa", "gaveta mesita",
                                            "gaveta mesita noche", "gaveta mesa noche"]) = "gaveta"
                               | (member x ["armario", "puerta armario"]) = "armario"
                               | (member x ["zapatos", "zapatos cuero"]) = "zapatos"
                               | (member x ["guantes", "guantes piel"]) = "guantes"
                               | (member x ["llave", "llave entrada",
                                            "llave entrada casa",
                                            "llave armario"]) = "llave"
                               | (member x ["puerta", "puerta pasillo", "pasillo"]) = "puerta"
                               | otherwise = x

--Primer lugar de la historia, el cuarto del 2do piso de la casa
place_1 :: Player -> [String] -> (Player, String)
--place_1 player [verb, sustantive, object_use, direction]

place_1 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar.")
               | (sustantive == "")
               = (player, "Observas la habitación, se encuentra algo desordenada.")

place_1 player ["mirar", "cama", "", "debajo"] = if member "Zapatos de Cuero" (equipment player) then
               (player, "Echas un vistazo debajo de la cama, deberían limpiarla algún día.") else
               (player, "Echas un vistazo debajo de la cama, encuentras unos Zapatos de Cuero.")

place_1 player ["mirar", "cama", "", "derecha"] = (player, "Solo ves la mesita con la lámpara.")

place_1 player ["mirar", "cama", "", direction] = if direction == "" || direction == "encima"
                                                  then (player, "Observas la cama, está destendida.")
                                                  else (player, "No hay nada interesante que mirar.")

place_1 player ["mirar", "armario", "", ""] = (player, "Observas un viejo armario. " ++ 
                                               "Te resulta familiar. \n")

place_1 player ["mirar", "armario", "", "dentro"] = if member "Camisa de Lana" (equipment player) then
               (player, "Revisas dentro del armario, solo encuentras algunos trapos.") else
               (player, "Revisas dentro del armario, encuentras Ropa.")

place_1 player ["mirar", "armario", "", "detras"] = if member "Llave de la Entrada" (inventory player) then
               (player, "Solo encuentras un montón de polvo.") else
               (player, "Revisas detrás del armario. Encuentras una Llave.")

place_1 player ["mirar", "puerta", "", ""] = (player, "Observas la puerta de tu cuarto. Está entreabierta, " ++
                                                       "alguien la abrió recientemente.")

place_1 player ["mirar", "mesa", "", ""] = (player, "Observas una vieja mesa de noche con una lámpara encima. " ++
                                             "La gaveta está entreabierta.")

place_1 player ["mirar", "mesa", "", "encima"] = if member "Pulsera" (inventory player) then
               (player, "La lámpara encima de la mesa de noche" ++
                        "emite una tenue luz.") else
               (player, "La lámpara encima de la mesa de noche" ++
                        "emite una tenue luz. A su lado ves una Pulsera.")

place_1 player ["mirar", "mesa", "", "dentro"] = if member "Guantes de Piel" (equipment player) then
               (player, "Está vacío") else
               (player, "Encuentras un par de Guantes de Piel.")

place_1 player ["mirar", "gaveta", "", ""] = (player, "Observas la gaveta de la mesa de noche. \n" ++
                                             "Está entreabierta. No hay nada peculiar")

place_1 player ["mirar", "pulsera", "", ""] = if member "Pulsera" (inventory player) then
               (player, "La Pulsera ahora está en tu inventario.") else
               (player, "Observas detenidamente la Pulsera sobre la mesa, fue " ++
                        "hecha cuidadosamente. Te resulta familiar.")

place_1 player ["mirar", "ventana", "", ""] = (player, "Miras por la ventana, ves unos pájaros volar.")

place_1 player ["mirar", "ventana", "", "afuera"] = (player, "Miras por la ventana, ves unos pájaros volar.")

place_1 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar.")

place_1 player ["coger", "pulsera", "", ""]
                = if member "Pulsera" (inventory player) then
                    (player, "La pulsera ya está en tu inventario.") else
                    ((add_player_inventory player "Pulsera"),"Te has colocado la pulsera en el brazo.")

place_1 player ["coger", "ropa", "", ""]
                = if member "Camisa de Lana" (equipment player) then
                    (player, "La ropa ya está en tu inventario.") else
                    ((change_player_equipment (change_player_equipment player "Camisa de Lana" 1) "Pantalón de Lana" 3),
                    "Te has vestido con una Camisa de Lana y un Pantalón de Lana.")

place_1 player ["coger", "zapatos", "", ""]
                = if member "Zapatos de Cuero" (equipment player) then
                    (player, "Los zapatos ya están en tu inventario.") else
                    ((change_player_equipment player "Zapatos de Cuero" 4), "Te has puesto los Zapatos de Cuero.")

place_1 player ["coger", "guantes", "", ""] = if member "Guantes de Piel" (equipment player) then
                    (player, "Ya tienes puestos los Guantes.") else
                    ((change_player_equipment player "Guantes de Piel" 2), "Te has puesto los Guantes de Piel.")

place_1 player ["coger", "llave", "", "detras"]
                = if member "Llave de la Entrada" (inventory player) then
                    (player, "La LLave de la Entrada ya está en tu inventario.") else
                    ((add_player_inventory player "Llave de la Entrada"), "Has cogido la Llave de la Entrada.")

place_1 player ["coger", _, "", _] = (player, "Debes saber qué quieres coger y dónde se encuentra.")

place_1 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres.")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora.")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío.")

place_1 player ["ser", sustantive, "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel.")

place_1 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo.")

place_1 player ["abrir", "armario", "", ""] = if member "Camisa de Lana" (equipment player) then
               (player, "Abres la puerta del armario y revisas dentro, solo encuentras algunos trapos.") else
               (player, "Abres la puerta del armario y revisas dentro, encuentras Ropa.")

place_1 player ["abrir", "mesa", "", ""] = if member "Guantes de Piel" (equipment player) then
               (player, "Abres la gaveta de la mesa. Está vacía.") else
               (player, "Abres la gaveta de la mesa. Encuentras un par de Guantes de Piel.")

place_1 player ["abrir", "gaveta", "", ""] = if member "Guantes de Piel" (equipment player) then
               (player, "Abres la gaveta de la mesa. Está vacía.") else
               (player, "Abres la gaveta de la mesa. Encuentras un par de Guantes de Piel.")

place_1 player ["abrir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales. \n" ++ place_2_info)

place_1 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo.")

place_1 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada.")

place_1 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo.")

place_1 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora.")

place_1 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento.")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio.")

place_1 player ["salir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales al pasillo. \n" ++ place_2_info)

place_1 player ["salir", "cuarto", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales del cuarto. \n" ++ place_2_info)

place_1 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_1 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento.")
               | (sustantive == "")
               = (player, "Debes querer entrar a algún lado, no?.")

place_1 player ["entrar", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y entras al pasillo. \n" ++ place_2_info)

place_1 player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")


place_1 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento.")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_1 player ["ir", "puerta", "", ""]
                = ((change_player_location player 2),"Abres la puerta y sales al pasillo. \n" ++ place_2_info)

place_1 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_1 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo.")

place_1 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo.")

place_1 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo.")

place_1 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde.")

place_1 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo.")

place_1 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado.")

place_1 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo.")

place_1 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa.")

place_1 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")

place_1 player ["limpiar", "cama", "", "debajo"] = 
               if member "Zapatos de Cuero" (equipment player) then
               (player, "Limpias bastante debajo de la cama, algo te mira fijamente desde una esquina.") else
               (player, "Limpias un poco debajo de la cama, unos zapatos te sonríen.")

place_1 player ["limpiar", "armario", "", "detras"] = 
               if (member "Llave de la Entrada" (inventory player) || member "Llave de la Entrada" (object_used player)) then
               (player, "Limpias bastante detrás del armario, un conejo de polvo salta y sale corriendo.") else
               (player, "Limpias un poco detrás del armario, algo brilla, pero no puedes ver qué es.")
    
place_1 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses.")

place_1 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_1 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece.")

place_1 player ["vestir", "ropa", "", ""]
                = if member "Camisa de Lana" (equipment player) then
                    (player, "La ropa ya está en tu inventario.") else
                    ((change_player_equipment (change_player_equipment player "Camisa de Lana" 1) "Pantalón de Lana" 3),
                    "Te has vestido con una Camisa de Lana y un Pantalón de Lana.")

place_1 player ["vestir", "zapatos", "", ""]
                = if member "Zapatos de Cuero" (equipment player) then
                    (player, "Los zapatos ya están en tu inventario") else
                    ((change_player_equipment player "Zapatos de Cuero" 4), "Te has puesto los Zapatos de Cuero.")

place_1 player ["vestir", "guantes", "", ""] = if member "Guantes de Piel" (equipment player) then
                    (player, "Ya tienes puestos los Guantes") else
                    ((change_player_equipment player "Guantes de Piel" 2), "Te has puesto los Guantes de Piel.")
                    
place_1 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner.")

place_1 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos.")

place_1 player ["acostar", "cama", _, _] = (player, "Te acuestas en la cama un rato, una mala sensación te hace " ++
                                            "querer levantarte de nuevo.")

place_1 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_1 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse.")

place_1 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_1 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos.")

place_1 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_1 player ["colocar", "ropa", "", ""]
                = if member "Camisa de Lana" (equipment player) then
                    (player, "La ropa ya está en tu inventario.") else
                    ((change_player_equipment (change_player_equipment player "Camisa de Lana" 1) "Pantalón de Lana" 3),
                    "Te has vestido con una Camisa de Lana y un Pantalón de Lana.")

place_1 player ["colocar", "zapatos", "", ""]
                = if member "Zapatos de Cuero" (equipment player) then
                    (player, "Los zapatos ya están en tu inventario") else
                    ((change_player_equipment player "Zapatos de Cuero" 4), "Te has puesto los Zapatos de Cuero.")

place_1 player ["colocar", "guantes", "", ""] = if member "Guantes de Piel" (equipment player) then
                    (player, "Ya tienes puestos los Guantes") else
                    ((change_player_equipment player "Guantes de Piel" 2), "Te has puesto los Guantes de Piel.")

place_1 player ["colocar", _, _, _] = (player, "No tienes qué colocar.")

place_1 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender.")
                                       else (player, "No hay nada con que puedas encender.")

place_1 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo.")

place_1 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo.")

place_1 player _ = (player, "Intenta hacer otra cosa.")
