
module Place_4
(place_4,
place_4_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info

--place_4_transform_sustantive
--Agrupa series de sustantivos de forma común para el cuarto lugar
place_4_transform_sustantive :: String -> String
place_4_transform_sustantive x | (member x ["pasillo", "segundo piso",
                                            "escaleras", "escaleras pasillo",
                                            "escalera", "escalera pasillo",
                                            "escaleras segundo piso",
                                            "escalera segundo piso"]) = "pasillo"
                               | (member x ["cocina", "puerta cocina"]) = "cocina"
                               | (member x ["cuarto", "puerta cuarto",
                                            "cuarto primer piso",
                                            "puerta cuarto primer piso"]) = "cuarto"
                               | (member x ["puerta entrada", "puerta entrada casa",
                                            "entrada", "entrada casa",
                                            "afuera", "afuera casa",
                                            "afueras", "afueras casa",
                                            "parte afuera", "parte fuera"]) = "entrada"
                               | (member x ["llave", "llave entrada",
                                            "llave entrada casa"]) = "llave"
                               | (member x ["palanca", "palanca mesa"]) = "palanca"
                               | (member x ["mesa", "objetos mesa"]) = "objetos"
                               | otherwise = x

--Cuarto lugar de la historia, la sala
place_4 :: Player -> [String] -> (Player, String)

place_4 player ["mirar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para mirar")
               | (sustantive == "")
               = (player, "Observas la sala, parece que no es usada desde hace tiempo")

place_4 player ["mirar", "entrada", "", ""] = 
               if (member "Llave de la Entrada" (inventory player) || member "Llave de la Entrada" (object_used player)) then
               (if ((decision_array player) !! 1) == 1 then
                    (player, "Observas la puerta de la entrada de la casa, ya puedes abrirla") else
                    (player, "Observas la puerta de la entrada de la casa, ya puedes abrirla" ++
                             "Tu dolor de cabeza comienza a empeorar, es muy peligroso salir ahora")) else
               (player, "Observas la puerta de la entrada de la casa, al parecer está cerrada")

place_4 player ["mirar", "muebles", "", ""] = (player, "Son muebles algo viejos, están rasgados por todas partes. \n" ++
                                                       "Una fina y polvorienta sábana los envuelve")

place_4 player ["mirar", "cuarto", "", ""] = (player, "Observas la puerta del cuarto del primer piso. \n" ++
                                               "Tiene varios agujeros verticales")

place_4 player ["mirar", "pasillo", "", ""] = (player, "Observas las escaleras al segundo piso. \n" ++
                                               "Uno de los escalonas está roto")

place_4 player ["mirar", "cocina", "", ""] = (player, "La puerta de la cocina está entreabierta")

place_4 player ["mirar", "mesa", "", ""] = (player, "La mesa se encuentra en medio de la sala. \n " ++
                                                    "Está algo regada, tiene varios objetos encima.")

place_4 player ["mirar", "mesa", "", "encima"] = (player, "Hay varias cosas rotas. La única que parece servir " ++
                                                           "es una palanca.")

place_4 player ["mirar", "objetos", "", "encima"] = (player, "Hay varias cosas rotas. La única que parece servir " ++
                                                             "es una palanca.")

place_4 player ["mirar", _ , "", _ ] = (player, "No observas nada peculiar")

place_4 player ["coger", "palanca", "", ""] = 
                    if (member "Palanca" (inventory player) || member "Palanca" (object_used player)) then
                    (player, "Ya cogiste la Palaca") else
                    ((add_player_inventory player "Palanca"), "Has cogido la palanca")

place_4 player ["coger", "palanca", "", "encima"] = 
                    if (member "Palanca" (inventory player) || member "Palanca" (object_used player)) then
                    (player, "Ya cogiste la Palaca") else
                    ((add_player_inventory player "Palanca"), "Has cogido la palanca")

place_4 player ["coger", _, "", _] = (player, "No parece que haya algo que puedas coger")

place_4 player ["ser", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No creo que necesites algo para ser como eres")
               | (direction /= "")
               = (player, "Serás algo en esa dirección, pero no ahora")
               | (sustantive == "")
               = (player, "Eres la nada, el vacío")

place_4 player ["ser", sustantive , "", ""] = (player, "Aunque quieras ser " ++ sustantive ++
                                              ", no lo lograrás, el mundo es cruel")

place_4 player ["abrir", sustantive, object_use, direction]
               | (sustantive == "")
               = (player, "Abre tu corazón al mundo")

place_4 player ["abrir", "entrada", "llave", ""] = if (decision_array player !! 3) == 1 then
               (if (decision_array player !! 1) == 1 then
                ((change_player_location player (-1)),
                "La puerta ya está abierta. Inmediatamente sales de la casa, y te encuentras en un bosque." ++
                "El sol brilla intensamente, pero puedes soportarlo. \n" ++ (name player) ++ "!! " ++
                (name player) ++ "!! \n" ++ "Oyes como gritan tu nombre. Varias personas han venido a rescatarte. \n" ++
                "Te sientes algo mareado mientras te cargan, y cierras los ojos... \n" ++
                "Dejas de sentir tus piernas por un rato... \n" ++
                "El dolor de cabeza vuelve poco a poco... comienzas a retomar tu conciencia... \n" ++
                "Abres los ojos y despiertas en una cama... te resulta familiar el cuarto... pero no puedes moverte... \n" ++
                "-Hay alguien en la puerta- Piensas mientras te agitas... sientes una voz... \n" ++
                "Recuerdas tu nombre... oh, espera, no me lo digas, es " ++ (name player) ++ ". \n" ++
                "Unas últimas palabras? \n" 
                ) else (change_player_location player (-1),
                "Sales de la casa corriendo, tu dolor de cabeza se intensifica, el sol brilla muy fuerte. \n" ++
                "Comienzas a fatigarte... Pierdes tus fuerzas... y finalmente... Te desmayas...") 
                ) else
                if member "Llave de la Entrada" (inventory player) then
                ((remove_player_inventory
                (add_player_object_used 
                (change_player_decision_array player 1 3)
                "Llave de la Entrada")
                "Llave de la Entrada"),
                "Has abierto la Puerta de la Entrada con la Llave, ya puedes salir de la casa") else
                (player, "No tienes esa llave")

place_4 player ["abrir", "entrada", "", ""] = if (decision_array player !! 3) == 1 then
               (if (decision_array player !! 1) == 1 then
                ((change_player_location player (-1)),
                "La puerta ya está abierta. Inmediatamente sales de la casa, y te encuentras en un bosque." ++
                "El sol brilla intensamente, pero puedes soportarlo. \n" ++ (name player) ++ "!! " ++
                (name player) ++ "!! \n" ++ "Oyes como gritan tu nombre. Varias personas han venido a rescatarte. \n" ++
                "Te sientes algo mareado mientras te cargan, y cierras los ojos... \n" ++
                "Dejas de sentir tus piernas por un rato... \n" ++
                "El dolor de cabeza vuelve poco a poco... comienzas a retomar tu conciencia... \n" ++
                "Abres los ojos y despiertas en una cama... te resulta familiar el cuarto... pero no puedes moverte... \n" ++
                "-Hay alguien en la puerta- Piensas mientras te agitas... sientes una voz... \n" ++
                "Recuerdas tu nombre... oh, espera, no me lo digas, es " ++ (name player) ++ ". \n" ++
                "Unas últimas palabras? \n" 
                ) else (change_player_location player (-1),
                "Sales de la casa corriendo, tu dolor de cabeza se intensifica, el sol brilla muy fuerte. \n" ++
                "Comienzas a fatigarte... Pierdes tus fuerzas... y finalmente... Te desmayas...") 
                ) else
                if member "Llave de la Entrada" (inventory player) then
                ((remove_player_inventory
                (add_player_object_used 
                (change_player_decision_array player 1 3)
                "Llave de la Entrada")
                "Llave de la Entrada"),
                "Has abierto la Puerta de la Entrada con la Llave, ya puedes salir de la casa") else
                (player, "No tienes esa llave")


place_4 player ["abrir", "cocina", "", ""]
                = ((change_player_location player 6), "Abres la puerta y entras. \n" ++ place_6_info)

place_4 player ["abrir", "cuarto", "", ""]
                = ((change_player_location player 7), "Abres la puerta y entras. \n" ++ place_7_info)

place_4 player ["abrir", _, _, _] = (player, "Al menos intenta abrir algo")

place_4 player ["llamar", _, _, "arriba"] = (player, "Dios no ha atendido su llamada")

place_4 player ["llamar", _, _, _] = (player, "No tienes a quien llamar ahora mismo")

place_4 player ["buscar", _, _, _] = (player, "No tienes algo qué buscar ahora")

place_4 player ["salir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Sal de este mundo demonio")

place_4 player ["salir", "entrada", "", ""] = if (decision_array player !! 3) == 1 then
               (if (decision_array player !! 1) == 1 then
                ((change_player_location player (-1)),
                "La puerta ya está abierta. Inmediatamente sales de la casa, y te encuentras en un bosque." ++
                "El sol brilla intensamente, pero puedes soportarlo. \n" ++ (name player) ++ "!! " ++
                (name player) ++ "!! \n" ++ "Oyes como gritan tu nombre. Varias personas han venido a rescatarte. \n" ++
                "Te sientes algo mareado mientras te cargan, y cierras los ojos... \n" ++
                "Dejas de sentir tus piernas por un rato... \n" ++
                "El dolor de cabeza vuelve poco a poco... comienzas a retomar tu conciencia... \n" ++
                "Abres los ojos y despiertas en una cama... te resulta familiar el cuarto... pero no puedes moverte... \n" ++
                "-Hay alguien en la puerta- Piensas mientras te agitas... sientes una voz... \n" ++
                "Recuerdas tu nombre... oh, espera, no me lo digas, es " ++ (name player) ++ ". \n" ++
                "Unas últimas palabras? \n" 
                ) else (change_player_location player (-1),
                "Sales de la casa corriendo, tu dolor de cabeza se intensifica, el sol brilla muy fuerte. \n" ++
                "Comienzas a fatigarte... Pierdes tus fuerzas... y finalmente... Te desmayas...")) else
                (player, "No puedes salir, está cerrado con llave")

place_4 player ["salir", "casa", "", ""] = if (decision_array player !! 3) == 1 then
               (if (decision_array player !! 1) == 1 then
                ((change_player_location player (-1)),
                "La puerta ya está abierta. Inmediatamente sales de la casa, y te encuentras en un bosque." ++
                "El sol brilla intensamente, pero puedes soportarlo. \n" ++ (name player) ++ "!! " ++
                (name player) ++ "!! \n" ++ "Oyes como gritan tu nombre. Varias personas han venido a rescatarte. \n" ++
                "Te sientes algo mareado mientras te cargan, y cierras los ojos... \n" ++
                "Dejas de sentir tus piernas por un rato... \n" ++
                "El dolor de cabeza vuelve poco a poco... comienzas a retomar tu conciencia... \n" ++
                "Abres los ojos y despiertas en una cama... te resulta familiar el cuarto... pero no puedes moverte... \n" ++
                "-Hay alguien en la puerta- Piensas mientras te agitas... sientes una voz... \n" ++
                "Recuerdas tu nombre... oh, espera, no me lo digas, es " ++ (name player) ++ ". \n" ++
                "Unas últimas palabras? \n" 
                ) else (change_player_location player (-1),
                "Sales de la casa corriendo, tu dolor de cabeza se intensifica, el sol brilla muy fuerte. \n" ++
                "Comienzas a fatigarte... Pierdes tus fuerzas... y finalmente... Te desmayas...")) else
                (player, "No puedes salir, está cerrado con llave")

place_4 player ["salir", "cocina", "", ""]
                = ((change_player_location player 6), "Abres la puerta y entras. \n" ++ place_6_info)

place_4 player ["salir", "cuarto", "", ""]
                = ((change_player_location player 7), "Abres la puerta y entras. \n" ++ place_7_info)

place_4 player ["salir", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Subes las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_4 player ["salir", _, "", _] = (player, "Debes querer salir a algún lado no?")

place_4 player ["entrar", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "Debes querer salir a algún lado no?")

place_4 player ["entrar", "cocina", "", ""]
                = ((change_player_location player 6), "Abres la puerta y entras. \n" ++ place_6_info)

place_4 player ["entrar", "cuarto", "", ""]
                = ((change_player_location player 7), "Abres la puerta y entras. \n" ++ place_7_info)

place_4 player ["entrar", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Subes las escaleras al segundo piso. Entras al pasillo. \n" ++ place_2_info)

place_4 player ["entrar", _, "", _] = (player, "Debes querer entrar a algún lado no?")

place_4 player ["ir", sustantive, object_use, direction]
               | (object_use /= "")
               = (player, "No necesitas nada en este momento")
               | (sustantive == "")
               = (player, "A dónde piensas ir?")

place_4 player ["ir", "entrada", "", ""] = if (decision_array player !! 3) == 1 then
               (if (decision_array player !! 1) == 1 then
                ((change_player_location player (-1)),
                "La puerta ya está abierta. Inmediatamente sales de la casa, y te encuentras en un bosque." ++
                "El sol brilla intensamente, pero puedes soportarlo. \n" ++ (name player) ++ "!! " ++
                (name player) ++ "!! \n" ++ "Oyes como gritan tu nombre. Varias personas han venido a rescatarte. \n" ++
                "Te sientes algo mareado mientras te cargan, y cierras los ojos... \n" ++
                "Dejas de sentir tus piernas por un rato... \n" ++
                "El dolor de cabeza vuelve poco a poco... comienzas a retomar tu conciencia... \n" ++
                "Abres los ojos y despiertas en una cama... te resulta familiar el cuarto... pero no puedes moverte... \n" ++
                "-Hay alguien en la puerta- Piensas mientras te agitas... sientes una voz... \n" ++
                "Recuerdas tu nombre... oh, espera, no me lo digas, es " ++ (name player) ++ ". \n" ++
                "Unas últimas palabras? \n" 
                ) else (change_player_location player (-1),
                "Sales de la casa corriendo, tu dolor de cabeza se intensifica, el sol brilla muy fuerte. \n" ++
                "Comienzas a fatigarte... Pierdes tus fuerzas... y finalmente... Te desmayas...")) else
                (player, "No puedes salir, está cerrado con llave")

place_4 player ["ir", "cocina", "", ""]
                = ((change_player_location player 6), "Abres la puerta y entras. \n" ++ place_6_info)

place_4 player ["ir", "cuarto", "", ""]
                = ((change_player_location player 7), "Abres la puerta y entras. \n" ++ place_7_info)

place_4 player ["ir", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Subes las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_4 player ["ir", _, "", _] = (player, "Debes querer ir a algún lado no?")

place_4 player ["comer", _, _, _] = (player, "No creo que puedas comer algo ahora mismo")

place_4 player ["escribir", _, _, _] = (player, "No hay dónde escribir ahora mismo")

place_4 player ["leer", _, _, _] = (player, "No hay nada que leer ahora mismo")

place_4 player ["hablar", _, _, _] = (player, "Ahora mismo estás más solo que Coraje el Perro Cobarde")

place_4 player ["quitar", _, _, _] = (player, "No hay nada para quitar ahora mismo")

place_4 player ["nadar", _, _, _] = (player, "No hay agua en ningún lado")

place_4 player ["bailar", _, _, _] = (player, "Te meneas un poco, uns fantasma te sigue el ritmo")

place_4 player ["escuchar", _, _, _] = (player, "Solo oyes algunas aves fuera de la casa")

place_4 player ["cocinar", _, _, _] = (player, "A menos que quieras asar unos zapatos...")
    
place_4 player ["limpiar", _, _, _] = (player, "Hay tanto polvo que tardarías meses")

place_4 player ["alimentar", _, _, _] = (player, "Ni las hormigas se pueden alimentar en este lugar...")

place_4 player ["ayudar", _, _, _] = (player, "Ayudas a un amigo imaginario, este te agradece")

place_4 player ["vestir", _, _, _] = (player, "No hay nada que te puedas poner")

place_4 player ["cambiar", _, _, _] = (player, "No cambies, aceptate, con tus virtudes y tus defectos")

place_4 player ["acostar", "piso", _, _] = (player, "Están algo frías las tablas del piso...")

place_4 player ["acostar", _, _, _] = (player, "No es un buen lugar para acostarse")

place_4 player ["subir", "pasillo", "", ""]
                = ((change_player_location player 2),
                "Subes las escaleras al segundo piso. Sales al pasillo. \n" ++ place_2_info)

place_4 player ["subir", _, _, _] = (player, "Al infinito y más allá...")

place_4 player ["bajar", _, _, _] = (player, "No hay a dónde bajar, el infierno está muy lejos")

place_4 player ["lanzar", _, _, _] = (player, "Lanzas tus esperanzas al aire, nunca regresan...")

place_4 player ["colocar", _, _, _] = (player, "No tienes qué colocar")

place_4 player ["encender", _, _, _] = if member "Mechero " (inventory player) then
                                            (player, "No hay nada que debas encender")
                                       else (player, "No hay nada con que puedas encender")

place_4 player ["curar", _, _, _] = (player, "Necesitas estar cerca de un espejo para curarte algo")

place_4 player ["destupir", _, _, _] = (player, "No necesitas destupir algo ahora mismo")

place_4 player _ = (player, "Intenta hacer otra cosa")