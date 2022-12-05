
module Place_0
(place_0,
place_0_transform_sustantive) where

import Player_Class
import Funtions
import Places_Info


--place_0_transform_sustantive
--Agrupa series de sustantivos de forma común en la parte de recibir el nombre
place_0_transform_sustantive :: String -> String
place_0_transform_sustantive x | (member x ["no", "no recuerdo"]) = "no"
                               | (member x ["no", "no recuerdo nombre"]) = "no"
                               | otherwise = x

--Momento de la historia en la que ser recibe el nombre
place_0 :: Player -> [String] -> (Player, String)
place_0 player ["", "", "", ""] = (player, "Cliente difícil eh. Vamos, diga su nombre, déjese de timidez.")
place_0 player [verb, "", object_use, direction] = (player, "Inserte su nombre, no otras cosas raras.")
place_0 player ["", "no", "", ""] = (player, "Como que no lo recuerdas?, vamos, esfuérzate.")
place_0 player ["", sustantive, "", ""] = let word = split sustantive
                                          in if (my_length word) > 1 then
                                          (player, "No estamos aceptando varios nombres (sentido común).")
                                          else ((change_player_location
                                               (change_player_name player (to_upper_first sustantive)) 1 ),
                                               (place_1_info ++ " \n Sientes un fuerte dolor de cabeza."))
place_0 player _ = (player, "Eso no es un nombre, vamos, esfuérzate.")