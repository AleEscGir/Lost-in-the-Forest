
module Player_Class
(Player(..),
change_player_name,
add_player_inventory,
remove_player_inventory,
change_player_location,
add_player_object_used,
remove_player_object_used,
change_player_equipment,
change_player_decision_array) where

import Funtions

--Contiene los datos del jugador actual
data Player = Player { name :: String, 
                       inventory :: [String],
                       location :: Int,
                       object_used :: [String],
                       equipment :: [String],
                       decision_array :: [Int]
                       } deriving Show

--name -> Nombre del jugador
--inventory -> Objetos que tienes en el inventario y que puedes usar
--location -> Lugar de la historia en el que te encuentras ahora mismo
--object_used -> Objetos que has usado
--equipment -> Objetos que traes puestos a modo de ropa
--             Pos 0 -> Indica que traes puesto en la cabeza
--             Pos 1 -> Indica que traes puesto en el torso
--             Pos 2 -> Indica que traes puesto en las manos
--             Pos 3 -> Indica que traes puesto en las piernas
--             Pos 4 -> Indica que traes puesto en los pies
--decision_array -> Contiene valores que permiten al programador saber qué
--                  acciones han sido realizadas:
--                  Pos 0 -> Es 1 si el jugador acaba de salir de una habitación, 0 en otro caso
--                  Pos 1 -> Es 1 si ya fue curada la herida de la cabeza, 0 en otro caso    
--                  Pos 2 -> Es 1 si el jugador ya abrió el desván, 0 si está cerrado aún
--                  Pos 3 -> Es 1 si el jugador ya abrió la entrada de la casa, 0 si está cerrado aún
--                  Pos 4 -> Es 1 si el jugador ya abrió el botiquín, 0 si está cerrado aún
--                  Pos 5 -> Es 1 si el jugador ya encendió el desván, 0 si aún está apagado
--                  Pos 6 -> Es 1 si el jugador ya abrió el horno, 0 si aún está cerrado
--                  Pos 7 -> Es 1 si el jugador ya abrió la gaveta del cuarto del primer piso, 0 en otro caso

--Las siguientes funciones consisten en recibir la clase player, y cambiarle algún
--atributo, devolviendo una nueva clase con el resto de atributos intactos

--Permite cambiar el nombre de Player
change_player_name :: Player -> String -> Player
change_player_name (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_name
                   = (Player new_name inventory_ location_ object_used_ equipment_ decision_array_)

--Permite agregar un objeto al inventario
add_player_inventory :: Player -> String -> Player
add_player_inventory (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_object
                   = (Player name_ (add inventory_ new_object) location_ object_used_ equipment_ decision_array_)

--Permite eliminar un objeto del inventario
remove_player_inventory :: Player -> String -> Player
remove_player_inventory (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) object_
                      = (Player name_ (remove object_ inventory_) location_ object_used_ equipment_ decision_array_)

--Permite cambiar la localización del jugador
change_player_location :: Player -> Int -> Player
change_player_location (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_location
                     = (Player name_ inventory_ new_location object_used_ equipment_ decision_array_)

--Permite añadir un objeto a la lista de objetos usados
add_player_object_used :: Player -> String -> Player
add_player_object_used (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_object
                     = (Player name_ inventory_ location_ (add object_used_ new_object) equipment_ decision_array_)

--Permite remover un objeto de la lista de objetos usados
remove_player_object_used :: Player -> String -> Player
remove_player_object_used (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) object_
                        = (Player name_ inventory_ location_ (remove object_ object_used_) equipment_ decision_array_)

--Permite cambiar uno de los equipamientos
change_player_equipment :: Player -> String -> Int -> Player
change_player_equipment (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_equipment pos
                      = (Player name_ inventory_ location_ object_used_ (replace equipment_ new_equipment pos) decision_array_)

--Permite cambiar un valor del array de decisión
change_player_decision_array :: Player -> Int -> Int -> Player
change_player_decision_array (Player name_ inventory_ location_ object_used_ equipment_ decision_array_) new_value pos
                           = (Player name_ inventory_ location_ object_used_ equipment_ (replace decision_array_ new_value pos))