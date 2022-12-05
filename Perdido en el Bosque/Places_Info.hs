
module Places_Info
(place_1_info,
 place_2_info,
 place_3_info,
 place_4_info,
 place_5_info,
 place_6_info,
 place_7_info) where

import Player_Class

--Descripción del primer lugar de la historia
place_1_info :: String
place_1_info = "Estás en la habitación del segundo piso, con unos pocos muebles: \n " ++
               "La cama, una mesa de noche y un armario, además de una puerta. \n" ++
               "La luz se asoma por una solitaria ventana."

--Descripción del segundo lugar
place_2_info :: String
place_2_info = "Te encuentras en un pasillo en forma de L. \n" ++
               "Los dos extremos son la puerta del cuarto y las escaleras \n" ++
               "para bajar al primer piso. \n" ++
               "Frente a las escaleras está la puerta que lleva al desván \n" ++
               "y al lado de esta, más cercana a la del cuarto, otra por la \n" ++
               "que se accede al aseo. \n" ++
               "Frente al aseo hay una ventana."

--Descripción del tercer lugar
place_3_info :: String
place_3_info = "Estás en el cuarto de aseo del segundo piso. \n A tu izquierda hay un lavamanos, " ++
               "encima de este un botiquín con espejo. A tu derecha el retrete y la ducha. \n" ++
               "Una pequeña ventana ilumina la habitación."

--Descripción del cuarto lugar
place_4_info :: String
place_4_info = "Te encuentras en la sala de la casa. \n" ++
               "Saltan a la vista unos muebles viejos alrededor, tapados por sábanas. \n" ++ 
               "Se puede ver la puerta de la entrada de la casa, " ++
               "una mesa con algunos objetos encima, y otra puerta al fondo, \n" ++
               "parece ser la de la cocina. A su lado hay una segunda puerta, \n " ++
               "a juzgar por su aspecto debe de ser un cuarto."

--Descripción del quinto lugar, dependiendo de si está encendido o no

place_5_info :: Player -> String
place_5_info player = if ((decision_array player) !! 5) == 1
                      then place_5_info_light else place_5_info_darkness


place_5_info_darkness :: String
place_5_info_darkness = "Te encuentras en el desván de la casa. \n" ++
                        "Todo está muy oscuro, es imposible ver algo. \n" ++
                        "Justo al lado de la entrada hay una lámpara"

place_5_info_light :: String
place_5_info_light = "Te encuentras en el desván de la casa. \n" ++
                     "La lámpara cerca de la entrada emite una ténue luz. \n" ++
                     "Hay una gran cantidad de objetos, muchos de ellos envueltos en sábanas. \n" ++
                     "Por el polvo acumulado deben llevar ahí mucho tiempo. \n" ++
                     "Hay un escaparate al fondo y un papel pegado en la pared cercana."

--Descripción del sexto lugar
place_6_info :: String
place_6_info = "Te encuentras en la cocina. A la vista salta el refrigerador, \n" ++
               "una meseta con varias cazuelas y un horno. \n" ++
               "Hay varios objetos debajo de la meseta. \n" ++ 
               "Hay tres taburetes, uno de ellos tiene algo arriba."

--Descripción del séptimo lugar
place_7_info :: String
place_7_info = "Te encuentras en el cuarto del primer piso. \n" ++
                "Enseguida visualizas una cama y una cómoda a su lado con una gaveta. \n" ++
                "Encima de la cómoda hay otras cosas."