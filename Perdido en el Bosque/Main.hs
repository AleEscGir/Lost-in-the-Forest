
import Player_Class
import Funtions
import Processing_Info
import Place_0
import Place_1
import Place_2
import Place_3
import Place_4
import Place_5
import Place_6
import Place_7
import Places_Info

main :: IO ()
main = do
        putStrLn ("\n Es de día, la luz se filtra por la ventana del cuarto y, " ++
                 "ya despierto a causa del resplandor, te levantas de la cama. \n" ++
                 "No llega a tus oidos ningún otro ruido salvo el silbido de los pájaros. \n" ++
                 "Recuerdas tu nombre? \n >>")
        sub_main (Player "" [""]
         0 [""] ["","Pijama superior","Pijama Inferior","",""] [0, 0, 0, 0, 0, 0, 0, 0])
        putStrLn "Gracias por jugar"

--sub_main
--Controla el orden de ejecución de las funciones puras
--y la entrada de escritura del usuario

sub_main :: Player -> IO ()
sub_main player = do
                  insert_line <- getLine
                  let processed = processing_info insert_line
                  let (new_player, text) = keys player processed
                  putStrLn ( "\n" ++ text ++ "\n")
                  sub_main_2 new_player text
                  

--División de sub_main que decide si continuar la acción
sub_main_2 :: Player -> String -> IO ()
sub_main_2 player text =
                  if text == "Juego Finalizado" then
                      return ()
                  else do
                      putStrLn ">> "
                      sub_main player

--Verifica si fueron usadas palabras claves
keys :: Player -> [String] -> (Player, String)
keys player action = let key = key_words player action
                        in 
                            if key == "" then
                                history player action 
                            else
                                (player, key)

--Palabras claves que puede utilizar el usuario
key_words :: Player -> [String] -> String
--key_words player [verb, sustantive, object_use, direction]
key_words player [verb, "inventario", object_use, direction]
                 | (not (member verb ["coger", "abrir", "leer", "usar", ""]))
                 = "No puedes " ++ verb ++ " con el inventario"
                 | (object_use /= "")
                 = "No necesitas objetos extras para " ++ verb ++ " tu inventario"
                    ++ "\n" ++ union (inventory player) " // "
                 | (direction /= "")
                 = if (member direction ["este", "oeste", "sur", "norte"]) then
                     "Al" ++ direction ++ " del inventario no encontrarás nada"
                   else direction ++ " del inventario no encontrarás nada" 
                 | otherwise = union (inventory player) " // "
--key_words player [verb, "obj", object_use, direction] = union (object_used player) " // "
key_words player ["", "fin", "", ""] = "Juego Finalizado"
key_words _ _ = ""

--Selecciona la parte de la historia en la que se jugará
history :: Player -> [String] -> (Player, String)

history player [verb, sustantive, 
               object_use, direction] | (location player == 0)
                                       = place_0 player
                                       [verb, place_0_transform_sustantive (sustantive),
                                        place_0_transform_sustantive (object_use), direction]

                                      | (location player == 1)
                                       = place_1 player
                                       [verb, place_1_transform_sustantive (sustantive),
                                        place_1_transform_sustantive (object_use), direction]

                                      | (location player == 2)
                                       = place_2 player
                                       [verb, place_2_transform_sustantive (sustantive),
                                        place_2_transform_sustantive (object_use), direction]

                                      | (location player == 3)
                                       = place_3 player
                                       [verb, place_3_transform_sustantive (sustantive),
                                        place_3_transform_sustantive (object_use), direction]

                                      | (location player == 4)
                                       = place_4 player
                                       [verb, place_4_transform_sustantive (sustantive),
                                        place_4_transform_sustantive (object_use), direction]
                                        
                                      | (location player == 5)
                                       = place_5_revision player
                                       [verb, place_5_transform_sustantive (sustantive),
                                        place_5_transform_sustantive (object_use), direction]

                                      | (location player == 6)
                                       = place_6 player
                                       [verb, place_6_transform_sustantive (sustantive),
                                        place_6_transform_sustantive (object_use), direction]

                                      | (location player == 7)
                                       = place_7 player
                                       [verb, place_7_transform_sustantive (sustantive),
                                        place_7_transform_sustantive (object_use), direction]

                                      | otherwise = (player, "Juego Finalizado")
