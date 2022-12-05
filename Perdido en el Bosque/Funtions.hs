
module Funtions
(my_length,
replace,
split,
take_word,
take_left,
clean_list,
clean_list_after,
member,
union,
add,
remove,
remove_at,
to_lower,
to_lower_all,
to_upper,
to_upper_first) where
 

--Función que dado una lista devuelve el length de la lista
my_length :: [a] -> Int
my_length [] = 0
my_length (x:xs) = 1 + my_length xs


--Función que toma una lista, un elemento y una posición,
--y sobreescribe lo que hay en esa posición por el elemento
replace :: [a] -> a -> Int -> [a]
replace [] _ _ = []
replace (x:xs) y pos = if pos < 1 then y:xs else x:(replace xs y (pos-1)) 

--Función que permite separar todas las palabras de un string
--que contenga espacios en blanco
split :: String -> [String]
split [] = []
split xs = let word = take_word xs
               left = take_left xs
           in [word] ++ split (left) 
              
--take_word toma un string y devuelve la primera palabra
--antes de un " "
take_word :: String -> String
take_word [] = ""
take_word (x:xs) = if (x == ' ') then "" else [x] ++ take_word xs

--take_left toma un string y devuelve todo menos
--la primera palabra antes de un " "
take_left :: String -> String
take_left [] = ""
take_left (x:xs) = if x == ' ' then xs else take_left xs


--clean_list
--Toma dos listas de palabras, y elimina de la primera lista
--toda palabra que se encuentre en la segunda
clean_list :: [String] -> [String]-> [String]
clean_list [] _ = []
clean_list (x:xs) ys = if member x ys then clean_list xs ys
                       else x : clean_list xs ys


--clean_list_after
--Toma una lista de palabras y una palabra, y corta
--la lista a partir de la primera aparición de la palabra
clean_list_after :: [String] -> String -> [String]
clean_list_after [] _ = []
clean_list_after (x:xs) y = if x == y then [] else
                            x : clean_list_after xs y

--member
--Devuelve si una palabra pertenece a una lista de palabras
member :: String -> [String] -> Bool
member _ [] = False
member x (y:ys) = if x == y then True else member x ys

--union
--recibe una lista de palabras y las une en un string
--con el segundo parámetro de por medio entre cada elemento
union :: [String] -> String -> String
union [] _ = ""
union (x:[]) _ = x
union (x:xs) word = x ++ word ++ union xs word

--add
--Recibe un lista y un elemento y agrega dicho elemento al inicio de la lista
add :: [a] -> a -> [a]
add list x = x:list

--remove
--recibe una palabra y una lista y devuelve la lista sin
--dicho elemento
remove :: String -> [String] -> [String]
remove _ [] = []
remove x (y:ys) = if x == y then ys else y : remove x ys

--remove_at
--recibe una lista y un entero y devuelve la lista sin
--el elemento en dicha posición
remove_at :: [a] -> Int -> [a]
remove_at [] _ = []
remove_at (x:xs) pos = if pos < 1 then xs else x:(remove_at xs (pos-1)) 

--to_lower_all
--Toma una lista de palabras y devuelve el resultado de
--realizar to_lower a cada una
to_lower_all :: [String] -> [String]
to_lower_all [] = []
to_lower_all (x:xs) = [map to_lower x] ++ to_lower_all xs

--to_lower
--Recibe un char y si este se encuentra en mayúscula,
--lo devuelve en minúscula
to_lower :: Char -> Char
to_lower 'A' = 'a'
to_lower 'B' = 'b'
to_lower 'C' = 'c'
to_lower 'D' = 'd'
to_lower 'E' = 'e'
to_lower 'F' = 'f'
to_lower 'G' = 'g'
to_lower 'H' = 'h'
to_lower 'I' = 'i'
to_lower 'J' = 'j'
to_lower 'K' = 'k'
to_lower 'L' = 'l'
to_lower 'M' = 'm'
to_lower 'N' = 'n'
to_lower 'O' = 'o'
to_lower 'P' = 'p'
to_lower 'Q' = 'q'
to_lower 'R' = 'r'
to_lower 'S' = 's'
to_lower 'T' = 't'
to_lower 'U' = 'u'
to_lower 'V' = 'v'
to_lower 'W' = 'w'
to_lower 'X' = 'x'
to_lower 'Y' = 'y'
to_lower 'Z' = 'z'
to_lower letter = letter

--to_upper_first
--Recibe un String y coloca su primera letra en mayúscula
to_upper_first :: String -> String
to_upper_first (x:xs) = (to_upper x) :xs

--to_upper
--Recibe un char y si este se encuentra en minúscula,
--lo devuelve en mayúscula
to_upper :: Char -> Char
to_upper 'a' = 'A'
to_upper 'b' = 'B'
to_upper 'c' = 'C'
to_upper 'd' = 'D'
to_upper 'e' = 'E'
to_upper 'f' = 'F'
to_upper 'g' = 'G'
to_upper 'h' = 'H'
to_upper 'i' = 'I'
to_upper 'j' = 'J'
to_upper 'k' = 'K'
to_upper 'l' = 'L'
to_upper 'm' = 'M'
to_upper 'n' = 'N'
to_upper 'o' = 'O'
to_upper 'p' = 'P'
to_upper 'q' = 'Q'
to_upper 'r' = 'R'
to_upper 's' = 'S'
to_upper 't' = 'T'
to_upper 'u' = 'U'
to_upper 'v' = 'V'
to_upper 'w' = 'W'
to_upper 'x' = 'X'
to_upper 'y' = 'Y'
to_upper 'z' = 'Z'
to_upper letter = letter
