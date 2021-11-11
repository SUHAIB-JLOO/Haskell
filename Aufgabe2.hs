-- Aufgabe 2 --- Gruppe King Salat ----

char2Int :: Char -> Char -> Int
char2Int x c = fromEnum c - fromEnum x

int2char :: Char -> Int -> Char
int2char x n = toEnum (fromEnum x + n)

checkBorS :: Char -> Int
checkBorS c = if c >= 'a' && c <= 'z'
              then char2Int 'a' c
              else if c >= 'A' && c <= 'Z'
                   then char2Int 'A' c 
                   else 0

int2Schar :: Int -> Char
int2Schar = int2char 'a'

int2Bchar :: Int -> Char
int2Bchar = int2char 'A'

addSpace :: [Char] -> [Char]
addSpace li = if length li <= 5
              then li
              else take 5 li ++ " " ++ addSpace (drop 5 li)

genkey :: [Int] -> [Char] -> [(Int, Char)]
genkey s txt = if length txt == 0
               then []
               else zip s txt ++ genkey s (drop (length s) txt)

makeSpace :: [Char] -> [Char] 
makeSpace li = addSpace $ filter (/=' ') li

shift :: Int -> Char -> Char
shift x c = if c >='a' && c <= 'z'
          then int2Schar ((char2Int 'a' c + x) `mod` 26)
          else if c >='A' && c <= 'Z' 
               then int2Bchar ((char2Int 'A' c + x) `mod` 26)
               else c

-- Cäsar-Chiffre --
encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar x txt = [shift x c | c <- makeSpace txt] 

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar x txt = [shift (-x) c | c <- filter (/=' ') txt ]

-- Vigenere-Chiffre --
encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere s txt = makeSpace $ [shift (fst x) (snd x) | x <- genkey (map (checkBorS) s) (filter(/=' ') txt)]

decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere s txt = [shift (- fst x) (snd x) | x <- genkey (map (checkBorS) s) (filter(/=' ') txt)]
 
