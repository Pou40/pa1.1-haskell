
import Test.Hspec 
import GHC.Base (rightSection)
import Control.Concurrent (yield)
import GHC.Arr (listArray)
import GHC.Read (list)



-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq) 

--Category: Easy

-- Question 1

--find two numbers from the array which make up the sum
--arrange those two numbers  in a manner that the first number is greater than the second number
--return the kist of such lists in an ascendinng order based on the first number

findSume :: [Int] -> Int -> [[Int]]
findSume [] _ = []
findSume (x:xs) n = if eleme (n-x) xs then [x,n-x] : findSume xs n else findSume xs n


eleme :: Int -> [Int] -> Bool
eleme x [] = False
eleme y (x:xs) = if y == x then True else eleme y xs

arrangelist :: [[Int]] -> [[Int]]
arrangelist [[]] = [[]]
arrangelist list = [arrangeSublist(x) | x <- list]

arrangeSublist :: [Int] -> [Int]
arrangeSublist [] = []
arrangeSublist list@(x:y:xs) = if x < y then [y,x] else list


arrangeasc :: [[Int]] -> [[Int]]
arrangeasc[] = []
arrangeasc [x] = [x]
arrangeasc (x@(a:b):y@(c:d):xs) = if a > c then y : arrangeasc (x:xs) else x : arrangeasc (y:xs)


finalsort :: [[Int]] -> [[Int]]
finalsort[] = []
finalsort[x] = [x]
finalsort list@(x@(a:b):y@(c:d):xs)
    | a > c = finalsort(arrangeasc(list))
    | otherwise = x : finalsort(y:xs)

targetSum :: [Int] -> Int ->[[Int]]
targetSum x y = finalsort(arrangelist (findSume x y))


-- maine :: IO ()
-- maine = do
--   let myList = [4,-1,0,1,-3,2,5,-6]

--   let arrangedList = targetSum myList 1
--   putStrLn $ "Original List: " ++ show myList
--   putStrLn $ "Arranged List: " ++ show arrangedList














-- Question 2

symmetricTree :: Eq a => Tree a -> Bool
symmetricTree Nil = True
symmetricTree (TreeNode left current right) = checkmirror left right

checkmirror ::Eq a => Tree a-> Tree a -> Bool
checkmirror Nil Nil = True
checkmirror (TreeNode left1 current1 right1) (TreeNode left2 current2 right2)
    | (current1 == current2) && (checkmirror left1 right2) && (checkmirror left2 right1) = True
    | otherwise = False











-- Question 3
palindromList :: Eq a => LinkedList a -> Bool
palindromList Null = True
palindromList (ListNode current next) = checklist (ListNode current next) (fReverse (ListNode current next))

--inorder to do this question we have to check whether the current list is the same as the list when it is reveresed
fReverse :: Eq a => LinkedList a -> LinkedList a
fReverse Null = Null
fReverse (ListNode current Null) = ListNode current Null
fReverse (ListNode current next) = addnode (ListNode current Null) (fReverse next)

--created addnode to add the current wala node as a next node  to the back coming recurion function fReverse
addnode :: Eq a => LinkedList a -> LinkedList a -> LinkedList a
addnode x Null = x
addnode x (ListNode current next) = ListNode current (addnode x next)

checklist :: Eq a => LinkedList a -> LinkedList a -> Bool
checklist Null Null = True
checklist (ListNode current1 next1) (ListNode current2 next2) = if current1 == current2 then checklist next1 next2 else False

--didn't needed this anymore
-- findlength :: Eq a => LinkedList a -> Int 
-- findlength Null = 0
-- findlength (ListNode x Null) = 1
-- findlength (ListNode current next) = 1 + findlength (next)









-- Question 4
snakeTraversal :: Tree a -> [a]
snakeTraversal Nil = []
snakeTraversal node = think node

-- node -> child list
hfunction5 :: Tree a -> Int -> [a]
hfunction5 Nil _ = []
hfunction5 (TreeNode Nil current Nil) _ = [current]
hfunction5 node@(TreeNode left current right) x =  hfunction1_2 (hfunction3 (hfunction1_1 node) x)

-- node -> child node list
hfunction4 :: Tree a -> Int -> [Tree a]
hfunction4 Nil _ = []
hfunction4 node@(TreeNode left current right) x = hfunction3 (hfunction1_1 node) x

-- [nodes] -> [child nodes]
hfunction3 :: [Tree a] -> Int -> [Tree a]
hfunction3 [] _ = []
hfunction3 list y =  [parsed | x <- list , parsed <- hfunction2 x y]


hfunctionfinel :: [Tree a] -> Int -> [a]
hfunctionfinel [] _ = []
hfunctionfinel list x =  hfunction1_2 list ++ hfunctionfinel y (upd x)
    where 
        y = rev (hfunction3 list x)

think :: Tree a -> [a]
think Nil = []
think node@(TreeNode left current right) = current : hfunctionfinel (hfunction4 node 1) (1)

rev :: [Tree a] -> [Tree a]
rev [] = []
rev [x] = [x]
rev (x:xs) = rev(xs) ++ [x]

--first I need to do simple bfs
--I need
--      A list whose children I need to find out
--      A list in which I will store the traversal

--given a tree I am returning the current in a list form
hfunction1 :: Tree a -> [a]
hfunction1 Nil  = []
hfunction1 node@(TreeNode _ current _ ) = [current] 

hfunction1_1 :: Tree a -> [Tree a]
hfunction1_1 Nil = []
hfunction1_1 x = [x]

hfunction1_2 :: [Tree a] -> [a]
hfunction1_2 [] = []
hfunction1_2 list = [new | x <- list, new <- hfunction1 x]

--given a tree I will find its child nodes and append it into the list
hfunction2 :: Tree a -> Int -> [Tree a]
hfunction2 Nil _ = []
hfunction2 (TreeNode Nil current Nil) _ = []
hfunction2 (TreeNode Nil current x) _ = [x]
hfunction2 (TreeNode x current Nil) _ = [x]
hfunction2 (TreeNode left current right) x
    | x < 0 = [left,right]
    | x > 0 = [right,left]


--evertime I compute hfunction3 I have to add the list I get in output and append it to my final list 
upd :: Int -> Int
upd x = x * (-1)




















-- Question 5


treeConstruction :: String -> Tree Char
treeConstruction "" = undefined
-- treeConstruction word@(x:xs)
--     | x /= '^' =  handlerFunction (helperFunction xs y y)  ((fiveFunction (helperFunction xs y y))!!0) 
--     | otherwise = [Nil]
--     where 
--         y = TreeNode Nil x Nil


helperFunction :: String -> Tree Char -> Tree Char -> [Tree Char]
helperFunction "" root parent = [parent]
helperFunction word@(x:[]) root parent = [parent]
helperFunction word@(x:y:xs) root parent@(TreeNode left current right)
    | x /= '^' = [parent] ++ helperFunction (y:xs) root (TreeNode (addnodl x left) current right)
    | otherwise = [parent] ++ [Nil] ++ helperFunction xs root (addnodr y right)



fiveFunction :: [Tree Char] -> [Tree Char]
fiveFunction []  = []
fiveFunction (x:[])
    | x /= Nil = [x]
    | otherwise = []
fiveFunction (x:y:xs)  
    | y == Nil = [x,y] ++ fiveFunction xs
    | otherwise = fiveFunction (y:xs)



handlerFunction :: [Tree Char] -> Tree Char -> [Tree Char]
handlerFunction [] x = [x]
handlerFunction list@(x:y:xs) node@(TreeNode left current right) = list


findNil :: [Tree Char] -> (Int,Tree Char)
findNil list@(x:xs)
    | x == Nil = addtuple (1,Nil) (findNil xs)
    | otherwise = (0,x)

addtuple :: (Int,Tree Char) -> (Int,Tree Char) -> (Int,Tree Char)
addtuple (num1,node1) (num2,node2) = (num1 + num2, node2)
-- now how do I use this list to make a tree 

-- connectNode :: Tree Char -> Tree Char -> Int



-- h5unction :: Tree Char  -> [Int] -> String -> Tree Char
-- h5unction Nil _ _ = Nil
-- h5unction x _ "" = x 
-- h5unction node@(TreeNode left current right) _ (x:[]) = if x /= '^' then addnodl x node else node
-- h5unction node@(TreeNode left current right) i word@(x:y:xs) = if checkchr x  /= Nil 
--     then h5unction (TreeNode (addnodl x left) current right) (i ++ [-1]) (y:xs) 
--     else h5unction (TreeNode left current (addnodr y right)) (i ++ [1]) xs 


addnodr :: Char  -> Tree Char -> Tree Char
addnodr ' ' _ = Nil
addnodr '^' x = x
addnodr x Nil = TreeNode Nil x Nil

addnodr x node@(TreeNode left current right)
    | right == Nil = TreeNode left current (TreeNode Nil x Nil)
    | otherwise = TreeNode left current (addnodr x right)

addnodl :: Char  -> Tree Char -> Tree Char
addnodl ' ' _ = Nil
addnodl '^' x = x
addnodl x Nil = TreeNode Nil x Nil

addnodl x node@(TreeNode left current right)
    | left == Nil = TreeNode (TreeNode Nil x Nil) current right
    | otherwise = TreeNode (addnodl x node) current right

-- checkchr :: Char -> Tree Char
-- checkchr x
--     | x == '^' = Nil
--     | otherwise = TreeNode Nil x Nil


--when I encounter a ^ character I will add to 





















-- Category: Medium

-- Attempy any 4 questions from this category

-- Question 1.1: Overload the (+) operator for Tree. You only need to overload (+). Keep the rest of the operators as undefined.   
instance Num (Tree Int) where
    (+) Nil Nil = Nil
    (+) Nil x@(TreeNode left current right) = (TreeNode ((+) Nil left) current ((+) Nil right)) 
    (+) x@(TreeNode left current right) Nil = (TreeNode ((+) left Nil) current ((+) right Nil)) 
    (+) tree1@(TreeNode left1 current1 right1) tree2@(TreeNode left2 current2 right2) = (TreeNode ((+) left1 left2) (current1 + current2) ((+) right1 right2))
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

-- Question 1.2


longestCommonString :: LinkedList Char -> LinkedList Char-> LinkedList Char
longestCommonString Null _ = Null
longestCommonString _ Null = Null
longestCommonString node1@(ListNode val1 next1) node2@(ListNode val2 next2) = strtolinkedlist (onecommon (mostcommon (listcommon (convertlist node1) (convertlist node2))))


convertlist :: LinkedList Char -> String
convertlist Null = ""
convertlist node@(ListNode val next) = val : convertlist next

listcommon :: [Char] -> [Char] -> [Char]
listcommon [] _ = []
listcommon _ [] = []
listcommon a@(x:xs) b@(y:ys)
    |   x == y = x : listcommon xs ys
    |   otherwise = ['_'] ++ listcommon a ys ++ ['_'] ++ listcommon b xs

mostcommon :: [Char] -> [String]
mostcommon [] = []
mostcommon list@(x:xs) = if x /= '_' 
                        then func2 list : mostcommon xs
                        else mostcommon xs

func2 :: [Char] -> [Char]
func2 [] = []
func2 (x:xs) = if x == '_'
                    then []
                    else x : func2 xs

onecommon :: [String] -> String
onecommon [] = ""
onecommon (x:[]) = x
onecommon (x:xs)
    | length x > length (onecommon xs) = x
    | otherwise = onecommon xs

strtolinkedlist :: String -> LinkedList Char
strtolinkedlist "" = Null
strtolinkedlist (x:xs) = ListNode x (strtolinkedlist xs)





-- Question 2
-- commonAncestor :: Ord a => Eq a => Tree a -> a -> a -> Maybe a
-- commonAncestor node@(TreeNode left current right) x y
--     |   current > x && current > y  = commonAncestor left x y
--     |   current < x && current < y = commonAncestor right x y 
--     |   otherwise = current 


commonAncestor :: Ord a => Eq a => Tree a -> a -> a -> Maybe a
commonAncestor Nil _ _ = Nothing
--check if both of the integers are part of the tree  
commonAncestor node@(TreeNode left current right) x y
    | checknode node x == False || checknode node y == False = Nothing
    | current > x && current > y  = commonAncestor left x y
    | current < x && current < y  = commonAncestor right x y
    | otherwise = Just current


checknode :: Eq a=> Tree a -> a -> Bool
checknode Nil _ = False
checknode node@(TreeNode left current right) x
    |   x == current = True
    |   otherwise = checknode left x || checknode right x   



-- Question 3
gameofLife :: [[Int]] -> [[Int]]
gameofLife = undefined

-- Question 4
waterCollection :: [Int] -> Int
waterCollection [] = 0
waterCollection list@(x:xs) = 2 * (wfunc2 (wfunc1 list) ((wfunc1 list)!!0))


wfunc2 :: [Int] -> Int -> Int
wfunc2 [] _ = 0
wfunc2 (x:xs) y
    |   (y - x) > 0 = (y-x) + wfunc2 xs y
    |   (y - x) <= 0 = wfunc2 xs (wfunc4 xs x)  

wfunc4 :: [Int] -> Int -> Int
wfunc4 [] _ = 0
wfunc4 list@(x:xs) y 
    |   wfunc3 list y == -1 =  wfunc4 list (y - 1)
    |   otherwise = y

wfunc3 :: [Int] -> Int -> Int
wfunc3 [] x = -1
wfunc3 [x] y = if x >= y then y else -1
wfunc3 (x:xs) y
    |  x >= y = y
    |  otherwise = wfunc3 xs y


--myfunction assumes the next highest integer will find a  counter part
-- when I come across a bigger integer
    -- check if its counter part exits in the list or not and then pass in the function

--find first non-zero integer
--find the next integer of same height or greater
-- kee stroing what comes in between
--compute the water in that section


--removes any s tarting zeros
wfunc1 :: [Int] -> [Int]
wfunc1 [] = []
wfunc1 list@(x:xs)
    |   x == 0 = wfunc1 xs
    |   otherwise = list






-- Question 5
minPathMaze :: [[Int]] -> Int
minPathMaze [[]] = 0
minPathMaze list = final list


-- initially called on param 2d list [0,0] and n(length of one side of the 2d array) amnd returns a list
pathfinder :: [[Int]] -> [Int] -> Int -> [Int]
pathfinder list@((x:xs):[]) (a:b:c) n
    |   (a == b) && (a == n) = [x] ++ [100001]
    |   otherwise = rowfinder (x:xs) (a:b:c) n ++ [100001]
pathfinder list@((x:xs):(y:ys):vs) (a:b:c) n
    |   (a == b) && (a == n) = []
    |   otherwise = x : (check right) ++ (check down)
    where
        right = if (a + 1) <= n then pathfinder (removecol list) ((a+1):[b]) n else [786]
        down = if (b + 1) <= n then pathfinder (removerow list)  (a:[b+1]) n else [786]




final :: [[Int]] -> Int
final [[]] = 0
final list@(x:xs) =   totalist (selection (drop 1 (alloutputs (splite 100001 (pathfinder list [1,1] (length x))) (splite 100001 (pathfinder list [1,1] (length x))!!0)))    (alloutputs (splite 100001 (pathfinder list [1,1] (length x))) (splite 100001 (pathfinder list [1,1] (length x))!!0)!!0))

selection :: [[Int]] -> [Int] -> [Int]
selection (x:[]) y = if totalist x < totalist y then x else y
selection (x:xs) lowest = if totalist x < totalist lowest then selection xs x else selection xs lowest

-- thia will aplit mai weird list into a sensible list
splite :: Int -> [Int] -> [[Int]]
splite _ [] = []
splite value list =
    let prefix = untile value list
        suffix = drop (length prefix) list
    in case suffix of
        [] -> [prefix]
        (_:xs) -> prefix : splite value xs

-- helper for splite
untile :: Int -> [Int] -> [Int]
untile _ [] = []
untile value (x:xs)
    | x == value = []
    | otherwise = x : untile value xs

rowfinder :: [Int] -> [Int] -> Int -> [Int]
rowfinder (x:xs) (a:b:c) n
    |   a + 1 <= n = x :  rowfinder xs ((a+1):[b]) n 
    |   otherwise = [x]

alloutputs :: [[Int]] -> [Int] -> [[Int]]
alloutputs (x:[]) (y:[])= [x]
alloutputs [[]] x = [[]] 
alloutputs (row:[]) traversal = [newparam]
    where
        newparam = (take (length traversal - length row) traversal) ++ row
alloutputs (row:rows) (traversal) = newparam : alloutputs rows newparam
    where
        newparam = (take (length traversal - length row) traversal) ++ row

removerow :: [[Int]] -> [[Int]]
removerow [[]] = [[]]
removerow (x:y:xs) = y:xs

removecol :: [[Int]] -> [[Int]]
removecol [[]] = [[]]
removecol (x:xs) = [drop 1 row | row <- (x:xs)]

bestof :: [Int] -> [Int] -> [Int]
bestof [] [] = []
bestof [] x = x
bestof x [] = x
bestof list1@(x:xs) list2@(y:ys) = if totalist list1 > totalist list2 then list1 else list2

totalist :: [Int] -> Int
totalist [] = 0
totalist (x:xs) = x + totalist xs


check :: [Int] -> [Int]
check [] = []
check liste@(x:xs) =  if liste!!(length liste - 1) == 786 then [] else liste













-- Main Function
main :: IO ()
main =
   hspec $ do

    -- Test List Target Sum
        describe "targetSum" $ do
            it "should return pairs whose sum is equal to the target" $ do
                targetSum [1,2,3,4,5] 5 `shouldBe` [[3,2], [4,1]]
                targetSum [1,2,3,4,5,6] 10 `shouldBe` [[6,4]]
                targetSum [1,2,3,4,5] 0 `shouldBe` []
                targetSum [1,10,8,7,6,2,3,4,5,-1,9] 10 `shouldBe` [[6,4],[7,3],[8,2],[9,1]]
    
    -- Test Symmetric Tree
        describe "symmetricTree" $ do
            it "should return True if the tree is symmetric" $ do
                symmetricTree (Nil :: Tree Int) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 1 Nil)) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 2 Nil)) `shouldBe` False
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 1 Nil))) `shouldBe` True
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 4 Nil))) `shouldBe` False
    
    -- Test Palindrom List
        describe "palindromList" $ do
            it "should return True if the list is a palindrome" $ do
                palindromList (Null :: LinkedList Int) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 Null))))) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 3 (ListNode 1 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 2 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 (ListNode 1 Null)))))) `shouldBe` False
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'b' (ListNode 'a' Null))))) `shouldBe` True
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'c' (ListNode 'a' Null))))) `shouldBe` False
    
    -- Test Snake Traversal
        describe "snakeTraversal" $ do
            it "should return the snake traversal of the tree" $ do
                snakeTraversal (Nil:: Tree Int) `shouldBe` []
                snakeTraversal (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) `shouldBe` [2,3,1]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil))) `shouldBe` [4,2,3,1,6,5,7]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode (TreeNode Nil 9 Nil) 7 Nil))) `shouldBe` [4,2,3,1,6,5,7,9]
    
    -- Test Tree Construction
        describe "treeConstruction" $ do
            it "should return the tree constructed from the string" $ do
                treeConstruction "" `shouldBe` Nil
                treeConstruction "a" `shouldBe` TreeNode Nil 'a' Nil
                treeConstruction "^a" `shouldBe` Nil
                treeConstruction "ab^c" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^c^" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^cde^f" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil)
                treeConstruction "abcde^f" `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil) 'b' Nil) 'a' Nil
    
    -- Test (+) operator for Tree
        describe "(+)" $ do
            it "should return the sum of the two trees" $ do
                let result1 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) :: Tree Int) 
                result1  `shouldBe` TreeNode (TreeNode Nil 2 Nil) 4 (TreeNode Nil 6 Nil) 
                let result2 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode Nil 2 (TreeNode Nil 3 Nil) :: Tree Int)
                result2 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 4 (TreeNode Nil 6 Nil)
                let result3 = (Nil + Nil :: Tree Int) 
                result3 `shouldBe` Nil
                let result4 = (Nil + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil):: Tree Int)
                result4 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)
                let result5 = (TreeNode (TreeNode (TreeNode Nil 1 (TreeNode Nil (-2) Nil)) 3 Nil) 4 (TreeNode Nil 2 (TreeNode Nil 7 (TreeNode Nil (-7) Nil))) + TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 1 Nil) 3 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 4 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 2 (TreeNode (TreeNode Nil (-5) Nil) 7 Nil)) :: Tree Int) 
                result5 `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 2 (TreeNode Nil (-2) Nil)) 6 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 8 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 4 (TreeNode (TreeNode Nil (-5) Nil) 14 (TreeNode Nil (-7) Nil)))
                let result6 = (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) + TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) :: Tree Int) 
                result6 `shouldBe` TreeNode (TreeNode (TreeNode Nil 2 Nil) 6 (TreeNode Nil 12 Nil)) 8 (TreeNode (TreeNode Nil 10 Nil) 4 (TreeNode Nil 14 Nil))
    
    -- Test Longest Common String
        describe "longestCommonString" $ do
            it "should return the longest common string" $ do
                longestCommonString Null Null `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) Null `shouldBe` Null
                longestCommonString Null (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'f' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' Null)))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'f' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' Null))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'f' (ListNode 'g' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' Null)
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'f' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'c' (ListNode 'd' (ListNode 'e' Null))
    
    -- Test Common Ancestor
        describe "commonAncestor" $ do
            it "should return the lowest common ancestor of the two nodes" $ do
                commonAncestor Nil 1 2 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 3 `shouldBe` Just 2
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 4 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 6 `shouldBe` Just 5
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 8 9 `shouldBe` Just 8
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 3 `shouldBe` Just 3
                
    
    -- Test Game of Life
        describe "gameofLife" $ do
            it "should return the next state" $ do
                gameofLife [[0,1,0],[0,0,1],[1,1,1],[0,0,0]] `shouldBe` [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
                gameofLife [[1,1],[1,0]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,1],[1,1]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,0],[0,1]] `shouldBe` [[0,0],[0,0]]
                gameofLife [[0,1,0,0],[0,1,1,1],[1,0,1,1]] `shouldBe` [[0,1,0,0],[1,0,0, 1],[0,0,0,1]]
    
    -- Test Water Collection
        describe "waterCollection" $ do
            it "should return the amount of water that can be trapped" $ do
                waterCollection [0,1,0,2,1,0,1,3,2,1,2,1] `shouldBe` 12
                waterCollection [4,2,0,3,2,5] `shouldBe` 18
                waterCollection [1,2,3,4,5] `shouldBe` 0
                waterCollection [5,4,3,2,1] `shouldBe` 0
                waterCollection [5,4,3,2,1,2,3,4,5] `shouldBe` 32  
                waterCollection [1, 0, 2, 3, 1, 4] `shouldBe` 6
                waterCollection [0, 4, 1, 2, 0, 1, 3] `shouldBe` 16
    
    -- Test Min Path Maze
        describe "minPathMaze" $ do
            it "should return the minimum cost to reach the bottom right cell" $ do
                minPathMaze [[1,3,1],[1,5,1],[4,2,1]] `shouldBe` 7
                minPathMaze [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` 21
                minPathMaze [[1,2,3,4],[4,5,6,7],[7,8,9,9],[10,11,1,13]] `shouldBe` 35
                minPathMaze [[1,2,3,4,5],[4,5,6,7,8],[7,8,9,9,10],[10,11,1,13,14],[15,16,17,18,19]] `shouldBe` 66
                minPathMaze [[1,2,3,4,5,6],[4,1,2,7,8,9],[7,8,1,2,10,11],[10,11,1,2,22,15],[15,16,17,1,2,20],[21,22,23,24,2,26]] `shouldBe` 41