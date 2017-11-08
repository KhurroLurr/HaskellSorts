-- Nicholas Espinosa
-- COP 4020
-- Haskell Sorts

-- Lists I used for testing
-- Other lists can be used if desired
intList1 = [20,5,10,16,12,6,9,3,10,20]
intList2 = [6,12,10,13,9,12,13,4,20,9]
intList3 = [2,17,20,4,9,14,15,11,1,11]
intList4 = [16,9,8,9,10,2,3,20,9,2]
intList5 = [4,11,8,12,1,2,14,4,15,4]

----------------------------------------
{- Insertion sort and helper functions-}
----------------------------------------
-- Main Function
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort x = innerInsert x 0 

-- First insert list handler, helps iterate through the list
innerInsert [] i = []
innerInsert x i
    | i > (length x) - 1 = x
    | otherwise = innerInsert (newList x i) (i+1)
 
-- Combines the separated lists into one
newList [] i = error "Empty List"
newList x i = (insertValue(splitList x i)) ++ (deleteValue(splitList x i))

-- Splits the list at a given position
splitList [] n = error "Empty List"
splitList x n = [take n x, drop n x]

-- Inserts the first value of the second half into the first
insertValue x = insertCheck (((x !! 1) !! 0) : (x !! 0)) 0

-- Base determination if a value should be inserted further
-- Insertion ends if value reaches the top or sets off the flag
insertCheck [] f = error "Empty List"
insertCheck x f
    | (length x) == 1 = x
    | f == 1 = x
    | otherwise = moveValue x f
 
-- Determines if the value moves up the list
moveValue [] f = error "Empty List"
moveValue (x:y:xs) f
    | x > y = y : insertCheck(x:xs) 0
    | otherwise = x : insertCheck(y : xs) 1   
    
-- Deletes the first value from the second list
deleteValue x = drop 1 (x !! 1)

{- 
Programmer's Note: Original version of insertValue function looked like this:
        insertValue x = insert ((x !! 1) !! 0)  (x !! 0)
However, I was uncertain as to whether this was acceptable as it did technically
sort the value into the first half of the list for me. To prevent any technicality
I used a modified version of the bubbleSort in the first half.
-}  

-------------------------------------
{- Bubble Sort and Helper Functions-}
-------------------------------------
-- Main function
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort x = innerBubble x 1 ((length x)-1)

-- Used to iterate through functions
innerBubble [] i j = error "Empty List"
innerBubble x i j
    | i > j = x
    | otherwise = innerBubble (swapCheck x i) (i+1) j 

-- Determines if the swapping should continue
swapCheck [] i = error "Empty List"
swapCheck x i
    | (length x) == i = x
    | otherwise = swapValues x i

-- Performs the swapping of values
swapValues [] i = []
swapValues (x:y:xs) i
    | x > y = y : swapCheck (x:xs) i
    | otherwise = x : swapCheck (y:xs) i

----------------------------------------
{- Selection Sort and helper functions-}
----------------------------------------
-- Main Functions
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort x = (minimum x) : selectionSort (deleteMin x)

-- Deletes the minimum value from the list
deleteMin [] = error "Empty List"
deleteMin x = delete (minimum x) x