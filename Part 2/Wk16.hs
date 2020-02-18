module Wk16 where

    data Tree a = Br a (Tree a) (Tree a) | Lf
        deriving(Show)
    
    linear :: Int -> Tree Int
    linear 0 = Lf
    linear n
        | n < 0     = undefined
        | otherwise = Br n Lf (linear (n - 1)) 
    
    preorder :: Tree a -> [a]
    preorder t = preorder_aux t []
    -- Inefficient Version (due to concat)
    -- preorder Lf           = []
    -- preorder (Br a t1 t2) = [a] ++ (preorder t1) ++ (preorder t2)
    
    preorder_aux :: Tree a -> [a] -> [a]
    preorder_aux Lf xs           = xs
    preorder_aux (Br a t1 t2) xs = a:(preorder_aux t1 ys)
        where ys = preorder_aux t2 xs 
    
    inorder :: Tree a -> [a]
    inorder t = inorder_aux t []
    -- Inefficient Version (due to concat)
    -- inorder Lf           = []
    -- inorder (Br a t1 t2) = (inorder t1) ++ [a] ++ (inorder t2)
    
    inorder_aux :: Tree a -> [a] -> [a]
    inorder_aux Lf xs           = xs
    inorder_aux (Br a t1 t2) xs = inorder_aux t1 (a:ys)
        where ys = inorder_aux t2 xs 
    
    postorder :: Tree a -> [a]
    postorder t = postorder_aux t []
    -- Inefficient Version (due to concat)
    -- postorder Lf           = []
    -- postorder (Br a t1 t2) = (postorder t1) ++ (postorder t2) ++ [a]
    
    postorder_aux :: Tree a -> [a] -> [a]
    postorder_aux Lf xs           = xs
    postorder_aux (Br a t1 t2) xs = postorder_aux t1 (postorder_aux t2 (a:xs))
    