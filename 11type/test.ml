let rec zip lst1 = fun lst2 ->
  try 
    match lst1 with
      [] -> (match lst2 with
          [] -> []
        | fst2 :: rest2 -> raise (Error 1))
    | fst1 :: rest1 -> (match lst2 with
          [] -> raise (Error 1)
        | fst2 :: rest2 ->
          (fst1 :: fst2 :: []) :: (zip rest1 rest2))
  with Error 1 -> false in
zip (1 :: 3 :: 5 :: []) (4 :: 6 :: [])
