let ft_is_palindrome (subject : string) =
        let rec check left right =
                if left < right then
                        String.get subject left = String.get subject right
                        && check (left + 1) (right - 1)
                else
                        true
        in check 0 (String.length subject - 1)

(* tests *)
let print_bool expr =
        if expr then print_endline "true"
        else print_endline "false"

let _ = print_bool (ft_is_palindrome "radar") (* should be true *)
let _ = print_bool (ft_is_palindrome "") (* should be true *)
let _ = print_bool (ft_is_palindrome "osso") (* should be true *)
let _ = print_bool (ft_is_palindrome "121") (* should be true *)
let _ = print_bool (ft_is_palindrome "car") (* should be false *)
let _ = print_bool (ft_is_palindrome "bone") (* should be false *)
let _ = print_bool (ft_is_palindrome "1234") (* should be false *)
