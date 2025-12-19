let ft_rot_n (n : int) (str : string) =
    let rotate c =
        let c_as_int = int_of_char c in

        if 97 <= c_as_int && c_as_int <= 122 then
            char_of_int ((((c_as_int + n) - 97) mod 26) + 97)
        else if 65 <= c_as_int && c_as_int <= 90 then
            char_of_int ((((c_as_int + n) - 65) mod 26) + 65)
        else
            c
    in
    String.map rotate str

(* tests *)
let _ = ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" |> print_endline
let _ = ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" |> print_endline
let _ = ft_rot_n 42 "0123456789" |> print_endline
let _ = ft_rot_n 2 "OI2EAS67B9" |> print_endline
let _ = ft_rot_n 0 "Damned !" |> print_endline
let _ = ft_rot_n 42 "" |> print_endline
let _ = ft_rot_n 1 "NBzlk qnbjr !" |> print_endline

