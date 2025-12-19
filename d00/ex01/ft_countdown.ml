let rec ft_countdown (num: int) : unit =
    if num >= 0 then (
        print_int num;
        print_char '\n';
        ft_countdown (num - 1)
    ) else
        ()

(* tests *)
let _ = ft_countdown 5
let _ = ft_countdown 0
let _ = ft_countdown (-1)
