let ft_printnum num = print_int num; print_char '\n'

let rec ft_countdown (num: int) : unit =
    if num < 0 then ()
    else begin
        ft_printnum num;
        ft_countdown (num - 1)
    end

(* tests *)
(* let _ = ft_countdown(5) *)
(* let _ = ft_countdown(0) *)
(* let _ = ft_countdown(-1) *)
