let ft_print_rev (s : string) =
        let final_char = (String.length s) - 1 in
        
        let rec rev_iterate pos =
                if pos < 0 then ()
                else (print_char (String.get s (pos)); rev_iterate (pos - 1))
        in rev_iterate final_char;
        print_char '\n'

(* tests *)
let _ = ft_print_rev "Hello, world!"
let _ = ft_print_rev "abcdef"
let _ = ft_print_rev "banana"
let _ = ft_print_rev "0123456789"
let _ = ft_print_rev "" (* this should just print a newline  *)
