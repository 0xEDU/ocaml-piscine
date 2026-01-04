type phosphate = string

type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | None

type nucleotide = {
    phosphate : phosphate;
    deoxyribose : deoxyribose;
    nucleobase : nucleobase
}

let generate_nucleotide (c : char) : nucleotide =
    let p = match c with
            | 'a' | 'A' -> A
            | 't' | 'T' -> T
            | 'c' | 'C' -> C
            | 'g' | 'G' -> G
            | _ -> None
    in
    {
        phosphate = "phosphate";
        deoxyribose = "deoxyribose";
        nucleobase = p
    }

(* tests *)
let main () =
    let string_of_nucleobase = function
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | None -> "None"
    in

    let print_nucleotide n =
      Printf.printf "{ phosphate = %S; deoxyribose = %S; nucleobase = %s }\n"
        n.phosphate
        n.deoxyribose
        (string_of_nucleobase n.nucleobase)
    in

    print_nucleotide (generate_nucleotide 'A');
    print_nucleotide (generate_nucleotide 'c');
    print_nucleotide (generate_nucleotide 'T');
    print_nucleotide (generate_nucleotide 'b')

let _ = main ()
