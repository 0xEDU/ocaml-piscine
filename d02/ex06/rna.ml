type phosphate = string

type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | U
    | None

type nucleotide = {
    phosphate : phosphate;
    deoxyribose : deoxyribose;
    nucleobase : nucleobase
}

let generate_nucleotide (c : char) : nucleotide = {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = match c with
        | 'a' | 'A' -> A
        | 't' | 'T' -> T
        | 'c' | 'C' -> C
        | 'g' | 'G' -> G
        | _ -> None
}

type helix = nucleotide list

let generate_helix (n : int) : helix =
    Random.self_init ();
    let rec generate_helix_loop n acc =
        if n = 0 then
            acc
        else
            let c = match Random.int 4 with
                    | 0 -> 'A'
                    | 1 -> 'T'
                    | 2 -> 'C'
                    | 3 -> 'G'
                    | _ -> '_'
            in
            generate_helix_loop (n - 1) (generate_nucleotide c :: acc)
    in
    generate_helix_loop n []

let helix_to_string (h : helix) : string =
    let nucleobase_to_string nb =
        match nb with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | _ -> ""
    in

    let rec helix_to_string_loop h acc =
        match h with
        | [] -> acc
        | head :: tail -> helix_to_string_loop tail (nucleobase_to_string head.nucleobase ^ acc)
    in
    helix_to_string_loop h ""

let complementary_helix (h : helix) : helix =
    let rec complementary_helix_loop h acc =
        match h with
        | [] -> acc
        | head :: tail ->
            let nb = match head.nucleobase with
                    | A -> 'T'
                    | T -> 'A'
                    | C -> 'G'
                    | G -> 'C'
                    | _ -> '_'
            in
            complementary_helix_loop tail (generate_nucleotide nb :: acc)
    in
    complementary_helix_loop h []

type rna = nucleobase list

let generate_rna (h : helix) : rna =
    let comp_h = complementary_helix h in

    let rec generate_rna_loop comp_h acc =
        match comp_h with
        | [] -> acc
        | head :: tail ->
            let nb = match head.nucleobase with
                    | T -> U
                    | _ -> head.nucleobase
            in
            generate_rna_loop tail (nb :: acc)
    in
    generate_rna_loop comp_h []

(* tests *)
let main () =
    let print_nucleobase nb =
        match nb with
            | A -> print_string "A"
            | T -> print_string "T"
            | C -> print_string "C"
            | G -> print_string "G"
            | U -> print_string "U"
            | _ -> print_string ""
    in

    let rec print_rna r =
        match r with
        | [] -> print_newline ()
        | head :: tail -> print_nucleobase head; print_rna tail
    in

    let h = [
        generate_nucleotide 'A';
        generate_nucleotide 'T';
        generate_nucleotide 'C';
        generate_nucleotide 'G';
        generate_nucleotide 'A'
    ] in

    print_endline "PDF Example:";
    generate_rna h |> print_rna;

    print_endline "\nRandom Example:";
    let h10 = generate_helix 5 in
    print_string "Original -> "; print_string (helix_to_string h10);
    print_string "\nRNA      -> "; generate_rna h10 |> print_rna

let _ = main ()
