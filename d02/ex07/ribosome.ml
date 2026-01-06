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

let generate_base_triplets (r : rna) : (nucleobase * nucleobase * nucleobase) list =
    let rec loop r acc =
        match r with
        | a :: b :: c :: tail -> loop tail ((a, b, c) :: acc)
        | [] | _ -> acc
    in
    loop r []

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

    let print_triplet (a, b, c) =
        print_string "(";
        print_nucleobase a;
        print_nucleobase b;
        print_nucleobase c;
        print_string ")";
    in

    let rec print_triplets tl =
        match tl with
        | [] -> print_newline ()
        | head :: tail -> print_triplet head; print_triplets tail
    in

    print_endline "Multiple of 3:";
    generate_helix 9
    |> generate_rna
    |> generate_base_triplets
    |> print_triplets;

    print_endline "\nNot multiple of 3:";
    generate_helix 11
    |> generate_rna
    |> generate_base_triplets
    |> print_triplets

let _ = main ()
