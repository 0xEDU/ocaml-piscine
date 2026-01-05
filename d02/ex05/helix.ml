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

(* tests *)
let main () =
    let h5 = generate_helix 5 in
    let h20 = generate_helix 20 in

    print_endline "Helixes:";
    helix_to_string h5 |> print_endline;
    helix_to_string h20 |> print_endline;

    print_endline "\nComplementaries:";
    complementary_helix h5 |> helix_to_string |> print_endline;
    complementary_helix h20 |> helix_to_string |> print_endline

let _ = main ()
