open Paritygame.Game;;
let () =
    let p = ParityGame.empty in

    (* <> is Odd, () is Even *)

    (* I also assume there are no same priority nodes to make life easy *)
    let (l_2, p)  = ParityGame.add_node  ParityGame.Even 2  p in
    let (l_15,p)  = ParityGame.add_node  ParityGame.Odd  15 p in
    let (l_4, p)  = ParityGame.add_node  ParityGame.Even 4  p in
    let (l_6, p)  = ParityGame.add_node  ParityGame.Even 6  p in
    let (l_8, p)  = ParityGame.add_node  ParityGame.Even 8  p in
    let (l_10,p)  = ParityGame.add_node  ParityGame.Even 10 p in
    let (l_3, p)  = ParityGame.add_node  ParityGame.Odd  3  p in
    let (l_5, p)  = ParityGame.add_node  ParityGame.Odd  5  p in
    let (l_7, p)  = ParityGame.add_node  ParityGame.Odd  7  p in
    let (l_9, p)  = ParityGame.add_node  ParityGame.Odd  9  p in
    let (l_11,p)  = ParityGame.add_node  ParityGame.Odd  11 p in
    let (l_13,p)  = ParityGame.add_node  ParityGame.Odd  13 p in
    let (l_99,p)  = ParityGame.add_node  ParityGame.Odd  99 p in

    let p_adjlist =
        [
            (l_2,  [l_4;  l_11]);
            (l_4,  [l_2;  l_8;  l_6]);
            (l_6,  [l_3;  l_5;  l_7; l_9]);
            (l_8,  [l_7;  l_5;  l_2]);
            (l_10, [l_13; l_15]);
            (l_3,  [l_2;  l_4]);
            (l_5,  [l_7;  l_9]);
            (l_7,  [l_10]);
            (l_9,  [l_3;  l_5;  l_10; l_99]);
            (l_11, [l_8]);
            (l_13, [l_15]);
            (l_99, [l_99]);
            (l_15, [l_13])
        ] in

    let p = ParityGame.Graph.of_list p_adjlist p  in
    ParityGame.Nodes.iter (fun node _value -> 
        Format.printf "priority value is %d\n" (ParityGame.valueof node)
    ) p
