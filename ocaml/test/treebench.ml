open Fungi
open Core_bench

let main () =
    (* TODO: do a csv export, bench only relevant operations *)
    Command_unix.run ~argv:[""; "-quota"; "3s"] (
        Bench.make_command [
            Bench.Test.create ~name:"id"
                (fun () -> ());
            Bench.Test.create ~name:"Time.now"
                (fun () -> ignore (Core.Time_float.now ()));
            Bench.Test.create ~name:"Array.create300"
                (fun () -> ignore (Array.init 300 (fun _idx -> 0)))
        ]
    )
;;

let () = main ()
;;
