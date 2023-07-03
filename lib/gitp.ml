
module Store=Git_unix.Store
module Search=Git.Search.Make(Digestif.SHA1) (Store)

let get_store ()=Store.v @@ Fpath.v @@ Sys.getcwd ()

let get_latest_commit store=Store.Ref.resolve store Git.Reference.master

let find_blob store cid path=Search.find store cid (`Commit (`Path path))

let split_path s=String.split_on_char '/' s |> List.filter ((<>) ".")
    
    

let read_blob store blob=Store.read store blob

module Command=struct
  open Cmdliner

  module Cat=struct
    let sub=
      let f cid file=
        let open Lwt_result.Syntax in
        let* store=get_store() in
        let* cid=if cid="" then get_latest_commit store else Lwt_result.return @@ Digestif.SHA1.of_hex cid in
        let open Lwt.Syntax in
        let* hash=find_blob store cid (split_path file) in
        match hash with
          | Some h->Store.read store h
          | None->Lwt.return (Error (`Not_found cid)) in
      let out=
        let sub fmtr=function
          |Git.Value.Blob b->Fmt.string fmtr (Git.Blob.to_string b)
          |_->Fmt.string  fmtr "this is not file" in
        Fmt.result ~ok:sub ~error:Store.pp_error in
      let cid=
        Arg.(value & opt string "" & info ["c";"commit"] ~docv:"COMMIT" ~doc:"specify commit id") in
      let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name") in
      let doc="show file in repository to standard output." in
      let info=Cmd.info "cat" ~doc in
      Cmd.v info Term.(const (fun c p->Lwt_main.run Lwt.Infix.(f c p>|= out Fmt.stdout)) $ cid $ file)
  end

  module Ls=struct
    let sub=
      let f cid dir=let open Lwt_result.Syntax in
        let* store=get_store() in
        let* cid=if cid="" then get_latest_commit store else Lwt_result.return @@ Digestif.SHA1.of_hex cid in
        let open Lwt.Syntax in
        let* hash=find_blob store cid (split_path dir) in
        let* value=match hash with
          | Some h->Store.read store h
          | None->Lwt.return (Error (`Not_found cid)) in
        match value with
          |Ok (Git.Value.Tree t)->let open Git.Tree in Git.Tree.to_list t |> List.map (fun e->e.name) |> Lwt.return_ok
          |_->Lwt.return (Error (`Not_found cid)) in
      let out=
        let sub fmtr l=List.iter (fun i->Fmt.string fmtr (i^"\n")) l in
        Fmt.result ~ok:sub ~error:Store.pp_error in
      let cid=
        Arg.(value & opt string "" & info ["c";"commit"] ~docv:"COMMIT" ~doc:"specify commit id") in
      let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name") in
      let info=Cmd.info "ls" in
      Cmd.v info Term.(const (fun c p->Lwt_main.run Lwt.Infix.(f c p>|= out Fmt.stdout)) $ cid $ file)
  end
(*
  module Save=struct
    let read_all ()=
      let rec sub l=
        let line,eof=try read_line (),false with End_of_file->"",true in
        if eof then l
        else sub (l^"\n"^line)
    in sub ""

    let sub=
      let f file=
        let data=read_all () in
        let blob=Git.Blob.of_string data in
        let path=split_path file in
        let open Lwt_result.Syntax in
        let* store=get_store() in
        let* cid=get_latest_commit store in
        let open Lwt.Syntax in
        let* hash=find_blob store cid (split_path @@ Filename.dirname file) in
        let* tree=match hash with
          |Some h->Store.read store h
          |None->Lwt.return (Error (`Not_found cid)) in
        let* new_tree=match tree with
          |Ok (Git.Value.Tree t)->
            t
            |> Git.Tree.remove ~name:(Filename.basename file)
            |> Git.Tree.add (Git.Tree.entry ~name:(Filename.basename file) `Normal (Git.Blob.digest blob))
            |>Lwt_result.return
          |_->Lwt.return (Error (`Not_found cid)) in
        print_endline data
      in let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name")
      in let info=Cmd.info "save" in
      Cmd.v info Term.(const f $ file)
  end
  *)

module Log=struct
end

  let main ()=
    let info=Cmd.info "gitp" in
    Cmd.group info [Cat.sub;Ls.sub;] |> Cmd.eval
end