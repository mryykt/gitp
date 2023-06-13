
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
      let f d=print_endline d in
      let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name") in
      let info=Cmd.info "ls" in
      Cmd.v info Term.(const f $ file)
  end

  module Save=struct
    let sub=
      let f d=print_endline d in
      let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name") in
      let info=Cmd.info "save" in
      Cmd.v info Term.(const f $ file)
  end

  let main ()=
    let info=Cmd.info "gitp" in
    Cmd.group info [Cat.sub;Ls.sub;Save.sub] |> Cmd.eval
end