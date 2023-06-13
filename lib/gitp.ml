
module Store=Git_unix.Store
module Search=Git.Search.Make(Digestif.SHA1) (Store)

let get_store ()=Store.v @@ Fpath.v @@ Sys.getcwd ()

let get_latest_commit store=Store.Ref.resolve store Git.Reference.main

let find_blob store cid filename=Search.find store cid (`Commit (`Path [filename]))

let read_blob store blob=Store.read store blob

module Command=struct
  open Cmdliner

  let split_path s=String.split_on_char '/' s |> List.filter ((<>) ".")

  module Cat=struct
    let sub=
      let f file=
        let open Lwt_result.Syntax in
        let* store=get_store() in
        let* cid=get_latest_commit store in
        let open Lwt.Syntax in
        let* hash=find_blob store cid file in
        match hash with
          | Some h->Store.read store h
          | None->Lwt.return (Error (`Not_found cid)) in
      let out=
        let sub i=function
          |Git.Value.Blob b->Fmt.string i (Git.Blob.to_string b)
          |_->Fmt.string  i "Not file" in
        Fmt.result ~ok:sub ~error:Store.pp_error in
      let doc="show file in repository to standard output." in
      let file=
        Arg.(value & pos 0 string "" & info [] ~docv:"FILE" ~doc:"file name") in
      let info=Cmd.info "cat" ~doc in
      Cmd.v info Term.(const (fun p->Lwt_main.run Lwt.Infix.(f p>|= out Fmt.stdout)) $ file)
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