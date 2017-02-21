(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Input *)

type input =
  { buf_size : int;
    inf : Fpath.t;
    input_string : bool;
    layout : bool; }

let with_input_decoder i f v =
  let wrap input () =
    let dec = Sexpm.decoder ~layout:i.layout input in
    f i dec v
  in
  match i.input_string with
  | true ->
      OS.File.read i.inf
      >>= fun s -> Ok (wrap (Sexpm.input_of_string s) ())
  | false ->
      let bytes = Bytes.create i.buf_size in
      OS.File.with_input ~bytes i.inf wrap ()

(* Output *)

type output =
  { buf_size : int;
    outf : Fpath.t;
    output_string : bool;
    style : Sexpm.encoder_style;
    lex : bool; }

let with_output_encoder o f v =
  let wrap output () =
    let bytes = Bytes.create o.buf_size in
    let enc = Sexpm.encoder ~bytes ~style:o.style output in
    let r = f o enc v in
    r
  in
  match o.output_string with
  | true ->
      let buf = Buffer.create o.buf_size in
      let r = wrap (Sexpm.output_of_buffer buf) () in
      OS.File.write o.outf (Buffer.contents buf) >>| fun () -> r
  | false ->
      OS.File.with_output o.outf wrap ()

let with_output_formatter o f v = match o.output_string with
| true ->
    let buf = Buffer.create o.buf_size in
    let ppf = Format.formatter_of_buffer buf in
    let r = f o ppf v in
    (Format.pp_print_flush ppf (); OS.File.write o.outf (Buffer.contents buf))
    >>= fun () -> Ok (Ok r)
| false ->
    let wrap oc () =
      let ppf = Format.formatter_of_out_channel oc in
      let r = f o ppf v in
      (Format.pp_print_flush ppf (); Ok r)
    in
    OS.File.with_oc o.outf wrap ()

(* Pretty printers *)

let pp_decode i dec ppf = function
| Ok None ->
    Format.fprintf ppf "%a:%a: EOI@,"
      Fpath.pp i.inf Sexpm.pp_range (Sexpm.decoded_range dec)
| Ok (Some l) ->
    Format.fprintf ppf "%a:%a: @[%a@]@,"
      Fpath.pp i.inf Sexpm.pp_range (Sexpm.decoded_range dec)
      Sexpm.pp_lexeme l
| Error (e, r) ->
    Format.fprintf ppf "%a:%a: @[%a@]@,"
      Fpath.pp i.inf Sexpm.pp_range (Sexpm.decoded_range dec)
      Sexpm.pp_error e

(* Decode *)

let decode err_ppf i =
  let decode i dec () =
    let rec loop () = match Sexpm.decode_lexeme dec with
    | Ok None -> ()
    | Ok (Some l) -> loop ()
    | Error _ as err -> pp_decode i dec err_ppf err; loop ()
    in
    Format.(fprintf err_ppf "@[<v>"; loop (); fprintf err_ppf "@]@?")
  in
  with_input_decoder i decode ()

(* Lex *)

let lex i o =
  let lex i dec o ppf () =
    let rec loop () = match Sexpm.decode_lexeme dec with
    | Ok None as d -> pp_decode i dec ppf d
    | d -> pp_decode i dec ppf d; loop ()
    in
    Format.(fprintf ppf "@[<v>"; loop (); fprintf ppf "@]@?")
  in
  R.join @@ with_input_decoder i
    (fun i dec () -> R.join @@ with_output_formatter o (lex i dec) ()) ()

(* Trip *)

let trip err_ppf i o =
  let trip i dec o enc () =
    let rec loop () = match Sexpm.decode_lexeme dec with
    | Ok None -> Sexpm.encode_lexeme enc None
    | Ok d -> Sexpm.encode_lexeme enc d; loop ()
    | Error _ as err -> pp_decode i dec err_ppf err; loop ()
    in
    Format.(fprintf err_ppf "@[<v>"; loop (); fprintf err_ppf "@]@?"; Ok ())
  in
  R.join @@ with_input_decoder i
    (fun i dec () -> R.join @@ with_output_encoder o (trip i dec) ()) ()


(* Random encode *)

type rand =
  { seed : int;
    count : int; (* number of sexps to generate. *)
    maxd : int; (* max depth of lists. *)
    maxl : int; (* max length of lists. *)
    maxa : int; (* max length of atoms. *) }

let r_ascii_lowercase_letter () =
  Uchar.unsafe_of_int (0x0061 (* a *) + Random.int 26)

let r_general_scripts_minus_ctrl () =
  Uchar.unsafe_of_int (0x0020 + Random.int 0x1FE0) (* < U+2000 *)

let r_white encode r buf =
  Buffer.clear buf;
  for i = 0 to Random.int 3 do match Random.int 100 with
  | n when n < 90 -> Buffer.add_char buf ' '
  | n when n < 94 -> Buffer.add_char buf '\t'
  | n when n < 98 -> Buffer.add_char buf '\n'
  | n when n < 100 -> Buffer.add_char buf '\r'
  | n -> assert false
  done;
  encode (Sexpm.White (Buffer.contents buf))

let r_comment encode r buf =
  Buffer.clear buf;
  for i = 0 to Random.int 15 do
    Uutf.Buffer.add_utf_8 buf (r_general_scripts_minus_ctrl ())
  done;
  encode (Sexpm.Comment (Buffer.contents buf))

let r_atom encode r buf =
  Buffer.clear buf;
  for i = 0 to Random.int (r.maxa + 1)
  do Uutf.Buffer.add_utf_8 buf (r_ascii_lowercase_letter ()) done;
  encode (Sexpm.Atom (Buffer.contents buf, None))

let rec r_list encode r buf depth count =
  let depth = Random.int (depth + 1) in
  encode (Sexpm.Ls);
  let count = r_sexp_seq encode r buf depth (count - 1) in
  encode (Sexpm.Le);
  count

and r_sexp_seq encode r buf depth count =
  let rec loop len count = match len = 0 || count = 0 with
  | true -> count
  | false ->
      begin match Random.int 100 with
      | n when n < 80 -> ()
      | n when n < 90 -> r_white encode r buf
      | n when n < 100 -> r_comment encode r buf
      | n -> assert false
      end;
      match depth = 0 || Random.bool () with
      | true -> r_atom encode r buf; loop (len - 1) (count - 1)
      | false ->
          let count = r_list encode r buf (depth - 1) count in
          loop (len - 1) count
  in
  loop (Random.int (r.maxl + 1)) count

let random o r =
  let random o e () =
    let buf = Buffer.create 255 in
    let encode l = Sexpm.encode_lexeme e (Some l) in
    let rec loop = function
    | 0 -> Sexpm.encode_lexeme e None; Ok ()
    | n -> loop (r_sexp_seq encode r buf r.maxd n)
    in
    loop r.count
  in
  R.join @@ with_output_encoder o random ()

(* Main command *)

let do_cmd (i, o) map rand = function
| `Decode -> decode Format.err_formatter i
| `Random -> random o rand
| `Trip when o.lex -> lex i o
| `Trip -> trip Format.err_formatter i o

(* Command line interface *)

open Cmdliner

let nat_arg =
  let kind = "positive integer "in
  let parse s = match Arg.parser_of_kind_of_string ~kind String.to_int s with
  | Ok n when n > 0 -> `Ok n
  | Ok n -> `Error (strf "%d is not positive" n)
  | Error (`Msg e) -> `Error e
  in
  Arg.pconv (parse, Format.pp_print_int) (* FIXME use Arg.conv *)

let fpath_arg =
  let parse s = match Fpath.of_string s with
  | Ok v -> `Ok v
  | Error (`Msg e) -> `Error e
  in
  Arg.pconv (parse, Fpath.pp) (* FIXME use Arg.conf *)

let buf_size =
  let doc = "Size of internal buffers." in
  Arg.(value & opt nat_arg 65536 & info ["buffer-size"] ~doc ~docv:"INT")

(* Input *)

let inf =
  let doc = "The input file. Reads from standard input if unspecified." in
  Arg.(value & pos 0 fpath_arg OS.File.dash & info [] ~doc ~docv:"FILE")

let input_section = "INPUT OPTIONS"
let docs = input_section

let input_string =
  let doc = "Input everything in a string and decode the string." in
  Arg.(value & flag & info ["input-string"] ~doc ~docs)

let no_layout =
  let doc = "Do not preserve concrete syntax layout on decode." in
  Arg.(value & flag & info ["no-layout"] ~doc ~docs)

let input =
  let input buf_size inf input_string no_layout =
    { buf_size; inf; input_string; layout = not no_layout }
  in
  Term.(const input $ buf_size $ inf $ input_string $ no_layout)

(* Output options. *)

let output_section = "OUTPUT OPTIONS"
let docs = output_section

let lex =
  let doc = "Output the decoded s-expression lexemes or errors, one per line
             with their position."
  in
  Arg.(value & flag & info ["lex"] ~docs ~doc)

let style =
  let doc = "Minify output." in
  let minify = `Minify, Arg.info ["m"; "minify"] ~docs ~doc in
  let doc = "Pretty-print output." in
  let pp = `Pp, Arg.info ["p"; "pretty-print"] ~docs ~doc in
  Arg.(value & vflag `Raw [minify; pp])

let output_string =
  let doc = "Encode everything in a string and output the string." in
  Arg.(value & flag & info ["output-string"] ~doc ~docs)

let outf =
  let doc = "Output result to $(docv)." in
  Arg.(value & opt (some fpath_arg) None & info ["o"] ~docs ~doc ~docv:"FILE")

let in_place =
  let doc = "Atomically write output, on success, to the file specified as
             input."
  in
  Arg.(value & flag & info ["i"; "in-place"] ~docs ~doc)

let io =
  let err_excl o0 o1 =
    R.error_msgf "option %s and %s are mutually exclusive" o0 o1
  in
  let io input lex output_string style outf in_place =
    if in_place && lex then err_excl "--in-place" "--lex" else
    let buf_size = (input : input).buf_size in
    let outf = match outf with
    | None when in_place ->
        if Fpath.equal input.inf OS.File.dash
        then R.error_msg ("option --in-place cannot be used with stdin")
        else Ok input.inf
    | Some _ when in_place -> err_excl "--in-place" "-o"
    | None -> Ok (OS.File.dash)
    | Some f -> Ok f
    in
    outf >>| fun outf ->
    (input, { buf_size; lex; output_string; style; outf })
  in
  Term.(cli_parse_result
         (const io $ input $ lex $ output_string $ style $ outf $ in_place))

(* Map lookup and update. *)

let map_section = "S-EXPRESSION MAP LOOKUP AND UPDATE OPTIONS"
let docs = map_section
let docv = "KEY"

type map =
  { allow_undef : bool;
    exists : string option;
    delete : string option;
    get : string option;
    set : string option;
    value : string option; }

let allow_undef =
  let doc = "For $(b,--get), $(b,--set) and $(b,--delete), do not error if
             the key $(i,KEY) is undefined." in
  Arg.(value & flag & info ["u"; "allow-undefined"] ~docs ~doc)

let exists =
  let doc = "Check for the existence of key $(docv). See section EXIT CODE." in
  Arg.(value & opt (some string) None & info ["e"; "exists"] ~docs ~doc ~docv)

let delete =
  let doc = "Output the input without the key $(docv) and its value." in
  Arg.(value & opt (some string) None & info ["d"; "delete"] ~docs ~doc ~docv)

let get =
  let doc = "Output the value of key $(docv) of the input." in
  Arg.(value & opt (some string) None & info ["g"; "get"] ~docs ~doc ~docv)

let set =
  let doc = "Output the input with the key $(docv) set to the value specified
             by $(b,--value)."
  in
  Arg.(value & opt (some string) None & info ["s"; "set"] ~docs ~doc ~docv)

let value =
  let doc = "s-expression $(docv) bound to the $(i,KEY) of $(b,--set).
             If unspecified a s-expression is read from standard input."
  in
  let docv = "SEXP" in
  Arg.(value & opt (some string) None & info ["v"; "value"] ~docs ~doc ~docv)

let map =
  let map allow_undef exists delete get set value =
    { allow_undef; exists; delete; get; set; value; }
  in
  Term.(const map $ allow_undef $ exists $ delete $ get $ set $ value)

(* Random generation parameters. *)

let rand_section = "RANDOM S-EXPRESSIONS OPTIONS"
let docs = rand_section

let rseed =
  let doc = "Random seed." in
  Arg.(value & opt (some nat_arg) None & info ["rseed"] ~docs ~doc ~docv:"INT")

let rcount =
  let doc = "Number of random s-expressions (atom or list) generated." in
  let c = 100 in
  Arg.(value & opt int c & info ["rcount"] ~docs ~doc ~docv:"INT")

let maxd =
  let doc = "Maximal depth of random s-expressions lists." in
  let d = 5 in
  Arg.(value & opt int d & info ["max-depth"] ~docs ~doc ~docv:"INT")

let maxl =
  let doc = "Maximal length of random s-expression lists." in
  let l = 20 in
  Arg.(value & opt int l & info ["max-list-len"] ~docs ~doc ~docv:"INT")

let maxa =
  let doc = "Maximal length of random s-expression atoms." in
  let l = 20 in
  Arg.(value & opt int l & info ["max-atom-len"] ~docs ~doc ~docv:"INT")

let rand =
  let rand seed count maxd maxl maxa =
    let seed = match seed with
    | None -> Random.self_init (); Random.int (1 lsl 30 - 1)
    | Some s -> s
    in
    { seed; count; maxd; maxl; maxa }
  in
  Term.(const rand $ rseed $ rcount $ maxd $ maxl $ maxa)

(* Command *)

let cmd =
  let doc = "Decode only, no encoding." in
  let dec = `Decode, Arg.info ["decode"] ~doc in
  let doc =
      strf "Output random s-expressions, no decoding. See section %s."
        rand_section
  in
  let rand = `Random, Arg.info ["random"] ~doc in
  Arg.(value & vflag `Trip [dec; rand])

let doc = "operate on sequences of s-expressions"
let exits =
  (Term.exit_info 1 ~doc:"the input was malformed") ::
  (Term.exit_info 2 ~doc:"the key does not exist, see the s-expression \
                          map options.") ::
  Term.default_exits

let man =
  [ `S "DESCRIPTION";
    `P "$(mname) is a command line tool to operate on sequences of
        s-expressions.";
    `P "Use '$(mname)' to decode and recode s-expressions."; `Noblank;
    `P "Use '$(mname) --get KEY' to get a value in a s-expression."; `Noblank;
    `P "Use '$(mname) --set KEY' to set a value in a s-expression."; `Noblank;
    `P "Use '$(mname) --random' to generate random s-expressions.";
    `S Manpage.s_options;
    `S input_section;
    `P "These options control the input process.";
    `S output_section;
    `P "These options control the output process.";
    `S map_section;
    `P "These options query or modify the input by interpreting
      s-expressions as key-value maps.";
    `S rand_section;
    `P "These options control the s-expressions randomly generated by
      the $(b,--random) option.";
    `S "S-EXPRESSION SYNTAX";
    `P "A more gentle introduction to s-expression is given
        in the API documentation available at:";
    `Pre "  %%PKG_DOC%%/Sexpm.html#syntax or $(b,odig doc sexpm)";
    `P "The following is the parsed syntax of s-expressions expressed
        as a RFC 5234 ABNF grammar:";
    `Pre "\
 sexp-seq = *(ws / comment / sexp)
     sexp = atom / list
     list = %x0028 sexp-seq %x0029
     atom = token / qtoken
    token = (t-char / escape) *(t-char / escape / cont)
   qtoken = %x0022 *(q-char / escape / cont) %x0022
   escape = %x005C (%x0020 / %x0022 / %x0028 / %x0029 / 0x003B / %x005C /
                    %x006E / %x0072 / %x0074 / %x0075 %x007B unum %x007D)
     unum = 1*6(HEXDIG)
     cont = %x005C nl
       ws = *(ws-char)
  comment = %x003B *(c-char) nl
       nl = %x000A / %x000D / %x000D %x000A
   t-char = %x0021 / %x0023-0027 / %x002A-%x003A / %x003C-005B /
            %x005D-D7FF / %xE000-10FFFF
   q-char = t-char / ws-char / %x0028 / %x0029 / %x003B
  ws-char = %x0020 / %x0009 / %x000A / %x000D
   c-char = %x0009 / %x0020-D7FF / %xE000-10FFFF
";
    `S "BUGS";
    `P "This program is distributed with the sexpm OCaml library.
        See %%HOMEPAGE%% for contact information." ]

let cmd =
  Term.(term_result (const do_cmd $ io $ map $ rand $ cmd)),
  Term.info "sexptrip" ~version:"%%VERSION%%" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
