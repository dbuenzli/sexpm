(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let log f = Format.printf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let pp_range ppf ((sc, sl), (ec, el)) =
  Format.fprintf ppf "(%d,%d),(%d,%d)" sc sl ec el

let pp_decode ppf = function
| Ok None -> Format.fprintf ppf "Ok None"
| Ok Some l -> Format.fprintf ppf "Ok (Some (%a))" Sexpm.pp_lexeme l
| Error (e, r) ->
    Format.fprintf ppf "Error (%a,(%a))" Sexpm.pp_error e pp_range r

let test_decode fnd exp = match fnd, exp with
| Ok _, Ok _
| Error _, Ok _
| Ok _, Error _ when fnd <> exp ->
    fail "found: %a expected: %a" pp_decode fnd pp_decode exp
| Error (e0, _), Error (e1, _) when e0 <> e1 ->
    fail "found: %a expected: %a" pp_decode fnd pp_decode exp
| _, _ -> ()

let test_dseq ?layout src seq =
  let d = Sexpm.(decoder ?layout (input_of_string src)) in
  let rec loop d = function
  | l :: ls -> test_decode (Sexpm.decode_lexeme d) l; loop d ls
  | [] ->
      match Sexpm.decode_lexeme d with
      | Ok None -> ()
      | d -> fail "decoder not at the end of stream (%a)" pp_decode d
  in
  loop d seq

let test_eseq ?nl ?quote ?style seq result =
  let b = Buffer.create 255 in
  let o = Sexpm.output_of_buffer b in
  let e = Sexpm.(encoder ?nl ?quote ?style o) in
  let rec loop = function
  | (Ok l) :: ls -> Sexpm.encode_lexeme e l; loop ls
  | (Error _) :: ls -> assert false
  | [] ->
      Sexpm.encode_lexeme e None;
      let r = Buffer.contents b in
      if r <> result then fail "encoded : %S\n\
                                expected: %S" r result
  in
  loop seq

let test_trip src =
  let b = Buffer.create 255 in
  let o = Sexpm.output_of_buffer b in
  let e = Sexpm.(encoder ~style:`Raw o ) in
  let d = Sexpm.(decoder ~layout:true (input_of_string src)) in
  let rec loop () = match Sexpm.decode_lexeme d with
  | Ok (Some _ as dec) -> Sexpm.encode_lexeme e dec; loop ()
  | Error _ -> assert false
  | Ok None ->
      Sexpm.encode_lexeme e None;
      let r = Buffer.contents b in
      if r <> src then fail "src : %S\n\
                             trip: %S" src r
  in
  loop ()

let ok v = Ok (Some v)
let w w = ok (Sexpm.White w)
let c c = ok (Sexpm.Comment c)
let a a r = ok (Sexpm.Atom (a, r))
let ls = ok Sexpm.Ls
let le = ok Sexpm.Le
let e err = Error (err, ((0,0),(0,0)))

let decode_whites () =
  log "Decoder whites.\n";
  let test_white s =
    test_dseq ~layout:false s [];
    test_dseq ~layout:true s [ w s ];
    test_dseq ~layout:false (strf "%sbu" s) [a "bu" None];
    test_dseq ~layout:false (strf "bu%s" s) [a "bu" None];
    test_dseq ~layout:true (strf "%sbu" s) [w s; a "bu" (Some "bu")];
    test_dseq ~layout:true (strf "bu%s" s) [a "bu" (Some "bu"); w s];
    test_dseq ~layout:false (strf "(%s)" s) [ls; le];
    test_dseq ~layout:true (strf "(%s)" s) [ls; w s; le];
    test_dseq ~layout:true (strf "(%s)%s" s s) [ls; w s; le; w s]
  in
  test_white " ";
  test_white "\t";
  test_white "\n";
  test_white "\r";
  test_white "\r\n";
  test_white "  \n \t\t";
  test_white " \n  ";
  test_white " \n  ";
  ()

let decode_comments () =
  log "Decoder comments.\n";
  let test_comment ?(context = true) s =
    let cs = String.sub s 1 (String.length s - 1) in
    test_dseq ~layout:false s [];
    test_dseq ~layout:true s [ c cs ];
    test_dseq ~layout:true (strf " %s\n " s) [ w " "; c cs; w "\n " ];
    test_dseq ~layout:true (strf " %s\r\n" s) [ w " "; c cs; w "\r\n" ];
    test_dseq ~layout:true (strf " %s" s) [ w " "; c cs ];
    test_dseq ~layout:true (strf " a%s" s) [ w " "; a "a" (Some "a"); c cs ];
    test_dseq ~layout:true (strf " a\\\n%s" s)
      [ w " "; a "a" (Some "a\\\n"); c cs ];
    test_dseq ~layout:true (strf " a\\\n%s\nb" s)
      [ w " "; a "a" (Some "a\\\n"); c cs; w "\n"; a "b" (Some "b") ];
    test_dseq ~layout:true (strf "(a%s\nb)" s)
      [ ls; a "a" (Some "a"); c cs; w "\n"; a "b" (Some "b"); le ];
  in
  test_dseq ~layout:true ";" [ c "" ];
  test_comment "; adfj";
  test_comment ";; adfj";
  test_comment "; adfj \" ; ()";
  test_comment ";; adfj \" ; ()";
  test_comment ";; adfj \\n \" ; ()";
  test_comment ";; adfj \\u{} \" ; ()";
  test_comment ";; adfj \\u{23422342324} \" ; ()";
  ()

let decode_atoms () =
  log "Decoder atoms.\n";
  let test_atom s at =
    test_dseq ~layout:false s [ a at None ];
    test_dseq ~layout:true s [ a at (Some s) ];
    test_dseq ~layout:true (strf "\n%s;b" s) [ w "\n"; a at (Some s); c "b" ];
    test_dseq ~layout:false (strf "\n%s;b" s) [ a at None ];
    test_dseq ~layout:true (strf "()%s()" s) [ ls; le; a at (Some s); ls; le ];
  in
  test_dseq ~layout:true "bla\\" [ a "bla" (Some "bla\\")];
  test_dseq ~layout:true "bla\\\r\n\\" [ a "bla" (Some "bla\\\r\n\\") ];
  test_dseq ~layout:true "bla\"bla\""
    [ a "bla" (Some "bla");a "bla" (Some "\"bla\"") ];
  test_atom "\"\"" "";
  test_atom "bla" "bla";
  test_atom "b\\\nla" "bla";
  test_atom "b\\\n  \tla" "bla";
  test_atom "b\\nla" "b\nla";
  test_atom "\"bla\"" "bla";
  test_atom "\"b la\"" "b la";
  test_atom "\"b\\\n la\"" "bla";
  test_atom "\"b\\\n la\"" "bla";
  test_atom "\"b\\nla\"" "b\nla";
  test_atom "\"b\\\n\\nla\"" "b\nla";
  test_atom "\\ " " ";
  test_atom "\\\"" "\"";
  test_atom "\\(" "(";
  test_atom "\\)" ")";
  test_atom "\\;" ";";
  test_atom "\\\\" "\\";
  test_atom "\\n" "\n";
  test_atom "\\r" "\r";
  test_atom "\\t" "\t";
  test_atom "\\ \\\n\\ \\\n\\n" "  \n";
  test_atom "\\u{0000}" "\x00";
  test_atom "\\u{0001}" "\x01";
  test_atom "\\u{D7FF}" "\xED\x9F\xBF";
  test_atom "\\u{E000}" "\xEE\x80\x80";
  test_atom "\\u{1F42B}" "\xF0\x9F\x90\xAB";
  test_atom "\\u{10FFFF}" "\xF4\x8F\xBF\xBF";
  ()

let decode_lists () =
  log "Decoder lists.\n";
  test_dseq ~layout:true "()()(a)" [ls; le; ls; le; ls; a "a" (Some "a"); le];
  test_dseq ~layout:true "()a()" [ls; le; a "a" (Some "a"); ls; le];
  test_dseq ~layout:true "(((a)))" [ls; ls; ls; a "a" (Some "a"); le; le; le];
  test_dseq ~layout:true "(())()" [ls; ls; le; le; ls; le];
  ()

let u_rep_utf8 = "\xEF\xBF\xBD"
let decode_errors () =
  log "Decoder errors.\n";
  test_dseq ~layout:false "\xFF"
    [e (`Illegal_bytes "\xFF"); a u_rep_utf8 None];
  test_dseq ~layout:false "a\xFFb"
    [e (`Illegal_bytes "\xFF"); a (strf "a%sb" u_rep_utf8) None ];
  test_dseq ~layout:false "a\x00b"
    [e (`Illegal_uchar (Uchar.of_int 0x0000)); a (strf "a%sb" u_rep_utf8) None];
  test_dseq ~layout:false "a \\"
    [a "a" None; e (`Unclosed `Escape); a "" None];
  test_dseq ~layout:false "a\\u"
    [e (`Unclosed `Escape); a "a" None ];
  test_dseq ~layout:false "\\A"
    [e (`Illegal_escape (`Not_esc (Uchar.of_int 0x41)));
       a u_rep_utf8 None ];
  test_dseq ~layout:false "\\\n"
    [e (`Illegal_escape (`Not_esc (Uchar.of_int 0x0A)));
     a u_rep_utf8 None ];
  test_dseq ~layout:false "\\uA"
    [e (`Illegal_escape (`Exp_lbrace (Uchar.of_int 0x41)));
     a u_rep_utf8 None ];
  test_dseq ~layout:false "\\u{}"
    [e (`Illegal_escape (`Exp_hex (Uchar.of_int 0x7D)));
     a u_rep_utf8 None ];
  test_dseq ~layout:false "\\u{+"
    [e (`Illegal_escape (`Exp_hex (Uchar.of_int 0x2B)));
     a u_rep_utf8 None ];
  test_dseq ~layout:false "\\u{A+}"
    [e (`Illegal_escape (`Exp_hex_rbrace (Uchar.of_int 0x2B)));
     a (strf "%s}" u_rep_utf8) None ];
  test_dseq ~layout:false "\\u{1234567}"
    [e (`Illegal_escape (`Exp_rbrace (Uchar.of_int 0x37)));
     a (strf "%s}" u_rep_utf8) None ];
  test_dseq ~layout:false "\\u{110000}"
    [e (`Illegal_escape (`Not_uchar 0x110000)); a u_rep_utf8 None ];
  test_dseq ~layout:false "\\u{D800}"
    [e (`Illegal_escape (`Not_uchar 0xD800)); a u_rep_utf8 None ];
  test_dseq ~layout:false "\\u{DFFF}"
    [e (`Illegal_escape (`Not_uchar 0xDFFF)); a u_rep_utf8 None ];
  test_dseq ~layout:false "())()"
    [ls; le; e (`Unexpected_le); ls; le ];
  test_dseq ~layout:false "()("
    [ls; le; ls; e (`Unclosed `List); le ];
  test_dseq ~layout:false "()((\""
    [ls; le; ls; ls; e (`Unclosed `Quoted_token); a "" None;
     e (`Unclosed `List); le; e (`Unclosed `List); le; ];
  test_dseq ~layout:false "(\"\\u{0000"
    [ls; e (`Unclosed `Escape); e (`Unclosed `Quoted_token); a "" None;
     e (`Unclosed `List); le; ];
  ()

let encode_raw () =
  log "Raw encoder.\n";
  let test_eseq ?quote = test_eseq ?quote ~style:`Raw in
  test_eseq ~quote:true [ a "" None; a "bla" None] "\"\"bla";
  test_eseq ~quote:false [ a "" None; a "bla" None] "\"\"bla";
  test_eseq ~quote:true [ a "" None; a "b la" None] "\"\"\"b la\"";
  test_eseq ~quote:false [ a "" None; a "b la" None] "\"\"b\\ la";
  test_eseq [ a "bla" None; a "bla" None] "bla bla";
  test_eseq ~quote:true [ a "b la" None; a "bla" None] "\"b la\"bla";
  test_eseq ~quote:true [ a "bla" None; a "b la" None] "bla\"b la\"";
  test_eseq ~quote:true [ a "b la" None; a "b la" None] "\"b la\"\"b la\"";
  test_eseq ~quote:false [ a "b la" None; a "bla" None] "b\\ la bla";
  test_eseq ~quote:false [ a "b la" None; a "b la" None] "b\\ la b\\ la";
  test_eseq [ a "bla" None; c ";"; a "bla" None] "bla;;\nbla";
  test_eseq [ a "bla" None; c ";" ] "bla;;";
  test_eseq [ c ""; a "bla" None; ] ";\nbla";
  test_eseq [ c ""; ls; le] ";\n()";
  test_eseq [ ls; c ""; le] "(;\n)";
  test_eseq [ ls; le; c ""] "();";
  test_eseq [ ls; c "a"; w "\r\n"; le ] "(;a\r\n)";
  test_eseq [ ls; c "a"; w "\n\r"; le ] "(;a\n\r)";
  ()

let encode_min () =
  log "Min encoder.\n";
  let test_eseq ?quote = test_eseq ?quote ~style:`Minify in
  test_eseq [ a "bla" None; a "bla" None] "bla bla";
  test_eseq [ a "bla" None; a "bla" None; w " "; c "bla"] "bla bla";
  test_eseq [ ls; le; w " "; ls; c "bla"; a "a" None; le; a "a" None] "()(a)a";
  test_eseq [ a "" None; a "bla" None] "\"\"bla";
  ()

let trip () =
  log "Trips.\n";
  test_trip "\\u{10FFFF}";
  test_trip "()\n\r   \\u{10FFFF}";
  test_trip "(  () () a a \" a   \" \n (bla ))";
  ()

let escape () =
  log "Sexpm.escape.\n";
  let escape s qesc esc =
    let qesc' = Sexpm.escape ~quote:true s in
    let esc' = Sexpm.escape ~quote:false s in
    if qesc' <> qesc then fail "found: %S exp: %S" qesc' qesc;
    if esc' <> esc then fail "found: %S exp: %S" esc' esc
  in
  escape "" "\"\"" "\"\"";
  escape " " "\" \"" "\\ ";
  escape ";" "\";\"" "\\;";
  escape "(" "\"(\"" "\\(";
  escape ")" "\")\"" "\\)";
  for i = 0x0000 to 0x001F do match i with
  | 0x0009 -> escape "\t" "\"\\t\"" "\\t"
  | 0x000A -> escape "\n" "\"\\n\"" "\\n"
  | 0x000D -> escape "\r" "\"\\r\"" "\\r"
  | u ->
      escape (strf "%c" @@ Char.chr u)
        (strf "\"\\u{%02X}\"" u) (strf "\\u{%02X}" u)
  done;
  escape "\x00hey ho ; () bla \n"
    "\"\\u{00}hey ho ; () bla \\n\""
    "\\u{00}hey\\ ho\\ \\;\\ \\(\\)\\ bla\\ \\n";
  ()

let test () =
  decode_whites ();
  decode_comments ();
  decode_atoms ();
  decode_lists ();
  decode_errors ();
  encode_raw ();
  encode_min ();
  trip ();
  escape ();
  log "All tests suceeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

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
