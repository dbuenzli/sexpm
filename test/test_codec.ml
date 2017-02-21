(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf
let log f = Format.printf (f ^^ "@.")
let fail fmt =
  let fail ppf = Format.fprintf ppf "@."; failwith "test" in
  Format.kfprintf fail Format.err_formatter fmt

let trip c src =
  let b = Buffer.create 255 in
  let o = Sexpm.output_of_buffer b in
  let e = Sexpm.(encoder ~style:`Raw o ) in
  let d = Sexpm.(decoder ~layout:true (input_of_string src)) in
  let rec loop () = match Sexpm.decode_traced_value c d with
  | Error (e, _) -> fail "%a" Sexpm.pp_error e
  | Ok (Some v, t) ->
      Sexpm.encode_traced_value c e (Some (v, v), t); loop ()
  | Ok (None, t) ->
      Sexpm.encode_traced_value c e (None, t);
      let r = Buffer.contents b in
      if r <> src then fail "src : %S@\n\
                             trip: %S" src r
  in
  loop ()

let test_trip () =
  log "Testing trips.";
  trip Sexpm.unit ";hu\nunit ; ha\nunit";
  trip Sexpm.bool "true \"false\" ; hey\n true ; blue";
  trip Sexpm.int  "1 \"2\" ; hey\n  \"0b11\" ; blue";
  trip Sexpm.(t2 int bool) "(2 false)\n( 3 true   )(\"4\" false)\n;ha\n  ";
  let r = Sexpm.(result ~ok:int ~error:(t2 int bool)) in
  trip r " (\\u{4f}k 3) (Error ;hey\n (2 true);ho\n)";
  let l = Sexpm.(list (t2 int int)) in
  trip l " ((3 2) (6 3) ; bla blo blu ()\n (4 2)) ()";
  ()

let upd c src o n =
  let dst = Printf.sprintf src n in
  let src = Printf.sprintf src o in
  let b = Buffer.create 255 in
  let o = Sexpm.output_of_buffer b in
  let e = Sexpm.(encoder ~style:`Raw o) in
  let deco = Sexpm.(decoder ~layout:true (input_of_string src)) in
  let decn = Sexpm.(decoder ~layout:true (input_of_string dst)) in
  let rec loop () =
    match Sexpm.decode_traced_value c deco,
          Sexpm.decode_traced_value c decn with
  | Ok (Some o, t), Ok (Some n, _) ->
      Sexpm.encode_traced_value c e (Some (o, n), t); loop ()
  | Ok (None, t), Ok (None, _) ->
      Sexpm.encode_traced_value c e (None, t);
      let r = Buffer.contents b in
      if r <> dst then fail "dst : %S@\n\
                             updt: %S" dst r
  | Error (e, _), _ -> fail "%a" Sexpm.pp_error e
  | _, Error (e, _) -> fail "%a" Sexpm.pp_error e
  | _ -> fail "fail"
  in
  loop ();
  ()

let test_update () =
  log "Testing update.";
  upd Sexpm.bool " %s; hehyo" "true" "false";
  upd Sexpm.bool " %s; hehyo" "false" "true";
  let r = Sexpm.(result ~ok:int ~error:(t2 int bool)) in
  upd r " (\\u{4f}k %s)" "2" "3";
  upd r " (\\u{4f}k %s) (Error ;hey\n (2 ;bla\n true);ho\n)" "2" "3";
  upd r " (\\u{4f}k 1) (Error ;hey\n (%s ;bla\n true);ho\n)" "2" "3";
  upd r " (\\u{4f}k 1) (Error ;hey\n (2 ;bla\n %s);ho\n)" "false" "true";
  upd r " (\\u{4f}k 1) ;heyho\n  %s;hu\n  "
    "(Error ;hey\n (2 true);bla\n)" "(Ok 3)";
  let l = Sexpm.(list (t2 int int)) in
  upd l " (%s) " "(2 ;a\n 3) (4 3)" "(2 ;a\n 4) (5 6)";
  upd l " (%s) " "(2 ;a\n 3) (4 3) (3 2)  ;bla\n" "(2 ;a\n 4) (5 6)   ;bla\n";
  upd l " (%s) " "(2 ;a\n 3) (4 3) ;bla\n(3 2)" "(2 ;a\n 4) (5 6) ;bla\n";
  upd l " (%s) "
    "(2 ;a\n 3) ;bla\n(3 2);hey\n" "(2 ;a\n 4) ;bla\n(5 6);hey\n(3 2)";
  ()

let test () =
  test_trip ();
  test_update ();
  log "All tests suceeded.\n"

let () = if not (!Sys.interactive) then test ()

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
