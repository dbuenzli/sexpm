(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let pf = Format.fprintf

(* Unsafe string and bytes manipulations. If you don't believe the authors's
   invariants, replacing with safe versions makes everything safe in the
   module. He won't be upset. *)

let unsafe_string_get = String.unsafe_get
let unsafe_array_get = Array.unsafe_get
let unsafe_bytes_set = Bytes.unsafe_set
let unsafe_bytes_set_byte s j byte = Bytes.unsafe_set s j (Char.unsafe_chr byte)
let unsafe_bytes_blit s soff d doff =
  Bytes.unsafe_blit (Bytes.unsafe_of_string s) soff d doff

(* Invalid argument strings *)

let err_empty_buf = "buffer size can't be 0"
let err_enc_unclosed = "unclosed list encoded"
let err_enc_unopened = "closing unopened list"

(* IO *)

let io_buffer_size = 65536                          (* IO_BUFFER_SIZE 4.0.0 *)
let bytes_buf = function
| None -> Bytes.create io_buffer_size
| Some bytes ->
    if Bytes.length bytes <> 0 then bytes else
    invalid_arg err_empty_buf

type input = unit -> (Bytes.t * int * int) option

let input_of_string s =
  let buf = ref (Some (Bytes.unsafe_of_string s, 0, String.length s)) in
  fun () -> let r = !buf in buf := None; r

let input_of_in_channel ?bytes ic =
  let bs = bytes_buf bytes in
  fun () ->
    let len = input ic bs 0 (Bytes.length bs) in
    if len = 0 then None else Some (bs, 0, len)

type output = (Bytes.t * int * int) option -> unit

let output_of_buffer buf = function
| None -> ()
| Some (bs, pos, len) -> Buffer.add_subbytes buf bs pos len

let output_of_out_channel oc = function
| None -> ()
| Some (bs, pos, len) -> output oc bs pos len

(* Characters and their classes *)

let ux_eoi = -1 (* end of input, oustide unicode range *)
let ux_soi = -2 (* start of input, outside unicode range. *)

let u_rep  = Uchar.to_int Uutf.u_rep
let u_tab    = 0x0009 (* '\t' *)
let u_lf     = 0x000A (* '\n' *)
let u_cr     = 0x000D (* '\r' *)
let u_sp     = 0x0020 (* ' ' *)
let u_quot   = 0x0022 (* '"' *)
let u_lpar   = 0x0028 (* '(' *)
let u_rpar   = 0x0029 (* ')' *)
let u_semi   = 0x003B (* ';' *)
let u_bslash = 0x005C (* '\\' *)

let uchar_is_illegal = function
| 0x0020 | 0x000A | 0x000D | 0x0009 -> false
| u when u <= 0x001F -> true
| _ -> false

(* The following predicates assume that illegal were filtered out. *)

let uchar_is_white = function
| 0x0020 | 0x000A | 0x000D | 0x0009 -> true
| _ -> false

let uchar_is_hex u =
   (0x0030 <= u && u <= 0x0039)
|| (0x0041 <= u && u <= 0x0046)
|| (0x0061 <= u && u <= 0x0066)

let uchar_is_token u =
   (0x0023 <= u && u <= 0x0027)
|| (0x002A <= u && u <= 0x003A)
|| (0x003C <= u && u <= 0x005B)
|| (0x005D <= u)
|| (0x0021 = u)

let uchar_is_qtoken u =
   (0x0023 <= u && u <= 0x005B)
|| (0x005D <= u)
|| uchar_is_white u
|| (0x0021 = u)

let uchar_is_comment = function
| 0x000A | 0x000D -> false
| u when u = ux_eoi -> false
| u -> true

let pp_dump_uchar ppf u = pf ppf "%04X" (Uchar.to_int u)
let pp_uchar ppf u =
  if Uchar.to_int u <= 0x1F (* most control chars *) then pp_dump_uchar ppf u else
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b u;
  pf ppf "'%s' (%a)" (Buffer.contents b) pp_dump_uchar u

(* Lexemes *)

type lexeme =
| White of string
| Comment of string
| Ls | Le
| Atom of string * string option

let pp_raw ppf = function
| None -> pf ppf "None"
| Some s -> pf ppf "@[Some %S@]" s

let pp_lexeme ppf = function
| White s -> pf ppf "@[White %S@]" s
| Comment s -> pf ppf "@[Comment %S@]" s
| Ls -> pf ppf "Ls"
| Le -> pf ppf "Le"
| Atom (a, raw) -> pf ppf "@[Atom @[<1>(%S,@ %a)@]@]" a pp_raw raw

(* Positions *)

type pos = int * int
type range = pos * pos
let pp_range ppf ((l0, c0), (l1, c1)) = pf ppf "%d.%d-%d.%d" l0 c0 l1 c1

(* Decode errors *)

type error =
[ `Illegal_bytes of string
| `Illegal_uchar of Uchar.t
| `Illegal_escape of
    [ `Exp_lbrace of Uchar.t
    | `Exp_rbrace of Uchar.t
    | `Exp_hex of Uchar.t
    | `Exp_hex_rbrace of Uchar.t
    | `Not_esc of Uchar.t
    | `Not_uchar of int ]
| `Unclosed of [ `Quoted_token | `List | `Escape ]
| `Unexpected_le
| `Codec of string (* FIXME *) ]

let pp_illegal_escape ppf = function
| `Exp_lbrace u -> pf ppf "expected '{' found %a" pp_uchar u
| `Exp_rbrace u -> pf ppf "expected '}' found %a" pp_uchar u
| `Exp_hex u -> pf ppf "expected hex digit found %a" pp_uchar u
| `Exp_hex_rbrace u -> pf ppf "expected hex digit or '}' found %a" pp_uchar u
| `Not_esc u -> pf ppf "%a is not a valid escape letter" pp_uchar u
| `Not_uchar i -> pf ppf "escape 0x%X is not a Unicode scalar value" i

let pp_error ppf = function
| `Illegal_bytes b -> pf ppf "illegal byte sequence %S in input" b
| `Illegal_uchar u -> pf ppf "illegal Unicode character %a in input" pp_uchar u
| `Illegal_escape esc -> pp_illegal_escape ppf esc
| `Unclosed `Quoted_token -> pf ppf "unclosed quoted token"
| `Unclosed `List -> pf ppf "unclosed list"
| `Unclosed `Escape -> pf ppf "unfinished escape"
| `Unexpected_le -> pf ppf "unexpected list end ')'"
| `Codec s -> pf ppf "codec error: %s" s

exception Err of error * range (* only used internally *)

(* Decoder *)

type decoder_state =
| Decode_next
| Decoding_qtoken
| Decoding_token
| Decoding_comment
| Decoding_white
| Close_qtoken
| Close_list

type decoder =
  { input : input; (* input byte stream. *)
    layout : bool; (* [true] to keep whitespace and raw tokens. *)
    u : Uutf.decoder; (* unicode character decoder. *)
    atom : Buffer.t; (* atom accumulation buffer. *)
    raw : Buffer.t; (* raw accumulation buffer. *)
    mutable c : int; (* character lookahead. *)
    mutable ss_line : int; (* last sexp start line. *)
    mutable ss_col : int; (* last sexp start column. *)
    mutable s_line : int; (* last saved start line. *)
    mutable s_col : int; (* last saved start column. *)
    mutable e_line : int; (* last saved end line. *)
    mutable e_col : int; (* last saved end column. *)
    mutable stack : pos list; (* stack of open lists and their start pos. *)
    mutable state : decoder_state; (* decoder state. *)
    mutable peek : (lexeme option, error * range) result option
 }

let decoder ?(layout = true) input =
  let u = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let buf_size = 1024 in
  let atom = Buffer.create buf_size in
  let raw = Buffer.create buf_size in
  { input; layout; u; atom; raw; c = ux_soi; (* overwritten *)
    ss_line = 1; ss_col = 0; s_line = 1; s_col = 0; e_line = 1; e_col = 0;
    stack = []; state = Decode_next; peek = None }

let decoder_layout d = d.layout
let decoder_sspos d = d.ss_line, d.ss_col
let decoder_spos d = d.s_line, d.s_col
let decoder_epos d = d.e_line, d.e_col
let decoded_range d = (decoder_spos d, decoder_epos d)
let decoded_sexp_range d = (decoder_sspos d, decoder_epos d)

let sspos d = Uutf.(d.ss_line <- decoder_line d.u; d.ss_col <- decoder_col d.u)
let spos d = Uutf.(d.s_line <- decoder_line d.u; d.s_col <- decoder_col d.u)
let epos d = Uutf.(d.e_line <- decoder_line d.u; d.e_col <- decoder_col d.u)

let buffer_flush b = let s = Buffer.contents b in (Buffer.clear b; s)
let buffer_add b c = Uutf.Buffer.add_utf_8 b (Uchar.unsafe_of_int c)

let raw_clear d = Buffer.clear d.raw
let raw_add d = if d.c <> ux_soi && d.layout then buffer_add d.raw d.c else ()
let raw d = buffer_flush d.raw
let raw_is_empty d = Buffer.length d.raw = 0

let atom_add d c = buffer_add d.atom c
let atom d =
  let raw = if d.layout then (Some (buffer_flush d.raw)) else None in
  Atom (buffer_flush d.atom, raw)

let err d e = epos d; raise_notrace (Err (e, decoded_range d))

let refill d = match d.input () with
| Some (b, s, l) -> Uutf.Manual.src d.u b s l
| None -> Uutf.Manual.src d.u Bytes.empty 0 0

let rec readc d = match Uutf.decode d.u with
| `Uchar u ->
    raw_add d;
    let i = Uchar.to_int u in
    begin match uchar_is_illegal i with
    | false -> d.c <- i
    | true -> d.c <- u_rep; err d (`Illegal_uchar u)
    end
| `End -> if d.c <> ux_eoi then (raw_add d; d.c <- ux_eoi) else ()
| `Await -> refill d; readc d
| `Malformed b -> d.c <- u_rep; err d (`Illegal_bytes b)

let err_illegal_esc d e = atom_add d u_rep; readc d; err d (`Illegal_escape e)

let d_uchar_esc d =
  let rec loop d acc count = match (readc d; d.c) with
  | u when uchar_is_hex u ->
      let count = count + 1 in
      if count > 6 then err_illegal_esc d (`Exp_rbrace (Uchar.of_int d.c)) else
      let acc = acc * 16 + (if d.c <= 0x39 (* 9 *) then d.c - 0x30 else
                            if d.c <= 0x46 (* F *) then d.c - 0x37 else
                            d.c - 0x57)
      in
      loop d acc count
  | 0x007D (* '}' *) ->
      if count = 0 then err_illegal_esc d (`Exp_hex (Uchar.of_int d.c)) else
      if not (Uchar.is_valid acc) then err_illegal_esc d (`Not_uchar acc) else
      (readc d; atom_add d acc)
  | u when u = ux_eoi -> err d (`Unclosed `Escape)
  | u ->
      if count = 0 then err_illegal_esc d (`Exp_hex (Uchar.of_int d.c)) else
      err_illegal_esc d (`Exp_hex_rbrace (Uchar.of_int d.c))
  in
  match d.c with
  | 0x007B (* '{' *) -> loop d 0 0
  | u when u = ux_eoi -> err d (`Unclosed `Escape)
  | u -> err_illegal_esc d (`Exp_lbrace (Uchar.of_int d.c))

let rec d_cont ~lf d = match (readc d; d.c) with
| 0x000A when lf -> d_cont ~lf:false d
| (0x0020 | 0x0009) -> d_cont ~lf:false d
| 0x005C (* '\\' *) -> d_escape ~or_cont:true d
| u -> ()

and d_escape ~or_cont d = match (readc d; d.c) with
| 0x0020 (* ' ' *)  -> atom_add d u_sp; readc d
| 0x0022 (* '"' *)  -> atom_add d u_quot; readc d
| 0x0028 (* '(' *)  -> atom_add d u_lpar; readc d
| 0x0029 (* ')' *)  -> atom_add d u_rpar; readc d
| 0x003B (* ';' *)  -> atom_add d u_semi; readc d
| 0x005C (* '\\' *) -> atom_add d u_bslash; readc d
| 0x006E (* 'n' *)  -> atom_add d u_lf; readc d
| 0x0072 (* 'r' *)  -> atom_add d u_cr; readc d
| 0x0074 (* 't' *)  -> atom_add d u_tab; readc d
| 0x0075 (* 'u' *)  -> readc d; d_uchar_esc d
| (0x000A | 0x000D) ->
    if or_cont then (d_cont ~lf:(d.c = u_cr) d) else
    err_illegal_esc d (`Not_esc (Uchar.of_int d.c))
| u when u = ux_eoi -> if not or_cont then err d (`Unclosed `Escape)
| u -> err_illegal_esc d (`Not_esc (Uchar.of_int u))

let d_token d =
  let rec loop ~or_cont d = match d.c with
  | 0x005C (* \\ *) -> d_escape ~or_cont d; loop ~or_cont:true d
  | u when uchar_is_token u ->
      atom_add d u; epos d; readc d; loop ~or_cont:true d
  | u ->
      let a = atom d in
      d.state <- Decode_next; raw_clear d; Some a
  in
  if (d.state <> Decoding_token) then (sspos d; spos d);
  d.state <- Decoding_token;
  loop ~or_cont:false d

let d_qtoken d =
  let rec loop d u = match u with
  | 0x005C (* '\\' *) -> d_escape ~or_cont:true d; loop d d.c
  | 0x0022 (* '\"' *) ->
      epos d; readc d;
      let a = atom d in
      d.state <- Decode_next; raw_clear d; Some a
  | u when u = ux_eoi ->
      d.state <- Close_qtoken; err d (`Unclosed `Quoted_token)
  | u -> atom_add d u; readc d; loop d d.c
  in
  if d.state = Close_qtoken then (d.state <- Decoding_qtoken; loop d u_quot)
  else begin
    if d.state <> Decoding_qtoken then (sspos d; spos d; readc d);
    d.state <- Decoding_qtoken; loop d d.c
  end

let d_ls d =
  d.state <- Decode_next;
  spos d; epos d; readc d; raw_clear d;
  d.stack <- (decoder_spos d) :: d.stack;
  Some Ls

let d_le d =
  d.state <- Decode_next;
  spos d; epos d; readc d; raw_clear d;
  match d.stack with
  | (l, c) :: stack -> d.ss_line <- l; d.ss_col <- c; d.stack <- stack; Some Le
  | [] -> err d (`Unexpected_le)

let d_eoi d = match d.stack with
| [] -> None
| (l, c) :: stack ->
    d.s_line <- l; d.s_col <- c; d.state <- Close_list;
    err d (`Unclosed `List)

let rec d_comment d =
  let rec loop d = match d.c with
  | u when uchar_is_comment u -> epos d; readc d; loop d
  | u ->
      d.state <- Decode_next;
      if not d.layout then d_next d else Some (Comment (raw d))
  in
  if d.state <> Decoding_comment then (spos d; readc d; raw_clear d);
  d.state <- Decoding_comment;
  loop d

and d_white d =
  let rec loop d = match (readc d; d.c) with
  | u when uchar_is_white u -> epos d; loop d
  | u ->
      d.state <- Decode_next;
      if raw_is_empty d then d_next d else Some (White (raw d))
  in
  if d.state <> Decoding_white then (spos d; epos d);
  d.state <- Decoding_white;
  loop d

and d_next d = match d.c with
| 0x0028 (* '(' *) -> d_ls d
| 0x0029 (* ')' *) -> d_le d
| 0x0022 (* '"' *) -> d_qtoken d
| 0x003B (* ';' *) -> d_comment d
| u when uchar_is_white u -> d_white d
| u when u = ux_eoi -> d_eoi d
| u when u = ux_soi -> readc d; d_next d
| u -> d_token d

and d_lexeme d = match d.state with
| Decode_next -> d_next d
(* The following cases occur after an error was decoded. *)
| Decoding_token -> d_token d
| Decoding_qtoken -> d_qtoken d
| Decoding_comment -> d_comment d
| Decoding_white -> d_white d
| Close_qtoken -> d_qtoken d
| Close_list -> d_le d

let decode_lexeme d = match d.peek with
| Some dec -> d.peek <- None; dec
| None ->
    try Ok (d_lexeme d) with
    | Err (e, r) -> Error (e, r)

let peek_lexeme d = match d.peek with
| Some dec -> dec
| None ->
    let dec = decode_lexeme d in
    d.peek <- Some dec;
    dec

let dec_lexeme d = match d.peek with
| None -> d_lexeme d
| Some dec ->
    d.peek <- None;
    match dec with
    | Ok l -> l
    | Error (e, r) -> raise_notrace (Err (e, r))

let pek_lexeme d = match d.peek with
| None ->
    let dec = d_lexeme d in
    d.peek <- Some (Ok dec);
    dec
| Some dec ->
    match dec with
    | Ok l -> l
    | Error (e, r) -> raise_notrace (Err (e, r))

(* Encoder *)

let hex_digit =
  [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F'|]

type encoder_style = [ `Pp | `Minify | `Raw ]
type last_encoded = Comment | Token (* unquoted *) | Other
type encoder =
  { output : output; (* output byte stream. *)
    style : encoder_style; (* encoder style. *)
    quote : bool; (* quote atoms that need escaping. *)
    nl : bool; (* final line feed. *)
    o : Bytes.t; (* output buffer. *)
    mutable o_pos : int; (* next output pos to write. *)
    mutable nest : int; (* nesting level *)
    mutable last : last_encoded;
    mutable col : int; (* approx we don't decode the UTF-8 *) }

let encoder ?bytes ?(nl = false) ?(quote = true) ?(style = `Raw) output =
  let o = bytes_buf bytes in
  { output; style; quote; nl; o; o_pos = 0; nest = 0; last = Other; col = 0; }

let flush e = e.output (Some (e.o, 0, e.o_pos)); e.o_pos <- 0

let o_rem e = Bytes.length e.o - e.o_pos    (* rem bytes to write in [e.o]. *)

let rec o_byte e b = match o_rem e = 0 with
| true -> flush e; o_byte e b
| false -> unsafe_bytes_set_byte e.o e.o_pos b; e.o_pos <- e.o_pos + 1

let rec o_string e s pos l =    (* write [l] bytes of [s] starting at [pos]. *)
  let rem = o_rem e in
  match rem >= l with
  | true -> unsafe_bytes_blit s pos e.o e.o_pos l; e.o_pos <- e.o_pos + l
  | false ->
      unsafe_bytes_blit s pos e.o e.o_pos rem; e.o_pos <- e.o_pos + rem;
      flush e;
      o_string e s (pos + rem) (l - rem)

let rec e_atom e a =
  if a = "" then (o_string e "\"\"" 0 2; e.last <- Other) else
  let flush ~quoted a spos len =
    if spos = 0 then begin match quoted with
    | true -> o_byte e u_quot
    | false -> if e.last = Token then o_byte e u_sp
    end;
    o_string e a spos len
  in
  let rec loop ~quoted a spos epos max = match epos > max with
  | true ->
      if not (spos > max) then (flush ~quoted a spos (epos - spos));
      if quoted then (o_byte e u_quot; e.last <- Other) else e.last <- Token
  | false ->
      let next = epos + 1 in
      let escape esc =
        flush ~quoted:e.quote a spos (epos - spos);
        o_string e esc 0 (String.length esc);
        loop e.quote a next next max
      in
      let may_escape esc = match e.quote with
      | true -> loop ~quoted:e.quote a spos next max
      | false -> escape esc
      in
      match unsafe_string_get a epos with
      | ' ' -> may_escape "\\ "
      | '(' -> may_escape "\\("
      | ')' -> may_escape "\\)"
      | ';' -> may_escape "\\;"
      | '\\' -> escape "\\\\"
      | '"'  -> escape "\\\""
      | '\t' -> escape "\\t"
      | '\n' -> escape "\\n"
      | '\r' -> escape "\\r"
      | c when c <= '\x1F' ->
          flush ~quoted:e.quote a spos (epos - spos);
          let byte = Char.code c in
          let hi = byte / 16 in
          let lo = byte mod 16 in
          o_string e "\\u00" 0 4;
          o_byte e (Char.code @@ unsafe_array_get hex_digit hi);
          o_byte e (Char.code @@ unsafe_array_get hex_digit lo);
          loop e.quote a next next max
      | c -> loop ~quoted a spos next max
  in
  loop ~quoted:false a 0 0 (String.length a - 1)

let e_raw_atom e r =
  begin match r.[0] = '"' (* raw is quoted *) with
  | true ->  e.last <- Other
  | false -> if e.last = Token then o_byte e u_sp;
  end;
  o_string e r 0 (String.length r)

let e_ls e =
  if e.last = Comment then o_byte e u_lf;
  o_byte e u_lpar; e.nest <- e.nest + 1; e.last <- Other

let e_le e =
  if e.nest = 0 then invalid_arg err_enc_unopened;
  if e.last = Comment then o_byte e u_lf;
  o_byte e u_rpar; e.nest <- e.nest - 1; e.last <- Other

let e_white e w =
  let len = String.length w in
  if e.style <> `Raw || len = 0 then () else
  begin
    if not (w.[0] = '\n' || w.[0] = '\r') && e.last = Comment
    then o_byte e u_lf;
    o_string e w 0 len;
    e.last <- Other
  end

let e_comment e c =
  if e.style <> `Raw then () else
  begin
    if e.last = Comment then o_byte e u_lf;
    o_byte e u_semi;
    o_string e c 0 (String.length c);
    e.last <- Comment
  end

let e_lexeme e = function
| Atom (a, raw) ->
    if e.last = Comment then o_byte e u_lf;
    if e.style <> `Raw then e_atom e a else
    begin match raw with
    | None -> e_atom e a;
    | Some r -> e_raw_atom e r
    end
| Ls -> e_ls e
| Le -> e_le e
| White w -> e_white e w
| Comment c -> e_comment e c

let e_end e = match e.nest <> 0 with
| true -> invalid_arg err_enc_unclosed
| false ->
    if e.nl then o_byte e u_lf;
    if e.o_pos <> 0 then flush e;
    e.output None

let encode_lexeme e = function
| Some l -> e_lexeme e l
| None -> e_end e

(* Custom s-expression codec *)

let rec decode_custom ~atom ~list d =
  let rec loop stack acc = match d_lexeme d with
  | None -> err d (`Unclosed `List)
  | Some v ->
      match v with
      | Ls -> loop (acc :: stack) []
      | Atom (a, _) -> loop stack (atom d a :: acc)
      | White _ | Comment _ -> loop stack acc
      | Le ->
          let list = list d (List.rev acc) in
          match stack with
          | [] -> Ok (Some list)
          | up :: stack -> loop stack (list :: up)
  in
  try match dec_lexeme d with
  | None -> Ok None
  | Some v ->
      match v with
      | Ls -> loop [] []
      | Atom (a, _) -> Ok (Some (atom d a))
      | White _ | Comment _ -> decode_custom ~atom ~list d
      | Le -> err d (`Unexpected_le)
  with Err (e, r) -> Error (e, r)

let encode_custom unfold e v =
  let rec loop stack = function
  | [] ->
      e_lexeme e Le;
      if stack = [] then () else loop (List.tl stack) (List.hd stack)
  | v :: vs ->
      match unfold v with
      | `Atom a -> e_lexeme e (Atom (a, None)); loop stack vs
      | `List l -> e_lexeme e Ls; loop (vs :: stack) l
  in
  match v with
  | None -> e_end e
  | Some v ->
      match unfold v with
      | `Atom a -> e_lexeme e (Atom (a, None))
      | `List l -> e_lexeme e Ls; loop [] l

(* Tagged s-expression codec *)

type 'a t = [ `Atom of string | `List of 'a t list ] * 'a
type 'a tagger = decoder -> [`Atom of string | `List of 'a t list] -> 'a t

let unit_tag _ se = se, ()
let range_tag d se = se, decoded_sexp_range d

let decode_tagged ~tag d =
  let atom d a = tag d (`Atom a) in
  let list d l = tag d (`List l) in
  decode_custom ~atom ~list d

let encode_tagged e se =
  let unfold (se, _) = se in
  encode_custom unfold e se

(* Escaping *)

let escape_info ~quote s = (* determines escape length, if needed *)
  let max = String.length s - 1 in
  let rec loop raw i l =
    if i > max then (if raw then None else Some l) else
    let next = i + 1 in
    match unsafe_string_get s i with
    | ' ' | '(' | ')' | ';' -> loop false next (l + if quote then 1 else 2)
    | '\\' | '"' | '\t' | '\n' | '\r' -> loop false next (l + 2)
    | c when c <= '\x1F' -> loop false next (l + 6)
    | c -> loop raw next (l + 1)
  in
  loop true 0 0

let escape_char b k c =
  unsafe_bytes_set b k '\\';
  unsafe_bytes_set b (k + 1) c;
  ()

let escape ?(quote = true) s =
  if s = "" then "\"\"" else
  match escape_info ~quote s with
  | None -> s
  | Some escaped_len ->
      let b, start = match quote with
      | false -> Bytes.create escaped_len, 0
      | true ->
          let escaped_len = escaped_len + 2 in
          let b = Bytes.create escaped_len in
          unsafe_bytes_set b 0 '"'; unsafe_bytes_set b (escaped_len - 1) '"';
          b, 1
      in
      let max = String.length s - 1 in
      let rec loop i k =
        if i > max then Bytes.unsafe_to_string b else
        let next = i + 1 in
        match unsafe_string_get s i with
        | ' ' ->
            if quote then (unsafe_bytes_set b k ' '; loop next (k + 1)) else
            (escape_char b k ' '; loop next (k + 2))
        | '(' ->
            if quote then (unsafe_bytes_set b k '('; loop next (k + 1)) else
            (escape_char b k '('; loop next (k + 2))
        | ')' ->
            if quote then (unsafe_bytes_set b k ')'; loop next (k + 1)) else
            (escape_char b k ')'; loop next (k + 2))
        | ';' ->
            if quote then (unsafe_bytes_set b k ';'; loop next (k + 1)) else
            (escape_char b k ';'; loop next (k + 2))
        | '\\' -> escape_char b k '\\'; loop next (k + 2)
        | '"'  -> escape_char b k '"'; loop next (k + 2)
        | '\t' -> escape_char b k 't'; loop next (k + 2)
        | '\n' -> escape_char b k 'n'; loop next (k + 2)
        | '\r' -> escape_char b k 'r'; loop next (k + 2)
        | c when c <= '\x1F' ->
            let byte = Char.code c in
            let hi = byte / 16 in
            let lo = byte mod 16 in
            unsafe_bytes_set b (k    )'\\';
            unsafe_bytes_set b (k + 1) 'u';
            unsafe_bytes_set b (k + 2) '{';
            unsafe_bytes_set b (k + 3) (unsafe_array_get hex_digit hi);
            unsafe_bytes_set b (k + 4) (unsafe_array_get hex_digit lo);
            unsafe_bytes_set b (k + 5) '}';
            loop next (k + 6)
        | c -> unsafe_bytes_set b k c; loop next (k + 1)
      in
      loop 0 start

(* Traces *)

type trace = lexeme list
type trace_rev = lexeme list

let dec_trace_to_peek_sexp d rt =
  let rec loop d rt = match d_lexeme d with
  | Some (Ls | Atom _) as dec -> d.peek <- Some (Ok dec); true, rt
  | Some l -> loop d (l :: rt)
  | None -> d.peek <- Some (Ok None); false, rt
  in
  match d.peek with
  | None -> loop d rt
  | Some (Ok (Some (Ls | Atom _))) -> true, rt
  | Some (Ok (Some l)) -> d.peek <- None; loop d (l :: rt)
  | Some (Ok None) -> false, rt
  | Some (Error (e, r)) -> raise_notrace (Err (e, r))

let dec_trace_ls d rt =
  let rec loop d rt = match dec_lexeme d with
  | Some (White _ | Comment _ as w) -> loop d (w :: rt)
  | Some Ls -> Ls :: rt
  | Some (Le | Atom _) -> err d (`Codec "expected start of list")
  | None -> err d (`Codec "expected start of list")
  in
  loop d rt

let dec_trace_le d rt =
  let rec loop d rt = match dec_lexeme d with
  | Some (White _ | Comment _ as w) -> loop d (w :: rt)
  | Some Le -> Le :: rt
  | Some (Ls | Atom _) -> err d (`Codec "expected end of list")
  | None -> err d (`Codec "expected end of list")
  in
  loop d rt

let dec_trace_atom d rt =
  let rec loop d rt = match dec_lexeme d with
  | Some (White _ | Comment _ as w) -> loop d (w :: rt)
  | Some (Atom (a, _) as at) -> a, (at :: rt)
  | Some (Ls | Le) -> err d (`Codec "expected atom") (* fixme more prec. *)
  | None -> err d (`Codec "expected atom")
  in
  loop d rt

let patch_layout (t, rt) =
  let rec loop rt = function
  | [] -> [], rt
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | (Atom _ | Le | Ls) :: _ as t -> t, rt
  in
  loop rt t

let patch_ls (t, rt) =
  let rec loop rt = function
  | [] -> [], Ls :: rt
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | Ls :: t -> t, Ls :: rt
  | (Le | Atom _) :: _ -> assert false
  in
  loop rt t

let patch_le (t, rt) =
  let rec loop rt = function
  | [] -> [], Le :: rt
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | Le :: t -> t, Le :: rt
  | (Ls | Atom _) :: _ -> assert false
  in
  loop rt t

let patch_atom a (t, rt) =
  let rec loop rt = function
  | [] -> [], a :: rt
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | Atom _ :: t -> t, a :: rt
  | (Ls | Le) :: _ -> assert false
  in
  loop rt t

let patch_drop_atom (t, rt) = (* keeps layout before atom (if any). *)
  let rec loop rt = function
  | [] -> [], rt
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | Atom _ :: t -> t, rt
  | (Ls | Le) :: _ -> assert false
  in
  loop rt t

let patch_drop_sexp (t, _ as recode) = (* keeps layout before sexp (if any). *)
  if t = [] then recode else
  let t, rt = patch_layout recode in
  let rec loop depth = function
  | Atom _ :: t when depth = -1 -> t
  | Ls :: t -> loop (depth + 1) t
  | Le :: t -> if depth = 0 then t else loop (depth - 1) t
  | l :: t -> loop depth t
  | [] -> assert false
  in
  loop (-1) t, rt

let patch_drop_until_le recode = (* keeps layout at the Le level *)
  let rec loop = function
  | (Le :: t, _ as recode) -> recode
  | (_ :: _, _ as recode) -> recode |> patch_drop_sexp |> patch_layout |> loop
  | ([], _) -> assert false
  in
  recode |> patch_layout |> loop

let patch_keep_atom (t, rt) =
  let rec loop rt = function
  | (White _ | Comment _ as w) :: t -> loop (w :: rt) t
  | Atom _ as a :: t -> t, a :: rt
  | (Ls | Le) :: _ | [] -> assert false
  in
  loop rt t

let patch_keep_list recoder =
  let rec loop depth rt = function
  | Ls :: t -> loop (depth + 1) (Ls :: rt) t
  | Le :: t ->
      begin match depth with
      | 0 -> t, Le :: rt
      | depth -> loop (depth - 1) (Le :: rt) t
      end
  | l :: t -> loop depth (l :: rt) t
  | [] -> assert false
  in
  let t, rt = patch_layout recoder in
  match t with
  | Ls :: t -> loop 0 (Ls :: rt) t
  | (Atom _ | White _ | Comment _ | Le) :: _ | [] -> assert false

let patch_keep_sexp (t, rt) =
  let rec loop depth rt = function
  | Atom _ as a :: t when depth = 0 -> t, a :: rt
  | Ls :: t -> loop (depth + 1) (Ls :: rt) t
  | Le :: t ->
      begin match depth with
      | 0 -> assert false
      | 1 -> t, Le :: rt
      | depth -> loop (depth - 1) (Le :: rt) t
      end
  | l :: t -> loop depth (l :: rt) t
  | [] -> assert false
  in
  loop 0 rt t

let add_atom a (t, rt) = (t, Atom (a, None) :: rt)
let add_ls (t, rt) = (t, Ls :: rt)
let add_le (t, rt) = (t, Le :: rt)

(* Codecs

   A few notes about encoder updates.

   * Fundamentally we don't need the old value as we could decode it
     back from the trace as needed when we encode, it just seems a bit
     cleaner that way.

   * Encoders see all the remaining data trace rather that just the data
     that concern themselves. We could chunk the trace so that sub-encoders
     see only what is needed but this involves a lot of list processing.

   * Regarding the last point we could also work on a sexp representation
     this would make chunking easier; but would also necessisate a
     clarification of white and comments are associated to tree nodes.
     Currently this is somehow left to the judgement of encoders
     (which may not be a bad idea). *)

type 'a codec_encoder =
  'a option -> 'a -> (trace * trace_rev) -> trace * trace_rev
  (* trace must be left untouched if option is None. *)

type 'a codec_decoder = decoder -> trace_rev -> 'a * trace_rev
type 'a codec =
  { eq : 'a -> 'a -> bool;
    default : 'a;
    enc : 'a codec_encoder;
    dec : 'a codec_decoder; }

let codec ~eq ~default enc dec = { eq; default; enc; dec; }
let codec_with_default default c = { c with default }
let codec_with_codec ?eq ?default c =
  let eq = match eq with None -> c.eq | Some eq -> eq in
  let default = match default with None -> c.default | Some d -> d in
  { c with eq; default }

let rec enc_lexemes e = function
| [] -> ()
| l :: ls -> encode_lexeme e (Some l); enc_lexemes e ls

let encode_traced_value c e (v, t) = match v with
| None -> enc_lexemes e t; encode_lexeme e None
| Some (o, n) -> enc_lexemes e (List.rev @@ snd @@ c.enc (Some o) n (t, []))

let decode_traced_value c d =
  try
    let has_sexp, rt = dec_trace_to_peek_sexp d [] in
    if not has_sexp then Ok (None, List.rev rt) else
    let v, rt = c.dec d rt in
    Ok (Some v, List.rev rt)
  with
  | Err (e, r) -> Error (e, r)

let encode_value c e v = match v with
| None -> encode_lexeme e None
| Some v -> enc_lexemes e (List.rev @@ snd @@ c.enc None v ([], []))

let decode_value c d = match decode_traced_value c d with
| Ok (v, _) -> Ok v
| Error _ as e -> e

(* Codecs *)

let atom_codec ~eq ~default enc dec =
  let rec atom_enc enc o n recode = match o with
  | None -> recode |> add_atom (enc n)
  | Some o when eq o n -> recode |> patch_keep_atom
  | Some o -> recode |> patch_drop_atom |> atom_enc enc None n
  in
  let atom_dec dec d rt =
    let rec loop d rt = match dec_lexeme d with
    | Some (Atom (a, _) as l) -> dec d a, (l :: rt)
    | Some (Comment _ | White _ as l) -> loop d (l :: rt)
    | Some (Ls | Le) | None  -> err d (`Codec "expected atom") (* FIXME *)
    in
    loop d rt
  in
  { eq; default; enc = atom_enc enc; dec = atom_dec dec; }

let unit =
  let eq = (( = ) : unit -> unit -> bool) in
  let default = () in
  let unit_enc () = "unit" in
  let unit_dec d = function
  | "unit" -> ()
  | _ -> err d (`Codec "expected `unit'")
  in
  atom_codec ~eq ~default unit_enc unit_dec

let bool =
  let eq = (( = ) : bool -> bool -> bool) in
  let default = false in
  let bool_enc = string_of_bool in
  let bool_dec d a = try bool_of_string a with
  | Invalid_argument _ -> err d (`Codec "expected boolean")
  in
  atom_codec ~eq ~default bool_enc bool_dec

let int =
  let eq = (( = ) : int -> int -> bool) in
  let default = 0 in
  let int_enc = string_of_int in
  let int_dec d a = try int_of_string a with
  | Failure _ -> err d (`Codec "expected integer")
  in
  atom_codec ~eq ~default int_enc int_dec

let float_fmt fmt =
  let eq = (( = ) : float -> float -> bool) in
  let default = 0. in
  let float_enc f = Printf.sprintf fmt f in
  let float_dec d a = try float_of_string a with
  | Failure _ -> err d (`Codec "expected float")
  in
  atom_codec ~eq ~default float_enc float_dec

let float = float_fmt "%g"
let float_hex = float_fmt "%h"

let string =
  let eq = String.equal in
  let default = "" in
  let string_enc x = x in
  let string_dec d x = x in
  atom_codec ~eq ~default string_enc string_dec

let result ~ok ~error =
  let eq r0 r1 = match r0, r1 with
  | Ok _, Error _ | Error _, Ok _ -> false
  | Ok v0, Ok v1 -> ok.eq v0 v1
  | Error e0, Error e1 -> error.eq e0 e1
  in
  let default = Ok ok.default in
  let rec enc o n recode = match o with
  | None ->
      let add_case c at n recode =
        recode |> add_ls |> add_atom at |> c.enc None n |> add_le
      in
      begin match n with
      | Ok n -> recode |> add_case ok "Ok" n
      | Error n -> recode |> add_case error "Error" n
      end
  | Some o when eq o n -> recode |> patch_keep_list
  | Some o ->
      let patch_case c o n recode =
        recode |> patch_ls |> patch_keep_atom |> c.enc o n |> patch_le
      in
      match o, n with
      | Ok o, Ok n -> recode |> patch_case ok (Some o) n
      | Error o, Error n -> recode |> patch_case error (Some o) n
      | _ -> recode |> patch_layout |> patch_drop_sexp |> enc None n
  in
  let dec d rt =
    let rt = dec_trace_ls d rt in
    let atom, rt = dec_trace_atom d rt in
    let v, rt = match atom with
    | "Ok" -> let v, rt = ok.dec d rt in Ok v, rt
    | "Error" -> let e, rt = error.dec d rt in Error e, rt
    | atom -> err d (`Codec "expected `Ok' or `Error'") (* FIXME *)
    in
    let rt = dec_trace_le d rt in
    v, rt
  in
  codec ~eq ~default enc dec

let t2 c0 c1 =
  let eq (l0, l1) (r0, r1) = c0.eq l0 r0 && c1.eq l1 r1 in
  let default = c0.default, c1.default in
  let enc o (n0, n1 as n) recode = match o with
  | None ->
      recode |> add_ls |> c0.enc None n0 |> c1.enc None n1 |> add_le
  | Some o when eq o n -> recode |> patch_keep_list
  | Some (o0, o1) ->
      recode
      |> patch_ls |> c0.enc (Some o0) n0 |> c1.enc (Some o1) n1 |> patch_le
  in
  let dec d rt =
    let rt = dec_trace_ls d rt in
    let v0, rt = c0.dec d rt in
    let v1, rt = c1.dec d rt in
    let rt = dec_trace_le d rt in
    (v0, v1), rt
  in
  codec ~eq ~default enc dec

let list c =
  let rec eq l0 l1 = match l0, l1 with
  | v0 :: l0, v1 :: l1 when c.eq v0 v1 -> eq l0 l1
  | [], [] -> true
  | _, _ -> false
  in
  let default = [] in
  let enc o n recode =
    let rec add_els n recode = match n with
    | nv :: n -> add_els n (c.enc None nv recode)
    | [] -> recode
    in
    match o with
    | None -> recode |> add_ls |> add_els n |> add_le
    | Some o when eq o n -> recode |> patch_keep_list
    | Some o ->
        let rec els o n recode = match o, n with
        | ov :: o, nv :: n -> els o n (c.enc (Some ov) nv recode)
        | [], [] -> recode
        | [], n -> recode |> patch_layout |> add_els n
        | o, [] -> patch_drop_until_le recode
        in
        recode |> patch_ls |> els o n |> patch_le
  in
  let dec d rt =
    let rt = dec_trace_ls d rt in
    let rec loop d acc rt = match pek_lexeme d with
    | None -> err d (`Codec "unexpected end of input")
    | Some Le -> ignore (dec_lexeme d); List.rev acc, (Le :: rt)
    | Some (Ls | Atom _) ->
        let v, rt = c.dec d rt in
        loop d (v :: acc) rt
    | Some (White _ | Comment _ as l) ->
        ignore (dec_lexeme d); loop d acc (l :: rt)
    in
    loop d [] rt
  in
  codec ~eq ~default enc dec

let set
    (type e) (type s) (module Set : Set.S with type elt = e and type t = s)
    c  =
  let eq = Set.equal in
  let default = Set.empty in
  let enc o n recode =
    let rec add_els n recode =
      if Set.is_empty n then recode else
      let v = Set.choose n in
      let n = Set.remove v n in
      add_els n (c.enc None v recode)
    in
    match o with
    | None -> recode |> add_ls |> add_els n |> add_le
    | Some o when eq o n -> recode |> patch_keep_list
    | Some o ->
        let rec els o n recode =
          let ov = Set.choose o in
          let _ = Set.remove ov o in
          failwith "TODO"
        in
        recode |> patch_ls |> els o n |> patch_le
  in
  let dec d rt =
    let rt = dec_trace_ls d rt in
    let rec loop d acc rt = match pek_lexeme d with
    | None -> err d (`Codec "unexpected end of input")
    | Some Le -> ignore (dec_lexeme d); acc, (Le :: rt)
    | Some (Ls | Atom _) ->
        let v, rt = c.dec d rt in
        if Set.mem v acc then err d (`Codec "not a set") else
        loop d (Set.add v acc) rt
    | Some (White _ | Comment _ as l) ->
        ignore (dec_lexeme d); loop d acc (l :: rt)
    in
    loop d Set.empty rt
  in
  codec ~eq ~default enc dec

type 'a atom = ('a -> string) * (string -> 'a)

and 'a variant_const = string * 'a
and ('a, 'b) variant_valued = string * 'b c * ('b -> 'a)

and 'a variant_proj =
| Pconst : 'a variant_const -> 'a variant_proj
| Pvalued : ('a, 'b) variant_valued * 'b -> 'a variant_proj

and 'a variant_inj =
| Iconst : 'a variant_const -> 'a variant_inj
| Ivalued : ('a, 'b) variant_valued -> 'a variant_inj

and 'a variant_projs = 'a -> 'a variant_proj
and 'a variant_injs = 'a variant_inj list

and 'a repr =
| Ground : 'a atom -> 'a repr
| List : 'a c -> 'a list repr
| Pair : 'a c * 'b c -> ('a * 'b) repr
| Variant : 'a variant_projs * 'a variant_injs -> 'a repr

and 'a c =
  { name : string;
    eq : 'a -> 'a -> bool;
    default : 'a;
    repr : 'a repr }

let encode_value c e v = match v with
| None -> encode_lexeme e None
| Some v ->
    let rec loop : type a. a c -> encoder -> a -> unit = (* FIXME not t.r. *)
    fun c e v -> match c.repr with
    | Ground (enc, _) -> encode_lexeme e (Some (Atom ((enc v), None)))
    | List c ->
        encode_lexeme e (Some Ls);
        List.iter (loop c e) v;
        encode_lexeme e (Some Le)
    | Pair (c0, c1) ->
        encode_lexeme e (Some Ls);
        loop c0 e (fst v);
        loop c1 e (snd v);
        encode_lexeme e (Some Le)
    | Variant (proj, _) ->
        begin match proj v with
        | Pconst (a, _) -> encode_lexeme e (Some (Atom (a, None)))
        | Pvalued ((a, c, _), v) ->
            encode_lexeme e (Some Ls);
            loop c e v;
            encode_lexeme e (Some Le);
        end
    in
    loop c e v

let rec decode_ground : type a. a c -> decoder -> (string -> a) -> a =
fun c d dec ->
  let at, _ = dec_trace_atom d [] in
  try dec at with
  | e -> err d (`Codec ("error decoding  " ^ c.name))

and decode_list : type a. a c -> decoder -> a list =
fun c d ->
  let _ = dec_trace_ls d [] in
  let rec els d acc = match pek_lexeme d with
  | None -> err d (`Codec "unexpected end of input")
  | Some Le -> ignore (dec_lexeme d); List.rev acc
  | Some (Ls | Atom _) ->
      let v = decode_value c d in
      els d (v :: acc)
  | Some (White _ | Comment _ as l) -> ignore (dec_lexeme d); els d acc
  in
  els d []

and decode_pair : type a b. a c -> b c -> decoder -> a * b =
fun c0 c1 d ->
  let _ = dec_trace_ls d [] in
  let v0 = decode_value c0 d in
  let v1 = decode_value c1 d in
  ignore (dec_trace_le d []);
  v0, v1

and decode_value : type a. a c -> decoder -> a = (* FIXME not t.r. *)
fun c d -> match c.repr with
| Ground (_, dec) -> decode_ground c d dec
| List c -> decode_list c d
| Pair (c0, c1) -> decode_pair c0 c1 d
| Variant (_, inj) ->
    let has_sexp, _ = dec_trace_to_peek_sexp d [] in
    if not has_sexp then err d (`Codec ("error decoding " ^ c.name)) else
    begin match dec_lexeme d with
    | Some Ls ->
        let at, _ = dec_trace_atom d [] in
        let rec dec_valued at = function
        | Ivalued (id, c, inj) :: _ when id = at ->
            let v = inj (decode_value c d) in
            let _ = dec_trace_le d in
            v
        | _ :: injs -> dec_valued at injs
              | [] -> err d (`Codec ("error decoding " ^ c.name))
        in
        dec_valued at inj
    | Some (Atom (at, _)) ->
        let rec find_const at = function
        | Iconst (id, v) :: _ when id = at -> v
        | _ :: injs -> find_const at injs
        | [] -> err d (`Codec ("error decoding " ^ c.name))
        in
        find_const at inj
    | _ -> assert false
    end

let decode_value c d =
  try match pek_lexeme d with
  | None -> ignore (dec_lexeme d); Ok None
  | Some _ -> decode_value c d
  with
  | Err (e, r) -> Error (e, r)

let c name ~eq ~default ~repr = { name; eq; default; repr }

let c_int =
  let eq = (( = ) : int -> int -> bool) in
  let default = 0 in
  let int_enc = string_of_int in
  let int_dec = int_of_string in
  c "integer" ~eq ~default ~repr:(Ground (int_enc, int_dec))

let c_t2 c0 c1 =
  let eq (l0, l1) (r0, r1) = c0.eq l0 r0 && c1.eq l1 r1 in
  let default = c0.default, c1.default in
  c "pair" ~eq ~default ~repr:(Pair (c0, c1))

let c_result ~ok ~error =
  let eq r0 r1 = match r0, r1 with
  | Ok _, Error _ | Error _, Ok _ -> false
  | Ok v0, Ok v1 -> ok.eq v0 v1
  | Error e0, Error e1 -> error.eq e0 e1
  in
  let default = Ok ok.default in
  let ok = "Ok", ok, fun v -> Ok v in
  let error = "Error", error, fun v -> Error v in
  let select = function
  | Ok v -> Pvalued (ok, v)
  | Error v -> Pvalued (error, v)
  in
  let result = Variant (select, [Ivalued ok; Ivalued error]) in
  c "result" ~eq ~default ~repr:result

let none = "None", None
let none_proj = Pconst none
let none_inj = Iconst none
let c_option some =
  let eq o0 o1 = match o0, o1 with
  | Some v0, Some v1 -> some.eq v0 v1
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  in
  let default = None in
  let some = "Some", some, fun v -> Some v in
  let select = function
  | None -> none_proj
  | Some v -> Pvalued (some, v)
  in
  let option = Variant (select, [none_inj; Ivalued some]) in
  c "option" ~eq ~default ~repr:option

(*
type t = A | B | C of string

let t cc =
  let eq v0 v1 = v0 = v1 in
  let default = A in
  let a = "A", A in
  let b = "B", B in
  let cc = "C", cc, (fun v -> C v) in
  let select = function
  | A -> Pconst a
  | B -> Pconst b
  | C v -> Pvalued (cc, v)
  in
  let t = Variant (select, [Iconst a; Iconst b; Ivalued cc]) in
  c "t" ~eq ~default ~repr:t
*)





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
