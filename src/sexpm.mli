(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** S-expression codecs and updates.

    [Sexpm] codecs sequences of s-expressions to different
    representations.  Support is provided for:
    {ul
    {- Generic {{!custom_codec}custom} or {{!tagged_codec}tagged}
       s-expressions.}
    {- Typed OCaml {{!typed_codecs}values codecs} with layout preserving
       concrete syntax updates.}
    {- Low-level sequences of {{!lexeme_codec}s-expression lexemes} with
       concrete syntax layout preservation.}}
    Consult the {{!syntax}syntax} of s-expressions,
    {{!tips_lims}tips and limitations} and {{!examples}examples} of use.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:io IO} *)

type input = unit -> (Bytes.t * int * int) option
(** The type for input byte streams. On [Some (b, pos, len)] the bytes
    of [b] from [pos] to [pos+len] are readable until the next call to
    the function. On [None] the end of stream is reached. *)

val input_of_string : string -> input
(** [input_of_string s] is a byte stream reading from [s]. *)

val input_of_in_channel : ?bytes:Bytes.t -> in_channel -> input
(** [input_of_in_channel ~bytes ic] is a byte stream reading from [ic]
    using [bytes] to read the data.

    @raise Invalid_argument if the length of [bytes] is [0]. *)

type output = (Bytes.t * int * int) option -> unit
(** The type for output byte streams. Given [Some (b, pos, len)] the bytes
    of [b] from [pos] to [pos+len] are readable until the function
    returns. On [None] the end of stream is signalled to the function. *)

val output_of_buffer : Buffer.t -> output
(** [output_to_buffer b] is an output stream writing to buffer [b]. *)

val output_of_out_channel : out_channel -> output
(** [output_of_out_channel oc] is an output stream writing to [oc].
    It is the client's duty to flush it or close it. *)

(** {1:decoder Decoders} *)

type pos = int * int
(** The type for character positions. A one-based line and zero-based
    column number. *)

type range = pos * pos
(** The type for character ranges. *)

val pp_range : Format.formatter -> range -> unit
(** [pp_range ppf r] prints [r] on [ppf] according to
    {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
    conventions}. *)

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
(** The type for decode errors. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints an unspecified representation of [e]
    on [ppf]. *)

type decoder
(** The type for UTF-8 s-expression decoders. *)

val decoder : ?layout:bool -> input -> decoder
(** [decoder i] is a decoder decoding s-expressions from [i] the UTF-8
    encoded byte stream [i].

    If [layout] is [true] (defaults to [false]), whitespace, comments and
    raw atoms are preserved by the underlying {{!lexeme} lexeme decoder}. This
    allows the layout of the decoded sequence's concrete syntax to be
    preserved. *)

val decoder_layout : decoder -> bool
(** [decoder_layout d] is the value of [layout], see {!decoder}. *)

val decoded_range : decoder -> range
(** [decoded_range d] is the range of characters spanning the last
    decoded lexeme or error. *)

val decoded_sexp_range : decoder -> range
(** [decoder_sexp_range d] is the range of characters spanning the
    last fully parsed s-expression. The value is accurate only after
    [Atom _] and [Le] lexemes. *)

(** {1:encoders Encoders} *)

type encoder
(** The type for UTF-8 s-expression encoders. *)

type encoder_style = [ `Pp | `Minify | `Raw ]
(** The type for the encoder output style.
    {ul
    {- [`Pp], pretty printed output. If {{!lexeme}lexemes}
       are encoded, whitespace and raw atoms are ignored.}
    {- [`Minify], compact output. If {{!lexeme}lexemes}
       are encoded, whitespace and raw atoms lexemes are ignored.}
    {- [`Raw], raw output. Like [`Minify] except whitespace and
       raw atoms lexemes are not ignored.}} *)

val encoder :
  ?bytes:Bytes.t -> ?nl:bool -> ?quote:bool -> ?style:encoder_style ->
  output -> encoder
(** [encoder ~nl ~bytes ~minify o] is an encoder encoding s-expressions on [o].
    {ul
    {- [style] indicates the {{!encoder_style}encoder style}, defaults
       to [`Pp].}
    {- [quote], quoting behaviour on non-raw atoms. If [true] (default)
       atoms are quoted iff they need escapes. If [false] no atoms except
       the empty string are quoted.}
    {- [nl] indicates whether the encoder makes sure a final line feed
       U+000A is encoded at the end of the sequence, defaults to [false].}
    {- [bytes] indicates the buffer to pass to [o].}}

    @raise Invalid_argument if the length of [bytes] is [0]. *)

(** {1:custom_codec Custom s-expression codec}

    Codecs {{!sexp}sequences} of s-expressions by mapping them
    to a custom s-expression type. See an
    {{!custom_example}example.} *)

val decode_custom :
  atom:(decoder -> string -> 'a) ->
  list:(decoder -> 'a list -> 'a) ->
  decoder -> ('a option, error * range) result
(** [decode_custom ~atom ~list d] is [Some v], constructed using [atom]
    and [list] by folding over a single s-expression decoded from [d]
    or [None] if the end stream was reached. *)

val encode_custom :
  ('a -> [ `Atom of string | `List of 'a list ]) ->
  encoder -> 'a option -> unit
(** [encode_custom unfold e v] is encodes a single s-expression on [e]
    by unfolding [v] using [unfold]. Use with [None] to signal the
    end of the sequence. *)

(** {1:tagged_codec Tagged s-expression codec}

    Codecs {{!sexp}sequences} of s-expressions to a generic
    tagged representation of s-expressions. *)

type 'a t = [ `Atom of string | `List of 'a t list ] * 'a
(** The type for s-expressions tagged with values of type ['a].
    Either an unescaped, UTF-8 encoded, atom or a list of
    s-expressions. *)

type 'a tagger =
  decoder -> [ `Atom of string | `List of 'a t list ] -> 'a t
(** The type for s-expression taggers. Given the decoder and the last
    decoded s-expression, returns a tagged expression. *)

val unit_tag : unit tagger
(** [unit_tag] is [fun _ se -> se, ()]. *)

val range_tag : range tagger
(** [range_tag] is [fun d se -> se, Sexpm.decoded_sexp_range d]. *)

val decode_tagged :
  tag:'a tagger -> decoder -> ('a t option, error * range) result
(** [decode_tagged tag d] is an s-expression decoded from [d] and tagged
    according to [tag] or [None] if the end of stream was reached. *)

val encode_tagged : encoder -> 'a t option -> unit
(** [encode_tagged e se] encodes the s-expression [se] on [e].
    Use with [None] to signal the end of sequence. *)

(** {1:lexeme_codec Lexeme sequence codec}

    Codecs {{!sexp}sequences} of s-expressions as layout preserving,
    {{!lexeme}well-formed}, sequences of lexemes. *)

type lexeme =
  | White of string
  | Comment of string
  | Ls | Le | Atom of string * string option (** *)
  (** The type for s-expression lexemes.

      {ul
      {- [White s] is {{!whitespace}whitespace}.}
      {- [Comment s] is a {{!comments}comment} without the starting
         semi-colon and ending new line.}
      {- [Ls]tart and [Le]nd delimit lists.}
      {- [Atom (a, raw)] is for the unescaped atom [a] that was parsed
         from the raw string [raw].}}

      On decoding, whitespace, comments and raw atoms are reported
      only if the decoder has {{!decoder_layout}layout}. No sequence of
      two [White] can be decoded from an input sequence.

      A {e well-formed} sequence of lexemes belongs to the language of the
      following {{:https://tools.ietf.org/html/rfc5234}RFC 5234} ABNF
      grammar defined on a sequence of lexemes.
{v
sexp-seq = *(White _ / Comment _ / sexp)
    sexp = Atom _ / Ls *(sexp-seq) Le
v}
      A {!decoder} returns only well-formed sequences of lexemes even
      when {!error}s are returned (see {!error_recovery}). The lexemes
      strings are always UTF-8 encoded and in these strings characters
      originally escaped in the input are in their unescaped representation.
      [White _], [Commment _] and raw atom representations are only ever
      returned if the decoder has {{!decoder_layout}layout}.

      An {!encoder} accepts only well-formed sequences of lexemes or
      [Invalid_argument] is raised. The strings given to the lexemes
      are assumed to be UTF-8 encoded, this is {b not} checked by the
      module. For atoms if [raw] is given this raw string will be used
      to output the token it is the client's duty to make sure it is
      properly quoted and escaped. If [raw] is not provided the module
      will handle this. For comments if the string does not end with a
      new line and further lexemes are encoded a new line is added. *)

val pp_lexeme : Format.formatter -> lexeme -> unit
(** [pp_lexeme ppf l] prints an unspecified non-sexp representation of
    [l] on [ppf]. *)

val decode_lexeme : decoder -> (lexeme option, error * range) result
(** [decode_lexeme d] is:
    {ul
    {- [Ok (Some l)] if a lexeme [l] was decoded.}
    {- [Ok None] if the end of input was reached.}
    {- [Error e] if a decoding error occurred. If the client is
       interested in a best-effort decoding it can still continue to
       decode afer an error (see {!error_recovery}) altough the
       resulting sequence of lexemes is undefined and may not be
       well-formed.}}

    {b Note.} Repeated invocation eventually always returns [Ok None], even
    in case of error. *)

val peek_lexeme : decoder -> (lexeme option, error * range) result
(** [peek_lexeme d] is like {!decode_lexeme} but doesn't remove
    the lexeme from the input sequence. *)

val encode_lexeme : encoder -> lexeme option -> unit
(** [encode_lexeme e l] encodes [l] on [e]. [None] should always
    be eventually encoded.

    @raise Invalid_argument if a non {{!lexeme}well-formed} sequence
    of lexemes is encoded.

    {b Warning.} The encoder does not check the following properties
    and  for the output to be valid it is the client's duty to make sure
    that in encoding:
    {ul
    {- [White s], [s] is only made of {{!whitespace}whitespace}}
    {- [Comment s], [s] is UTF-8 encoded, does not contain US-ASCII control
       characters except spaces or tabs and does not contain any end of
       line except possibly at the end.}
    {- [Atom (a, None)], [a] is UTF-8 encoded.}
    {- [Atom (_, Some r)], [r] is UTF-8 encoded and, if needed, properly
       quoted and escaped. {!escape} can be used for this.}} *)

(** {1:escaping Atom escaping} *)

val escape : ?quote:bool -> string -> string
(** [escape ~quote s] escapes the UTF-8 encoded string [s] to an
    s-expression {{!atoms}token or quoted token}. If [quote] is [true]
    (default) the token is quoted iff [s] needs escapes or is the empty
    string. If [quote] is [false] the token is never quoted except if [s]
    is the empty string. *)

(** {1:typed_codecs OCaml values codecs} *)

type trace = lexeme list
(** The type for the list of lexemes that described a value. *)

type 'a codec
(** The type for codecs for values of type ['a]. *)

val codec_with_default : 'a -> 'a codec -> 'a codec
(** [codec_with_default v c] is [c] with default value [c]. *)

val codec_with_codec :
  ?eq:('a -> 'a -> bool) -> ?default:'a -> 'a codec -> 'a codec
(** [codec_with_codec ~eq ~default c] is [c] with equality [eq] (unchanged
    if unspecified) and default value [c] *)

(** {2:codec_values Encoding and decoding values} *)

type 'a c

val encode_value : 'a c -> encoder -> 'a option -> unit

(*
val decode_value  : 'a codec -> decoder -> ('a option, error * range) result
val encode_values : 'a codec -> encoder -> 'a list -> unit
val decode_values : 'a codec -> encoder -> ('a list, error * range) result
*)

val encode_traced_value :
  'a codec -> encoder -> ('a * 'a) option * trace -> unit

val decode_traced_value :
  'a codec -> decoder -> ('a option * trace, error * range) result

(** {2:std_type_codecs OCaml standard library type codecs} *)

val unit : unit codec
val bool : bool codec
val int : int codec
val float : float codec
val float_fmt : (float -> string, unit, string) format -> float codec
val float_hex : float codec
val string : string codec
val result : ok:'a codec -> error:'b codec -> ('a, 'b) result codec
val t2 : 'a codec -> 'b codec -> ('a * 'b) codec
val list : 'a codec -> 'a list codec
val set :
  (module Set.S with type elt = 'a and type t = 'b) -> 'a codec -> 'b codec

(** {1:syntax S-expression syntax}

    We define the syntax of s-expressions over a sequence of
   {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode
   characters} in which all US-ASCII control characters except
   {{!whitespace}whitespace} are forbidden in unescaped form.

    {2:sexp S-expressions and sequences thereof}

    A {e s-expression} is either an {{!atoms}{e atom}} or a
    {{!lists}{e list}} of s-expressions interspaced with
    {{!whitespace}{e whitespace}} and {{!comments}{e comments}}. A {e
    sequence of s-expressions} is simply a succession of s-expressions
    interspaced with whitespace and comments.

    These elements are informally described below and finally made
    precise via an ABNF {{!grammar}grammar}.

    {2:whitespace Whitespace}

    Whitespace is a sequence of whitespace characters, namely, space
    (U+0020), tab (U+0009), line feed (U+000A) or carriage return
    (U+000D).

    {2:comments Comments}

    Unless escaped or inside quoted tokens (see below) anything that
    follows a semicolon (U+003B) is ignored until the next {e end of line},
    that is either a line feed
    (U+000A), a carriage return (U+000D) or a carriage return and a line
    feed (<U+000D,U+000A>).
{v
(this is not a comment) ; This is a comment
(this is not a comment)
v}

    {2:atoms Atoms}

    An atom represents ground data as a string of Unicode characters.
    It can, via escapes, represent any sequence of Unicode characters,
    including control characters and U+0000. It cannot represent an
    arbitrary byte sequence except via a client-defined encoding
    convention (e.g. Base64, OCaml byte escape codes, etc.).

    An atom is specified either via a token or a quoted token.  Quoted
    tokens can represent atoms with spaces, parentheses and semicolons
    without having to escape them.
{v
abc        ; a token for the atom "abc"
abc\;\ \(d ; a token for the atom "abc; (d"
"abc"      ; a quoted token for the atom "abc"
"abc; (d"  ; a quoted token for the atom "abc; (d"
""         ; the quoted token for the atom ""
v}

    A token and its quoted form represent the same atom. All atoms
    except the empty atom [""] can be represented in both forms.

    Escapes are introduced by a backslash. Double quotes and
    backslashes have to be escaped everywhere except in comments.
{v
"\u\{1F42B\}\n\"\\" ; atom with U+1F42B, a new line, a quote and a backslash
\u\{1F42B\}\n\"\\   ; idem
v}
    The following escape sequences are recognized:
    {ul
    {- ["\""] (<U+005C,U+0022>) for double quote (U+0022) (mandatory)}
    {- ["\\"] (<U+005C,U+005C>) for backslash (U+005C) (mandatory)}
    {- ["\("] (<U+005C,U+0028>) for left parenthesis (U+0028)}
    {- ["\)"] (<U+005C,U+0029>) for right parenthesis (U+0029)}
    {- ["\;"] (<U+005C,U+003B>) for semicolon (U+003B)}
    {- ["\ "] (<U+005C,U+0020>) for space (U+0020)}
    {- ["\t"] (<U+005C,U+0074>) for tab (U+0009)}
    {- ["\n"] (<U+005C,U+006E>) for line feed (U+000A)}
    {- ["\r"] (<U+005C,U+0072>) for carriage return (U+000D)}
    {- ["\u{X}"] with [X] is from 1 to at most 6 upper or lower case
       hexadecimal digits standing for the corresponding
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode character}
       U+X.}
    {- Any other character except line feed (U+000A) or carriage return
       (U+000D), following
       a backslash is an illegal sequence of characters.}}

    Tokens and quoted tokens can be split across lines by using a backslash
    (U+005C) followed by a line feed (<U+000A>), a carriage return
    (U+000D) or a carriage return and a line feed
    (<U+000D,U+000A>). Any space (U+0020) or tab (U+0009) character
    at the beginning of the new line is ignored.
{v
"\
  a\
  \ " ; the atom "a "
  a\
  \   ; the atom "a "
v}

    {2:lists Lists}

    Lists are delimited by left and right parentheses (U+0028, U+0029)
    and their elements are s-expressions separated by optional
    whitespace and comments. For example:
{v
(a list (of four) expressions)
(a list(of four)expressions)
("a"list("of"four)expressions)
(a list (of ; This is a comment
four) expressions)
() ; the empty list
v}

    {2:grammar S-expression grammar}

    The following {{:https://tools.ietf.org/html/rfc5234}RFC 5234}
    ABNF grammar is defined on a sequence of
    {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode characters}.
{v
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
v}
    A few additional constraints not expressed by the grammar:
    {ul
    {- If an initial {{:http://unicode.org/glossary/#byte_order_mark}BOM
       character} (U+FEFF) is present it is discarded.}
    {- On parsing [token] and [qtoken], any [%x0020] and [%x0009] following
       a [cont] is discarded and not part of the parsed atom.}
    {- [unum] once interpreted as an hexadecimal number must be a
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode scalar
       value.}}
    {- A comment can be ended by the end of the character sequence.}} *)

(** {1:error_recovery Error recovery}

    After a decoding error, if best-effort decoding is performed, the
    following happens before continuing:
    {ul
    {- [`Illegal_bytes _], [`Illegal_escape _], [`Illegal_uchar _], a Unicode
       replacement character (U+FFFD) is substituted to the illegal byte
       sequence, bogus escape or Unicode character}
    {- [`Unexpected_le], the right parenthesis is skipped.}
    {- [`Unclosed `Escape], the escape is skipped and the atom returned.}
    {- [`Unclosed `Quoted_token], the token is closed and the atom returned.}
    {- [`Unclosed `List], a [Le] token is returned.}}
    Due to the U+FFFD insertions layout cannot be assumed to be
    preserved when error recovery is performed, however outputing an
    erroring stream of lexemes results in a valid sequence of s-expressions. *)

(** {1:tips_lims Tips and limitations}

{ul
{- Usage of decoding and encoding functions of the various
   representations on decoders and encoders is not mutually exclusive.
   However if you are for example using the lexeme decoder you need
   to make sure you are at the beginning of a sexp before invoking
   the higher-level decoders.}
{- Concrete syntax layout preservation will in general be slower.}} *)

(** {1:examples Examples}

    {2:custom_example Custom representation}

    The following codecs a sequence of s-expressions to a custom
    definition of s-expressions. Note that for illustration purposes
    this representation is exactly the tagged s-expression
    representation of the {{!tagged_example}next example}.
{[
type sexp = [ `Atom of string | `List of sexp list ] * Sexpm.range

let of_string : string -> (sexp list, Sexpm.error * Sexpm.range) result =
fun s ->
  let d = Sexpm.(decoder @@ input_of_string s) in
  let atom d a = `Atom a, Sexpm.decoded_sexp_range d in
  let list d l = `List l, Sexpm.decoded_sexp_range d in
  let rec loop acc = match Sexpm.decode_custom ~atom ~list d with
  | Ok Some v -> loop (v :: acc)
  | Ok None -> Ok (List.rev acc)
  | Error _ as e -> e
  in
  loop []

let to_string : ?style:Sexpm.encoder_style -> sexp list -> string
fun ses ->
  let unfold = fst in
  let b = Buffer.create 256 in
  let d = Sexpm.(encoder ?style @@ output_to_buffer b) in
  let rec loop = function
  | se :: ses -> Sexpm.encode_custom unfold d (Some se); loop ses
  | [] -> Sexpm.encode_custom d None; Buffer.contents b
  in
  loop ses
]}

   {2:tagged_example Tagged representation}

{[
type sexp = Sexpm.range Sexpm.t

let of_string : string -> (sexp list, Sexpm.error * Sexpm.range) result =
fun s ->
  let d = Sexpm.(decoder @@ input_of_string s) in
  let rec loop acc = match Sexpm.decode_tagged ~tag:Sexpm.range_tag d with
  | Ok Some v -> loop (v :: acc)
  | Ok None -> Ok (List.rev acc)
  | Error _ as e -> e
  in
  loop []

let to_string : ?style:Sexpm.encoder_style -> sexp list -> string
fun ses ->
  let b = Buffer.create 256 in
  let d = Sexpm.(encoder ?style @@ output_to_buffer b) in
  let rec loop = function
  | se :: ses -> Sexpm.encode_tagged d (Some se); loop ses
  | [] -> Sexpm.encode_tagged d None; Buffer.contents b
  in
  loop ss
]}
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
