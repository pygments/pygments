(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: format.ml,v 1.65 2005/09/26 10:13:08 weis Exp $ *)

(**************************************************************

  Data structures definitions.

 **************************************************************)

type size;;

external size_of_int : int -> size = "%identity";;
external int_of_size : size -> int = "%identity";;

(* Tokens are one of the following : *)

type pp_token =
| Pp_text of string            (* normal text *)
| Pp_break of int * int        (* complete break *)
| Pp_tbreak of int * int       (* go to next tabulation *)
| Pp_stab                      (* set a tabulation *)
| Pp_begin of int * block_type (* beginning of a block *)
| Pp_end                       (* end of a block *)
| Pp_tbegin of tblock          (* beginning of a tabulation block *)
| Pp_tend                      (* end of a tabulation block *)
| Pp_newline                   (* to force a newline inside a block *)
| Pp_if_newline                (* to do something only if this very
                                  line has been broken *)
| Pp_open_tag of string        (* opening a tag name *)
| Pp_close_tag                 (* closing the most recently opened tag *)

and tag = string

and block_type =
| Pp_hbox   (* Horizontal block no line breaking *)
| Pp_vbox   (* Vertical block each break leads to a new line *)
| Pp_hvbox  (* Horizontal-vertical block: same as vbox, except if this block
               is small enough to fit on a single line *)
| Pp_hovbox (* Horizontal or Vertical block: breaks lead to new line
               only when necessary to print the content of the block *)
| Pp_box    (* Horizontal or Indent block: breaks lead to new line
               only when necessary to print the content of the block, or
               when it leads to a new indentation of the current line *)
| Pp_fits   (* Internal usage: when a block fits on a single line *)

and tblock = Pp_tbox of int list ref  (* Tabulation box *)
;;

(* The Queue:
   contains all formatting elements.
   elements are tuples (size, token, length), where
   size is set when the size of the block is known
   len is the declared length of the token. *)
type pp_queue_elem = {
  mutable elem_size : size; token : pp_token; length : int
};;

(* Scan stack:
   each element is (left_total, queue element) where left_total
   is the value of pp_left_total when the element has been enqueued. *)
type pp_scan_elem = Scan_elem of int * pp_queue_elem;;

(* Formatting stack:
   used to break the lines while printing tokens.
   The formatting stack contains the description of
   the currently active blocks. *)
type pp_format_elem = Format_elem of block_type * int;;

(* General purpose queues, used in the formatter. *)
type 'a queue_elem = | Nil | Cons of 'a queue_cell
and 'a queue_cell = {mutable head : 'a; mutable tail : 'a queue_elem};;

type 'a queue = {
 mutable insert : 'a queue_elem;
 mutable body : 'a queue_elem
};;

(* The formatter specific tag handling functions. *)
type formatter_tag_functions = {
 mark_open_tag : tag -> string;
 mark_close_tag : tag -> string;
 print_open_tag : tag -> unit;
 print_close_tag : tag -> unit;

};;

(* A formatter with all its machinery. *)
type formatter = {
 mutable pp_scan_stack : pp_scan_elem list;
 mutable pp_format_stack : pp_format_elem list;
 mutable pp_tbox_stack : tblock list;
 mutable pp_tag_stack : tag list;
 mutable pp_mark_stack : tag list;
 (* Global variables: default initialization is
    set_margin 78
    set_min_space_left 0. *)
 (* Value of right margin. *)
 mutable pp_margin : int;
 (* Minimal space left before margin, when opening a block. *)
 mutable pp_min_space_left : int;
 (* Maximum value of indentation:
    no blocks can be opened further. *)
 mutable pp_max_indent : int;
 (* Space remaining on the current line. *)
 mutable pp_space_left : int;
 (* Current value of indentation. *)
 mutable pp_current_indent : int;
 (* True when the line has been broken by the pretty-printer. *)
 mutable pp_is_new_line : bool;
 (* Total width of tokens already printed. *)
 mutable pp_left_total : int;
 (* Total width of tokens ever put in queue. *)
 mutable pp_right_total : int;
 (* Current number of opened blocks. *)
 mutable pp_curr_depth : int;
 (* Maximum number of blocks which can be simultaneously opened. *)
 mutable pp_max_boxes : int;
 (* Ellipsis string. *)
 mutable pp_ellipsis : string;
 (* Output function. *)
 mutable pp_output_function : string -> int -> int -> unit;
 (* Flushing function. *)
 mutable pp_flush_function : unit -> unit;
 (* Output of new lines. *)
 mutable pp_output_newline : unit -> unit;
 (* Output of indentation spaces. *)
 mutable pp_output_spaces : int -> unit;
 (* Are tags printed ? *)
 mutable pp_print_tags : bool;
 (* Are tags marked ? *)
 mutable pp_mark_tags : bool;
 (* Find opening and closing markers of tags. *)
 mutable pp_mark_open_tag : tag -> string;
 mutable pp_mark_close_tag : tag -> string;
 mutable pp_print_open_tag : tag -> unit;
 mutable pp_print_close_tag : tag -> unit;
 (* The pretty-printer queue. *)
 mutable pp_queue : pp_queue_elem queue
};;

(**************************************************************

  Auxilliaries and basic functions.

 **************************************************************)


(* Queues auxilliaries. *)
let make_queue () = {insert = Nil; body = Nil};;

let clear_queue q = q.insert <- Nil; q.body <- Nil;;

let add_queue x q =
 let c = Cons {head = x; tail = Nil} in
 match q with
 | {insert = Cons cell} -> q.insert <- c; cell.tail <- c
 (* Invariant: when insert is Nil body should be Nil. *)
 | _ -> q.insert <- c; q.body <- c;;

exception Empty_queue;;

let peek_queue = function
 | {body = Cons {head = x}} -> x
 | _ -> raise Empty_queue;;

let take_queue = function
 | {body = Cons {head = x; tail = tl}} as q ->
    q.body <- tl;
    if tl = Nil then q.insert <- Nil; (* Maintain the invariant. *)
    x
 | _ -> raise Empty_queue;;

(* Enter a token in the pretty-printer queue. *)
let pp_enqueue state ({length = len} as token) =
    state.pp_right_total <- state.pp_right_total + len;
    add_queue token state.pp_queue;;

let pp_clear_queue state =
    state.pp_left_total <- 1; state.pp_right_total <- 1;
    clear_queue state.pp_queue;;

(* Pp_infinity: large value for default tokens size.

   Pp_infinity is documented as being greater than 1e10; to avoid
   confusion about the word ``greater'', we choose pp_infinity greater
   than 1e10 + 1; for correct handling of tests in the algorithm,
   pp_infinity must be even one more than 1e10 + 1; let's stand on the
   safe side by choosing 1.e10+10.

   Pp_infinity could probably be 1073741823 that is 2^30 - 1, that is
   the minimal upper bound for integers; now that max_int is defined,
   this limit could also be defined as max_int - 1.

   However, before setting pp_infinity to something around max_int, we
   must carefully double-check all the integer arithmetic operations
   that involve pp_infinity, since any overflow would wreck havoc the
   pretty-printing algorithm's invariants. Given that this arithmetic
   correctness check is difficult and error prone and given that 1e10
   + 1 is in practice large enough, there is no need to attempt to set
   pp_infinity to the theoretically maximum limit. Is it not worth the
   burden ! *)

let pp_infinity = 1000000010;;

(* Output functions for the formatter. *)
let pp_output_string state s = state.pp_output_function s 0 (String.length s)
and pp_output_newline state = state.pp_output_newline ();;

let pp_display_blanks state n = state.pp_output_spaces n;;

(* To format a break, indenting a new line. *)
let break_new_line state offset width =
    pp_output_newline state;
    state.pp_is_new_line <- true;
    let indent = state.pp_margin - width + offset in
    (* Don't indent more than pp_max_indent. *)
    let real_indent = min state.pp_max_indent indent in
    state.pp_current_indent <- real_indent;
    state.pp_space_left <- state.pp_margin - state.pp_current_indent;
    pp_display_blanks state state.pp_current_indent;;

(* To force a line break inside a block: no offset is added. *)
let break_line state width = break_new_line state 0 width;;

(* To format a break that fits on the current line. *)
let break_same_line state width =
    state.pp_space_left <- state.pp_space_left - width;
    pp_display_blanks state width;;

(* To indent no more than pp_max_indent, if one tries to open a block
   beyond pp_max_indent, then the block is rejected on the left
   by simulating a break. *)
let pp_force_break_line state =
    match state.pp_format_stack with
    | Format_elem (bl_ty, width) :: _ ->
        if width > state.pp_space_left then
         (match bl_ty with
          | Pp_fits -> () | Pp_hbox -> () | _ -> break_line state width)
    | _ -> pp_output_newline state;;

(* To skip a token, if the previous line has been broken. *)
let pp_skip_token state =
    (* When calling pp_skip_token the queue cannot be empty. *)
    match take_queue state.pp_queue with
    {elem_size = size; length = len} ->
       state.pp_left_total <- state.pp_left_total - len;
       state.pp_space_left <- state.pp_space_left + int_of_size size;;

(**************************************************************

  The main pretting printing functions.

 **************************************************************)

(* To format a token. *)
let format_pp_token state size = function

  | Pp_text s ->
      state.pp_space_left <- state.pp_space_left - size;
      pp_output_string state s;
      state.pp_is_new_line <- false

  | Pp_begin (off, ty) ->
      let insertion_point = state.pp_margin - state.pp_space_left in
      if insertion_point > state.pp_max_indent then
         (* can't open a block right there. *)
         begin pp_force_break_line state end;
      let offset = state.pp_space_left - off in
      let bl_type =
       begin match ty with
        | Pp_vbox -> Pp_vbox
        | _ -> if size > state.pp_space_left then ty else Pp_fits
       end in
       state.pp_format_stack <-
        Format_elem (bl_type, offset) :: state.pp_format_stack

  | Pp_end ->
      begin match state.pp_format_stack with
        | x :: (y :: l as ls) -> state.pp_format_stack <- ls
        | _ -> () (* No more block to close. *)
      end

  | Pp_tbegin (Pp_tbox _ as tbox) ->
      state.pp_tbox_stack <- tbox :: state.pp_tbox_stack

  | Pp_tend ->
      begin match state.pp_tbox_stack with
        | x :: ls -> state.pp_tbox_stack <- ls
        | _ -> () (* No more tabulation block to close. *)
      end

  | Pp_stab ->
     begin match state.pp_tbox_stack with
     | Pp_tbox tabs :: _ ->
        let rec add_tab n = function
          | [] -> [n]
          | x :: l as ls -> if n < x then n :: ls else x :: add_tab n l in
        tabs := add_tab (state.pp_margin - state.pp_space_left) !tabs
     | _ -> () (* No opened tabulation block. *)
     end

  | Pp_tbreak (n, off) ->
      let insertion_point = state.pp_margin - state.pp_space_left in
      begin match state.pp_tbox_stack with
      | Pp_tbox tabs :: _ ->
         let rec find n = function
           | x :: l -> if x >= n then x else find n l
           | [] -> raise Not_found in
         let tab =
             match !tabs with
             | x :: l ->
                begin try find insertion_point !tabs with Not_found -> x end
             | _ -> insertion_point in
         let offset = tab - insertion_point in
         if offset >= 0 then break_same_line state (offset + n) else
          break_new_line state (tab + off) state.pp_margin
      | _ -> () (* No opened tabulation block. *)
      end

  | Pp_newline ->
     begin match state.pp_format_stack with
     | Format_elem (_, width) :: _ -> break_line state width
     | _ -> pp_output_newline state
     end

  | Pp_if_newline ->
     if state.pp_current_indent != state.pp_margin - state.pp_space_left
     then pp_skip_token state

  | Pp_break (n, off) ->
     begin match state.pp_format_stack with
     | Format_elem (ty, width) :: _ ->
        begin match ty with
        | Pp_hovbox ->
           if size > state.pp_space_left
           then break_new_line state off width
           else break_same_line state n
        | Pp_box ->
           (* Have the line just been broken here ? *)
           if state.pp_is_new_line then break_same_line state n else
           if size > state.pp_space_left
            then break_new_line state off width else
           (* break the line here leads to new indentation ? *)
           if state.pp_current_indent > state.pp_margin - width + off
           then break_new_line state off width
           else break_same_line state n
        | Pp_hvbox -> break_new_line state off width
        | Pp_fits -> break_same_line state n
        | Pp_vbox -> break_new_line state off width
        | Pp_hbox -> break_same_line state n
        end
     | _ -> () (* No opened block. *)
     end

   | Pp_open_tag tag_name ->
      let marker = state.pp_mark_open_tag tag_name in
      pp_output_string state marker;
      state.pp_mark_stack <- tag_name :: state.pp_mark_stack

   | Pp_close_tag ->
      begin match state.pp_mark_stack with
      | tag_name :: tags ->
          let marker = state.pp_mark_close_tag tag_name in
          pp_output_string state marker;
          state.pp_mark_stack <- tags
      | _ -> () (* No more tag to close. *)
      end;;

(* Print if token size is known or printing is delayed.
   Size is known when not negative.
   Printing is delayed when the text waiting in the queue requires
   more room to format than exists on the current line. *)
let rec advance_left state =
    try
     match peek_queue state.pp_queue with
      {elem_size = size; token = tok; length = len} ->
       let size = int_of_size size in
       if not
        (size < 0 &&
         (state.pp_right_total - state.pp_left_total < state.pp_space_left))
        then begin
         ignore(take_queue state.pp_queue);
         format_pp_token state (if size < 0 then pp_infinity else size) tok;
         state.pp_left_total <- len + state.pp_left_total;
         advance_left state
        end
    with Empty_queue -> ();;

let enqueue_advance state tok = pp_enqueue state tok; advance_left state;;

(* To enqueue a string : try to advance. *)
let make_queue_elem size tok len =
 {elem_size = size; token = tok; length = len};;

let enqueue_string_as state size s =
  let len = int_of_size size in
  enqueue_advance state (make_queue_elem size (Pp_text s) len);;

let enqueue_string state s =
  let len = String.length s in
  enqueue_string_as state (size_of_int len) s;;

(* Routines for scan stack
   determine sizes of blocks. *)

(* The scan_stack is never empty. *)
let scan_stack_bottom =
  let q_elem = make_queue_elem (size_of_int (-1)) (Pp_text "") 0 in
  [Scan_elem (-1, q_elem)];;

(* Set size of blocks on scan stack:
   if ty = true then size of break is set else size of block is set;
   in each case pp_scan_stack is popped. *)
let clear_scan_stack state = state.pp_scan_stack <- scan_stack_bottom;;

(* Pattern matching on scan stack is exhaustive,
   since scan_stack is never empty.
   Pattern matching on token in scan stack is also exhaustive,
   since scan_push is used on breaks and opening of boxes. *)
let set_size state ty =
    match state.pp_scan_stack with
    | Scan_elem
        (left_tot,
         ({elem_size = size; token = tok} as queue_elem)) :: t ->
       let size = int_of_size size in
       (* test if scan stack contains any data that is not obsolete. *)
       if left_tot < state.pp_left_total then clear_scan_stack state else
        begin match tok with
        | Pp_break (_, _) | Pp_tbreak (_, _) ->
           if ty then
            begin
             queue_elem.elem_size <- size_of_int (state.pp_right_total + size);
             state.pp_scan_stack <- t
            end
        | Pp_begin (_, _) ->
           if not ty then
            begin
             queue_elem.elem_size <- size_of_int (state.pp_right_total + size);
             state.pp_scan_stack <- t
            end
        | _ -> () (* scan_push is only used for breaks and boxes. *)
        end
    | _ -> () (* scan_stack is never empty. *);;

(* Push a token on scan stack. If b is true set_size is called. *)
let scan_push state b tok =
    pp_enqueue state tok;
    if b then set_size state true;
    state.pp_scan_stack <-
     Scan_elem (state.pp_right_total, tok) :: state.pp_scan_stack;;

(* To open a new block :
   the user may set the depth bound pp_max_boxes
   any text nested deeper is printed as the ellipsis string. *)
let pp_open_box_gen state indent br_ty =
    state.pp_curr_depth <- state.pp_curr_depth + 1;
    if state.pp_curr_depth < state.pp_max_boxes then
      let elem =
        make_queue_elem
          (size_of_int (- state.pp_right_total))
          (Pp_begin (indent, br_ty))
          0 in
      scan_push state false elem else
    if state.pp_curr_depth = state.pp_max_boxes
    then enqueue_string state state.pp_ellipsis;;

(* The box which is always opened. *)
let pp_open_sys_box state = pp_open_box_gen state 0 Pp_hovbox;;

(* Close a block, setting sizes of its subblocks. *)
let pp_close_box state () =
    if state.pp_curr_depth > 1 then
     begin
      if state.pp_curr_depth < state.pp_max_boxes then
       begin
        pp_enqueue state
          {elem_size = size_of_int 0; token = Pp_end; length = 0};
        set_size state true; set_size state false
       end;
      state.pp_curr_depth <- state.pp_curr_depth - 1;
     end;;

(* Open a tag, pushing it on the tag stack. *)
let pp_open_tag state tag_name =
    if state.pp_print_tags then begin
      state.pp_tag_stack <- tag_name :: state.pp_tag_stack;
      state.pp_print_open_tag tag_name end;
    if state.pp_mark_tags then
      pp_enqueue state
        {elem_size = size_of_int 0; token = Pp_open_tag tag_name; length = 0};;

(* Close a tag, popping it from the tag stack. *)
let pp_close_tag state () =
    if state.pp_mark_tags then
      pp_enqueue state
        {elem_size = size_of_int 0; token = Pp_close_tag; length = 0};
    if state.pp_print_tags then
      begin match state.pp_tag_stack with
      | tag_name :: tags ->
          state.pp_print_close_tag tag_name;
          state.pp_tag_stack <- tags
      | _ -> () (* No more tag to close. *)
      end;;

let pp_set_print_tags state b = state.pp_print_tags <- b;;
let pp_set_mark_tags state b = state.pp_mark_tags <- b;;
let pp_get_print_tags state () = state.pp_print_tags;;
let pp_get_mark_tags state () = state.pp_mark_tags;;
let pp_set_tags state b = pp_set_print_tags state b; pp_set_mark_tags state b;;

let pp_get_formatter_tag_functions state () = {
   mark_open_tag = state.pp_mark_open_tag;
   mark_close_tag = state.pp_mark_close_tag;
   print_open_tag = state.pp_print_open_tag;
   print_close_tag = state.pp_print_close_tag;
};;

let pp_set_formatter_tag_functions state {
     mark_open_tag = mot;
     mark_close_tag = mct;
     print_open_tag = pot;
     print_close_tag = pct;
  } =
   state.pp_mark_open_tag <- mot;
   state.pp_mark_close_tag <- mct;
   state.pp_print_open_tag <- pot;
   state.pp_print_close_tag <- pct;;

(* Initialize pretty-printer. *)
let pp_rinit state =
    pp_clear_queue state;
    clear_scan_stack state;
    state.pp_format_stack <- [];
    state.pp_tbox_stack <- [];
    state.pp_tag_stack <- [];
    state.pp_mark_stack <- [];
    state.pp_current_indent <- 0;
    state.pp_curr_depth <- 0;
    state.pp_space_left <- state.pp_margin;
    pp_open_sys_box state;;

(* Flushing pretty-printer queue. *)
let pp_flush_queue state b =
    while state.pp_curr_depth > 1 do
     pp_close_box state ()
    done;
    state.pp_right_total <- pp_infinity;
    advance_left state;
    if b then pp_output_newline state;
    pp_rinit state;;

(**************************************************************

  Procedures to format objects, and use boxes

 **************************************************************)

(* To format a string. *)
let pp_print_as_size state size s =
  if state.pp_curr_depth < state.pp_max_boxes
  then enqueue_string_as state size s;;

let pp_print_as state isize s =
  pp_print_as_size state (size_of_int isize) s;;

let pp_print_string state s =
  pp_print_as state (String.length s) s;;

(* To format an integer. *)
let pp_print_int state i = pp_print_string state (string_of_int i);;

(* To format a float. *)
let pp_print_float state f = pp_print_string state (string_of_float f);;

(* To format a boolean. *)
let pp_print_bool state b = pp_print_string state (string_of_bool b);;

(* To format a char. *)
let pp_print_char state c =
  let s = String.create 1 in
  s.[0] <- c;
  pp_print_as state 1 s;;

(* Opening boxes. *)
let pp_open_hbox state () = pp_open_box_gen state 0 Pp_hbox
and pp_open_vbox state indent = pp_open_box_gen state indent Pp_vbox

and pp_open_hvbox state indent = pp_open_box_gen state indent Pp_hvbox
and pp_open_hovbox state indent = pp_open_box_gen state indent Pp_hovbox
and pp_open_box state indent = pp_open_box_gen state indent Pp_box;;

(* Print a new line after printing all queued text
   (same for print_flush but without a newline). *)
let pp_print_newline state () =
    pp_flush_queue state true; state.pp_flush_function ()
and pp_print_flush state () =
    pp_flush_queue state false; state.pp_flush_function ();;

(* To get a newline when one does not want to close the current block. *)
let pp_force_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state (make_queue_elem (size_of_int 0) Pp_newline 0);;

(* To format something if the line has just been broken. *)
let pp_print_if_newline state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    enqueue_advance state (make_queue_elem (size_of_int 0) Pp_if_newline 0);;

(* Breaks: indicate where a block may be broken.
   If line is broken then offset is added to the indentation of the current
   block else (the value of) width blanks are printed.
   To do (?) : add a maximum width and offset value. *)
let pp_print_break state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem
        (size_of_int (- state.pp_right_total))
        (Pp_break (width, offset))
        width in
    scan_push state true elem;;

let pp_print_space state () = pp_print_break state 1 0
and pp_print_cut state () = pp_print_break state 0 0;;

(* Tabulation boxes. *)
let pp_open_tbox state () =
  state.pp_curr_depth <- state.pp_curr_depth + 1;
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem (size_of_int 0) (Pp_tbegin (Pp_tbox (ref []))) 0 in
    enqueue_advance state elem;;

(* Close a tabulation block. *)
let pp_close_tbox state () =
  if state.pp_curr_depth > 1 then begin
   if state.pp_curr_depth < state.pp_max_boxes then
     let elem = make_queue_elem (size_of_int 0) Pp_tend 0 in
     enqueue_advance state elem;
     state.pp_curr_depth <- state.pp_curr_depth - 1 end;;

(* Print a tabulation break. *)
let pp_print_tbreak state width offset =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem
        (size_of_int (- state.pp_right_total))
        (Pp_tbreak (width, offset))
        width in
    scan_push state true elem;;

let pp_print_tab state () = pp_print_tbreak state 0 0;;

let pp_set_tab state () =
  if state.pp_curr_depth < state.pp_max_boxes then
    let elem =
      make_queue_elem (size_of_int 0) Pp_stab 0 in
    enqueue_advance state elem;;

(**************************************************************

  Procedures to control the pretty-printers

 **************************************************************)

(* Fit max_boxes. *)
let pp_set_max_boxes state n = if n > 1 then state.pp_max_boxes <- n;;

(* To know the current maximum number of boxes allowed. *)
let pp_get_max_boxes state () = state.pp_max_boxes;;

let pp_over_max_boxes state () = state.pp_curr_depth = state.pp_max_boxes;;

(* Ellipsis. *)
let pp_set_ellipsis_text state s = state.pp_ellipsis <- s
and pp_get_ellipsis_text state () = state.pp_ellipsis;;

(* To set the margin of pretty-printer. *)
let pp_limit n =
  if n < pp_infinity then n else pred pp_infinity;;

let pp_set_min_space_left state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_min_space_left <- n;
    state.pp_max_indent <- state.pp_margin - state.pp_min_space_left;
    pp_rinit state;;

(* Initially, we have :
  pp_max_indent = pp_margin - pp_min_space_left, and
  pp_space_left = pp_margin. *)
let pp_set_max_indent state n =
  pp_set_min_space_left state (state.pp_margin - n);;
let pp_get_max_indent state () = state.pp_max_indent;;

let pp_set_margin state n =
  if n >= 1 then
    let n = pp_limit n in
    state.pp_margin <- n;
    let new_max_indent =
        (* Try to maintain max_indent to its actual value. *)
        if state.pp_max_indent <= state.pp_margin
        then state.pp_max_indent else
        (* If possible maintain pp_min_space_left to its actual value,
           if this leads to a too small max_indent, take half of the
           new margin, if it is greater than 1. *)
         max (max (state.pp_margin - state.pp_min_space_left)
                  (state.pp_margin / 2)) 1 in
    (* Rebuild invariants. *)
    pp_set_max_indent state new_max_indent;;

let pp_get_margin state () = state.pp_margin;;

let pp_set_formatter_output_functions state f g =
  state.pp_output_function <- f; state.pp_flush_function <- g;;
let pp_get_formatter_output_functions state () =
  (state.pp_output_function, state.pp_flush_function);;

let pp_set_all_formatter_output_functions state
    ~out:f ~flush:g ~newline:h ~spaces:i =
  pp_set_formatter_output_functions state f g;
  state.pp_output_newline <- (function () -> h ());
  state.pp_output_spaces <- (function n -> i n);;
let pp_get_all_formatter_output_functions state () =
  (state.pp_output_function, state.pp_flush_function,
   state.pp_output_newline, state.pp_output_spaces);;

let pp_set_formatter_out_channel state os =
  state.pp_output_function <- output os;
  state.pp_flush_function <- (fun () -> flush os);;

(**************************************************************

  Creation of specific formatters

 **************************************************************)

let default_pp_mark_open_tag s = "<" ^ s ^ ">";;
let default_pp_mark_close_tag s = "</" ^ s ^ ">";;

let default_pp_print_open_tag s = ();;
let default_pp_print_close_tag = default_pp_print_open_tag;;

let pp_make_formatter f g h i =
 (* The initial state of the formatter contains a dummy box. *)
 let pp_q = make_queue () in
 let sys_tok =
   make_queue_elem (size_of_int (-1)) (Pp_begin (0, Pp_hovbox)) 0 in
 add_queue sys_tok pp_q;
 let sys_scan_stack =
     (Scan_elem (1, sys_tok)) :: scan_stack_bottom in
 {pp_scan_stack = sys_scan_stack;
  pp_format_stack = [];
  pp_tbox_stack = [];
  pp_tag_stack = [];
  pp_mark_stack = [];
  pp_margin = 78;
  pp_min_space_left = 10;
  pp_max_indent = 78 - 10;
  pp_space_left = 78;
  pp_current_indent = 0;
  pp_is_new_line = true;
  pp_left_total = 1;
  pp_right_total = 1;
  pp_curr_depth = 1;
  pp_max_boxes = max_int;
  pp_ellipsis = ".";
  pp_output_function = f;
  pp_flush_function = g;
  pp_output_newline = h;
  pp_output_spaces = i;
  pp_print_tags = false;
  pp_mark_tags = false;
  pp_mark_open_tag = default_pp_mark_open_tag;
  pp_mark_close_tag = default_pp_mark_close_tag;
  pp_print_open_tag = default_pp_print_open_tag;
  pp_print_close_tag = default_pp_print_close_tag;
  pp_queue = pp_q
 };;

(* Default function to output spaces. *)
let blank_line = String.make 80 ' ';;
let rec display_blanks state n =
    if n > 0 then
    if n <= 80 then state.pp_output_function blank_line 0 n else
     begin
      state.pp_output_function blank_line 0 80;
      display_blanks state (n - 80)
     end;;

(* Default function to output new lines. *)
let display_newline state () = state.pp_output_function "\n" 0  1;;

let make_formatter f g =
  let ff = pp_make_formatter f g ignore ignore in
  ff.pp_output_newline <- display_newline ff;
  ff.pp_output_spaces <- display_blanks ff;
  ff;;

let formatter_of_out_channel oc =
  make_formatter (output oc) (fun () -> flush oc);;

let formatter_of_buffer b =
  make_formatter (Buffer.add_substring b) ignore;;

let stdbuf = Buffer.create 512;;

let str_formatter = formatter_of_buffer stdbuf;;
let std_formatter = formatter_of_out_channel stdout;;
let err_formatter = formatter_of_out_channel stderr;;

let flush_str_formatter () =
  pp_flush_queue str_formatter false;
  let s = Buffer.contents stdbuf in
  Buffer.reset stdbuf;
  s;;

(**************************************************************

  Basic functions on the standard formatter

 **************************************************************)

let open_hbox = pp_open_hbox std_formatter
and open_vbox = pp_open_vbox std_formatter
and open_hvbox = pp_open_hvbox std_formatter
and open_hovbox = pp_open_hovbox std_formatter
and open_box = pp_open_box std_formatter
and close_box = pp_close_box std_formatter
and open_tag = pp_open_tag std_formatter
and close_tag = pp_close_tag std_formatter
and print_as = pp_print_as std_formatter
and print_string = pp_print_string std_formatter
and print_int = pp_print_int std_formatter
and print_float = pp_print_float std_formatter
and print_char = pp_print_char std_formatter
and print_bool = pp_print_bool std_formatter
and print_break = pp_print_break std_formatter
and print_cut = pp_print_cut std_formatter
and print_space = pp_print_space std_formatter
and force_newline = pp_force_newline std_formatter
and print_flush = pp_print_flush std_formatter
and print_newline = pp_print_newline std_formatter
and print_if_newline = pp_print_if_newline std_formatter

and open_tbox = pp_open_tbox std_formatter
and close_tbox = pp_close_tbox std_formatter
and print_tbreak = pp_print_tbreak std_formatter

and set_tab = pp_set_tab std_formatter
and print_tab = pp_print_tab std_formatter

and set_margin = pp_set_margin std_formatter
and get_margin = pp_get_margin std_formatter

and set_max_indent = pp_set_max_indent std_formatter
and get_max_indent = pp_get_max_indent std_formatter

and set_max_boxes = pp_set_max_boxes std_formatter
and get_max_boxes = pp_get_max_boxes std_formatter
and over_max_boxes = pp_over_max_boxes std_formatter

and set_ellipsis_text = pp_set_ellipsis_text std_formatter
and get_ellipsis_text = pp_get_ellipsis_text std_formatter

and set_formatter_out_channel =
    pp_set_formatter_out_channel std_formatter

and set_formatter_output_functions =
    pp_set_formatter_output_functions std_formatter
and get_formatter_output_functions =
    pp_get_formatter_output_functions std_formatter

and set_all_formatter_output_functions =
    pp_set_all_formatter_output_functions std_formatter
and get_all_formatter_output_functions =
    pp_get_all_formatter_output_functions std_formatter

and set_formatter_tag_functions =
    pp_set_formatter_tag_functions std_formatter
and get_formatter_tag_functions =
    pp_get_formatter_tag_functions std_formatter
and set_print_tags =
    pp_set_print_tags std_formatter
and get_print_tags =
    pp_get_print_tags std_formatter
and set_mark_tags =
    pp_set_mark_tags std_formatter
and get_mark_tags =
    pp_get_mark_tags std_formatter
and set_tags =
    pp_set_tags std_formatter
;;


(**************************************************************

  Printf implementation.

 **************************************************************)

(* Error messages when processing formats. *)

(* Trailer: giving up at character number ... *)
let giving_up mess fmt i =
  "fprintf: " ^ mess ^ " ``" ^ fmt ^ "'', \
   giving up at character number " ^ string_of_int i ^
  (if i < String.length fmt
   then " (" ^ String.make 1 fmt.[i] ^ ")."
   else String.make 1 '.');;

(* When an invalid format deserves a special error explanation. *)
let format_invalid_arg mess fmt i = invalid_arg (giving_up mess fmt i);;

(* Standard invalid format. *)
let invalid_format fmt i = format_invalid_arg "bad format" fmt i;;

(* Cannot find a valid integer into that format. *)
let invalid_integer fmt i =
  invalid_arg (giving_up "bad integer specification" fmt i);;

(* Finding an integer out of a sub-string of the format. *)
let format_int_of_string fmt i s =
  let sz =
    try int_of_string s with
    | Failure s -> invalid_integer fmt i in
  size_of_int sz;;

(* Getting strings out of buffers. *)
let get_buffer_out b =
 let s = Buffer.contents b in
 Buffer.reset b;
 s;;

(* [ppf] is supposed to be a pretty-printer that outputs in buffer [b]:
   to extract contents of [ppf] as a string we flush [ppf] and get the string
   out of [b]. *)
let string_out b ppf =
 pp_flush_queue ppf false;
 get_buffer_out b;;

(* Applies [printer] to a formatter that outputs on a fresh buffer,
   then returns the resulting material. *)
let exstring printer arg =
 let b = Buffer.create 512 in
 let ppf = formatter_of_buffer b in
 printer ppf arg;
 string_out b ppf;;

(* To turn out a character accumulator into the proper string result. *)
let implode_rev s0 = function
  | [] -> s0
  | l -> String.concat "" (List.rev (s0 :: l));;

external format_to_string : ('a, 'b, 'c, 'd) format4 -> string = "%identity";;

(* [fprintf_out] is the printf-like function generator: given the
   - [str] flag that tells if we are printing into a string,
   - the [out] function that has to be called at the end of formatting,
   it generates a [fprintf] function that takes as arguments a [ppf]
   formatter and a printing format to print the rest of arguments
   according to the format.
   Regular [fprintf]-like functions of this module are obtained via partial
   applications of [fprintf_out]. *)
let mkprintf str get_out =
  let rec kprintf k fmt =
    let fmt = format_to_string fmt in
    let len = String.length fmt in

    let kpr fmt v =
      let ppf = get_out fmt in
      let print_as = ref None in
      let pp_print_as_char c =
          match !print_as with
          | None -> pp_print_char ppf c
          | Some size ->
             pp_print_as_size ppf size (String.make 1 c);
             print_as := None
      and pp_print_as_string s =
          match !print_as with
          | None -> pp_print_string ppf s
          | Some size ->
             pp_print_as_size ppf size s;
             print_as := None in

      let rec doprn n i =
        if i >= len then Obj.magic (k ppf) else
        match fmt.[i] with
        | '%' ->
            Printf.scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
        | '@' ->
            let i = succ i in
            if i >= len then invalid_format fmt i else
            begin match fmt.[i] with
            | '[' ->
               do_pp_open_box ppf n (succ i)
            | ']' ->
               pp_close_box ppf ();
               doprn n (succ i)
            | '{' ->
               do_pp_open_tag ppf n (succ i)
            | '}' ->
               pp_close_tag ppf ();
               doprn n (succ i)
            | ' ' ->
               pp_print_space ppf ();
               doprn n (succ i)
            | ',' ->
               pp_print_cut ppf ();
               doprn n (succ i)
            | '?' ->
               pp_print_flush ppf ();
               doprn n (succ i)
            | '.' ->
               pp_print_newline ppf ();
               doprn n (succ i)
            | '\n' ->
               pp_force_newline ppf ();
               doprn n (succ i)
            | ';' ->
               do_pp_break ppf n (succ i)
            | '<' ->
               let got_size size n i =
                 print_as := Some size;
                 doprn n (skip_gt i) in
               get_int n (succ i) got_size
            | '@' as c ->
               pp_print_as_char c;
               doprn n (succ i)
            | c -> invalid_format fmt i
            end
        | c ->
           pp_print_as_char c;
           doprn n (succ i)

      and cont_s n s i =
        pp_print_as_string s; doprn n i
      and cont_a n printer arg i =
        if str then
          pp_print_as_string ((Obj.magic printer : unit -> _ -> string) () arg)
        else
          printer ppf arg;
        doprn n i
      and cont_t n printer i =
        if str then
          pp_print_as_string ((Obj.magic printer : unit -> string) ())
        else
          printer ppf;
        doprn n i
      and cont_f n i =
        pp_print_flush ppf (); doprn n i

      and cont_m n sfmt i =
        kprintf (Obj.magic (fun _ -> doprn n i)) sfmt

      and get_int n i c =
       if i >= len then invalid_integer fmt i else
       match fmt.[i] with
       | ' ' -> get_int n (succ i) c
       | '%' ->
          let cont_s n s i = c (format_int_of_string fmt i s) n i
          and cont_a n printer arg i = invalid_integer fmt i
          and cont_t n printer i = invalid_integer fmt i
          and cont_f n i = invalid_integer fmt i
          and cont_m n sfmt i = invalid_integer fmt i in
          Printf.scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
       | _ ->
          let rec get j =
           if j >= len then invalid_integer fmt j else
           match fmt.[j] with
           | '0' .. '9' | '-' -> get (succ j)
           | _ ->
             let size =
             if j = i then size_of_int 0 else
                format_int_of_string fmt j (String.sub fmt i (j - i)) in
             c size n j in
          get i

      and skip_gt i =
       if i >= len then invalid_format fmt i else
       match fmt.[i] with
       | ' ' -> skip_gt (succ i)
       | '>' -> succ i
       | _ -> invalid_format fmt i

      and get_box_kind i =
       if i >= len then Pp_box, i else
       match fmt.[i] with
       | 'h' ->
          let i = succ i in
          if i >= len then Pp_hbox, i else
          begin match fmt.[i] with
          | 'o' ->
             let i = succ i in
             if i >= len then format_invalid_arg "bad box format" fmt i else
             begin match fmt.[i] with
             | 'v' -> Pp_hovbox, succ i
             | c ->
                format_invalid_arg
                  ("bad box name ho" ^ String.make 1 c) fmt i end
          | 'v' -> Pp_hvbox, succ i
          | c -> Pp_hbox, i
          end
       | 'b' -> Pp_box, succ i
       | 'v' -> Pp_vbox, succ i
       | _ -> Pp_box, i

      and get_tag_name n i c =
       let rec get accu n i j =
        if j >= len
        then c (implode_rev (String.sub fmt i (j - i)) accu) n j else
        match fmt.[j] with
        | '>' -> c (implode_rev (String.sub fmt i (j - i)) accu) n j
        | '%' ->
          let s0 = String.sub fmt i (j - i) in
          let cont_s n s i = get (s :: s0 :: accu) n i i
          and cont_a n printer arg i =
            let s =
              if str
              then (Obj.magic printer : unit -> _ -> string) () arg
              else exstring printer arg in
            get (s :: s0 :: accu) n i i
          and cont_t n printer i =
            let s =
              if str
              then (Obj.magic printer : unit -> string) ()
              else exstring (fun ppf () -> printer ppf) () in
            get (s :: s0 :: accu) n i i
          and cont_f n i =
            format_invalid_arg "bad tag name specification" fmt i
          and cont_m n sfmt i =
            format_invalid_arg "bad tag name specification" fmt i in
          Printf.scan_format fmt v n j cont_s cont_a cont_t cont_f cont_m
        | c -> get accu n i (succ j) in
       get [] n i i

      and do_pp_break ppf n i =
       if i >= len then begin pp_print_space ppf (); doprn n i end else
       match fmt.[i] with
       | '<' ->
          let rec got_nspaces nspaces n i =
            get_int n i (got_offset nspaces)
          and got_offset nspaces offset n i =
            pp_print_break ppf (int_of_size nspaces) (int_of_size offset);
            doprn n (skip_gt i) in
          get_int n (succ i) got_nspaces
       | c -> pp_print_space ppf (); doprn n i

      and do_pp_open_box ppf n i =
       if i >= len then begin pp_open_box_gen ppf 0 Pp_box; doprn n i end else
       match fmt.[i] with
       | '<' ->
          let kind, i = get_box_kind (succ i) in
          let got_size size n i =
            pp_open_box_gen ppf (int_of_size size) kind;
            doprn n (skip_gt i) in
          get_int n i got_size
       | c -> pp_open_box_gen ppf 0 Pp_box; doprn n i

      and do_pp_open_tag ppf n i =
       if i >= len then begin pp_open_tag ppf ""; doprn n i end else
       match fmt.[i] with
       | '<' ->
          let got_name tag_name n i =
            pp_open_tag ppf tag_name;
            doprn n (skip_gt i) in
          get_tag_name n (succ i) got_name
       | c -> pp_open_tag ppf ""; doprn n i in

      doprn (Printf.index_of_int 0) 0 in

   Printf.kapr kpr fmt in

  kprintf;;

(**************************************************************

  Defining [fprintf] and various flavors of [fprintf].

 **************************************************************)

let kfprintf k ppf = mkprintf false (fun _ -> ppf) k;;

let fprintf ppf = kfprintf ignore ppf;;
let printf fmt = fprintf std_formatter fmt;;
let eprintf fmt = fprintf err_formatter fmt;;

let kbprintf k b =
  mkprintf false (fun _ -> formatter_of_buffer b) k;;

let bprintf b = kbprintf ignore b;;

let ksprintf k =
  let b = Buffer.create 512 in
  let k ppf = k (string_out b ppf) in
  mkprintf true (fun _ -> formatter_of_buffer b) k;;

let kprintf = ksprintf;;

let sprintf fmt = ksprintf (fun s -> s) fmt;;

at_exit print_flush;;
