(* The answer to GNU Stow

Copyright (C) 2005 Art Yerkes

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

*)

open Str
open Unix

type disp_flag_t = Concat | Nolink

type 'a file_c_t = { fc_package : string ;
		     fc_path : string ;
		     fc_flags : disp_flag_t list ;
		     fc_content : 'a }

type file_t = 
    Directory of file_t list file_c_t 
  | Symlink of string file_c_t 
  | File of unit file_c_t

type option_t = string * disp_flag_t list

type target_file_t = { t_name : string ; 
		       t_flags : disp_flag_t ; 
		       t_orig : file_t }

let read_dir_opt dirh () = try Some (readdir dirh) with _ -> None
let rec filter_dots g = 
  let gout = g () in
    match gout with
	Some "" -> filter_dots g 
      | Some n -> if n.[0] == '.' then filter_dots g else gout 
      | None -> gout
let read_dir_opt dirh () = filter_dots (read_dir_opt dirh)

let enlist gen =
  let rec enlist gen l =
    match gen () with None -> l | Some x -> enlist gen (x :: l) in
    List.rev (enlist gen [])

let names2paths dir names = List.map (fun x -> dir ^ "/" ^ x) names

let contains_char ch str = try ignore (String.index str ch) ; 
  true with _ -> false

let (++) x f = f x

let process_option str =
  let flname = Str.split (Str.regexp "[ \t]+") str in
    match flname with
	[ flags ; name ] -> 
	  let (@@) fl ch = if contains_char ch flags then [fl] else [] in
	    ((Concat @@ 'c') @ (Nolink @@ 'n'))
      | _ -> []

let lookup_source_flags f = []

let get_file_info reccal parent paths =
  List.map
    (fun f ->
       let st_info = lstat f in
	 match st_info.st_kind with
	     S_DIR -> 
	       Directory { fc_package = parent ;
			   fc_path = f ;
			   fc_flags = lookup_source_flags f ;
			   fc_content = reccal f }
	   | S_LNK ->
	       Symlink { fc_package = parent ;
			 fc_path = f ;
			 fc_flags = lookup_source_flags f ;
			 fc_content = readlink f }
	   | _ -> 
	       File { fc_package = parent ;
		      fc_path = f ;
		      fc_flags = lookup_source_flags f ;
		      fc_content = () })
    paths

let build_view_of_package pkg = 
  let rec build_view_of_package pkg =
    let dir = opendir pkg in
    let content = (read_dir_opt dir) ++ enlist ++ (names2paths pkg) ++ 
		  (get_file_info build_view_of_package pkg) in
      closedir dir ; content in
    List.hd (get_file_info build_view_of_package pkg [pkg])

let s2t pkg target path =
  let slpkg = String.length pkg 
  and pl = String.length path in
    target ^ (String.sub path slpkg (pl - slpkg))
  
(* Globals *)

let target = ref "/usr/local"
let just_kidding = ref false
	       
(* Filesystem operations *)

let type2tname t =
  match t with
      S_DIR -> "directory"
    | S_SOCK -> "socket"
    | S_REG -> "file"
    | S_LNK -> "link"
    | S_FIFO -> "fifo"
    | S_CHR -> "character device"
    | S_BLK -> "block device"

let safe_unlink s d =
  begin
    print_endline ("Attempting to remove " ^ d) ;
    (try 
       if (lstat d).st_kind == S_LNK then 
	 (if !just_kidding then 
	    print_endline ("Would delete " ^ d) 
	  else 
	    if readlink d = s then
	      (print_endline ("Removing " ^ d) ; Unix.unlink d)
	    else
	      print_endline ("Won't remove " ^ d ^ 
			     " because it doesn't point to " ^ s))
       else
	 print_endline ("Can't delete " ^ d ^ " because it's a " ^
			(type2tname (lstat d).st_kind))
     with _ -> print_endline ("Failed to lstat " ^ d)) ;
  end
  
let create_ifne src dir = 
  if !just_kidding then
    print_endline ("Would mkdir " ^ dir)
  else
    try ignore (lstat dir) with _ -> ignore 
      (safe_unlink src dir ; Unix.mkdir dir 0o755)
	       
(* link a package *)
	       
let rec link target pkg = 
  ("Link called on " ^ pkg ^ " with target " ^ target) ++ print_endline ;
  let pcont = build_view_of_package pkg 
  and safe_symlink s d =
    if !just_kidding then
	    print_endline ("Would unlink " ^ d ^ " to replace with " ^ 
			   s ^ " -> " ^ d)
    else
      begin 
	if try (lstat d).st_kind == S_LNK with _ -> false then
	  (unlink d ; symlink s d)
	else 
	  symlink s d
      end in
  let rec link pcont = 
    let target_path = s2t pkg target in
    match pcont with
	Directory d -> 
	  d.fc_path ++ target_path ++ (create_ifne d.fc_path) ;
	  List.iter link d.fc_content
      | Symlink s -> 
	  ("Processing symlink " ^ s.fc_path ^ " -> " ^
	   target_path s.fc_path) ++ print_endline ;
	  s.fc_path ++ target_path ++ (safe_symlink s.fc_path)
      | File f -> 
	  ("Processing file " ^ f.fc_path ^ " -> " ^
	   target_path f.fc_path) ++ print_endline ;
	  f.fc_path ++ target_path ++ (safe_symlink f.fc_path) in
    link pcont

(* Unlink a package *)
let unlink target pkg = 
  ("Unlink called on " ^ pkg ^ " with target " ^ target) ++ print_endline ;
  let pcont = build_view_of_package pkg in
  let rec unlink pcont = 
    let target_path = s2t pkg target in
      match pcont with
	  Directory d -> 
            List.iter unlink d.fc_content
	| Symlink s -> 
	    ("Removing target for symlink " ^ s.fc_path ^ " -> " ^ 
	     (target_path s.fc_path)) ++ print_endline ;
	    s.fc_path ++ target_path ++ (safe_unlink s.fc_path)
	| File f -> 
	    ("Removing target for file " ^ f.fc_path ^ " -> " ^ 
	     (target_path f.fc_path)) ++ print_endline ;
	    f.fc_path ++ target_path ++ (safe_unlink f.fc_path) in

    unlink pcont
   

let op = ref link
let set_target oldt newt = (target := newt ; op := link)

let main argv =
  for arg = 1 to (Array.length argv) - 1 do
    match argv.(arg) with
        "-t" -> op := set_target
      | "-u" -> op := unlink 
      | "-j" -> just_kidding := true
      | _ -> !op !target argv.(arg)
  done

let _ = main Sys.argv
