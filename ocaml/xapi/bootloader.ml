(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* TODO:
   1. Modify pygrub to extract all possible boot options
   2. Parse the results into some kind of option list
   3. Ensure all our guests have complete grub menu.lst (no hacks please!)
   4. Add support to control a slave screen process, to make a 'bios'
*)

open Stringext
open Pervasiveext
open Forkhelpers

module D=Debug.Debugger(struct let name="bootloader" end)
open D


exception Error of string

(** Helper function to generate a bootloader commandline *)
let bootloader_args q extra_args legacy_args pv_bootloader_args image vm_uuid = 
  (* Let's not do anything fancy while parsing the pv_bootloader_args string:
     no escaping of spaces or quotes for now *)
  let pv_bootloader_args = if pv_bootloader_args = "" then [] else String.split ' ' pv_bootloader_args in

  let rules = [ '"', "\\\""; '\\', "\\\\" ] in
  [ if q then "-q" else "";
    Printf.sprintf "--default_args=%s" (String.escaped ~rules legacy_args);
    Printf.sprintf "--extra_args=%s" (String.escaped ~rules extra_args);
    Printf.sprintf "--vm=%s" vm_uuid;
  ] @ pv_bootloader_args @ [
    image ]

(** Parsed representation of bootloader's stdout, as used by xend (XXX: need HVM) *)
type extracted_kernel = {
  kernel_path: string;
  initrd_path: string option;
  kernel_args: string;
}

(* The string to parse comes from eliloader or pygrub, which builds it based on
 * reading and processing the grub configuration from the guest's disc.
 * Therefore it may contain malicious content from the guest if pygrub has not
 * cleaned it up sufficiently. *)
(* Example of a valid three-line string to parse, with blank third line:
 * kernel <kernel:/vmlinuz-2.6.18-412.el5xen>
 * args ro root=/dev/VolGroup00/LogVol00 console=ttyS0,115200n8
 *
 *)
type acc_t = {kernel: string option; ramdisk: string option; args: string option}
let parse_output_simple x =
  let parse_line_optimistic acc l =
    (* String.index will raise Not_found on the empty line that pygrub includes
     * at the end of its simple-format output. *)
    let space_pos = String.index l ' ' in
    let first_word = String.sub l 0 space_pos in
    let pos = space_pos + 1 in
	match first_word with
      | "kernel" -> (
        match acc.kernel with
          | Some _ -> raise (Bad_error ("More than one kernel line when parsing bootloader result: "^x))
          | None ->
            debug "Using kernel line from bootloader output: %s" l;
            {acc with kernel = Some (String.sub l pos (String.length l - pos))} )
      | "ramdisk" -> (
        match acc.ramdisk with
          | Some _ -> raise (Bad_error ("More than one ramdisk line when parsing bootloader result: "^x))
          | None ->
            debug "Using ramdisk line from bootloader output: %s" l;
            {acc with ramdisk = Some (String.sub l pos (String.length l - pos))} )
      | "args" -> (
        match acc.args with
          | Some _ -> raise (Bad_error ("More than one args line when parsing bootloader result: "^x))
          | None ->
            debug "Using args line from bootloader output: %s" l;
            {acc with args = Some (String.sub l pos (String.length l - pos))} )
      | "" -> acc
      | _ -> raise (Bad_error ("Unrecognised start of line when parsing bootloader result: line="^l))
  in
  let parse_line acc l =
    try parse_line_optimistic acc l
    with Not_found -> acc
  in
  let linelist = Stdext.Xstringext.String.split '\n' x in
  let content = List.fold_left parse_line {kernel=None; ramdisk=None; args=None} linelist in
  {
    kernel_path = (match content.kernel with
      | None -> raise (Bad_error ("No kernel found in "^x))
      | Some p -> p);
    initrd_path = content.ramdisk;
    kernel_args = (match content.args with
      | None -> ""
      | Some a -> a)
  }

let parse_exception x = 
  match Stringext.String.split '\n' x with
  | code :: params -> raise (Api_errors.Server_error(code, params))
  | _ -> failwith (Printf.sprintf "Failed to parse stderr output of bootloader: %s" x)

(** Extract the default kernel using the -q option *)
let extract_default_kernel bootloader disks legacy_args extra_args pv_bootloader_args vm_uuid =
  let bootloader_path = List.assoc bootloader Xapi_globs.supported_bootloaders in
  if List.length disks = 0 then
    raise (Error("no bootable disk"));
  if List.length disks > 1 then
    raise (Error(Printf.sprintf "too many bootable disks (%d disks)" (List.length disks)));
  let disk = List.hd disks in
  let cmdline = bootloader_args true extra_args legacy_args pv_bootloader_args disk vm_uuid in
  debug "Bootloader commandline: %s %s\n" bootloader_path (String.concat " " cmdline);
  try
	let output = Helpers.call_script ~log_successful_output:false bootloader_path cmdline in
		debug "Bootloader output: %s" output;
		parse_output_simple output
  with Forkhelpers.Spawn_internal_error(stderr, stdout, _) ->
	  parse_exception stderr

let delete_extracted_kernel x = 
  Unix.unlink x.kernel_path;
  match x.initrd_path with
  | None -> ()
  | Some x -> Unix.unlink x
