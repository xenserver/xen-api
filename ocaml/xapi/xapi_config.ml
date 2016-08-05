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

(* Note: this used to be in Helpers; moved due to cyclic dependencies relating to License *)

open Xapi_globs
open Printf
open Stringext

module D=Debug.Debugger(struct let name="xapi" end)
open D

let rec string_trim s =
  let l = String.length s in
  if l = 0 then
    s
  else if s.[0] = ' ' || s.[0] = '\t' || s.[0] = '\n' || s.[0] = '\r' then
    string_trim (String.sub s 1 (l-1))
  else if s.[l-1] = ' ' || s.[l-1] = '\t' || s.[l-1] = '\n' || s.[l-1] = '\r' then
    string_trim (String.sub s 0 (l-1))
  else
    s

let read_config filename =
	let configargs = [
		"use-xenopsd", Config.Set_bool Xapi_globs.use_xenopsd;
		Config_shared.disable_logging_for;
		"relax-xsm-sr-check", Config.Set_bool Xapi_globs.relax_xsm_sr_check;
		(let cgo = "ciphersuites-good-outbound" in
		cgo, Config.String (fun s ->
			D.debug "Processing config %s=%s" cgo s;
			Xapi_globs.ciphersuites_good_outbound :=
				(if string_trim s <> "" then Some s else None)
		));
		"ciphersuites-legacy-outbound", Config.Set_string Xapi_globs.ciphersuites_legacy_outbound;
	] in
	try
		Config.read filename configargs (fun _ _ -> ())
	with Config.Error ls ->
		List.iter (fun (p,s) ->
								 eprintf "config file error: %s: %s\n" p s) ls;
		exit 2

let log_if_not_empty format_string value =
	if value <> "" then debug format_string value

let dump_config () =
	debug "Server configuration:";
	log_if_not_empty "product_version: %s" Version.product_version;
	log_if_not_empty "product_brand: %s" Version.product_brand;
	debug "platform_version: %s" Version.platform_version;
	debug "platform_name: %s" Version.platform_name;
	debug "build_number: %s" Version.build_number;
	debug "git changeset: %s" Version.git_id;
	debug "version: %d.%d" version_major version_minor;
	debug "use-xenopsd: %b" !Xapi_globs.use_xenopsd
	(* debug "License filename: %s" !License_file.filename *)
