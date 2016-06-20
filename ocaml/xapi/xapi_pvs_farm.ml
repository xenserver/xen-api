(*
 * Copyright (C) 2016 Citrix Systems Inc.
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

(* This module implements methods for the PVS_farm class *)

module D = Debug.Make(struct let name = "xapi_pvs_farm" end)

let not_allowed x =
	let open Api_errors in raise (Server_error (operation_not_allowed, [x]))

let introduce ~__context ~name =
	let pvs_farm = Ref.make () in
	let uuid = Uuid.to_string (Uuid.make_uuid ()) in
	Db.PVS_farm.create ~__context
		~ref:pvs_farm ~uuid ~name ~cache_storage:[];
	pvs_farm

(** [proxies] returns all currently attached proxies *)
let proxies ~__context ~self =
	let open Db_filter_types in
	Db.PVS_proxy.get_refs_where ~__context
		~expr:
			(And
				((Eq (Field "farm", Literal (Ref.string_of self)))
				,(Eq (Field "currently_attached", Literal "true"))
				))

let forget ~__context ~self =
	let open Db_filter_types in
	(* Check there are no running proxies. *)
	let running_proxies = proxies ~__context ~self in
	if running_proxies <> [] then
		raise Api_errors.(Server_error
			(pvs_farm_contains_running_proxies,
			List.map Ref.string_of running_proxies));
	(* Check there are no servers. *)
	let servers = Db.PVS_farm.get_servers ~__context ~self in
	if servers <> [] then raise Api_errors.(Server_error
		(pvs_farm_contains_servers, List.map Ref.string_of servers));
	Db.PVS_farm.destroy ~__context ~self

(** set the name of [self] *)
let set_name ~__context ~self ~value =
	if proxies ~__context ~self <> [] then
		not_allowed "can't set a name while proxies are still active"
	else
		Db.PVS_farm.set_name ~__context ~self ~value

(** [disjoint] is [true] if two lists don't share at least one element *)
let disjoint xs ys =
	List.for_all (fun x -> not @@ List.mem x ys) xs

(** [shared] is [true] if at least one [sr] in [srs] is shared *)
let shared ~__context srs =
	List.exists (fun sr -> Db.SR.get_shared ~__context ~self:sr) srs

(** [sr_hosts] returns all hosts attached to a list of [sr]s. *)
let sr_hosts ~__context srs =
	srs
	|> List.map (fun sr  -> Db.SR.get_PBDs ~__context ~self:sr)
	|> List.concat
	|> List.map (fun pbd -> Db.PBD.get_host ~__context ~self:pbd)

(** [clients] returns all hosts that use the farm as a client *)
let clients ~__context ~self =
	proxies ~__context ~self
	|> List.map (fun proxy -> Db.PVS_proxy.get_VIF ~__context ~self:proxy)
	|> List.map (fun vif -> Db.VIF.get_VM ~__context ~self:vif)
	|> List.map (fun vm  -> Db.VM.get_resident_on ~__context ~self:vm)

(** add a shared SR *)
let add_shared_cache_storage ~__context ~self ~value =
	let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
	match cache with
	| []    -> Db.PVS_farm.add_cache_storage ~__context ~self ~value
	| [_]   -> not_allowed "can't add a shared SR to an existing SR"
	| _     -> not_allowed "can't add a shared SR to a existing SRs"

(* add a local SR *)
let add_local_cache_storage ~__context ~self ~value =
	let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
	if shared ~__context cache then
		not_allowed "can't add SR to a cache using shared storage"
	else if not @@ disjoint
			(sr_hosts ~__context cache) (sr_hosts ~__context [value]) then
		not_allowed "SR's host is already providing storage"
	else
		Db.PVS_farm.add_cache_storage ~__context ~self ~value

let add_cache_storage ~__context ~self ~value =
	match Db.SR.get_shared ~__context ~self:value with
	| true  -> add_shared_cache_storage ~__context ~self ~value
	| false -> add_local_cache_storage  ~__context ~self ~value

(** remove a shared SR. This should be the only one *)
let remove_shared_cache_storage ~__context ~self ~value =
	let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
	if not @@ List.mem value cache then
		not_allowed "SR is not part of the farm's storage"
	else if proxies ~__context ~self <> [] then
		not_allowed "can't remove SR while proxies are still active"
	else
		assert (cache = [value]);
		Db.PVS_farm.remove_cache_storage ~__context ~self ~value

(** remove a local SR *)
let remove_local_cache_storage ~__context ~self ~value =
	let cache = Db.PVS_farm.get_cache_storage ~__context ~self in
	if not @@ List.mem value cache then
		not_allowed "SR is not part of the farm's storage"
	else if not @@ disjoint
			(clients ~__context ~self) (sr_hosts ~__context [value]) then
		not_allowed "SR's host is used by the farm - can't remove it"
	else
		Db.PVS_farm.remove_cache_storage ~__context ~self ~value

let remove_cache_storage ~__context ~self ~value =
	match Db.SR.get_shared ~__context ~self:value with
	| true  -> remove_shared_cache_storage ~__context ~self ~value
	| false -> remove_local_cache_storage  ~__context ~self ~value


