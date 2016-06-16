(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make(struct let name="cluster_stack_manager" end)
open D

open Client

let enable () =
	match !Xapi_globs.cluster_stack_default with
	| "corosync" -> Corosync.enable ()
	| _ -> ()

let update ~__context =
	match !Xapi_globs.cluster_stack_default with
	| "corosync" -> Corosync.ensure_started ~__context
	| _ -> ()

let on_xapi_start ~__context =
	enable ();
	update ~__context

let pool_update ~__context =
	let hosts = Db.Host.get_all ~__context in
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		List.iter
			(fun host ->
				try Client.Host.update_cluster_stack ~rpc ~session_id ~host
				with Api_errors.Server_error (code, _)
					when code = Api_errors.host_offline -> ())
			hosts)

let manage ~__context =
	let classes = ["host"] in
	let timeout = 30.0 in
	let token = "" in
	Helpers.call_api_functions ~__context (fun rpc session_id ->
		let rec watch ~token =
			let open Event_types in
			let event_from_rpc =
				Client.Event.from ~rpc ~session_id ~classes ~token ~timeout in
			let event_from = Event_types.event_from_of_rpc event_from_rpc in
			if List.exists
				(fun event -> event.op = `add || event.op = `del)
				event_from.events
			then begin
				try pool_update ~__context
				with e -> debug "Cluster_stack.manage caught %s" (Printexc.to_string e)
			end;
			watch ~token:event_from.token
		in
		watch ~token)
