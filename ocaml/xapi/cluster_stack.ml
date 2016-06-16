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
