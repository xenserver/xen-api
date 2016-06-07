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

let systemctl = "/usr/bin/systemctl"

let call_systemctl args =
	Forkhelpers.execute_command_get_output systemctl args

let test args =
	try
		ignore (call_systemctl args);
		true
	with Forkhelpers.Spawn_internal_error (_, _, Unix.WEXITED n) when n <> 0 ->
		false

type service = string

let is_enabled service =
	test ["is-enabled"; service]

let enable service =
	ignore (call_systemctl ["enable"; service])

let disable service =
	ignore (call_systemctl ["disable"; service])

let is_active service =
	test ["is-active"; service]

let start service =
	ignore (call_systemctl ["start"; service])

let stop service =
	ignore (call_systemctl ["stop"; service])
