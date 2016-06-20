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

let modprobe = "/usr/sbin/modprobe"

let call_modprobe driver =
	ignore (Forkhelpers.execute_command_get_output modprobe [driver])

let corosync_cfgtool = "/usr/sbin/corosync-cfgtool"
let corosync = "corosync.service"
let dlm = "dlm.service"

let write_config ~__context =
	let cluster_name =
		String.sub
			(Db.Pool.get_uuid ~__context ~self:(Helpers.get_pool ~__context))
			0 8
	in
	let addresses =
		Db.Host.get_all_records ~__context
		|> List.map (fun (_, host_rec) -> host_rec.API.host_address)
	in
	let config_format = format_of_string "totem {
  version: 2
  secauth: off
  cluster_name: %s
  transport: udpu
  token_retransmits_before_loss_const: 10
  token: 10000
}

logging {
  debug: on
  to_logfile: yes
  logfile: /var/log/corosync.log
  to_syslog: yes
}

quorum {
  provider: corosync_votequorum
}

nodelist {
%s
}"
	in
	let config = Printf.sprintf config_format
		cluster_name
		(List.map
			(fun address ->
				Printf.sprintf "  node {\n    ring0_addr: %s\n  }" address)
			addresses
		|> String.concat "\n")
	in
	Unixext.write_string_to_file "/etc/corosync/corosync.conf" config

let reload_corosync () =
	ignore (Forkhelpers.execute_command_get_output corosync_cfgtool ["-R"])

let enable () =
	call_modprobe "dlm";
	call_modprobe "xen_wdt";
	call_modprobe "gfs2";
	if not (Systemctl.is_enabled corosync)
	then Systemctl.enable corosync;
	if not (Systemctl.is_enabled dlm)
	then Systemctl.enable dlm

let ensure_started ~__context =
	write_config ~__context;
	if (Systemctl.is_active corosync)
	then reload_corosync ()
	else Systemctl.start corosync
