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

open OUnit
open Test_common

let test_create_ok () =
	let __context = make_test_database () in
	let farm = make_pvs_farm ~__context () in
	let vIF = make_vif ~__context ~device:"0" () in
	let pvs_proxy = Xapi_pvs_proxy.create ~__context
		~farm ~vIF ~prepopulate:true in
	assert_equal farm (Db.PVS_proxy.get_farm ~__context ~self:pvs_proxy);
	assert_equal vIF (Db.PVS_proxy.get_VIF ~__context ~self:pvs_proxy);
	assert_bool "prepopulate should be true"
		(Db.PVS_proxy.get_prepopulate ~__context ~self:pvs_proxy)

let test_create_invalid_device () =
	let __context = make_test_database () in
	let farm = make_pvs_farm ~__context () in
	let vIF = make_vif ~__context ~device:"1" () in
	assert_raises_api_error
		Api_errors.invalid_device
		~args:["1"]
		(fun () -> Xapi_pvs_proxy.create ~__context
			~farm ~vIF ~prepopulate:true)

let test_create_invalid_farm () =
	let __context = make_test_database () in
	let farm = Ref.make () in
	let vIF = make_vif ~__context ~device:"0" () in
	assert_raises_api_error
		Api_errors.invalid_value
		~args:["farm"; Ref.string_of farm]
		(fun () -> Xapi_pvs_proxy.create ~__context
			~farm ~vIF ~prepopulate:true)

let test_create_invalid_vif () =
	let __context = make_test_database () in
	let farm = make_pvs_farm ~__context () in
	let vIF = Ref.make () in
	assert_raises_api_error
		Api_errors.invalid_value
		~args:["VIF"; Ref.string_of vIF]
		(fun () -> Xapi_pvs_proxy.create ~__context
			~farm ~vIF ~prepopulate:true)

let test_destroy () =
	let __context = make_test_database () in
	let farm = make_pvs_farm ~__context () in
	let vIF = make_vif ~__context ~device:"0" () in
	let pvs_proxy = Xapi_pvs_proxy.create ~__context
		~farm ~vIF ~prepopulate:true in
	Xapi_pvs_proxy.destroy ~__context ~self:pvs_proxy;
	assert_equal (Db.is_valid_ref __context pvs_proxy) false

let test_gc_proxy () =
	let __context = make_test_database () in
	let farm = make_pvs_farm ~__context () in
	let vIF = make_vif ~__context ~device:"0" () in
	let proxy1 = Xapi_pvs_proxy.create ~__context ~farm ~vIF ~prepopulate:true in
	let proxy2 = Xapi_pvs_proxy.create ~__context ~farm ~vIF ~prepopulate:true in
		( Db_gc.gc_PVS_proxies ~__context
		; assert_equal (Db.PVS_proxy.get_farm ~__context ~self:proxy1) farm
		; assert_equal (Db.PVS_proxy.get_VIF ~__context ~self:proxy1) vIF
		; Db.PVS_proxy.set_farm ~__context ~self:proxy1 ~value:Ref.null
		; Db_gc.gc_PVS_proxies ~__context (* should collect the proxy *)
		; assert_equal false (Db.is_valid_ref __context proxy1)
		; Db.PVS_proxy.set_VIF ~__context ~self:proxy2 ~value:Ref.null
		; Db_gc.gc_PVS_proxies ~__context (* should collect the proxy *)
		; assert_equal false (Db.is_valid_ref __context proxy2)
		)

let test =
	"test_pvs_proxy" >:::
		[
			"test_create_ok" >:: test_create_ok;
			"test_create_invalid_device" >:: test_create_invalid_device;
			"test_create_invalid_farm" >:: test_create_invalid_farm;
			"test_create_invalid_vif" >:: test_create_invalid_vif;
			"test_destroy" >:: test_destroy;
			"test_gc_proxy" >:: test_gc_proxy
		]
