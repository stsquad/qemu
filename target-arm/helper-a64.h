/*
 *  AArch64 specific helper definitions
 *
 *  Copyright (c) 2013 Alexander Graf <agraf@suse.de>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

DEF_HELPER_2(set_fpcr, void, env, i64)
DEF_HELPER_FLAGS_1(get_fpcr, TCG_CALL_NO_SE, i64, env)
DEF_HELPER_2(set_fpsr, void, env, i64)
DEF_HELPER_FLAGS_1(get_fpsr, TCG_CALL_NO_SE, i64, env)

DEF_HELPER_FLAGS_2(udiv64, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(sdiv64, TCG_CALL_NO_RWG_SE, s64, s64, s64)
DEF_HELPER_FLAGS_1(clz64, TCG_CALL_NO_RWG_SE, i64, i64)
DEF_HELPER_FLAGS_1(cls64, TCG_CALL_NO_RWG_SE, i64, i64)
DEF_HELPER_FLAGS_1(cls32, TCG_CALL_NO_RWG_SE, i32, i32)
DEF_HELPER_FLAGS_1(rbit64, TCG_CALL_NO_RWG_SE, i64, i64)
DEF_HELPER_FLAGS_2(umulh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(smulh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
