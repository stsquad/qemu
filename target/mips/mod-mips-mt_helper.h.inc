/*
 * MIPS Multi-Threading helpers for QEMU.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

#ifndef CONFIG_USER_ONLY
DEF_HELPER_2(mftgpr, tl, env, i32)
DEF_HELPER_2(mftlo, tl, env, i32)
DEF_HELPER_2(mfthi, tl, env, i32)
DEF_HELPER_2(mftacx, tl, env, i32)
DEF_HELPER_1(mftdsp, tl, env)
DEF_HELPER_3(mttgpr, void, env, tl, i32)
DEF_HELPER_3(mttlo, void, env, tl, i32)
DEF_HELPER_3(mtthi, void, env, tl, i32)
DEF_HELPER_3(mttacx, void, env, tl, i32)
DEF_HELPER_2(mttdsp, void, env, tl)

DEF_HELPER_0(dmt, tl)
DEF_HELPER_0(emt, tl)

DEF_HELPER_1(dvpe, tl, env)
DEF_HELPER_1(evpe, tl, env)

/* R6 Multi-threading */
DEF_HELPER_1(dvp, tl, env)
DEF_HELPER_1(evp, tl, env)
#endif /* !CONFIG_USER_ONLY */

DEF_HELPER_2(fork, void, tl, tl)
DEF_HELPER_2(yield, tl, env, tl)
