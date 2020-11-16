/*
 * microMIPS instruction emulation helpers for QEMU.
 *
 *  Copyright (c) 2004-2005 Jocelyn Mayer
 *  Copyright (c) 2006 Marius Groeger (FPU operations)
 *  Copyright (c) 2006 Thiemo Seufer (MIPS32R2 support)
 *  Copyright (c) 2009 CodeSourcery (MIPS16 and microMIPS support)
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

DEF_HELPER_4(lwm, void, env, tl, tl, i32)
DEF_HELPER_4(swm, void, env, tl, tl, i32)
#ifdef TARGET_MIPS64
DEF_HELPER_4(ldm, void, env, tl, tl, i32)
DEF_HELPER_4(sdm, void, env, tl, tl, i32)
#endif
