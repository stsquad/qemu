DEF_HELPER_3(raise_exception_err, noreturn, env, i32, int)
DEF_HELPER_2(raise_exception, noreturn, env, i32)
DEF_HELPER_1(raise_exception_debug, noreturn, env)

#ifndef CONFIG_USER_ONLY
DEF_HELPER_1(do_semihosting, void, env)
#endif

#ifdef TARGET_MIPS64
DEF_HELPER_4(sdl, void, env, tl, tl, int)
DEF_HELPER_4(sdr, void, env, tl, tl, int)
#endif
DEF_HELPER_4(swl, void, env, tl, tl, int)
DEF_HELPER_4(swr, void, env, tl, tl, int)

#ifndef CONFIG_USER_ONLY
DEF_HELPER_3(ll, tl, env, tl, int)
#ifdef TARGET_MIPS64
DEF_HELPER_3(lld, tl, env, tl, int)
#endif
#endif

DEF_HELPER_FLAGS_1(bitswap, TCG_CALL_NO_RWG_SE, tl, tl)
#ifdef TARGET_MIPS64
DEF_HELPER_FLAGS_1(dbitswap, TCG_CALL_NO_RWG_SE, tl, tl)
#endif

DEF_HELPER_FLAGS_4(rotx, TCG_CALL_NO_RWG_SE, tl, tl, i32, i32, i32)

#ifndef CONFIG_USER_ONLY
/* CP0 helpers */
DEF_HELPER_1(mfc0_mvpcontrol, tl, env)
DEF_HELPER_1(mfc0_mvpconf0, tl, env)
DEF_HELPER_1(mfc0_mvpconf1, tl, env)
DEF_HELPER_1(mftc0_vpecontrol, tl, env)
DEF_HELPER_1(mftc0_vpeconf0, tl, env)
DEF_HELPER_1(mfc0_random, tl, env)
DEF_HELPER_1(mfc0_tcstatus, tl, env)
DEF_HELPER_1(mftc0_tcstatus, tl, env)
DEF_HELPER_1(mfc0_tcbind, tl, env)
DEF_HELPER_1(mftc0_tcbind, tl, env)
DEF_HELPER_1(mfc0_tcrestart, tl, env)
DEF_HELPER_1(mftc0_tcrestart, tl, env)
DEF_HELPER_1(mfc0_tchalt, tl, env)
DEF_HELPER_1(mftc0_tchalt, tl, env)
DEF_HELPER_1(mfc0_tccontext, tl, env)
DEF_HELPER_1(mftc0_tccontext, tl, env)
DEF_HELPER_1(mfc0_tcschedule, tl, env)
DEF_HELPER_1(mftc0_tcschedule, tl, env)
DEF_HELPER_1(mfc0_tcschefback, tl, env)
DEF_HELPER_1(mftc0_tcschefback, tl, env)
DEF_HELPER_1(mfc0_count, tl, env)
DEF_HELPER_1(mfc0_saar, tl, env)
DEF_HELPER_1(mfhc0_saar, tl, env)
DEF_HELPER_1(mftc0_entryhi, tl, env)
DEF_HELPER_1(mftc0_status, tl, env)
DEF_HELPER_1(mftc0_cause, tl, env)
DEF_HELPER_1(mftc0_epc, tl, env)
DEF_HELPER_1(mftc0_ebase, tl, env)
DEF_HELPER_2(mftc0_configx, tl, env, tl)
DEF_HELPER_1(mfc0_lladdr, tl, env)
DEF_HELPER_1(mfc0_maar, tl, env)
DEF_HELPER_1(mfhc0_maar, tl, env)
DEF_HELPER_2(mfc0_watchlo, tl, env, i32)
DEF_HELPER_2(mfc0_watchhi, tl, env, i32)
DEF_HELPER_2(mfhc0_watchhi, tl, env, i32)
DEF_HELPER_1(mfc0_debug, tl, env)
DEF_HELPER_1(mftc0_debug, tl, env)
#ifdef TARGET_MIPS64
DEF_HELPER_1(dmfc0_tcrestart, tl, env)
DEF_HELPER_1(dmfc0_tchalt, tl, env)
DEF_HELPER_1(dmfc0_tccontext, tl, env)
DEF_HELPER_1(dmfc0_tcschedule, tl, env)
DEF_HELPER_1(dmfc0_tcschefback, tl, env)
DEF_HELPER_1(dmfc0_lladdr, tl, env)
DEF_HELPER_1(dmfc0_maar, tl, env)
DEF_HELPER_2(dmfc0_watchlo, tl, env, i32)
DEF_HELPER_2(dmfc0_watchhi, tl, env, i32)
DEF_HELPER_1(dmfc0_saar, tl, env)
#endif /* TARGET_MIPS64 */

DEF_HELPER_2(mtc0_index, void, env, tl)
DEF_HELPER_2(mtc0_mvpcontrol, void, env, tl)
DEF_HELPER_2(mtc0_vpecontrol, void, env, tl)
DEF_HELPER_2(mttc0_vpecontrol, void, env, tl)
DEF_HELPER_2(mtc0_vpeconf0, void, env, tl)
DEF_HELPER_2(mttc0_vpeconf0, void, env, tl)
DEF_HELPER_2(mtc0_vpeconf1, void, env, tl)
DEF_HELPER_2(mtc0_yqmask, void, env, tl)
DEF_HELPER_2(mtc0_vpeopt, void, env, tl)
DEF_HELPER_2(mtc0_entrylo0, void, env, tl)
DEF_HELPER_2(mtc0_tcstatus, void, env, tl)
DEF_HELPER_2(mttc0_tcstatus, void, env, tl)
DEF_HELPER_2(mtc0_tcbind, void, env, tl)
DEF_HELPER_2(mttc0_tcbind, void, env, tl)
DEF_HELPER_2(mtc0_tcrestart, void, env, tl)
DEF_HELPER_2(mttc0_tcrestart, void, env, tl)
DEF_HELPER_2(mtc0_tchalt, void, env, tl)
DEF_HELPER_2(mttc0_tchalt, void, env, tl)
DEF_HELPER_2(mtc0_tccontext, void, env, tl)
DEF_HELPER_2(mttc0_tccontext, void, env, tl)
DEF_HELPER_2(mtc0_tcschedule, void, env, tl)
DEF_HELPER_2(mttc0_tcschedule, void, env, tl)
DEF_HELPER_2(mtc0_tcschefback, void, env, tl)
DEF_HELPER_2(mttc0_tcschefback, void, env, tl)
DEF_HELPER_2(mtc0_entrylo1, void, env, tl)
DEF_HELPER_2(mtc0_context, void, env, tl)
DEF_HELPER_2(mtc0_memorymapid, void, env, tl)
DEF_HELPER_2(mtc0_pagemask, void, env, tl)
DEF_HELPER_2(mtc0_pagegrain, void, env, tl)
DEF_HELPER_2(mtc0_segctl0, void, env, tl)
DEF_HELPER_2(mtc0_segctl1, void, env, tl)
DEF_HELPER_2(mtc0_segctl2, void, env, tl)
DEF_HELPER_2(mtc0_pwfield, void, env, tl)
DEF_HELPER_2(mtc0_pwsize, void, env, tl)
DEF_HELPER_2(mtc0_wired, void, env, tl)
DEF_HELPER_2(mtc0_srsconf0, void, env, tl)
DEF_HELPER_2(mtc0_srsconf1, void, env, tl)
DEF_HELPER_2(mtc0_srsconf2, void, env, tl)
DEF_HELPER_2(mtc0_srsconf3, void, env, tl)
DEF_HELPER_2(mtc0_srsconf4, void, env, tl)
DEF_HELPER_2(mtc0_hwrena, void, env, tl)
DEF_HELPER_2(mtc0_pwctl, void, env, tl)
DEF_HELPER_2(mtc0_count, void, env, tl)
DEF_HELPER_2(mtc0_saari, void, env, tl)
DEF_HELPER_2(mtc0_saar, void, env, tl)
DEF_HELPER_2(mthc0_saar, void, env, tl)
DEF_HELPER_2(mtc0_entryhi, void, env, tl)
DEF_HELPER_2(mttc0_entryhi, void, env, tl)
DEF_HELPER_2(mtc0_compare, void, env, tl)
DEF_HELPER_2(mtc0_status, void, env, tl)
DEF_HELPER_2(mttc0_status, void, env, tl)
DEF_HELPER_2(mtc0_intctl, void, env, tl)
DEF_HELPER_2(mtc0_srsctl, void, env, tl)
DEF_HELPER_2(mtc0_cause, void, env, tl)
DEF_HELPER_2(mttc0_cause, void, env, tl)
DEF_HELPER_2(mtc0_ebase, void, env, tl)
DEF_HELPER_2(mttc0_ebase, void, env, tl)
DEF_HELPER_2(mtc0_config0, void, env, tl)
DEF_HELPER_2(mtc0_config2, void, env, tl)
DEF_HELPER_2(mtc0_config3, void, env, tl)
DEF_HELPER_2(mtc0_config4, void, env, tl)
DEF_HELPER_2(mtc0_config5, void, env, tl)
DEF_HELPER_2(mtc0_lladdr, void, env, tl)
DEF_HELPER_2(mtc0_maar, void, env, tl)
DEF_HELPER_2(mthc0_maar, void, env, tl)
DEF_HELPER_2(mtc0_maari, void, env, tl)
DEF_HELPER_3(mtc0_watchlo, void, env, tl, i32)
DEF_HELPER_3(mtc0_watchhi, void, env, tl, i32)
DEF_HELPER_3(mthc0_watchhi, void, env, tl, i32)
DEF_HELPER_2(mtc0_xcontext, void, env, tl)
DEF_HELPER_2(mtc0_framemask, void, env, tl)
DEF_HELPER_2(mtc0_debug, void, env, tl)
DEF_HELPER_2(mttc0_debug, void, env, tl)
DEF_HELPER_2(mtc0_performance0, void, env, tl)
DEF_HELPER_2(mtc0_errctl, void, env, tl)
DEF_HELPER_2(mtc0_taglo, void, env, tl)
DEF_HELPER_2(mtc0_datalo, void, env, tl)
DEF_HELPER_2(mtc0_taghi, void, env, tl)
DEF_HELPER_2(mtc0_datahi, void, env, tl)

#if defined(TARGET_MIPS64)
DEF_HELPER_2(dmtc0_entrylo0, void, env, i64)
DEF_HELPER_2(dmtc0_entrylo1, void, env, i64)
#endif

#endif /* !CONFIG_USER_ONLY */

/* CP1 functions */
DEF_HELPER_2(cfc1, tl, env, i32)
DEF_HELPER_4(ctc1, void, env, tl, i32, i32)

DEF_HELPER_2(float_cvtd_s, i64, env, i32)
DEF_HELPER_2(float_cvtd_w, i64, env, i32)
DEF_HELPER_2(float_cvtd_l, i64, env, i64)
DEF_HELPER_2(float_cvtps_pw, i64, env, i64)
DEF_HELPER_2(float_cvtpw_ps, i64, env, i64)
DEF_HELPER_2(float_cvts_d, i32, env, i64)
DEF_HELPER_2(float_cvts_w, i32, env, i32)
DEF_HELPER_2(float_cvts_l, i32, env, i64)
DEF_HELPER_2(float_cvts_pl, i32, env, i32)
DEF_HELPER_2(float_cvts_pu, i32, env, i32)

DEF_HELPER_3(float_addr_ps, i64, env, i64, i64)
DEF_HELPER_3(float_mulr_ps, i64, env, i64, i64)

DEF_HELPER_FLAGS_2(float_class_s, TCG_CALL_NO_RWG_SE, i32, env, i32)
DEF_HELPER_FLAGS_2(float_class_d, TCG_CALL_NO_RWG_SE, i64, env, i64)

#define FOP_PROTO(op)                                     \
DEF_HELPER_4(float_ ## op ## _s, i32, env, i32, i32, i32) \
DEF_HELPER_4(float_ ## op ## _d, i64, env, i64, i64, i64)
FOP_PROTO(maddf)
FOP_PROTO(msubf)
#undef FOP_PROTO

#define FOP_PROTO(op)                                \
DEF_HELPER_3(float_ ## op ## _s, i32, env, i32, i32) \
DEF_HELPER_3(float_ ## op ## _d, i64, env, i64, i64)
FOP_PROTO(max)
FOP_PROTO(maxa)
FOP_PROTO(min)
FOP_PROTO(mina)
#undef FOP_PROTO

#define FOP_PROTO(op)                            \
DEF_HELPER_2(float_ ## op ## _l_s, i64, env, i32) \
DEF_HELPER_2(float_ ## op ## _l_d, i64, env, i64) \
DEF_HELPER_2(float_ ## op ## _w_s, i32, env, i32) \
DEF_HELPER_2(float_ ## op ## _w_d, i32, env, i64)
FOP_PROTO(cvt)
FOP_PROTO(round)
FOP_PROTO(trunc)
FOP_PROTO(ceil)
FOP_PROTO(floor)
FOP_PROTO(cvt_2008)
FOP_PROTO(round_2008)
FOP_PROTO(trunc_2008)
FOP_PROTO(ceil_2008)
FOP_PROTO(floor_2008)
#undef FOP_PROTO

#define FOP_PROTO(op)                            \
DEF_HELPER_2(float_ ## op ## _s, i32, env, i32)  \
DEF_HELPER_2(float_ ## op ## _d, i64, env, i64)
FOP_PROTO(sqrt)
FOP_PROTO(rsqrt)
FOP_PROTO(recip)
FOP_PROTO(rint)
#undef FOP_PROTO

#define FOP_PROTO(op)                       \
DEF_HELPER_1(float_ ## op ## _s, i32, i32)  \
DEF_HELPER_1(float_ ## op ## _d, i64, i64)  \
DEF_HELPER_1(float_ ## op ## _ps, i64, i64)
FOP_PROTO(abs)
FOP_PROTO(chs)
#undef FOP_PROTO

#define FOP_PROTO(op)                            \
DEF_HELPER_2(float_ ## op ## _s, i32, env, i32)  \
DEF_HELPER_2(float_ ## op ## _d, i64, env, i64)  \
DEF_HELPER_2(float_ ## op ## _ps, i64, env, i64)
FOP_PROTO(recip1)
FOP_PROTO(rsqrt1)
#undef FOP_PROTO

#define FOP_PROTO(op)                                  \
DEF_HELPER_3(float_ ## op ## _s, i32, env, i32, i32)   \
DEF_HELPER_3(float_ ## op ## _d, i64, env, i64, i64)   \
DEF_HELPER_3(float_ ## op ## _ps, i64, env, i64, i64)
FOP_PROTO(add)
FOP_PROTO(sub)
FOP_PROTO(mul)
FOP_PROTO(div)
FOP_PROTO(recip2)
FOP_PROTO(rsqrt2)
#undef FOP_PROTO

#define FOP_PROTO(op)                                      \
DEF_HELPER_4(float_ ## op ## _s, i32, env, i32, i32, i32)  \
DEF_HELPER_4(float_ ## op ## _d, i64, env, i64, i64, i64)  \
DEF_HELPER_4(float_ ## op ## _ps, i64, env, i64, i64, i64)
FOP_PROTO(madd)
FOP_PROTO(msub)
FOP_PROTO(nmadd)
FOP_PROTO(nmsub)
#undef FOP_PROTO

#define FOP_PROTO(op)                                    \
DEF_HELPER_4(cmp_d_ ## op, void, env, i64, i64, int)     \
DEF_HELPER_4(cmpabs_d_ ## op, void, env, i64, i64, int)  \
DEF_HELPER_4(cmp_s_ ## op, void, env, i32, i32, int)     \
DEF_HELPER_4(cmpabs_s_ ## op, void, env, i32, i32, int)  \
DEF_HELPER_4(cmp_ps_ ## op, void, env, i64, i64, int)    \
DEF_HELPER_4(cmpabs_ps_ ## op, void, env, i64, i64, int)
FOP_PROTO(f)
FOP_PROTO(un)
FOP_PROTO(eq)
FOP_PROTO(ueq)
FOP_PROTO(olt)
FOP_PROTO(ult)
FOP_PROTO(ole)
FOP_PROTO(ule)
FOP_PROTO(sf)
FOP_PROTO(ngle)
FOP_PROTO(seq)
FOP_PROTO(ngl)
FOP_PROTO(lt)
FOP_PROTO(nge)
FOP_PROTO(le)
FOP_PROTO(ngt)
#undef FOP_PROTO

#define FOP_PROTO(op) \
DEF_HELPER_3(r6_cmp_d_ ## op, i64, env, i64, i64) \
DEF_HELPER_3(r6_cmp_s_ ## op, i32, env, i32, i32)
FOP_PROTO(af)
FOP_PROTO(un)
FOP_PROTO(eq)
FOP_PROTO(ueq)
FOP_PROTO(lt)
FOP_PROTO(ult)
FOP_PROTO(le)
FOP_PROTO(ule)
FOP_PROTO(saf)
FOP_PROTO(sun)
FOP_PROTO(seq)
FOP_PROTO(sueq)
FOP_PROTO(slt)
FOP_PROTO(sult)
FOP_PROTO(sle)
FOP_PROTO(sule)
FOP_PROTO(or)
FOP_PROTO(une)
FOP_PROTO(ne)
FOP_PROTO(sor)
FOP_PROTO(sune)
FOP_PROTO(sne)
#undef FOP_PROTO

/* Special functions */
#ifndef CONFIG_USER_ONLY
DEF_HELPER_1(tlbwi, void, env)
DEF_HELPER_1(tlbwr, void, env)
DEF_HELPER_1(tlbp, void, env)
DEF_HELPER_1(tlbr, void, env)
DEF_HELPER_1(tlbinv, void, env)
DEF_HELPER_1(tlbinvf, void, env)
DEF_HELPER_1(di, tl, env)
DEF_HELPER_1(ei, tl, env)
DEF_HELPER_1(eret, void, env)
DEF_HELPER_1(eretnc, void, env)
DEF_HELPER_1(deret, void, env)
DEF_HELPER_3(ginvt, void, env, tl, i32)
#endif /* !CONFIG_USER_ONLY */
DEF_HELPER_1(rdhwr_cpunum, tl, env)
DEF_HELPER_1(rdhwr_synci_step, tl, env)
DEF_HELPER_1(rdhwr_cc, tl, env)
DEF_HELPER_1(rdhwr_ccres, tl, env)
DEF_HELPER_1(rdhwr_performance, tl, env)
DEF_HELPER_1(rdhwr_xnp, tl, env)
DEF_HELPER_2(pmon, void, env, int)
DEF_HELPER_1(wait, void, env)

/* Loongson multimedia functions.  */
DEF_HELPER_FLAGS_2(paddsh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddush, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddsb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddusb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(paddb, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(psubsh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubush, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubsb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubusb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psubb, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(pshufh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(packsswh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(packsshb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(packushb, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(punpcklhw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(punpckhhw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(punpcklbh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(punpckhbh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(punpcklwd, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(punpckhwd, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(pavgh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pavgb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pmaxsh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pminsh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pmaxub, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pminub, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(pcmpeqw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pcmpgtw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pcmpeqh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pcmpgth, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pcmpeqb, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pcmpgtb, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(psllw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psllh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psrlw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psrlh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psraw, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(psrah, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(pmullh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pmulhh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pmulhuh, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_2(pmaddhw, TCG_CALL_NO_RWG_SE, i64, i64, i64)

DEF_HELPER_FLAGS_2(pasubub, TCG_CALL_NO_RWG_SE, i64, i64, i64)
DEF_HELPER_FLAGS_1(biadd, TCG_CALL_NO_RWG_SE, i64, i64)
DEF_HELPER_FLAGS_1(pmovmskb, TCG_CALL_NO_RWG_SE, i64, i64)

DEF_HELPER_3(cache, void, env, tl, i32)

#include "isa-micromips_helper.h.inc"

#include "mod-mips-dsp_helper.h.inc"
#include "mod-mips-msa_helper.h.inc"
#include "mod-mips-mt_helper.h.inc"

#include "vendor-vr54xx_helper.h.inc"
