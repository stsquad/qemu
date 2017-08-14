/*
 *  Generic vector operation expansion
 *
 *  Copyright (c) 2017 Linaro
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

/*
 * "Generic" vectors.  All operands are given as offsets from ENV,
 * and therefore cannot also be allocated via tcg_global_mem_new_*.
 * OPSZ is the byte size of the vector upon which the operation is performed.
 * CLSZ is the byte size of the full vector; bytes beyond OPSZ are cleared.
 *
 * All sizes must be 8 or any multiple of 16.
 * When OPSZ is 8, the alignment may be 8, otherwise must be 16.
 * Operands may completely, but not partially, overlap.
 */

/* Fundamental operation expanders.  These are exposed to the front ends
   so that target-specific SIMD operations can be handled similarly to
   the standard SIMD operations.  */

typedef struct {
    /* "Small" sizes: expand inline as a 64-bit or 32-bit lane.
       Generally only one of these will be non-NULL.  */
    void (*fni8)(TCGv_i64, TCGv_i64, TCGv_i64);
    void (*fni4)(TCGv_i32, TCGv_i32, TCGv_i32);
    /* Similarly, but load up a constant and re-use across lanes.  */
    void (*fni8x)(TCGv_i64, TCGv_i64, TCGv_i64, TCGv_i64);
    uint64_t extra_value;
    /* Larger sizes: expand out-of-line helper w/size descriptor.  */
    void (*fno)(TCGv_ptr, TCGv_ptr, TCGv_ptr, TCGv_i32);
} GVecGen3;

void tcg_gen_gvec_3(uint32_t dofs, uint32_t aofs, uint32_t bofs,
                    uint32_t opsz, uint32_t clsz, const GVecGen3 *);

#define DEF_GVEC_2(X) \
    void tcg_gen_gvec_##X(uint32_t dofs, uint32_t aofs, uint32_t bofs, \
                          uint32_t opsz, uint32_t clsz)

DEF_GVEC_2(add8);
DEF_GVEC_2(add16);
DEF_GVEC_2(add32);
DEF_GVEC_2(add64);

DEF_GVEC_2(sub8);
DEF_GVEC_2(sub16);
DEF_GVEC_2(sub32);
DEF_GVEC_2(sub64);

DEF_GVEC_2(and8);
DEF_GVEC_2(or8);
DEF_GVEC_2(xor8);
DEF_GVEC_2(andc8);
DEF_GVEC_2(orc8);

#undef DEF_GVEC_2

/*
 * 64-bit vector operations.  Use these when the register has been
 * allocated with tcg_global_mem_new_i64.  OPSZ = CLSZ = 8.
 */

#define DEF_VEC8_2(X) \
    void tcg_gen_vec8_##X(TCGv_i64 d, TCGv_i64 a, TCGv_i64 b)

DEF_VEC8_2(add8);
DEF_VEC8_2(add16);
DEF_VEC8_2(add32);

DEF_VEC8_2(sub8);
DEF_VEC8_2(sub16);
DEF_VEC8_2(sub32);

#undef DEF_VEC8_2
