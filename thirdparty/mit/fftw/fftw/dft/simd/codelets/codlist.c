#include "ifftw.h"

extern void X(codelet_n1fv_2)(planner *);
extern void X(codelet_n1fv_3)(planner *);
extern void X(codelet_n1fv_4)(planner *);
extern void X(codelet_n1fv_5)(planner *);
extern void X(codelet_n1fv_6)(planner *);
extern void X(codelet_n1fv_7)(planner *);
extern void X(codelet_n1fv_8)(planner *);
extern void X(codelet_n1fv_9)(planner *);
extern void X(codelet_n1fv_10)(planner *);
extern void X(codelet_n1fv_11)(planner *);
extern void X(codelet_n1fv_12)(planner *);
extern void X(codelet_n1fv_13)(planner *);
extern void X(codelet_n1fv_14)(planner *);
extern void X(codelet_n1fv_15)(planner *);
extern void X(codelet_n1fv_16)(planner *);
extern void X(codelet_n1fv_32)(planner *);
extern void X(codelet_n1fv_64)(planner *);
extern void X(codelet_n1bv_2)(planner *);
extern void X(codelet_n1bv_3)(planner *);
extern void X(codelet_n1bv_4)(planner *);
extern void X(codelet_n1bv_5)(planner *);
extern void X(codelet_n1bv_6)(planner *);
extern void X(codelet_n1bv_7)(planner *);
extern void X(codelet_n1bv_8)(planner *);
extern void X(codelet_n1bv_9)(planner *);
extern void X(codelet_n1bv_10)(planner *);
extern void X(codelet_n1bv_11)(planner *);
extern void X(codelet_n1bv_12)(planner *);
extern void X(codelet_n1bv_13)(planner *);
extern void X(codelet_n1bv_14)(planner *);
extern void X(codelet_n1bv_15)(planner *);
extern void X(codelet_n1bv_16)(planner *);
extern void X(codelet_n1bv_32)(planner *);
extern void X(codelet_n1bv_64)(planner *);
extern void X(codelet_n2fv_2)(planner *);
extern void X(codelet_n2fv_4)(planner *);
extern void X(codelet_n2fv_6)(planner *);
extern void X(codelet_n2fv_8)(planner *);
extern void X(codelet_n2fv_10)(planner *);
extern void X(codelet_n2fv_12)(planner *);
extern void X(codelet_n2fv_14)(planner *);
extern void X(codelet_n2fv_16)(planner *);
extern void X(codelet_n2fv_32)(planner *);
extern void X(codelet_n2fv_64)(planner *);
extern void X(codelet_n2bv_2)(planner *);
extern void X(codelet_n2bv_4)(planner *);
extern void X(codelet_n2bv_6)(planner *);
extern void X(codelet_n2bv_8)(planner *);
extern void X(codelet_n2bv_10)(planner *);
extern void X(codelet_n2bv_12)(planner *);
extern void X(codelet_n2bv_14)(planner *);
extern void X(codelet_n2bv_16)(planner *);
extern void X(codelet_n2bv_32)(planner *);
extern void X(codelet_n2bv_64)(planner *);
extern void X(codelet_n2sv_4)(planner *);
extern void X(codelet_n2sv_8)(planner *);
extern void X(codelet_n2sv_16)(planner *);
extern void X(codelet_n2sv_32)(planner *);
extern void X(codelet_n2sv_64)(planner *);
extern void X(codelet_t1fv_2)(planner *);
extern void X(codelet_t1fv_3)(planner *);
extern void X(codelet_t1fv_4)(planner *);
extern void X(codelet_t1fv_5)(planner *);
extern void X(codelet_t1fv_6)(planner *);
extern void X(codelet_t1fv_7)(planner *);
extern void X(codelet_t1fv_8)(planner *);
extern void X(codelet_t1fv_9)(planner *);
extern void X(codelet_t1fv_10)(planner *);
extern void X(codelet_t1fv_12)(planner *);
extern void X(codelet_t1fv_15)(planner *);
extern void X(codelet_t1fv_16)(planner *);
extern void X(codelet_t1fv_32)(planner *);
extern void X(codelet_t1fv_64)(planner *);
extern void X(codelet_t2fv_2)(planner *);
extern void X(codelet_t2fv_4)(planner *);
extern void X(codelet_t2fv_8)(planner *);
extern void X(codelet_t2fv_16)(planner *);
extern void X(codelet_t2fv_32)(planner *);
extern void X(codelet_t2fv_64)(planner *);
extern void X(codelet_t3fv_4)(planner *);
extern void X(codelet_t3fv_8)(planner *);
extern void X(codelet_t3fv_16)(planner *);
extern void X(codelet_t3fv_32)(planner *);
extern void X(codelet_t1bv_2)(planner *);
extern void X(codelet_t1bv_3)(planner *);
extern void X(codelet_t1bv_4)(planner *);
extern void X(codelet_t1bv_5)(planner *);
extern void X(codelet_t1bv_6)(planner *);
extern void X(codelet_t1bv_7)(planner *);
extern void X(codelet_t1bv_8)(planner *);
extern void X(codelet_t1bv_9)(planner *);
extern void X(codelet_t1bv_10)(planner *);
extern void X(codelet_t1bv_12)(planner *);
extern void X(codelet_t1bv_15)(planner *);
extern void X(codelet_t1bv_16)(planner *);
extern void X(codelet_t1bv_32)(planner *);
extern void X(codelet_t1bv_64)(planner *);
extern void X(codelet_t2bv_2)(planner *);
extern void X(codelet_t2bv_4)(planner *);
extern void X(codelet_t2bv_8)(planner *);
extern void X(codelet_t2bv_16)(planner *);
extern void X(codelet_t2bv_32)(planner *);
extern void X(codelet_t2bv_64)(planner *);
extern void X(codelet_t3bv_4)(planner *);
extern void X(codelet_t3bv_8)(planner *);
extern void X(codelet_t3bv_16)(planner *);
extern void X(codelet_t3bv_32)(planner *);
extern void X(codelet_t1sv_2)(planner *);
extern void X(codelet_t1sv_4)(planner *);
extern void X(codelet_t1sv_8)(planner *);
extern void X(codelet_t1sv_16)(planner *);
extern void X(codelet_t1sv_32)(planner *);
extern void X(codelet_t2sv_4)(planner *);
extern void X(codelet_t2sv_8)(planner *);
extern void X(codelet_t2sv_16)(planner *);
extern void X(codelet_t2sv_32)(planner *);
extern void X(codelet_q1fv_2)(planner *);
extern void X(codelet_q1fv_4)(planner *);
extern void X(codelet_q1fv_8)(planner *);
extern void X(codelet_q1bv_2)(planner *);
extern void X(codelet_q1bv_4)(planner *);
extern void X(codelet_q1bv_8)(planner *);


extern const solvtab X(solvtab_dft_simd);
const solvtab X(solvtab_dft_simd) = {
   SOLVTAB(X(codelet_n1fv_2)),
   SOLVTAB(X(codelet_n1fv_3)),
   SOLVTAB(X(codelet_n1fv_4)),
   SOLVTAB(X(codelet_n1fv_5)),
   SOLVTAB(X(codelet_n1fv_6)),
   SOLVTAB(X(codelet_n1fv_7)),
   SOLVTAB(X(codelet_n1fv_8)),
   SOLVTAB(X(codelet_n1fv_9)),
   SOLVTAB(X(codelet_n1fv_10)),
   SOLVTAB(X(codelet_n1fv_11)),
   SOLVTAB(X(codelet_n1fv_12)),
   SOLVTAB(X(codelet_n1fv_13)),
   SOLVTAB(X(codelet_n1fv_14)),
   SOLVTAB(X(codelet_n1fv_15)),
   SOLVTAB(X(codelet_n1fv_16)),
   SOLVTAB(X(codelet_n1fv_32)),
   SOLVTAB(X(codelet_n1fv_64)),
   SOLVTAB(X(codelet_n1bv_2)),
   SOLVTAB(X(codelet_n1bv_3)),
   SOLVTAB(X(codelet_n1bv_4)),
   SOLVTAB(X(codelet_n1bv_5)),
   SOLVTAB(X(codelet_n1bv_6)),
   SOLVTAB(X(codelet_n1bv_7)),
   SOLVTAB(X(codelet_n1bv_8)),
   SOLVTAB(X(codelet_n1bv_9)),
   SOLVTAB(X(codelet_n1bv_10)),
   SOLVTAB(X(codelet_n1bv_11)),
   SOLVTAB(X(codelet_n1bv_12)),
   SOLVTAB(X(codelet_n1bv_13)),
   SOLVTAB(X(codelet_n1bv_14)),
   SOLVTAB(X(codelet_n1bv_15)),
   SOLVTAB(X(codelet_n1bv_16)),
   SOLVTAB(X(codelet_n1bv_32)),
   SOLVTAB(X(codelet_n1bv_64)),
   SOLVTAB(X(codelet_n2fv_2)),
   SOLVTAB(X(codelet_n2fv_4)),
   SOLVTAB(X(codelet_n2fv_6)),
   SOLVTAB(X(codelet_n2fv_8)),
   SOLVTAB(X(codelet_n2fv_10)),
   SOLVTAB(X(codelet_n2fv_12)),
   SOLVTAB(X(codelet_n2fv_14)),
   SOLVTAB(X(codelet_n2fv_16)),
   SOLVTAB(X(codelet_n2fv_32)),
   SOLVTAB(X(codelet_n2fv_64)),
   SOLVTAB(X(codelet_n2bv_2)),
   SOLVTAB(X(codelet_n2bv_4)),
   SOLVTAB(X(codelet_n2bv_6)),
   SOLVTAB(X(codelet_n2bv_8)),
   SOLVTAB(X(codelet_n2bv_10)),
   SOLVTAB(X(codelet_n2bv_12)),
   SOLVTAB(X(codelet_n2bv_14)),
   SOLVTAB(X(codelet_n2bv_16)),
   SOLVTAB(X(codelet_n2bv_32)),
   SOLVTAB(X(codelet_n2bv_64)),
   SOLVTAB(X(codelet_n2sv_4)),
   SOLVTAB(X(codelet_n2sv_8)),
   SOLVTAB(X(codelet_n2sv_16)),
   SOLVTAB(X(codelet_n2sv_32)),
   SOLVTAB(X(codelet_n2sv_64)),
   SOLVTAB(X(codelet_t1fv_2)),
   SOLVTAB(X(codelet_t1fv_3)),
   SOLVTAB(X(codelet_t1fv_4)),
   SOLVTAB(X(codelet_t1fv_5)),
   SOLVTAB(X(codelet_t1fv_6)),
   SOLVTAB(X(codelet_t1fv_7)),
   SOLVTAB(X(codelet_t1fv_8)),
   SOLVTAB(X(codelet_t1fv_9)),
   SOLVTAB(X(codelet_t1fv_10)),
   SOLVTAB(X(codelet_t1fv_12)),
   SOLVTAB(X(codelet_t1fv_15)),
   SOLVTAB(X(codelet_t1fv_16)),
   SOLVTAB(X(codelet_t1fv_32)),
   SOLVTAB(X(codelet_t1fv_64)),
   SOLVTAB(X(codelet_t2fv_2)),
   SOLVTAB(X(codelet_t2fv_4)),
   SOLVTAB(X(codelet_t2fv_8)),
   SOLVTAB(X(codelet_t2fv_16)),
   SOLVTAB(X(codelet_t2fv_32)),
   SOLVTAB(X(codelet_t2fv_64)),
   SOLVTAB(X(codelet_t3fv_4)),
   SOLVTAB(X(codelet_t3fv_8)),
   SOLVTAB(X(codelet_t3fv_16)),
   SOLVTAB(X(codelet_t3fv_32)),
   SOLVTAB(X(codelet_t1bv_2)),
   SOLVTAB(X(codelet_t1bv_3)),
   SOLVTAB(X(codelet_t1bv_4)),
   SOLVTAB(X(codelet_t1bv_5)),
   SOLVTAB(X(codelet_t1bv_6)),
   SOLVTAB(X(codelet_t1bv_7)),
   SOLVTAB(X(codelet_t1bv_8)),
   SOLVTAB(X(codelet_t1bv_9)),
   SOLVTAB(X(codelet_t1bv_10)),
   SOLVTAB(X(codelet_t1bv_12)),
   SOLVTAB(X(codelet_t1bv_15)),
   SOLVTAB(X(codelet_t1bv_16)),
   SOLVTAB(X(codelet_t1bv_32)),
   SOLVTAB(X(codelet_t1bv_64)),
   SOLVTAB(X(codelet_t2bv_2)),
   SOLVTAB(X(codelet_t2bv_4)),
   SOLVTAB(X(codelet_t2bv_8)),
   SOLVTAB(X(codelet_t2bv_16)),
   SOLVTAB(X(codelet_t2bv_32)),
   SOLVTAB(X(codelet_t2bv_64)),
   SOLVTAB(X(codelet_t3bv_4)),
   SOLVTAB(X(codelet_t3bv_8)),
   SOLVTAB(X(codelet_t3bv_16)),
   SOLVTAB(X(codelet_t3bv_32)),
   SOLVTAB(X(codelet_t1sv_2)),
   SOLVTAB(X(codelet_t1sv_4)),
   SOLVTAB(X(codelet_t1sv_8)),
   SOLVTAB(X(codelet_t1sv_16)),
   SOLVTAB(X(codelet_t1sv_32)),
   SOLVTAB(X(codelet_t2sv_4)),
   SOLVTAB(X(codelet_t2sv_8)),
   SOLVTAB(X(codelet_t2sv_16)),
   SOLVTAB(X(codelet_t2sv_32)),
   SOLVTAB(X(codelet_q1fv_2)),
   SOLVTAB(X(codelet_q1fv_4)),
   SOLVTAB(X(codelet_q1fv_8)),
   SOLVTAB(X(codelet_q1bv_2)),
   SOLVTAB(X(codelet_q1bv_4)),
   SOLVTAB(X(codelet_q1bv_8)),
   SOLVTAB_END
};
