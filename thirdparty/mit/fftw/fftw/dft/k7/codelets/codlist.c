#include "ifftw.h"

extern void X(codelet_n1k7_2)(planner *);
extern void X(codelet_n1k7_3)(planner *);
extern void X(codelet_n1k7_4)(planner *);
extern void X(codelet_n1k7_5)(planner *);
extern void X(codelet_n1k7_6)(planner *);
extern void X(codelet_n1k7_7)(planner *);
extern void X(codelet_n1k7_8)(planner *);
extern void X(codelet_n1k7_9)(planner *);
extern void X(codelet_n1k7_10)(planner *);
extern void X(codelet_n1k7_11)(planner *);
extern void X(codelet_n1k7_12)(planner *);
extern void X(codelet_n1k7_13)(planner *);
extern void X(codelet_n1k7_14)(planner *);
extern void X(codelet_n1k7_15)(planner *);
extern void X(codelet_n1k7_16)(planner *);
extern void X(codelet_n1k7_32)(planner *);
extern void X(codelet_n1k7_64)(planner *);
extern void X(codelet_n1k7_128)(planner *);
extern void X(codelet_n1k7i_2)(planner *);
extern void X(codelet_n1k7i_3)(planner *);
extern void X(codelet_n1k7i_4)(planner *);
extern void X(codelet_n1k7i_5)(planner *);
extern void X(codelet_n1k7i_6)(planner *);
extern void X(codelet_n1k7i_7)(planner *);
extern void X(codelet_n1k7i_8)(planner *);
extern void X(codelet_n1k7i_9)(planner *);
extern void X(codelet_n1k7i_10)(planner *);
extern void X(codelet_n1k7i_11)(planner *);
extern void X(codelet_n1k7i_12)(planner *);
extern void X(codelet_n1k7i_13)(planner *);
extern void X(codelet_n1k7i_14)(planner *);
extern void X(codelet_n1k7i_15)(planner *);
extern void X(codelet_n1k7i_16)(planner *);
extern void X(codelet_n1k7i_32)(planner *);
extern void X(codelet_n1k7i_64)(planner *);
extern void X(codelet_n1k7i_128)(planner *);
extern void X(codelet_t1k7_2)(planner *);
extern void X(codelet_t1k7_3)(planner *);
extern void X(codelet_t1k7_4)(planner *);
extern void X(codelet_t1k7_5)(planner *);
extern void X(codelet_t1k7_6)(planner *);
extern void X(codelet_t1k7_7)(planner *);
extern void X(codelet_t1k7_8)(planner *);
extern void X(codelet_t1k7_9)(planner *);
extern void X(codelet_t1k7_10)(planner *);
extern void X(codelet_t1k7_12)(planner *);
extern void X(codelet_t1k7_15)(planner *);
extern void X(codelet_t1k7_16)(planner *);
extern void X(codelet_t1k7_32)(planner *);
extern void X(codelet_t1k7_64)(planner *);
extern void X(codelet_t1k7i_2)(planner *);
extern void X(codelet_t1k7i_3)(planner *);
extern void X(codelet_t1k7i_4)(planner *);
extern void X(codelet_t1k7i_5)(planner *);
extern void X(codelet_t1k7i_6)(planner *);
extern void X(codelet_t1k7i_7)(planner *);
extern void X(codelet_t1k7i_8)(planner *);
extern void X(codelet_t1k7i_9)(planner *);
extern void X(codelet_t1k7i_10)(planner *);
extern void X(codelet_t1k7i_12)(planner *);
extern void X(codelet_t1k7i_15)(planner *);
extern void X(codelet_t1k7i_16)(planner *);
extern void X(codelet_t1k7i_32)(planner *);
extern void X(codelet_t1k7i_64)(planner *);
extern void X(codelet_f1k7_2)(planner *);
extern void X(codelet_f1k7_4)(planner *);
extern void X(codelet_f1k7_8)(planner *);
extern void X(codelet_f1k7_16)(planner *);
extern void X(codelet_f1k7_32)(planner *);
extern void X(codelet_f1k7_64)(planner *);
extern void X(codelet_f1k7i_2)(planner *);
extern void X(codelet_f1k7i_4)(planner *);
extern void X(codelet_f1k7i_8)(planner *);
extern void X(codelet_f1k7i_16)(planner *);
extern void X(codelet_f1k7i_32)(planner *);
extern void X(codelet_f1k7i_64)(planner *);


extern const solvtab X(solvtab_dft_k7);
const solvtab X(solvtab_dft_k7) = {
   SOLVTAB(X(codelet_n1k7_2)),
   SOLVTAB(X(codelet_n1k7_3)),
   SOLVTAB(X(codelet_n1k7_4)),
   SOLVTAB(X(codelet_n1k7_5)),
   SOLVTAB(X(codelet_n1k7_6)),
   SOLVTAB(X(codelet_n1k7_7)),
   SOLVTAB(X(codelet_n1k7_8)),
   SOLVTAB(X(codelet_n1k7_9)),
   SOLVTAB(X(codelet_n1k7_10)),
   SOLVTAB(X(codelet_n1k7_11)),
   SOLVTAB(X(codelet_n1k7_12)),
   SOLVTAB(X(codelet_n1k7_13)),
   SOLVTAB(X(codelet_n1k7_14)),
   SOLVTAB(X(codelet_n1k7_15)),
   SOLVTAB(X(codelet_n1k7_16)),
   SOLVTAB(X(codelet_n1k7_32)),
   SOLVTAB(X(codelet_n1k7_64)),
   SOLVTAB(X(codelet_n1k7_128)),
   SOLVTAB(X(codelet_n1k7i_2)),
   SOLVTAB(X(codelet_n1k7i_3)),
   SOLVTAB(X(codelet_n1k7i_4)),
   SOLVTAB(X(codelet_n1k7i_5)),
   SOLVTAB(X(codelet_n1k7i_6)),
   SOLVTAB(X(codelet_n1k7i_7)),
   SOLVTAB(X(codelet_n1k7i_8)),
   SOLVTAB(X(codelet_n1k7i_9)),
   SOLVTAB(X(codelet_n1k7i_10)),
   SOLVTAB(X(codelet_n1k7i_11)),
   SOLVTAB(X(codelet_n1k7i_12)),
   SOLVTAB(X(codelet_n1k7i_13)),
   SOLVTAB(X(codelet_n1k7i_14)),
   SOLVTAB(X(codelet_n1k7i_15)),
   SOLVTAB(X(codelet_n1k7i_16)),
   SOLVTAB(X(codelet_n1k7i_32)),
   SOLVTAB(X(codelet_n1k7i_64)),
   SOLVTAB(X(codelet_n1k7i_128)),
   SOLVTAB(X(codelet_t1k7_2)),
   SOLVTAB(X(codelet_t1k7_3)),
   SOLVTAB(X(codelet_t1k7_4)),
   SOLVTAB(X(codelet_t1k7_5)),
   SOLVTAB(X(codelet_t1k7_6)),
   SOLVTAB(X(codelet_t1k7_7)),
   SOLVTAB(X(codelet_t1k7_8)),
   SOLVTAB(X(codelet_t1k7_9)),
   SOLVTAB(X(codelet_t1k7_10)),
   SOLVTAB(X(codelet_t1k7_12)),
   SOLVTAB(X(codelet_t1k7_15)),
   SOLVTAB(X(codelet_t1k7_16)),
   SOLVTAB(X(codelet_t1k7_32)),
   SOLVTAB(X(codelet_t1k7_64)),
   SOLVTAB(X(codelet_t1k7i_2)),
   SOLVTAB(X(codelet_t1k7i_3)),
   SOLVTAB(X(codelet_t1k7i_4)),
   SOLVTAB(X(codelet_t1k7i_5)),
   SOLVTAB(X(codelet_t1k7i_6)),
   SOLVTAB(X(codelet_t1k7i_7)),
   SOLVTAB(X(codelet_t1k7i_8)),
   SOLVTAB(X(codelet_t1k7i_9)),
   SOLVTAB(X(codelet_t1k7i_10)),
   SOLVTAB(X(codelet_t1k7i_12)),
   SOLVTAB(X(codelet_t1k7i_15)),
   SOLVTAB(X(codelet_t1k7i_16)),
   SOLVTAB(X(codelet_t1k7i_32)),
   SOLVTAB(X(codelet_t1k7i_64)),
   SOLVTAB(X(codelet_f1k7_2)),
   SOLVTAB(X(codelet_f1k7_4)),
   SOLVTAB(X(codelet_f1k7_8)),
   SOLVTAB(X(codelet_f1k7_16)),
   SOLVTAB(X(codelet_f1k7_32)),
   SOLVTAB(X(codelet_f1k7_64)),
   SOLVTAB(X(codelet_f1k7i_2)),
   SOLVTAB(X(codelet_f1k7i_4)),
   SOLVTAB(X(codelet_f1k7i_8)),
   SOLVTAB(X(codelet_f1k7i_16)),
   SOLVTAB(X(codelet_f1k7i_32)),
   SOLVTAB(X(codelet_f1k7i_64)),
   SOLVTAB_END
};
