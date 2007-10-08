#include "ifftw.h"

extern void X(codelet_r2hc_2)(planner *);
extern void X(codelet_r2hc_3)(planner *);
extern void X(codelet_r2hc_4)(planner *);
extern void X(codelet_r2hc_5)(planner *);
extern void X(codelet_r2hc_6)(planner *);
extern void X(codelet_r2hc_7)(planner *);
extern void X(codelet_r2hc_8)(planner *);
extern void X(codelet_r2hc_9)(planner *);
extern void X(codelet_r2hc_10)(planner *);
extern void X(codelet_r2hc_11)(planner *);
extern void X(codelet_r2hc_12)(planner *);
extern void X(codelet_r2hc_13)(planner *);
extern void X(codelet_r2hc_14)(planner *);
extern void X(codelet_r2hc_15)(planner *);
extern void X(codelet_r2hc_16)(planner *);
extern void X(codelet_r2hc_32)(planner *);
extern void X(codelet_r2hc_64)(planner *);
extern void X(codelet_r2hc_128)(planner *);
extern void X(codelet_hf_2)(planner *);
extern void X(codelet_hf_3)(planner *);
extern void X(codelet_hf_4)(planner *);
extern void X(codelet_hf_5)(planner *);
extern void X(codelet_hf_6)(planner *);
extern void X(codelet_hf_7)(planner *);
extern void X(codelet_hf_8)(planner *);
extern void X(codelet_hf_9)(planner *);
extern void X(codelet_hf_10)(planner *);
extern void X(codelet_hf_12)(planner *);
extern void X(codelet_hf_15)(planner *);
extern void X(codelet_hf_16)(planner *);
extern void X(codelet_hf_32)(planner *);
extern void X(codelet_hf_64)(planner *);
extern void X(codelet_hf2_4)(planner *);
extern void X(codelet_hf2_8)(planner *);
extern void X(codelet_hf2_16)(planner *);
extern void X(codelet_hf2_32)(planner *);
extern void X(codelet_hf2_64)(planner *);
extern void X(codelet_r2hcII_2)(planner *);
extern void X(codelet_r2hcII_3)(planner *);
extern void X(codelet_r2hcII_4)(planner *);
extern void X(codelet_r2hcII_5)(planner *);
extern void X(codelet_r2hcII_6)(planner *);
extern void X(codelet_r2hcII_7)(planner *);
extern void X(codelet_r2hcII_8)(planner *);
extern void X(codelet_r2hcII_9)(planner *);
extern void X(codelet_r2hcII_10)(planner *);
extern void X(codelet_r2hcII_12)(planner *);
extern void X(codelet_r2hcII_15)(planner *);
extern void X(codelet_r2hcII_16)(planner *);
extern void X(codelet_r2hcII_32)(planner *);
extern void X(codelet_r2hcII_64)(planner *);


extern const solvtab X(solvtab_rdft_r2hc);
const solvtab X(solvtab_rdft_r2hc) = {
   SOLVTAB(X(codelet_r2hc_2)),
   SOLVTAB(X(codelet_r2hc_3)),
   SOLVTAB(X(codelet_r2hc_4)),
   SOLVTAB(X(codelet_r2hc_5)),
   SOLVTAB(X(codelet_r2hc_6)),
   SOLVTAB(X(codelet_r2hc_7)),
   SOLVTAB(X(codelet_r2hc_8)),
   SOLVTAB(X(codelet_r2hc_9)),
   SOLVTAB(X(codelet_r2hc_10)),
   SOLVTAB(X(codelet_r2hc_11)),
   SOLVTAB(X(codelet_r2hc_12)),
   SOLVTAB(X(codelet_r2hc_13)),
   SOLVTAB(X(codelet_r2hc_14)),
   SOLVTAB(X(codelet_r2hc_15)),
   SOLVTAB(X(codelet_r2hc_16)),
   SOLVTAB(X(codelet_r2hc_32)),
   SOLVTAB(X(codelet_r2hc_64)),
   SOLVTAB(X(codelet_r2hc_128)),
   SOLVTAB(X(codelet_hf_2)),
   SOLVTAB(X(codelet_hf_3)),
   SOLVTAB(X(codelet_hf_4)),
   SOLVTAB(X(codelet_hf_5)),
   SOLVTAB(X(codelet_hf_6)),
   SOLVTAB(X(codelet_hf_7)),
   SOLVTAB(X(codelet_hf_8)),
   SOLVTAB(X(codelet_hf_9)),
   SOLVTAB(X(codelet_hf_10)),
   SOLVTAB(X(codelet_hf_12)),
   SOLVTAB(X(codelet_hf_15)),
   SOLVTAB(X(codelet_hf_16)),
   SOLVTAB(X(codelet_hf_32)),
   SOLVTAB(X(codelet_hf_64)),
   SOLVTAB(X(codelet_hf2_4)),
   SOLVTAB(X(codelet_hf2_8)),
   SOLVTAB(X(codelet_hf2_16)),
   SOLVTAB(X(codelet_hf2_32)),
   SOLVTAB(X(codelet_hf2_64)),
   SOLVTAB(X(codelet_r2hcII_2)),
   SOLVTAB(X(codelet_r2hcII_3)),
   SOLVTAB(X(codelet_r2hcII_4)),
   SOLVTAB(X(codelet_r2hcII_5)),
   SOLVTAB(X(codelet_r2hcII_6)),
   SOLVTAB(X(codelet_r2hcII_7)),
   SOLVTAB(X(codelet_r2hcII_8)),
   SOLVTAB(X(codelet_r2hcII_9)),
   SOLVTAB(X(codelet_r2hcII_10)),
   SOLVTAB(X(codelet_r2hcII_12)),
   SOLVTAB(X(codelet_r2hcII_15)),
   SOLVTAB(X(codelet_r2hcII_16)),
   SOLVTAB(X(codelet_r2hcII_32)),
   SOLVTAB(X(codelet_r2hcII_64)),
   SOLVTAB_END
};
