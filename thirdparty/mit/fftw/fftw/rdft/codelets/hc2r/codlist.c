#include "ifftw.h"

extern void X(codelet_hc2r_3)(planner *);
extern void X(codelet_hc2r_4)(planner *);
extern void X(codelet_hc2r_5)(planner *);
extern void X(codelet_hc2r_6)(planner *);
extern void X(codelet_hc2r_7)(planner *);
extern void X(codelet_hc2r_8)(planner *);
extern void X(codelet_hc2r_9)(planner *);
extern void X(codelet_hc2r_10)(planner *);
extern void X(codelet_hc2r_11)(planner *);
extern void X(codelet_hc2r_12)(planner *);
extern void X(codelet_hc2r_13)(planner *);
extern void X(codelet_hc2r_14)(planner *);
extern void X(codelet_hc2r_15)(planner *);
extern void X(codelet_hc2r_16)(planner *);
extern void X(codelet_hc2r_32)(planner *);
extern void X(codelet_hc2r_64)(planner *);
extern void X(codelet_hc2r_128)(planner *);
extern void X(codelet_hb_2)(planner *);
extern void X(codelet_hb_3)(planner *);
extern void X(codelet_hb_4)(planner *);
extern void X(codelet_hb_5)(planner *);
extern void X(codelet_hb_6)(planner *);
extern void X(codelet_hb_7)(planner *);
extern void X(codelet_hb_8)(planner *);
extern void X(codelet_hb_9)(planner *);
extern void X(codelet_hb_10)(planner *);
extern void X(codelet_hb_12)(planner *);
extern void X(codelet_hb_15)(planner *);
extern void X(codelet_hb_16)(planner *);
extern void X(codelet_hb_32)(planner *);
extern void X(codelet_hb_64)(planner *);
extern void X(codelet_hc2rIII_2)(planner *);
extern void X(codelet_hc2rIII_3)(planner *);
extern void X(codelet_hc2rIII_4)(planner *);
extern void X(codelet_hc2rIII_5)(planner *);
extern void X(codelet_hc2rIII_6)(planner *);
extern void X(codelet_hc2rIII_7)(planner *);
extern void X(codelet_hc2rIII_8)(planner *);
extern void X(codelet_hc2rIII_9)(planner *);
extern void X(codelet_hc2rIII_10)(planner *);
extern void X(codelet_hc2rIII_12)(planner *);
extern void X(codelet_hc2rIII_15)(planner *);
extern void X(codelet_hc2rIII_16)(planner *);
extern void X(codelet_hc2rIII_32)(planner *);
extern void X(codelet_hc2rIII_64)(planner *);


extern const solvtab X(solvtab_rdft_hc2r);
const solvtab X(solvtab_rdft_hc2r) = {
   SOLVTAB(X(codelet_hc2r_3)),
   SOLVTAB(X(codelet_hc2r_4)),
   SOLVTAB(X(codelet_hc2r_5)),
   SOLVTAB(X(codelet_hc2r_6)),
   SOLVTAB(X(codelet_hc2r_7)),
   SOLVTAB(X(codelet_hc2r_8)),
   SOLVTAB(X(codelet_hc2r_9)),
   SOLVTAB(X(codelet_hc2r_10)),
   SOLVTAB(X(codelet_hc2r_11)),
   SOLVTAB(X(codelet_hc2r_12)),
   SOLVTAB(X(codelet_hc2r_13)),
   SOLVTAB(X(codelet_hc2r_14)),
   SOLVTAB(X(codelet_hc2r_15)),
   SOLVTAB(X(codelet_hc2r_16)),
   SOLVTAB(X(codelet_hc2r_32)),
   SOLVTAB(X(codelet_hc2r_64)),
   SOLVTAB(X(codelet_hc2r_128)),
   SOLVTAB(X(codelet_hb_2)),
   SOLVTAB(X(codelet_hb_3)),
   SOLVTAB(X(codelet_hb_4)),
   SOLVTAB(X(codelet_hb_5)),
   SOLVTAB(X(codelet_hb_6)),
   SOLVTAB(X(codelet_hb_7)),
   SOLVTAB(X(codelet_hb_8)),
   SOLVTAB(X(codelet_hb_9)),
   SOLVTAB(X(codelet_hb_10)),
   SOLVTAB(X(codelet_hb_12)),
   SOLVTAB(X(codelet_hb_15)),
   SOLVTAB(X(codelet_hb_16)),
   SOLVTAB(X(codelet_hb_32)),
   SOLVTAB(X(codelet_hb_64)),
   SOLVTAB(X(codelet_hc2rIII_2)),
   SOLVTAB(X(codelet_hc2rIII_3)),
   SOLVTAB(X(codelet_hc2rIII_4)),
   SOLVTAB(X(codelet_hc2rIII_5)),
   SOLVTAB(X(codelet_hc2rIII_6)),
   SOLVTAB(X(codelet_hc2rIII_7)),
   SOLVTAB(X(codelet_hc2rIII_8)),
   SOLVTAB(X(codelet_hc2rIII_9)),
   SOLVTAB(X(codelet_hc2rIII_10)),
   SOLVTAB(X(codelet_hc2rIII_12)),
   SOLVTAB(X(codelet_hc2rIII_15)),
   SOLVTAB(X(codelet_hc2rIII_16)),
   SOLVTAB(X(codelet_hc2rIII_32)),
   SOLVTAB(X(codelet_hc2rIII_64)),
   SOLVTAB_END
};
