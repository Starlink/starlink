/*
 * gkwca.h: Enables access to RAL GKS common blocks /GKYWCA/ and /GKZWCA/.
 *
 * Maintenance Log:
 *
 *  10/12/86  PJWR  Created.
 */

#ifndef GKWCA			/* Guarantee inclusion is unique. */
#define GKWCA

#define KRPNO	0
#define KRPYES	1
#define KRPVIS	2
#define KRPINV	3

#define KRPDRW	1
#define KRPERA	2

#define KRFUSE	0
#define KACEPT	1

#define KSNOPN	0
#define KSOPN	1

#define KNKWI	10
#define KNQWR	16

#define KNUM	0
#define KREL	1

#define KWCST	512

extern struct
{
  f77_real	qwra[KNQWR];

  /* The following #defines perform EQUIVALENCE(QWRA(1), QWR1) */

#define qwr1	qwra[0]
#define qwr2	qwra[1]
#define qwr3	qwra[2]
#define qwr4	qwra[3]
#define qwr5	qwra[4]
#define qwr6	qwra[5]
#define qwr7	qwra[6]
#define qwr8	qwra[7]
#define qwr9	qwra[8]
#define qwr10	qwra[9]
#define qwr11	qwra[10]
#define qwr12	qwra[11]
#define qwr13	qwra[12]
#define qwr14	qwra[13]
#define qwr15	qwra[14]
#define qwr16	qwra[15]

  f77_real	qilnwd, qimksz, qichxp, qichsp, qichhx, qichhy, qichwx,
		qichwy, qipahx, qipahy, qipawx, qipawy, qipax, qipay;

  f77_integer	knir, knrr, kncr;
  f77_integer	kwia[KNKWI];

  /* The following #defines perform EQUIVALENCE(KWIA(1), KWI1) */

#define kwi1	kwia[0]
#define kwi2	kwia[1]
#define kwi3	kwia[2]
#define kwi4	kwia[3]
#define kwi5	kwia[4]
#define kwi6	kwia[5]
#define kwi7	kwia[6]
#define kwi8	kwia[7]
#define kwi9	kwia[8]
#define kwi10	kwia[9]

  f77_integer	kwktyp, kwkix;

  f77_logical	krgn, kwrgn[KWK];

  f77_integer	kipli, kilnty, kiplci, kiplaf[3], kipmi, kimkty, kipmci,
		kipmaf[3], kitxi, kitxfn, kitxpr, kitxci, kitxaf[4],
		kitxp, kihtxa, kivtxa, kifai, kifais, kifasi, kifaci,
		kifaaf[3], kwdone, krpcc, krptyp, krpsg, kinent, kcvis,
		kchlt, ksgrq;
} gkywca_;

extern struct
{
  f77_character	ch[80];
  f77_character	cstr[KWCST];
} gkzwca_;

#endif
