/*
 * gkdt.h: This is the equivalent of the RAL GKS parameter file gkdt.par for
 *         C.  As gkdt.par is system dependant,  this file should be checked
 *         against it before use.
 *
 * Maintenance Log:
 *
 *  10/12/86  PJWR  Created.
 *  07/07/87  PJWR  IS:  Deleted KMIWK,  changed value of KNIL,  added QNIL.
 */

#ifndef GKDT			/* Guarantee inclusion is unique. */
#define GKDT

#define KLVKS	4
#define KMXWKT	200

#ifdef pyr
#define KWK	8
#else
#define KWK	4
#endif

#define KT	11
#define KNIL	-9999
#define QNIL	-9999.0

#endif
