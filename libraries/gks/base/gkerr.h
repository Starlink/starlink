/*
 * gkerr.h: Provides a C equivalent of the RAL GKS parameter file gkerr.par
 *          and access to the RAL GKS common block /GKYERR/.
 *
 * Maintenance Log:
 *
 *  10/12/86  PJWR  Created.
 */

#ifndef GKERR			/* Guarantee inclusion is unique. */
#define GKERR

#define KOFF	0		/* From gkerr.par. */
#define KON	1

extern struct			/* From gkerr.cmn. */
{
  f77_integer	kerrs, kerrfl, kerror, krtnm;
} gkyerr_;

#endif
