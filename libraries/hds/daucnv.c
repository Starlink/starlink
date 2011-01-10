#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <limits.h>
#include <float.h>               /* DBL_DIG */

#include "f77.h"                 /* F7 <=> C interface macros               */
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int dat1_cvt_dtype( bad, nval, imp, exp, nbad )

/*+
 * dat1_cvt_dtype - Translate data
 *
 * This routine 'translates' a contiguous sequence of data values from one
 * location to another. If the data types differ, then conversion is per-
 * formed (if possible), otherwise the values are copied directly.
 *
 * (Note, that during conversion, any data values that cannot be sensibly
 * translated from the source type to the destination type are substituted
 * by a specific 'bad' value, and the return status set accordingly).
 *
 * Calling sequence:
 *
 *        dat1_cvt_dtype(NVAL,IMP,EXP)
 *
 * NVAL   is the number of values to be translated.
 * IMP    is the address of the import data descriptor.
 * EXP    is the address of the export data descriptor.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__CONER if any conversion errors have been detected.

 * Notes:
 *    Based on the LIB$CVT_DX_DX VAX/VMS routine functionality

 * Authors:
 *    RFWS: Rodney Warren-Smith (Starlink)

 * History:
 *    2007-03-08 (TIMJ):
 *       Use full precision for _DOUBLE to _CHAR conversion.
 *    2007-05-24 (TIMJ):
 *       Write an informative DAT__TRUNC error message including any input string
 *       that has been truncated.
 *    2011-01-10 (TIMJ):
 *       Make sure the truncation error message includes the correct length of the
 *       source string by not including trailing spaces.

 *-
 */

   int bad;
   int nval;
   struct PDD *imp;
   struct PDD *exp;
   int *nbad;

{
   int sl;
   int n;

/* Check the inherited global status. Allow the routine to execute if it is */
/* set to DAT__CONER, indicating a previous conversion error.               */
   if (!(_ok(hds_gl_status) || (hds_gl_status == DAT__CONER)))
      return hds_gl_status;

   *nbad = 0;

/* Perform the appropriate translation  */

   switch (imp->dtype)
   {
      case DAT__I:
         switch (exp->dtype)
         {
            case DAT__I:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__R:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_REAL) src[n];
                  break;
               }

            case DAT__D:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (src[n] & 1)
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_BYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UBYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_WORD) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__W ].bad.W;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UW:
               {
                  _INTEGER *src = (_INTEGER *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UWORD) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }
         }
         break;

      case DAT__R:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];        /* Overflow? */
                  break;
               }

            case DAT__R:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__D:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (((int) src[n]) & 1)    /* Overflow? */
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     sl = (int) src[n];
                     des[n] = (_BYTE) sl;
                     if (des[n] != sl)
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  _REAL hi = (_REAL) (UCHAR_MAX + 1);
                  _REAL lo = (_REAL) - 1;
                  for (n = 0; n < nval; n++)
                  {
                     if ((src[n] < hi) && (src[n] > lo))
                     {
                        des[n] = (_UBYTE) src[n];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     sl = (int) src[n];
                     des[n] = (_WORD) sl;
                     if (des[n] != sl)
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__W ].bad.W;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UW:
               {
                  _REAL *src = (_REAL *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  _REAL hi = (_REAL) (USHRT_MAX + 1);
                  _REAL lo = (_REAL) - 1;
                  for (n = 0; n < nval; n++)
                  {
                     if ((src[n] < hi) && (src[n] > lo))
                     {
                        des[n] = (_UWORD) src[n];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

         }
         break;

      case DAT__D:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];        /* Overflow? */
                  break;
               }

            case DAT__R:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  _DOUBLE hi = (_DOUBLE) dat_gl_ndr[ DAT__R ].max.R;
                  _DOUBLE lo = (_DOUBLE) dat_gl_ndr[ DAT__R ].min.R;
                  for (n = 0; n < nval; n++)
                  {
                     if ( ( src[ n ] <= hi ) && ( src[ n ] >= lo ) )
                     {
                        des[ n ] = (_REAL) src[ n ];
                     }
                     else
                     {
                        (*nbad)++;
                        des[ n ] = dat_gl_ndr[ DAT__R ].bad.R;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__D:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__L:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (((int) src[n]) & 1)    /* Overflow? */
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     sl = (int) src[n];
                     des[n] = (_BYTE) sl;
                     if (des[n] != sl)
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  _DOUBLE hi = (_DOUBLE) (UCHAR_MAX + 1);
                  _DOUBLE lo = (_DOUBLE) - 1;
                  for (n = 0; n < nval; n++)
                  {
                     if ((src[n] < hi) && (src[n] > lo))
                     {
                        des[n] = (_UBYTE) src[n];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     sl = (int) src[n];
                     des[n] = (_WORD) sl;
                     if (des[n] != sl)
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__W ].bad.W;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UW:
               {
                  _DOUBLE *src = (_DOUBLE *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  _DOUBLE hi = (_DOUBLE) (USHRT_MAX + 1);
                  _DOUBLE lo = (_DOUBLE) - 1;
                  for (n = 0; n < nval; n++)
                  {
                     if ((src[n] < hi) && (src[n] > lo))
                     {
                        des[n] = (_UWORD) src[n];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

         }
         break;

      case DAT__L:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1;
                     else
                        des[n] = 0;
                  break;
               }

            case DAT__R:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1.0;
                     else
                        des[n] = 0.0;
                  break;
               }

            case DAT__D:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1.0;
                     else
                        des[n] = 0.0;
                  break;
               }

            case DAT__L:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__C:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _CHAR *des = (_CHAR *) exp->body;
                  int len = exp->length;
                  for (n = 0; n < nval; n++)
                  {
                     if ( F77_ISTRUE( src[n] ) )
                     {
                        _chcopy(4, "TRUE", ' ', len, des);
                     }
                     else
                     {
                        _chcopy(5, "FALSE", ' ', len, des);
                     }
                     des += len;
                  }
                  break;
               }

            case DAT__B:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1;
                     else
                        des[n] = 0;
                  break;
               }

            case DAT__UB:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1;
                     else
                        des[n] = 0;
                  break;
               }

            case DAT__W:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1;
                     else
                        des[n] = 0;
                  break;
               }

            case DAT__UW:
               {
                  _LOGICAL *src = (_LOGICAL *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  for (n = 0; n < nval; n++)
                     if ( F77_ISTRUE( src[n] ) )
                        des[n] = 1;
                     else
                        des[n] = 0;
                  break;
               }
         }
         break;

      case DAT__C:
         switch (exp->dtype)
         {
            case DAT__L:
               {
                  _CHAR *src = (_CHAR *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  int len = imp->length;
                  for ( n = 0; n < nval; n++ )
                  {
                     if ( ( *src == 't' ) ||
                          ( *src == 'T' ) ||
                          ( *src == 'y' ) ||
                          ( *src == 'Y' ) )
                     {
                        des[ n ] = F77_TRUE;
                     }
                     else
                     {
                        des[ n ] = F77_FALSE;
                     }
                     src += len;
                  }
                  break;
               }

            case DAT__C:
               dau_move_data(nval, imp, exp);
               break;

            default:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;
         }
         break;

      case DAT__B:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];
                  break;
               }

            case DAT__R:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_REAL) src[n];
                  break;
               }

            case DAT__D:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (src[n] & 1)
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__UB:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  for ( n = 0; n < nval; n++ )
                  {
                     if ( src[ n ] > ( (_BYTE) -1 ) )
                     {
                        des[ n ] = (_UBYTE) src[ n ];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_WORD) src[n];
                  break;
               }

            case DAT__UW:
               {
                  _BYTE *src = (_BYTE *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UWORD) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }
         }
         break;

      case DAT__UB:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];
                  break;
               }

            case DAT__R:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_REAL) src[n];
                  break;
               }

            case DAT__D:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (src[n] & 1)
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_BYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__W:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_WORD) src[n];
                  break;
               }

            case DAT__UW:
               {
                  _UBYTE *src = (_UBYTE *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UWORD) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }
         }
         break;

      case DAT__W:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];
                  break;
               }

            case DAT__R:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_REAL) src[n];
                  break;
               }

            case DAT__D:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (src[n] & 1)
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_BYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UBYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               dau_move_data(nval, imp, exp);
               break;

            case DAT__UW:
               {
                  _WORD *src = (_WORD *) imp->body;
                  _UWORD *des = (_UWORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     if ( src[ n ] >= (_WORD) 0 )
                     {
                        des[n] = (_UWORD) src[n];
                     }
                     else
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UW ].bad.UW;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }
         }
         break;

      case DAT__UW:
         switch (exp->dtype)
         {
            case DAT__I:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _INTEGER *des = (_INTEGER *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_INTEGER) src[n];
                  break;
               }

            case DAT__R:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _REAL *des = (_REAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_REAL) src[n];
                  break;
               }

            case DAT__D:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _DOUBLE *des = (_DOUBLE *) exp->body;
                  for (n = 0; n < nval; n++)
                     des[n] = (_DOUBLE) src[n];
                  break;
               }

            case DAT__L:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _LOGICAL *des = (_LOGICAL *) exp->body;
                  for (n = 0; n < nval; n++)
                     if (src[n] & 1)
                        des[n] = F77_TRUE;
                     else
                        des[n] = F77_FALSE;
                  break;
               }

            case DAT__C:
               dat1_cvt_char(bad, nval, imp, exp, nbad);
               break;

            case DAT__B:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _BYTE *des = (_BYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_BYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__B ].bad.B;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UB:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _UBYTE *des = (_UBYTE *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_UBYTE) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__UB ].bad.UB;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__W:
               {
                  _UWORD *src = (_UWORD *) imp->body;
                  _WORD *des = (_WORD *) exp->body;
                  for (n = 0; n < nval; n++)
                  {
                     des[n] = (_WORD) src[n];
                     if (des[n] != src[n])
                     {
                        (*nbad)++;
                        des[n] = dat_gl_ndr[ DAT__W ].bad.W;
                        hds_gl_status = DAT__CONER;
                     }
                  }
                  break;
               }

            case DAT__UW:
               dau_move_data(nval, imp, exp);
               break;

         }
         break;

   }
   /* Make sure we keep EMS happy */
   if (hds_gl_status == DAT__CONER) {
     emsSetc( "I", dat_gl_ndr[ imp->dtype ].name );
     emsSetc( "O", dat_gl_ndr[ exp->dtype ].name );
     emsRep(" ", "Error converting from ^I to ^O",
	    &hds_gl_status);
   } else if (hds_gl_status == DAT__TRUNC) {
     /* work out the length of the string taking into account fortran trailing
        spaces. We can not use cnfLenc because the buffer is not terminated. */
     int slen = imp->length - 1;
     char * sourcestr = (_CHAR*)imp->body;
     for( slen-- ; ( slen >= 0 ) && ( sourcestr[slen] == ' ' ) ; slen-- )
       ;
     slen++;

     emsSeti( "SLEN", slen );
     emsSeti( "DLEN", (int)(exp->length) );
     /* If we are converting a string tell people the string in the error message */
     if (imp->dtype == DAT__C) {
       emsSetnc( "STR", sourcestr, slen );
     } else {
       emsSetc( "STR", "<numeric>");
     }

     emsRep(" ","Truncation during copy (destination ^DLEN < source ^SLEN) of '^STR'",
	    &hds_gl_status);
   }

   return hds_gl_status;
}


int
dat1_cvt_char(bad, nval, imp, exp, nbad)

/*+
 * TRANS_CHAR - Translate data to or from character format
 *
 * This routine 'translates' a contiguous sequence of data values from one
 * location to another. It is only intended for conversions to and from
 * character formats and is relatively inefficient. In particular it treats
 * logical values in the same way as integer values.
 *
 * (Note, that during conversion, any data values that cannot be sensibly
 * translated from the source type to the destination type are substituted
 * by a specific 'bad' value, and the return status set accordingly).
 *
 * Calling sequence:
 *
 *        TRANS_CHAR(NVAL,IMP,EXP)
 *
 * NVAL   is the number of values to be translated.
 * IMP    is the address of the import data descriptor.
 * EXP    is the address of the export data descriptor.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__CONER if any conversion errors have been detected.
 */

   int bad;
   int nval;
   struct PDD *imp;
   struct PDD *exp;
   int *nbad;

{
   struct PDD src = *imp;
   struct PDD des = *exp;
   int nchar;
   int nitem;
   int n;
   short int word;
   char buffer[STR_K_LENGTH + 1];/* Huge */

/* Check the inherited global status. Allow the routine to execute if it is */
/* set to DAT__CONER, indicating a previous conversion error.               */
   if (!(_ok(hds_gl_status) || (hds_gl_status == DAT__CONER)))
      return hds_gl_status;

   *nbad = 0;

/* If import and export paths are the same, do the translation directly */

   if (imp->dtype == exp->dtype)
      dau_move_data(nval, imp, exp);

/* Perform the appropriate translation for each value   */

   else
   {
      for (n = 0; n < nval; n++)

/* Convert the source to characters. The individual cases are necessary
   because of C argument type coercions */

      {
         buffer[0] = '\0';
         switch (imp->dtype)
         {
            case DAT__I:
               (void) sprintf( buffer, "%d%n", *((_INTEGER *) src.body),
                                               &nchar );
               break;

/* HORRIBLE TRUTH: we can't allow two "%" signs with a G between them in   */
/* the format strings that follow, or SCCS replaces them with today's date.*/
/* I love UNIX (RFWS).                                                     */
            case DAT__R:
               (void) sprintf( buffer, "%G""%n", *((_REAL *) src.body),
                                               &nchar );
               break;
            case DAT__D:
	      (void) sprintf( buffer, "%.*G%n", DBL_DIG,*((_DOUBLE *) src.body),
                                               &nchar );
               break;
            case DAT__L:
               (void) sprintf( buffer, "%d%n", *((int *) src.body),
                                               &nchar );
               break;
            case DAT__C:
               strncpy(buffer, (_CHAR *) src.body, src.length);
               buffer[src.length] = '\0';
               nchar = src.length;
               break;
            case DAT__B:
               (void) sprintf( buffer, "%d%n", *((_BYTE *) src.body),
                                               &nchar );
               break;
            case DAT__UB:
               (void) sprintf( buffer, "%u%n", *((_UBYTE *) src.body),
                                               &nchar );
               break;
            case DAT__W:
               (void) sprintf( buffer, "%d%n", *((_WORD *) src.body),
                                               &nchar );
               break;
            case DAT__UW:
               (void) sprintf( buffer, "%u%n", *((_UWORD *) src.body),
                                               &nchar );
         }

/* Convert the characters to the destination. The sscanf routine does not
   support all the necessary formats so some of this is a bit messy. In
   particular note the handling of bytes and unsigned bytes     */

         switch (exp->dtype)
         {
            case DAT__I:
               nitem = sscanf(buffer, "%d", (_INTEGER *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_INTEGER *) des.body) = dat_gl_ndr[ DAT__I ].bad.I;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__R:
               nitem = sscanf(buffer, "%f", (_REAL *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_REAL *) des.body) = dat_gl_ndr[ DAT__R ].bad.R;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__D:
               nitem = sscanf(buffer, "%lf", (_DOUBLE *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_DOUBLE *) des.body) = dat_gl_ndr[ DAT__D ].bad.D;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__L:
               nitem = sscanf(buffer, "%d", (int *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_LOGICAL *) des.body) = dat_gl_ndr[ DAT__L ].bad.L;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__C:
               if ((nchar > 0) && (nchar <= (int) des.length))
               {
                  _chcopy(nchar, buffer, ' ', (int) des.length, des.body);
               }
               else if ((nchar > 0) && (imp->dtype == DAT__C))
               {
                  (*nbad)++;
                  _chcopy(nchar, buffer, ' ', (int) des.length, des.body);
                  hds_gl_status = DAT__CONER;
               }
               else
               {
                  (*nbad)++;
                  memset( (void *) des.body, (int) dat_gl_ndr[ DAT__C ].bad.C,
                                             des.length );
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__B:
               nitem = sscanf(buffer, "%hd", &word);
               if ((nitem > 0) &&
                   (word <= SCHAR_MAX) &&
                   (word >= SCHAR_MIN))
                  *((_BYTE *) des.body) = word;
               else
               {
                  (*nbad)++;
                  *((_BYTE *) des.body) = dat_gl_ndr[ DAT__B ].bad.B;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__UB:
               nitem = sscanf(buffer, "%hd", &word);
               if ((nitem > 0) &&
                   (word <= UCHAR_MAX) &&
                   (word >= 0))
                  *((_UBYTE *) des.body) = word;
               else
               {
                  (*nbad)++;
                  *((_UBYTE *) des.body) = dat_gl_ndr[ DAT__UB ].bad.UB;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__W:
               nitem = sscanf(buffer, "%hd", (_WORD *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_WORD *) des.body) = dat_gl_ndr[ DAT__W ].bad.W;
                  hds_gl_status = DAT__CONER;
               }
               break;
            case DAT__UW:
               nitem = sscanf(buffer, "%hu", (_UWORD *) des.body);
               if (nitem == 0)
               {
                  (*nbad)++;
                  *((_UWORD *) des.body) = dat_gl_ndr[ DAT__UW ].bad.UW;
                  hds_gl_status = DAT__CONER;
               }
         }

/* Increment the source and destination pointers for the next value     */

         src.body += src.length;
         des.body += des.length;
      }
   }
   return hds_gl_status;
}
