#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAUMOVE.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int
dau_move_data(UINT_BIG nval, struct PDD *imp, struct PDD *exp)

/*+
 * DAU_MOVE_DATA - Move data
 *
 * This routine copies a contiguous sequence of data values from one location to
 * another. Although generally intended to be used for copying character strings
 * (where truncation or padding may be performed), the routine may also be used
 * to make a direct copy of data values of any of the other primitive types.
 *
 * Calling sequence:
 *
 *        DAU_MOVE_DATA(NVAL,IMP,EXP)
 *
 * NVAL    is the number of data values to be copied.
 * IMP     is the address of the import data descriptor.
 * EXP     is the address of the export data descriptor.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__TRUNC if any of the source values are greater in length than
 *                   the destination values (trailing blanks are ignored).
 */

{
   unsigned char *srcbuf = imp->body;
   unsigned char *desbuf = exp->body;
   short          srclen = imp->length;
   short          deslen = exp->length;
   UINT_BIG       n;
   UINT_BIG       m;

/* If the source strings are shorter than the destination strings, then they
   must be copied one by one and padded with spaces.    */

   if (srclen < deslen)
      for (n=0; n<nval; n++)
      {
         _chcopy(srclen, &srcbuf[n*srclen], ' ', deslen, &desbuf[n*deslen]);
      }

/* If the destination strings are shorter, then they must also be copied one
   by one, but truncated if necessary.  */

   else if (deslen < srclen)
      for (n=0; n<nval; n++)
      {
         _chcopy(srclen, &srcbuf[n*srclen], ' ', deslen, &desbuf[n*deslen]);
         for (m = n*srclen + deslen; m < (n+1)*srclen; m++)
            if (srcbuf[m] != ' ')
               hds_gl_status = DAT__TRUNC;
      }

/* If the data values lengths are the same, then they can be copied in one go.*/

   else
      _chmove(nval*srclen, srcbuf, desbuf);

   return hds_gl_status;
}
