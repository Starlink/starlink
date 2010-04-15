#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAUSHAPE.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Control Blocks */

int
dau_get_shape(struct LCP_DATA *data, int *naxes, HDS_PTYPE *axis)

/*+
 * DAU_GET_SHAPE - Enquire object shape
 *
 * This routine will return the 'apparent' shape of an object. If the
 * # of dimensions is greater than DAT__MXSLICE, then the shape has
 * to be read from the Object Descriptor Label; otherwise the Dimension
 * Bounds Table in the LCP will contain the necessary information.
 *
 * Calling sequence:
 *
 *    DAU_GET_SHAPE(DATA,NAXES,AXIS)
 *
 * DATA    is the address of the data part of the Locator Control Packet.
 * NAXES  is the address of a longword which is to receive the # of
 *    axes in the object.
 * AXIS    is the address of a longword vector which is to receive
 *    the axis sizes.
 *
 * Routine value:
 *
 *    DAT__OK    if successful.
 */

{
   HDS_PTYPE    (*dbt)[2];
   struct ODL   odl;
   int          i;

/* Get the # of axes from the LCP and check if less than slice limit.  */

   *naxes = data->naxes;
   if (*naxes <= DAT__MXSLICE)
   {

/*    If so, then determine the size of each axis from the Dimension Bounds
      Table.  */

      dbt = data->bounds;
      for (i=0; i<*naxes; i++)
         axis[i] = dbt[i][UPPER] - dbt[i][LOWER] + 1;
   } else {

/*    Otherwise, read the Object Descriptor Label and copy the axis vector. */

      dat1_get_odl(&data->han, &odl);
      for (i=0; i<*naxes; i++)
         axis[i] = 1;
      for ( i = 0; i < odl.naxes; i++ )
      {
         axis[ i ] = odl.axis[ i ];
      }
   }
   return hds_gl_status;
}
