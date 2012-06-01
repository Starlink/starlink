#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DAUSCATGATH.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Control Blocks */

int
dau_gather_data( int bad, struct LCP_DATA *data, int *nbad )

/*+
 * DAU_GATHER_DATA - Gather discontiguous data
 *
 * This routine gathers discontiguous data from a sliced primitive object.
 * The object slice is mapped from 'corner to corner' if the process can
 * accommodate the increase in virtual memory space, otherwise the data
 * must be read in chunks.
 *
 * Calling sequence:
 *
 *        DAU_GATHER_DATA(DATA)
 *
 * DATA   is the address of the data part of the Locator Control Packet.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 */

{
   struct PDD    app;
   struct PDD    obj;
   HDS_PTYPE     *axis;
   HDS_PTYPE     (*dbt)[2];
   struct ODL    odl;
   HDS_PTYPE     stride[DAT__MXSLICE];
   HDS_PTYPE     mult[DAT__MXSLICE];
   int           naxes;
   int           nval;
   int           nchunk;
   int           nplane;
   INT_BIG       lower;
   INT_BIG       upper;
   INT_BIG       len;
   INT_BIG       pos;
   INT_BIG       off=0;
   unsigned char *dom;
   int           bitty;
   int           count;
   int           i;
   int           n;
   int           nbad1;
   int           temp_status;

   *nbad = 0;

/* Copy the object data and application data attributes descriptors.    */

   obj = data->obj;
   app = data->app;

/* Read the Object Descriptor Label and associate the axes vector.      */

   _invoke(dat1_get_odl(&data->han, &odl));
   axis  = odl.axis;
   naxes = odl.naxes;

/* Calculate the object data strides.   */

   stride[0] = obj.length;
   for (i=1; i<naxes; i++)
      stride[i] = stride[i-1] * axis[i-1];

/* Associate the Dimension Bounds Table and calculate the bounds multipliers. */

   dbt = data->bounds;
   for (i=0; i<naxes; i++)
      mult[i]   = dbt[i][UPPER] - dbt[i][LOWER] + 1;

/* Calculate the # of values in a chunk and the # of chunks in a plane. */

   nval   = mult[0];
   nchunk = 1;
   if (mult[0] == axis[0])
      nval     *= mult[1];
   else
      nchunk    = mult[1];

/* Determine the # of planes in the slice.      */

   nplane = (naxes == 3) ? mult[2] : 1;

/* Save the lower offset and calculate the upper offset for a 2-D object.
   (Adjust the upper offset if the object is 3-D).      */

   lower = data->offset;
   upper = dbt[0][UPPER] - 1 + axis[0]*(dbt[1][UPPER]-1);
   if (naxes == 3)
      upper    += axis[0] * axis[1] * (dbt[2][UPPER]-1);

/* Change the offset values to bytes and calculate the # of bytes between
   them.        */

   lower *= obj.length;
   upper = ( upper * obj.length ) + obj.length - 1; /* RFWS changed 22/12/92 */
                                                    /* because only the first*/
                                                    /* byte of the last      */
                                                    /* element was being     */
                                                    /* used.                 */
   len = upper - lower + 1;

/* Determine the # of pages available for virtual memory expansion and
   whether the object data has to be mapped in chunks.  */

   bitty     = (len > 512 * hds_gl_nblocks);

/* Map all the object data if it can be done.   */

   if (!bitty)
      _invoke(rec_locate_data(&data->han, len, lower, 'R', &dom));

/* Initialise the count of values remaining and scan through the planes of
   the slice.   */

   pos = 0;
   count = data->size;
   for (i=1; i<=nplane; i++)

/*    Travel through a plane, chunk by chunk.   */

   {
      for (n=0; n<nchunk; n++)

/*      Calculate the # of values remaining in the chunk and the offset
        into the domain. Assume the data was mapped in one go and save
        the address accordingly.        */

      {
         nval = _min(nval, count);
         obj.body = dom + pos + (INT_BIG)stride[ 1 ] * (INT_BIG)n;

/* If the data could not be mapped in one go, then map this chunk.  Ensure  */
/* that this occurs even if the global status is set to DAT__CONER because  */
/* of a previous conversion error.                                          */
         if (bitty)
         {
            len= nval * obj.length;
            off = lower + pos + (INT_BIG)stride[ 1 ] * (INT_BIG)n;
            temp_status = hds_gl_status;
            if ( hds_gl_status == DAT__CONER )
               hds_gl_status = DAT__OK;
            _invoke( rec_locate_data( &data->han, len, off, 'R', &dom ) );
            if ( _ok( hds_gl_status ) )
               hds_gl_status = temp_status;
            obj.body = dom;
         }

/* Translate the data in the chunk and accumulate the count of conversion   */
/* errors.                                                                  */
         dat1_cvt( bad, nval, &obj, &app, &nbad1 );
         *nbad += nbad1;

/* Unmap the chunk if necessary.                                            */
         if ( bitty )
         {
            temp_status = hds_gl_status;
            if ( hds_gl_status == DAT__CONER )
               hds_gl_status = DAT__OK;
            rec_release_data( &data->han, len, off, 'R', &dom );
            if ( _ok( hds_gl_status ) ) hds_gl_status = temp_status;
         }

/*      Adjust the remaining count and exit if all done. Otherwise,
        modify the applications data pointer accordingly.       */

         count -= nval;
         if (count <= 0)
            break;
         app.body += app.length * nval;
      }

/*    Increment the position for the next plane.        */

      pos += stride[2];
   }

/* Ditch the frame if mapped in one go.                                     */
   if ( !bitty )
   {
      temp_status = hds_gl_status;
      if ( hds_gl_status == DAT__CONER )
         hds_gl_status = DAT__OK;
      rec_release_data( &data->han, len, lower, 'R', &dom );
      if ( _ok( hds_gl_status ) )
         hds_gl_status = temp_status;
   }

   return hds_gl_status;
}

int
dau_scatter_data( int bad, struct LCP_DATA *data, int *nbad )

/*+
 * DAU_SCATTER_DATA - Scatter discontiguous data
 *
 * This routine scatters discontiguous data to a sliced primitive object.
 * The object slice is mapped from 'corner to corner' if the process can
 * accommodate the increase in virtual memory space, otherwise the data
 * must be written in chunks.
 *
 * Calling sequence:
 *
 *        DAU_SCATTER_DATA(DATA)
 *
 * DATA   is the address of the data part of the Locator Control Packet.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 */

{
   struct PDD    app;
   struct PDD    obj;
   HDS_PTYPE     *axis;
   HDS_PTYPE     (*dbt)[2];
   struct ODL    odl;
   HDS_PTYPE     stride[DAT__MXSLICE];
   HDS_PTYPE     mult[DAT__MXSLICE];
   int           naxes;
   int           nval;
   int           nchunk;
   int           nplane;
   INT_BIG       lower;
   INT_BIG       upper;
   INT_BIG       len;
   INT_BIG       pos;
   INT_BIG       off=0;
   unsigned char *dom;
   int           bitty;
   int           count;
   int           i;
   int           n;
   int           nbad1;
   int           temp_status;

   *nbad = 0;

/* Copy the object data and application data attributes descriptors.    */

   obj = data->obj;
   app = data->app;

/* Read the Object Descriptor Label and associate the axes vector.      */

   _invoke(dat1_get_odl(&data->han, &odl));
   axis  = odl.axis;
   naxes = odl.naxes;

/* Calculate the object data strides.   */

   stride[0] = obj.length;
   for (i=1; i<naxes; i++)
      stride[i] = stride[i-1] * axis[i-1];

/* Associate the Dimension Bounds Table and calculate the bounds multipliers. */

   dbt = data->bounds;
   for (i=0; i<naxes; i++)
      mult[i]   = dbt[i][UPPER] - dbt[i][LOWER] + 1;

/* Calculate the # of values in a chunk and the # of chunks in a plane. */

   nval = mult[0];
   nchunk = 1;
   if (mult[0] == axis[0])
      nval   *= mult[1];
   else
      nchunk = mult[1];

/* Determine the # of planes in the slice.      */

   nplane = (naxes == 3) ? mult[2] : 1;

/* Save the lower offset and calculate the upper offset for a 2-D object.
   (Adjust the upper offset if the object is 3-D).      */

   lower = data->offset;
   upper = dbt[0][UPPER] - 1 + axis[0]*(dbt[1][UPPER]-1);
   if (naxes == 3)
      upper += axis[0] * axis[1] * (dbt[2][UPPER]-1);

/* Change the offset values to bytes and calculate the # of bytes between
   them.        */

   lower *= obj.length;
   upper = ( upper * obj.length ) + obj.length - 1; /* RFWS changed 22/12/92 */
                                                    /* because only the first*/
                                                    /* byte of the last      */
                                                    /* element was being     */
                                                    /* used.                 */
   len = upper - lower + 1;

/* Determine the # of pages available for virtual memory expansion and
   whether the object data has to be mapped in chunks.  */

   bitty = (len > 512 * hds_gl_nblocks);

/* Map all the object data if it can be done.   */

   if (!bitty)
      _invoke(rec_locate_data(&data->han, len, lower, 'U', &dom));

/* Initialise the count of values remaining and scan through the planes of
   the slice.   */

   pos = 0;
   count = data->size;
   for (i=1; i<=nplane; i++)

/*    Travel through a plane, chunk by chunk.   */

   {
      for (n=0; n<nchunk; n++)

/*      Calculate the # of values remaining in the chunk and the offset
        into the domain. Assume the data was mapped in one go and save
        the address accordingly.        */

      {
         nval = _min(nval, count);
         obj.body = dom + pos + (INT_BIG)stride[ 1 ] * (INT_BIG)n;

/* If the data could not be mapped in one go, then map this chunk. Ensure   */
/* that this occurs even if the global status is set to DAT__CONER because  */
/* of a previous conversion error.                                          */

         if ( bitty )
         {
            len = nval * obj.length;
            off = lower + pos + (INT_BIG)stride[ 1 ] * (INT_BIG)n;
            temp_status = hds_gl_status;
            if ( hds_gl_status == DAT__CONER )
               hds_gl_status = DAT__OK;
            _invoke( rec_locate_data( &data->han, len, off, 'W', &dom ) );
            if ( _ok( hds_gl_status ) )
               hds_gl_status = temp_status;
            obj.body = dom;
         }

/* Translate the data in the chunk, accumulating the count of conversion    */
/* errors.                                                                  */
         dat1_cvt( bad, nval, &app, &obj, &nbad1 );
         *nbad += nbad1;

/* Unmap the chunk if necessary.                                            */
         if ( bitty )
         {
            temp_status = hds_gl_status;
            if ( hds_gl_status == DAT__CONER )
               hds_gl_status = DAT__OK;
            rec_release_data( &data->han, len, off, 'W', &dom );
            if ( _ok( hds_gl_status ) )
               hds_gl_status = temp_status;
         }

/*      Adjust the remaining count and exit if all done. Otherwise,
        modify the applications data pointer accordingly.       */

         count -= nval;
         if (count <= 0)
            break;
         app.body += app.length * nval;
      }

/*    Increment the position for the next plane.        */

      pos += stride[2];
   }

/* Ditch the frame if mapped in one go.                                     */
   if ( !bitty )
   {
      temp_status = hds_gl_status;
      if ( hds_gl_status == DAT__CONER )
         hds_gl_status = DAT__OK;
      rec_release_data( &data->han, len, lower, 'W', &dom );
      if ( _ok( hds_gl_status ) )
         hds_gl_status = temp_status;
   }
   return hds_gl_status;
}
