#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATLOCOPS.C-*/

/* Include files */

#include "ems.h"                 /* EMS error reporting routines            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/* Control Blocks */

/*==========================*/
/* DAT_SLICE - Locate slice */
/*==========================*/
int
datSlice(const HDSLoc   *locator1,
         int      ndim,
         const HDS_PTYPE  lower[],
         const HDS_PTYPE  upper[],
         HDSLoc   **locator2,
         int      *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_SLICE_ERR"
#define context_message\
        "DAT_SLICE: Error obtaining a locator to a slice of an HDS array."

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   struct LCP_STATE *state2;
   HDS_PTYPE        (*dbt1)[2];
   HDS_PTYPE        (*dbt2)[2];
   HDS_PTYPE        axis[DAT__MXDIM];
   HDS_PTYPE        off[2];
   int              naxes;
   HDS_PTYPE        stride;
   int              i;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the source locator.   */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1 = &lcp1->data;

/* Get the current object shape and check that the # of dimensions matches
   the # of subscript bounds specified. */

   _call(dau_get_shape( data1, &naxes, axis ))
   if (naxes != ndim || naxes > DAT__MXSLICE)
     _call(DAT__DIMIN)

/* Export the destination locator and copy all the LCP data fields.     */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2 = &lcp2->data;
   state2 = &data2->state;
   *data2 = *data1;

/* Mark the locator as invalid until the subscript bounds have been checked
   and clear the LCP state flags.       */

   data2->valid   = 0;
   state2->mapped = 0;
   state2->vmcopy = 0;
   state2->unlike = 0;
   state2->slice  = 0;
   state2->cell   = 0;
   state2->vector = 0;
   state2->broken = 0;

/* Associate both Dimension Bounds Tables.      */

   dbt1 = data1->bounds;
   dbt2 = data2->bounds;

/* Fill in the Dimension Bounds Table for the sliced object, calculate the
   # of values in the slice and the lower and upper offsets.    */

   stride = 1;
   data2->size = 1;
   off[UPPER] = 0;
   off[LOWER] = 0;
   for (i=0; i<naxes; i++)
   {
      dbt2[i][LOWER] = dbt1[i][LOWER] + lower[i] - 1;
      dbt2[i][UPPER] = (upper[i] <= 0) ?
                        dbt1[i][UPPER] : dbt1[i][LOWER] + upper[i] - 1;
      if (dbt2[i][LOWER] < dbt1[i][LOWER] ||
          dbt2[i][UPPER] > dbt1[i][UPPER] ||
          dbt2[i][LOWER] > dbt2[i][UPPER])
             _call(DAT__SUBIN)
      data2->size *= dbt2[i][UPPER] - dbt2[i][LOWER] + 1;
      off[LOWER] += (stride * (dbt2[i][LOWER] - 1));
      off[UPPER] += (stride * (dbt2[i][UPPER] - 1));
      stride *= axis[i];
   }

/* If the opposite corners of the slice are not 'size' units apart, then
   the slice is broken (discontiguous). */

   state2->broken = ((off[LOWER] + data2->size) != (off[UPPER] + 1));
   data2->offset  = off[LOWER];
   state2->cell   = (data2->naxes == 0);
   state2->slice  = !state2->cell;
   data2->valid   = 1;
   return hds_gl_status;
}


/* F77_INTEGER_FUNCTION(dat_cell)(struct STR *locator1_str,
 *                              F77_INTEGER_TYPE *nsub,
 *                              F77_INTEGER_TYPE *subs,
 *                              struct STR *locator2_str,
 *                              F77_INTEGER_TYPE *status
 *                             TRAIL(locator1_str)
 *                              TRAIL(locator2_str) )
 */


/*========================*/
/* DAT_CELL - Locate cell */
/*========================*/

int
datCell( const HDSLoc *locator1,
         int      ndim,
         const HDS_PTYPE subs[],
         HDSLoc **locator2,
         int      *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_CELL_ERR"
#define context_message\
        "DAT_CELL: Error obtaining a locator to a cell of an HDS array."

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP_STATE *state1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   struct LCP_STATE *state2;
   HDS_PTYPE        axis[DAT__MXDIM];
   int              naxes;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the source locator.   */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1  = &lcp1->data;
   state1 = &data1->state;

/* Return if the object is discontiguous.       */

   if (state1->broken)
      _call(DAT__OBJIN)

/* Get the current object shape and check that the # of dimensions matches
   the # of subscripts specified.       */

   _call(dau_get_shape(data1, &naxes, axis))
   if (naxes != ndim) {
      emsSeti( "NAX", naxes );
      emsSeti( "NDIM" , ndim );
      _callm(DAT__DIMIN," Arguments have ^NDIM axes but locator refers to ^NAX axes")
   }

/* Export the destination locator and copy all the LCP data fields.     */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2  = &lcp2->data;
   state2 = &data2->state;
   *data2 = *data1;

/* Mark the locator as invalid until the subscript bounds have been checked
   and clear the LCP state flags.       */

   data2->valid   = 0;
   state2->mapped = 0;
   state2->vmcopy = 0;
   state2->unlike = 0;
   state2->slice  = 0;
   state2->cell   = 0;
   state2->vector = 0;
   state2->broken = 0;

/* Convert the subscript information to a zero-based offset and ensure
   that the required cell is within the bounds of the object.   */

   _call(dat1_get_off( naxes, axis, subs, &data2->offset) )
   if (data2->offset >= data1->size) {
     dat1emsSetBigu( "OFF", data2->offset );
     dat1emsSetBigu( "SIZE", data1->size );
     _callm(DAT__SUBIN, " Offset into data array (^OFF) exceeds size (^SIZE).")
   }

/* Adjust the cell offset and mark the object as scalar.        */

   data2->offset += data1->offset;
   data2->naxes = 0;
   data2->size  = 1;
   state2->cell = 1;
   data2->valid = 1;

   return hds_gl_status;
}

/*F77_INTEGER_FUNCTION(dat_vec)(struct STR *locator1_str,
 *                             struct STR *locator2_str,
 *                             F77_INTEGER_TYPE *status
 *                             TRAIL(locator1_str)
 *                             TRAIL(locator2_str) )
 */

/*============================*/
/* DAT_VEC - Vectorise object */
/*============================*/
int
datVec(const HDSLoc *locator1,
       HDSLoc **locator2,
       int  *status )
{
#undef context_name
#undef context_message
#define context_name "DAT_VEC_ERR"
#define context_message\
        "DAT_VEC: Error vectorising an HDS object."

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP_STATE *state1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   struct LCP_STATE *state2;
   HDS_PTYPE        (*dbt)[2];

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the source locator.   */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1  = &lcp1->data;
   state1 = &data1->state;

/* Return if the object is discontiguous.       */

   if (state1->broken)
      _call(DAT__OBJIN)

/* Export the destination locator and copy all the LCP data fields.     */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2  = &lcp2->data;
   state2 = &data2->state;
   *data2 = *data1;

/* Clear the LCP state flags.   */

   state2->mapped = 0;
   state2->vmcopy = 0;
   state2->unlike = 0;
   state2->slice  = 0;
   state2->cell   = 0;
   state2->vector = 0;
   state2->broken = 0;

/* Mark the object as a vector. */

   data2->naxes   = 1;
   state2->vector = 1;
   dbt            = data2->bounds;
   dbt[0][LOWER]  = 1;
   dbt[0][UPPER]  = data2->size;

   return hds_gl_status;
}

/* F77_INTEGER_FUNCTION(dat_coerc)(struct STR *locator1_str,
 *                                F77_INTEGER_TYPE *ndim,
 *                                struct STR *locator2_str,
 *                                F77_INTEGER_TYPE *status
 *                               TRAIL(locator1_str)
 *                               TRAIL(locator2_str) )
 */

/*=================================*/
/* DAT_COERC - Coerce object shape */
/*=================================*/
int
datCoerc(const HDSLoc *locator1,
         int ndim,
         HDSLoc **locator2,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_COERC_ERR"
#define context_message\
        "DAT_COERC: Error coercing an HDS object to change its shape."

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   struct LCP_STATE *state2;
   HDS_PTYPE        axis[DAT__MXDIM];
   HDS_PTYPE        (*dbt)[2];
   int              naxes;
   int              i;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the source locator.   */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1 = &lcp1->data;

/* Ensure that the requested # dimensions is valid.     */

   if (ndim < 1 || ndim > DAT__MXSLICE)
      _call(DAT__DIMIN)

/* Get the current object shape and return if any discarded dimension
   sizes are not 1.     */

   _call(dau_get_shape( data1, &naxes, axis ))
   for (i=ndim; i<naxes; i++)
      if (axis[i] != 1)
         _call(DAT__DIMIN)

/* Export the destination locator and copy all the LCP data fields.     */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2  = &lcp2->data;
   state2 = &data2->state;
   *data2 = *data1;

/* Clear the LCP state flags.   */

   state2->mapped = 0;
   state2->vmcopy = 0;
   state2->unlike = 0;
   state2->slice  = 0;
   state2->cell   = 0;
   state2->vector = 0;
   state2->broken = 0;

/* Coerce the shape.    */

   data2->naxes = ndim;
   dbt = data2->bounds;
   for (i=naxes; i<ndim; i++)
   {
      dbt[i][LOWER] = 1;
      dbt[i][UPPER] = 1;
   }
   return hds_gl_status;
}

/*F77_INTEGER_FUNCTION(dat_clone)(struct STR *locator1_str,
 *                               struct STR *locator2_str,
 *                               F77_INTEGER_TYPE *status
 *                               TRAIL(locator1_str)
 *                               TRAIL(locator2_str) )
 */

/*===========================*/
/* DAT_CLONE - Clone locator */
/*===========================*/
int
datClone(const HDSLoc *locator1,
         HDSLoc **locator2,
         int *status)
{
#undef context_name
#undef context_message
#define context_name "DAT_CLONE_ERR"
#define context_message\
        "DAT_CLONE: Error cloning (duplicating) an HDS locator."

   struct LCP       *lcp1;
   struct LCP_DATA  *data1;
   struct LCP       *lcp2;
   struct LCP_DATA  *data2;
   struct LCP_STATE *state2;

/* Enter routine.       */

   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the source locator.   */

   _call(dat1_import_loc(locator1, &lcp1 ));
   data1 = &lcp1->data;

/* Export the destination locator and copy all the LCP data fields.     */

   _call(dat1_alloc_lcp(locator2, &lcp2 ))
   data2  = &lcp2->data;
   state2 = &data2->state;
   *data2 = *data1;

/* Ensure that the mapped data flags are cleared.       */

   state2->mapped = 0;
   state2->vmcopy = 0;
   state2->unlike = 0;

   return hds_gl_status;
}

int
dat1_get_off(int ndim, const HDS_PTYPE *dims, const HDS_PTYPE *subs,
             UINT_BIG *offset)

/*+
 * GET_OFF - Get offset (from subscripts)
 *
 * This routine converts a set of array subscripts to a linear zero-based
 * offset.
 *
 * Calling sequence:
 *
 *        GET_OFF(NDIM,DIMS,SUBS,OFFSET)
 *
 * NDIM   is the number of dimensions.
 * DIMS   is the address of a longword vector whose elements contain the
 *        size of each dimension.
 * SUBS   is the address of a longword vector whose elements contain the
 *        subscript for each dimension.
 * OFFSET is the address of a longword which is to receive the zero-based
 *        offset.
 *
 * Routine value:
 *
 *        DAT__OK    if successful.
 *        DAT__SUBIN if any of the subscripts are outside the corresponding
 *                   dimension bound.
 */
{
   INT_BIG stride;
   int i;

   if (hds_gl_status != DAT__OK) return hds_gl_status;

   stride  = 1;
   *offset = 0;
   for (i=0; i<ndim; i++)
   {
      if (subs[i]>dims[i] || subs[i]<1)  {
	hds_gl_status = DAT__SUBIN;
	emsSeti( "I", i+1 ); /* Fortran indexing */
	emsSeti( "ND", ndim);
	if (subs[i]>dims[i]) {
	  dat1emsSetHdsdim( "SUBS", subs[i]);
	  dat1emsSetHdsdim( "SIZE", dims[i] );
	  emsRep( " ", "Subscript for dimension ^I of ^ND exceeds max allowed value"
		  " (^SUBS > ^SIZE) ", &hds_gl_status );
	} else if (subs[i] < 1) {
	  dat1emsSetHdsdim( "SUBS", subs[i]);
	  emsRep( " ", "Subscript for dimension ^I of ^ND is out of range (^SUBS < 1)",
		  &hds_gl_status );
	}
	return hds_gl_status;
      }
      *offset += (subs[i]-1) * stride;
      stride  *= dims[i];
   }
   return hds_gl_status;
}
