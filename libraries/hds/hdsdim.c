
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"
#include "sae_par.h"
#include "star/mem.h"

#include "hds_types.h"
#include "dat_err.h"

#include "hds1_types.h"
#include "hds_fortran.h"

/* Routines to import and export arrays of type hdsdim for use in
   Fortran libraries. */

/*
 *+
 *  Name:
 *    hdsDimC2F

 *  Purpose:
 *    Convert an array of hdsdim[] to F77_INTEGER_TYPE[]

 *  Invocation:
 *    outdims = hdsDimC2F( int ndim, const hdsdim dims[],
 *                         F77_INTEGER_TYPE fdims[DAT__MXDIM], int * status );

 *  Description:
 *    This function should be used to convert a an array of dimensions
 *    of type hdsdim to an array of dimensions suitable for Fortran
 *    usage. Returns a pointer to an array of F77_INTEGER_TYPE suitable
 *    for the Fortran routine and always fills the Fortran array.

 *  Arguments
 *    ndim = int (Given)
 *       Number of relevant dimensions. Should not exceed DAT__MXDIM.
 *    dims[] = const hdsdim (Given)
 *       Input dimensions to copy, of size ndim.
 *    fdims[DAT__MXDIM] = F77_INTEGER_TYPE (Given & Returned)
 *       Buffer space that can be used to store the copied dimensions.
 *       Will contain the dimensions even if the types for dims and
 *       fdims match.
 *    int *status = Given and Returned
 *       Inherited status. If set, this routine will return NULL.

 *  Return Value:
 *    outdims = F77_INTEGER_TYPE*
 *       Pointer to an array of Fortran integers containing the dimensions.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)
 *    Tim Jenness (Cornell University)

 *  History:
 *    11-JUL-2005 (TIMJ):
 *      Initial version
 *    2014-09-15 (TIMJ):
 *      To ensure that this routine can be used in the fortran
 *      interface we now always guarantee to fill fdims. This is a change of
 *      behavior but allows HDSDIM2INT to be removed from interfaces.

 *  Notes:
 *    - This routine is commonly used to copy the output of a C routine
 *      to a pre-existing Fortran buffer. This requires that fdims[] is
 *      filled regardless of type matching. If the types match then a
 *      fast memmove will be used to do the filling rather than a for loop.
 *    - Status will be set to bad if the C dimension can not fit into
 *      the corresponding Fortran integer.
 *    - The expectation is that this routine is used solely for C
 *      interfaces to Fortran library routines.
 *    - fdims is always filled, so the return value is now always fdims.
 *      It is retained for API compatibility.

 *  Copyright:
 *    Copyright (C) 2014 Cornell University.
 *    Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

F77_INTEGER_TYPE *
hdsDimC2F( int ndim, const hdsdim dims[],
	   F77_INTEGER_TYPE fdims[DAT__MXDIM], int * status ) {

#if HDS_COPY_FORTRAN_DIMS
  int i;   /* loop counter */
#endif
  F77_INTEGER_TYPE * retval = NULL;

  if ( *status != SAI__OK ) return NULL;

#if HDS_COPY_FORTRAN_DIMS
  /* sizes or signs differ so we need to copy one at a time
     and cast to the new type */

  for (i = 0; i < ndim; i++ ) {
    /* need to test for overflow - compare hdsdim to fortran max. Assume
       Fortran is a signed 32-bit int and hdsdim is signed. Do not test
       INT_MIN.
     */
    HDSDIM2INT( "hdsDimC2F", dims[i], fdims[i], status );
  }

  /* check status is good before deciding to use this array */
  if (*status == SAI__OK) retval = fdims;

#else
  /* hdsdim is the same size and sign so fast copy can
     be used. */
  memmove( fdims, dims, ndim*sizeof(*fdims) );
  retval = fdims;
#endif

  return retval;

}


/*
 *+
 *  Name:
 *    hdsDimF2C

 *  Purpose:
 *    Convert an array of F77_INTEGER_TYPE[] to hdsdim[]

 *  Invocation:
 *    outdims = hdsDimF2C( int ndim, const F77_INTEGER_TYPE fdims[],
 *                         hdsdim cdims[DAT__MXDIM], int * status );

 *  Description:
 *    This function should be used to convert a an array of Fortran dimensions
 *    of type F77_INTEGER_TYPE to an array of dimensions suitable for HDS C
 *    usage. Returns a pointer to an array of hdsdim[] suitable
 *    for the C function.

 *  Arguments
 *    ndim = int (Given)
 *       Number of relevant dimensions. Should not exceed DAT__MXDIM.
 *    fdims[DAT__MXDIM] = const F77_INTEGER_TYPE (Given)
 *       Input dimensions to copy, of size ndim.
 *    cdims[] = hdsdim (Given)
 *       Buffer space that can be used to store the copied dimensions.
 *       Note that there is no guarantee that at exit this array will
 *       have been used.
 *    int *status = Given and Returned
 *       Inherited status. If set, this routine will return NULL.

 *  Return Value:
 *    outdims = hdsdim*
 *       Pointer to an array of HDS C integers containing the dimensions.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *    12-JUL-2005 (TIMJ):
 *      Initial version

 *  Notes:
 *    - Only use the pointer returned by this routine. Do not
 *      assume that cdims[] will be filled since it may not be
 *      used if the type of hdsdim is the same as a F77_INTEGER_TYPE.
 *    - The expectation is that this routine is used solely for C
 *      interfaces to Fortran library routines or converting from Fortran
 *      to C.
 *    - A Fortran INTEGER will always fit in a hdsdim without overflow.

 *  Copyright:
 *    Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

hdsdim *
hdsDimF2C( int ndim, const F77_INTEGER_TYPE fdims[],
	   hdsdim cdims[DAT__MXDIM], int * status ) {

#if HDS_COPY_FORTRAN_DIMS
  int i;   /* loop counter */
#endif
  hdsdim * retval = NULL;

  if ( *status != SAI__OK ) return NULL;

#if HDS_COPY_FORTRAN_DIMS
  /* sizes or signs differ so we need to copy one at a time
     and cast to the new type */

  for (i = 0; i < ndim; i++ ) {
      cdims[i] = (hdsdim)fdims[i];
  }

  /* check status is good before deciding to use this array */
  if (*status == SAI__OK) retval = cdims;

#else
  /* hdsdim is the same size and sign so no copy required */
  retval = (hdsdim*)fdims;
#endif

  return retval;

}



/* Versions of the above functions that use F77_INTEGER8_TYPE to
   represent an hdsdim in Fortran rather than an F77_INTEGER_TYPE.
   These assume that an hdsdim is a 64 bit signed integer, so a simple
   copy is all that is needed. */

F77_INTEGER8_TYPE *hdsDimC2F8( int ndim, const hdsdim dims[],
                               F77_INTEGER8_TYPE fdims[DAT__MXDIM],
                               int *status ) {
  if ( *status != SAI__OK ) return NULL;
  memmove( fdims, dims, ndim*sizeof(*fdims) );
  return fdims;
}


hdsdim *hdsDimF2C8( int ndim, const F77_INTEGER8_TYPE fdims[],
                    hdsdim cdims[DAT__MXDIM], int *status ) {
  if ( *status != SAI__OK ) return NULL;
  return (hdsdim*) fdims;
}

