
#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "ems.h"
#include "star/mem.h"

#include "hds1.h"
#include "rec.h"
#include "dat1.h"
#include "hds_types.h"
#include "dat_err.h"

#include "hds1_types.h"
#include "hds_fortran.h"

/* Routines to import and export arrays of type hdsdim for use in
   Fortran libraries which require simple INTEGER*4 */

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
 *    for the Fortran routine

 *  Arguments
 *    ndim = int (Given)
 *       Number of relevant dimensions. Should not exceed DAT__MXDIM.
 *    dims[] = const hdsdim (Given)
 *       Input dimensions to copy, of size ndim.
 *    fdims[DAT__MXDIM] = F77_INTEGER_TYPE (Given)
 *       Buffer space that can be used to store the copied dimensions.
 *       Note that there is no guarantee that at exit this array will
 *       have been used.
 *    int *status = Given and Returned
 *       Inherited status. If set, this routine will return NULL.

 *  Return Value:
 *    outdims = F77_INTEGER_TYPE*
 *       Pointer to an array of Fortran integers containing the dimensions.

 *  Authors:
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *    11-JUL-2005 (TIMJ):
 *      Initial version

 *  Notes:
 *    - Only use the pointer returned by this routine. Do not
 *      assume that fdims[] will be filled since it may not be
 *      used if the type of hdsdim is the same as a F77_INTEGER_TYPE.
 *    - Status will be set to bad if the C dimension can not fit into
 *      the corresponding Fortran integer.
 *    - The expectation is that this routine is used solely for C
 *      interfaces to Fortran library routines.

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

  if ( *status != DAT__OK ) return NULL;

#if HDS_COPY_FORTRAN_DIMS
  /* sizes or signs differ so we need to copy one at a time
     and cast to the new type */

  for (i = 0; i < ndim; i++ ) {
    /* need to test for overflow - compare hdsdim to fortran max. Assume
       Fortran is a signed 32-bit int and negative dims will not
       be allowed (unsigned int would fit but fortran would treat that
       as negative dim). Do not test INT_MIN.
     */
    HDSDIM2INT( "hdsDimC2F", dims[i], fdims[i], status );
  }

  /* check status is good before deciding to use this array */
  if (*status == DAT__OK) retval = fdims;

#else
  /* hdsdim is the same size and sign so no copy required */
  retval = (F77_INTEGER_TYPE*)dims;
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

  if ( *status != DAT__OK ) return NULL;

#if HDS_COPY_FORTRAN_DIMS
  /* sizes or signs differ so we need to copy one at a time
     and cast to the new type */

  for (i = 0; i < ndim; i++ ) {
      cdims[i] = (hdsdim)fdims[i];
  }

  /* check status is good before deciding to use this array */
  if (*status == DAT__OK) retval = cdims;

#else
  /* hdsdim is the same size and sign so no copy required */
  retval = (hdsdim*)fdims;
#endif

  return retval;

}

