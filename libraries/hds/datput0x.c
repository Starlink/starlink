#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>

#include "hds1.h"
#include "rec.h"
#include "dat1.h"
#include "hds_types.h"
#include "dat_err.h"
#include "hds.h"

/*
 *+
 *  Name:
 *    datPut0X

 *  Purpose:
 *    Put a scalar value into an HDS component

 *  Invocation:
 *    status = datPut0X( HDSLoc * loc, <type> value, int * status );

 *  Description:
 *     This routine writes a value into a scalar primitive object.
 *     There is a routine for each access type,
 *
 *        datPut0D    DOUBLE PRECISION
 *        datPut0R    REAL / FLOAT
 *        datPut0I    INTEGER
 *        datPut0W    WORD / SHORT
 *        datPut0L    LOGICAL
 *        datPut0C    CHARACTER[*n]
 *
 *     If the object data type differs from the access type, then
 *     conversion is performed.
 *
 *     Note that a Vector (1-D) object containing a single value is
 *     different from a Scalar (0-D).

 *  Arguments
 *    HDSLoc * loc = Given
 *       HDS locator associated with a primitive data object.
 *    <type> value = Given
 *       Value to be stored in the primitive data object
 *    int * status = Given & Returned
 *       Global inherited status.

 *  Authors:
 *    Jack Giddings (UCL::JRG)
 *    Sid Wright (UCL::SLW)
 *    Dennis Kelly (REVAD::BDK)
 *    Alan Chipperfield (RAL::AJC)
 *    Tim Jenness (JAC, Hawaii)

 *  History:
 *     3-JAN-1983 (UCL::JRG):
 *       Original.
 *     31-AUG-1983 (UCL::SLW):
 *       Standardise.
 *     05-NOV-1984: (REVAD::BDK)
 *       Remove calls to error system
 *     15-APR-1987 (RAL::AJC):
 *       Improved prologue layout
 *     21-NOV-2005 (TIMJ):
 *       Rewrite in C

 *  Notes:
 *    For datPut0C the supplied string must be nul-terminated.

 *  Copyright:
 *    Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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

int datPut0C ( const HDSLoc * loc, const char * value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutC( loc, ndims, dim, value, strlen(value), status );
  return *status;
}

int datPut0D ( const HDSLoc * loc, double value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutD( loc, ndims, dim, &value, status );
  return *status;
}

int datPut0R ( const HDSLoc * loc, float value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutR( loc, ndims, dim, &value, status );
  return *status;
}

int datPut0I ( const HDSLoc * loc, int value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutI( loc, ndims, dim, &value, status );

  return *status;
}

int datPut0W ( const HDSLoc * loc, short value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutW( loc, ndims, dim, &value, status );

  return *status;
}

int datPut0L ( const HDSLoc * loc, int value, int * status ) {

  int ndims = 0;
  hdsdim dim[] = { 0 };

  if ( *status != DAT__OK ) return *status;

  datPutL( loc, ndims, dim, &value, status );
  return *status;
}
