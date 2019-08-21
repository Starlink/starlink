#include "dat_err.h"
#include "ndf_err.h"
#include "ndf1.h"

int ndf1Absnt( int istat ){
/*
*+
*  Name:
*     ndf1Absnt

*  Purpose:
*     Test status codes for absent NDF data structures or components.

*  Synopsis:
*     int ndf1Absnt( int istat )

*  Description:
*     The function tests an error status code and returns a non-zero result
*     if it is one of those indicating that an NDF data structure or an HDS
*     object (e.g. an NDF component) is absent. It exists so that this test
*     need only be defined in one place.

*  Parameters:
*     istat
*        The error status code to be tested.

*  Returned Value:
*     Whether an absent data structure or component is indicated.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int result;           /* Returned value */

/* Test the status value against those which indicate an absent data
   structure or component. */
   result = ( ( istat == DAT__FILNF ) || ( istat == DAT__OBJNF ) ||
              ( istat == DAT__NAMIN ) || ( istat == NDF__CNMIN ) ||
              ( istat == NDF__FILNF ) );

/* Return the result */
   return result;
}

