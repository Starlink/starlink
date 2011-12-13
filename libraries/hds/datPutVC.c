
#include <stdlib.h>

#include "dat_err.h"
#include "hds1.h"
#include "hds.h"
#include "ems.h"

/*
*+
*  Name:
*     datPutVC

*  Purpose:
*     Write 1-D C string array to vectorized object

*  Invocation:
*     datPutVC( const HDSLoc * locator, size_t nval, const char *values[], int * status );
*     DAT_PUTVC( LOCATOR, NVAL, VALUES(NVAL), STATUS )

*  Description :
*     Write the values into a vectorized primitive object.

*  Parameters :
*     locator = const HDSLoc * (Given)
*        Variable containing a locator associated with a primitive
*        data object. It will be vectorized before storing the data.
*     nval = size_t (Given)
*        Expression specifying the number of values that are to be
*        written.   This must match the object size.
*     values = const char* [] (Given)
*        Array of pointers to C nul-terminated strings. Must contain
*        at least nval pointers.
*     status = int* (Given & Returned)
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Notes:
*     The C implementation takes an array of pointers to character strings whereas
*     the fortran interface takes a pointer to a single character buffer. Use the
*     datPutC interface if a fortran style character array is to be stored.
*     Since a C string array will have strings of differing length, they may be
*     truncated on store. If that happens status will be set to DAT__TRUNC.

*  Authors
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (UKATC)
*     AJC: Alan Chipperfield (RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     3-JAN-1983 (JRG):
*        Original.
*     31-Aug-1983 (SLW):
*        Standardise.
*     05.11.1984 (BDK):
*        Remove calls to error system
*     15-APR-1987 (AJC):
*        Improved prologue layout
*     05-DEC-2005 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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

int
datPutVC( const HDSLoc * locator, size_t nval, const char *values[], int * status ) {

  HDSLoc * vecloc = NULL;

  if (*status != DAT__OK) return *status;

  /* Vectorize */
  datVec( locator, &vecloc, status );

  /* Store the character data */
  datPut1C( vecloc, nval, values, status );

  /* Annul the vectorized locator */
  datAnnul( &vecloc, status );

  return *status;
}

