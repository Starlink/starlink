
#include "hds1.h"
#include "hds.h"
#include "dat_err.h"
#include "ems.h"

/*
*  Name:
*    datMapN

*  Purpose:
*     Map object values a N-dimensional array

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datMapN( const HDSLoc* loc, const char * type, const char * mode,
*              int ndim, void **pntr, hdsdim dims[], int * status )
*     DAT_MAPN( LOC, TYPE, MODE, NDIM, PNTR, DIMS, STATUS )

*  Description:
*     This routine maps the primitive object data for reading, writing
*     or updating.   The caller is expected to know the number of
*     object dimensions, NDIM.   The object dimensions are returned
*     in the array, DIMS(NDIM).
*
*     Note that it is not possible to map data of type '_CHAR'.

*  Parameters:
*     loc = const HDSLoc * (Given)
*        Locator associated with a structured data object.
*     type = const char * (Given)
*        Expression specifying the data type of the mapped values.
*        If the actual type of the data object differs from this,
*        then conversion will be performed in 'READ' and 'UPDATE'
*        modes.
*     mode = const char * (Given)
*        Expression specifying the mode in which the data are to be
*        mapped.  (Either 'READ', 'WRITE' or 'UPDATE'.)
*     ndim = int (Given)
*        The number of array dimensions allocated to the dims[]
*        array. This must match the actual number of object dimensions.
*     pntr = void** (Returned)
*        Variable to receive the pointer to the memory mapped values.
*     dims[] = hdsdim (Returned)
*        Array to receive the dimensions of the mapped object.
*     status = int * (Given & Returned)
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Returns:
*     Returns status value on exit.

*  Authors:
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (UKATC)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1983-JAN-04 (SLW):
*        Original.
*     1984-NOV-05 (BDK):
*        Remove calls to error system.
*     1987-APR-15 (AJC):
*        Improved prologue layout.
*     2005-DEC02 (TIMJ):
*        Rewrite in C.
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


int datMapN( const HDSLoc* loc, const char * type, const char * mode,
	     int ndim, void **pntr, hdsdim dims[], int * status ) {

  int actdim;

  if (*status != DAT__OK) return *status;

  datShape( loc, ndim, dims, &actdim, status );

  if (*status != DAT__OK) return *status;

  if (actdim != ndim) {
    *status = DAT__DIMIN;
    emsSeti( "N", ndim );
    emsSeti( "A", actdim );
    emsRep( "DAT_MAPN_ERR", "Number of dimensions supplied (^N) does not match actual number of dimensions (^A)", status);
  } else {
    datMap( loc, type, mode, ndim, dims, pntr, status );
  }

  return *status;
}

