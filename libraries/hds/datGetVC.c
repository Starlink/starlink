
#include <stdlib.h>

#include "dat_err.h"
#include "hds1.h"
#include "hds.h"
#include "ems.h"

/*
*+
*  Name:
*     datGetVC

*  Purpose:
*     Get 1-D C string array from vectorized object

*  Invocation:
*     datGetVC( const HDSLoc * locator, size_t maxval, size_t bufsize, char *buffer,
*               char *pntrs[], size_t * actval, int * status );

*  Description :
*     Retrieves the values from a _CHAR*n vectorized primitive array object into a
*     character buffer and optionally an array of pointers. The C implementation
*     will populate the buffer with nul terminated strings one after the other.
*     Sufficient space must be made available to include the nul terminators.
*     An array of pointers to char* will also be filled (if non-NULL) corresponding
*     to the start position of each of the "actval" strings.

*  Parameters :
*     locator = const HDSLoc * (Given)
*        Variable containing a locator associated with a primitive
*        data object.
*     maxval = size_t (Given)
*        The allocated size of the pntrs array. Status is set to DAT__BOUND
*        if this value is too small and pntrs[] is non-NULL. Not used if the
*        pntrs[] array is NULL.
*     bufsize = size_t (Given)
*        Number of bytes allocated to the string "buffer" to receive the strings.
*        Returns a status of DAT__TRUNC if the buffer is not large enough.
*     buffer = char * (Returned)
*        Memory allocated to receive the string arrays from this routine.
*        Should be at least "bufsize" characters. Status is set
*        to DAT__TRUNC if sufficient room is not available to receive the strings.
*        The caller should not assume that the strings are stored equally spaced
*        within this buffer.
*     pntrs[] = char * (Returned)
*        On return will be filled with pointers to the start of each string
*        in the string array (which will be within "buffer"). Must be allocated
*        to hold at least "maxval" pointers. If this pointer is NULL, the
*        buffer will be populated and each string terminated but the start
*        positions will not be recorded.
*     actval = size_t * (Returned)
*        Actual number of array elements read from the object and stored in pntrs[].
*     status = int* (Given & Returned)
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Return Value:
*     Returns global status on exit.

*  See Also:
*     datGet1C

*  Notes:
*     This routine should not be called from Fortran or if a Fortran
*     style array of space separated strings in a contiguous buffer is required.
*     If a single buffer (Without nuls) is required call datVec and datGetC directly.
*     This routine exists specifically to provide the HDS user with a "standard"
*     C-style char** array.

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
*     05-NOV-1984 (BDK):
*        Remove calls to error system
*     15-APR-1987 (AJC):
*        Improved prologue layout
*     16-DEC-2005 (TIMJ):
*        Rewrite as datGetVC in C (same API as datGet1C)
*     29-DEC-2005 (TIMJ):
*        Modify documentation to match change to datGet1C so that pntrs[] is no
*        longer terminated.
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
datGetVC( const HDSLoc * locator,  size_t maxval, size_t bufsize, char *buffer,
	  char *pntrs[], size_t *actval, int * status )
{

  HDSLoc * vecLoc = NULL;

  if (*status != DAT__OK) return *status;

  datVec( locator, &vecLoc, status );

  datGet1C( vecLoc, maxval, bufsize, buffer, pntrs, actval, status );

  datAnnul( &vecLoc, status );

  return *status;
}

