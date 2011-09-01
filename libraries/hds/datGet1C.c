
#include <stdlib.h>

#include "dat_err.h"
#include "hds1.h"
#include "hds.h"
#include "ems.h"
#include "star/mem.h"

/*
*+
*  Name:
*     datGet1C

*  Purpose:
*     Get 1-D C string array from object

*  Invocation:
*     datGet1C( const HDSLoc * locator, size_t maxval, size_t bufsize, char *buffer,
*               char *pntrs[], size_t * actval, int * status );

*  Description :
*     Retrieves the values from a _CHAR*n primitive array object into a
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

*  Notes:
*     This routine should not be called from Fortran or if a Fortran
*     style array of space separated strings in a contiguous buffer is required.
*     If a single buffer (Without nuls) is required call datGetC directly.
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
*     05.11.1984 (BDK):
*        Remove calls to error system
*     15-APR-1987 (AJC):
*        Improved prologue layout
*     08-DEC-2005 (TIMJ):
*        Rewrite in C. Make more C-like.
*     13-DEC-2005 (TIMJ):
*        Specify bufsize rather than length of individual string.
*     29-DEC-2005 (TIMJ):
*        No longer terminate pntrs[]
*     23-FEB-2006 (TIMJ):
*        Use starmem
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

int
datGet1C( const HDSLoc * locator,  size_t maxval, size_t bufsize, char *buffer,
	  char *pntrs[], size_t *actval, int * status )
{
  char * tmpbuf;
  size_t tmpbufsize;
  size_t lenstr;
  char * inpos;
  char * outpos;
  hdsdim dims[1];
  size_t nleft;
  int n;
  int i;

  /* Initialize return variables in case bad status */
  *actval = 0;
  if (pntrs != NULL) pntrs[0] = NULL;

  /* Do nothing if bad status */
  if (*status != DAT__OK) return *status;

  /* Verify that we have the correct number of values */
  datSize( locator, actval, status );

  /* Error, but only if we are looking in the pntrs[] array */
  if ( pntrs != NULL ) {
    if ( maxval < *actval ) {
      *status = DAT__BOUND;
      emsSeti( "NV", maxval );
      emsSeti( "SZ", *actval );
      emsRep( "DAT_GET1C_ERR",
	      "datGet1C: Input array bounds (maxval) does not match HDS object (^NV < ^SZ)",
	      status);
      return *status;
    }
  }

  /* Need to know the actual allocated size of each string */
  /* Note that since we are allocating memory for HDS we do not
   * need to abort with DAT__TRUNC at this point if char_len < lenstr
   * since we can report DAT__TRUNC only if the C-style strings do not
   * fit in the supplied space. */
  datClen(locator, &lenstr, status);

  if (*status == DAT__OK ) {
    /* How big a temp buffer to we need? */
    tmpbufsize = *actval * lenstr;

    /* We need to get some memory to receive the Fortran buffer */
    tmpbuf = MEM_MALLOC( tmpbufsize );

    /* need to be careful */
    if ( tmpbuf == NULL ) {
      *status = DAT__NOMEM;
      emsSeti( "NB", tmpbufsize );
      emsRep("DAT_GET1C_ERR2",
	     "Unable to allocate ^NB bytes for temporary buffer",
	     status);
    }

    /* Now retrieve the fortran buffer */
    dims[0] = *actval;
    datGetC( locator, 1, dims, tmpbuf, lenstr, status );

    /* For each string copy and terminate */
    if (*status == DAT__OK) {
      /* Get pointers to the start of the HDS buffer and the user supplied
	 buffer and note the number of characters we can copy */
      inpos = tmpbuf;
      outpos = buffer;
      nleft = bufsize;

      for ( n = 0; n < *actval ; n++ ) {
	if (*status != DAT__OK) break;

	/* Store the pointer to the start of the nth string */

	if (pntrs != NULL) pntrs[n] = outpos;

	/* Find out how many characters to copy out of the temp buffer */
	for (i = lenstr-1; i >=0; i-- ) {
	  if (inpos[i] != ' ') break;
	}

	/* Number of characters to copy is 1 plus position at exit of loop */
	i++;

	if ( nleft < (i+1) ) {
	  *status = DAT__TRUNC;
	  emsSeti( "N", *actval );
	  emsSeti( "SZ", lenstr );
	  emsSeti( "NEED", i+1);
	  emsSeti( "LEFT", nleft );
	  emsRep("datGet1C","datGet1C: Insufficient space supplied by caller to receive ^N strings from _CHAR*^SZ array."
		 " Need ^NEED but only have ^LEFT bytes remaining in buffer", status);
	}

	/* Copy i characters to output buffer if the will fit */
	strncpy( outpos, inpos, i );

	/* Terminate */
	outpos[i] = '\0';

	/* Increment the buffer positions */
	nleft  -= i+1;
	outpos += i + 1;
	inpos  += lenstr;

      }

    }

    /* Free the temporary buffer */
    MEM_FREE( tmpbuf );

  }

  return *status;
}

