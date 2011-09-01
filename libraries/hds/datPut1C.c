
#include <stdlib.h>

#include "dat_err.h"
#include "hds1.h"
#include "hds.h"
#include "ems.h"
#include "star/mem.h"

/*
*+
*  Name:
*     datPut1C

*  Purpose:
*     Write 1-D C string array to object

*  Invocation:
*     datPut1C( const HDSLoc * locator, size_t nval, const char *values[], int * status );
*     DAT_PUT1C( LOCATOR, NVAL, VALUES(NVAL), STATUS )

*  Description :
*     Write the values into a vector primitive object.

*  Parameters :
*     locator = const HDSLoc * (Given)
*        Variable containing a locator associated with a primitive
*        data object.
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
*     03-DEC-2005 (TIMJ):
*        Rewrite in C
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
datPut1C( const HDSLoc * locator, size_t nval, const char *values[], int * status ) {

  size_t actval;
  char * buffer;
  char * bufpos;
  size_t bufsize;
  hdsdim dims[1];
  int i;
  size_t lenstr;
  size_t thislen;
  int trunc = 0;   /* Were we truncated? */

  if (*status != DAT__OK) return *status;

  /* Verify that we have the correct number of values */
  datSize( locator, &actval, status );

  if ( nval != actval ) {
    *status = DAT__BOUND;
    emsSeti( "NV", nval );
    emsSeti( "SZ", actval );
    emsRep( "DAT_PUT1C_ERR",
	    "datPut1C: Bounds do not match HDS object (^NV != ^SZ)",
	    status);
    return *status;
  }

  /* Now we have to work out how big the strings are */
  datClen( locator, &lenstr, status );

  if (*status == DAT__OK ) {

    /* Get some memory and copy in the individual strings */
    bufsize = nval * lenstr;
    buffer = MEM_MALLOC( bufsize + 1);
    bufpos = buffer;

    for (i = 0; i < nval; i++ ) {

      thislen = strlen( values[i] );
      if (thislen > lenstr) trunc = 1;

      strncpy( bufpos, values[i], lenstr );
      bufpos += lenstr;
    }

    /* Now remove all the nuls */
    for ( i = 0; i < bufsize ; i++ ) {
      if ( buffer[i] == '\0' ) buffer[i] = ' ';
    }

    /* terminate */
    buffer[bufsize] = '\0';

    /* Now store it in the HDS object */
    dims[0] = nval;
    datPutC( locator, 1, dims, buffer, lenstr, status );

    /* Tidy up */
    MEM_FREE( buffer );

    /* if we were truncated at all, let the user know */
    if ( *status == DAT__OK && trunc ) {
      *status = DAT__TRUNC;
      emsSeti( "NN", lenstr );
      emsRep( "DAT_PUT1C_TRN", "datPut1C: Some strings were truncated when stored in _CHAR*^NN array",
	      status );
    }

  }

  return *status;
}

