/*
*+
*  Name:
*     smf_string_to_dtype

*  Purpose:
*     Return data type representation of a string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_string_to_dtype ( const char * datatype, smf_dtype *dtype, int * status );

*  Arguments:
*     datatype = const char* (Given)
*        String representation of data type
*     dtype = smf_dtype* (Returned)
*        smf_dtype equivalent
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function returns the smf_dtype representation of a data type string.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-08-04 (JB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_string_to_dtype"

void smf_string_to_dtype ( const char * datatype, smf_dtype *dtype, int * status ) {

   /* Check entry status */
   if (*status != SAI__OK) return;

   if ( strcmp ( datatype, "_INTEGER" ) == 0 ) {
      *dtype = SMF__INTEGER;
   } else if ( strcmp ( datatype, "_REAL" ) == 0 )
      *dtype = SMF__FLOAT;
   else if ( strcmp ( datatype, "_DOUBLE" ) == 0 )
      *dtype = SMF__DOUBLE;
   else if ( strcmp ( datatype, "_UWORD" ) == 0 )
      *dtype = SMF__USHORT;
   else {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
	      "Supplied string is not a valid data type.   Possible programming error.",
	      status);
      return;
   }

}

