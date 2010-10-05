
#include "ems.h"
#include "hds1.h"
#include "rec.h"
#include "str.h"
#include "dat1.h"
#include "hds.h"

/*
*+
*  Name:
*     datNew0

*  Purpose:
*     Create new scalar component

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datNew0( const HDSLoc* loc, const char * name, const char *type, int * status);
*     CALL DAT_NEW0( LOC, NAME, TYPE, STATUS )

*  Description:
*     Create a scalar structure component with specified type.

*  Parameters:
*     loc = const HDSLoc * (Given)
*        Locator associated with a structured data object.
*     name = const char * (Given)
*        Expression specifying the name of the component to be
*        created in the structure.
*     type = const char * (Given)
*        Expression specifying the type of the component.
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
*     1983-AUG-31 (SLW):
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

int datNew0( const HDSLoc* locator, const char * name, const char * type, int *status ) {

  int ndim = 0;
  hdsdim dims[] = { 0 };   /* Required */

  if (*status == DAT__OK ) {
    datNew( locator, name, type, ndim, dims, status );
  }

  return *status;
}

/* Add in the very simply datNew0x variants */
/* Could put them in a separate file if the prolog is important */

/*
  DAT_NEW0D
  DAT_NEW0I
  DAT_NEW0W
  DAT_NEW0UW
  DAT_NEW0L
  DAT_NEW0R
*/

int datNew0D( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_DOUBLE", status );
  return *status;
}

int datNew0I( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_INTEGER", status );
  return *status;
}

int datNew0W( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_WORD", status );
  return *status;
}

int datNew0UW( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_UWORD", status );
  return *status;
}

int datNew0L( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_LOGICAL", status );
  return *status;
}

int datNew0R( const HDSLoc * locator, const char * name, int * status ) {
  datNew0( locator, name, "_REAL", status );
  return *status;
}

int datNew0C( const HDSLoc * locator, const char * name, size_t len, int * status ) {

#undef context_name
#undef context_message
#define context_name "DAT_NEW0C_ERR"
#define context_message\
        "DAT_NEW0C: Error creating a new HDS scalar character component."

  char type[DAT__SZTYP+1];

  if (*status != DAT__OK) return *status;

  datCctyp( len, type );
  _call(datNew0( locator, name, type, status ));

  return *status;
}
