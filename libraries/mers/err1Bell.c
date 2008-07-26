/*
*+
*  Name:
*     err1Bell

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     CALL ERR1_BELL( STATUS )

*  Description:
*     A bell character is delivered to the user. If the user interface 
*     in use supports this character, this will ring a bell on the 
*     terminal.

*  Arguments:
*     status = int * (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     25-JUL-2008 (TIMJ):
*        Rewrite in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "sae_par.h"

void err1Bell( int * status ) {

  /* Fortran version only set status when return status from Prerr
     was bad but this could leave it uninitialised */
  *status = SAI__OK;

  /*  deliver the bell character. */
  err1Prerr( "\a", status );

}

/* Remove fortran interface when no longer needed */

/*
*+
*  Name:
*     ERR1_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink Fortran 77

*  Invocation:
*     CALL ERR1_BELL( STATUS )

*  Description:
*     A bell character is delivered to the user. If the user interface 
*     in use supports this character, this will ring a bell on the 
*     terminal.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.
*/

#include "f77.h"
F77_SUBROUTINE(err1_bell)( INTEGER(STATUS) );

F77_SUBROUTINE(err1_bell)( INTEGER(STATUS) ) {
  int status;
  err1Bell( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
