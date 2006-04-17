      SUBROUTINE ERR_LEVEL( LEVEL )
*+
*  Name:
*     ERR_LEVEL

*  Purpose:
*     Inquire the current error context level. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_LEVEL( LEVEL )

*  Description:
*     Return the number of context markers set in the error message table. 

*  Arguments:
*     LEVEL = INTEGER (Returned)
*        The error context level: all values greater than one indicate 
*        the deferral of reported error messages.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     9-OCT-1990 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Returned:
      INTEGER LEVEL

*.

*  Call EMS_LEVEL.
      CALL EMS_LEVEL( LEVEL )

      END
