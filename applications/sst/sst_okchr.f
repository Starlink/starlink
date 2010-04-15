      CHARACTER * 1 FUNCTION SST_OKCHR( OK )
*+
*  Name:
*     SST_OKCHR

*  Purpose:
*     Return a flag character if a logical value is .FALSE..

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SST_OKCHR( OK )

*  Description:
*     The function returns the '*' character if its logical argument is
*     .FALSE.. Otherwise, a blank is returned. It is intended for
*     generating flag characters to appear in tables next to entries
*     which are considered "not OK" for some reason.

*  Arguments:
*     OK = LOGICAL (Given)
*        Logical value to be translated into a flag character.

*  Returned Value:
*     SST_OKCHR = CHARACTER * ( 1 )
*        Either ' ' or '*', depending on whether the OK argument is
*        .TRUE. or .FALSE..

*  Algorithm:
*     -  Simply return the appropriate character.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Changed name to SST_OKCHR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL OK

*.

*  Return the required character.
      IF( OK ) THEN
         SST_OKCHR = ' '
      ELSE
         SST_OKCHR = '*'
      ENDIF

      END
* @(#)sst_okchr.f   1.1   94/12/05 11:31:31   96/07/05 10:27:29
