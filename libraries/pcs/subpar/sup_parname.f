      SUBROUTINE SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)
*+
*  Name:
*     SUBPAR_PARNAME

*  Purpose:
*     Get the name of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PARNAME( NAMECODE, NAME, NAMELEN, STATUS)

*  Description:
*     Get the name of a parameter

*  Arguments:
*     NAMECODE=INTEGER(INPUT)
*        Identifier of the parameter
*     NAME=CHARACTER*(*)(OUTPUT)
*        Name of the parameter
*     NAMELEN=INTEGER(OUTPUT)
*        Length of the parameter name
*     STATUS=INTEGER(UPDATE)
*        SSE status variable

*  Algorithm:
*     Get the name from the global variable

*  Copyright:
*     Copyright (C) 1986, 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     JHF: Jon Fairclough (UKTH::JHF)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-May-1986 (JHF):
*        Original
*     03-MAR-1993 (AJC):
*        Use SAE_PAR not ADAMERRS
*        Add DAT_PAR for SUBPAR_CMN
*     07-SEP-1993 (AJC):
*        Report if error
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! ADAM Symbolic Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER NAMECODE

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER NAMELEN

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.

      IF (STATUS .NE. SAI__OK) RETURN
*
*    Begin
*
      IF (NAMECODE .GE. 1 .AND. NAMECODE .LE. SUBPAR__MAXPAR) THEN
         NAME = PARNAMES(NAMECODE)
         NAMELEN = PARLEN(NAMECODE)
      ELSE
         STATUS = SUBPAR__NOPAR
         CALL EMS_REP('SUP_PARNAME1',
     :   'SUBPAR_PARNAME: NAMECODE out of range', STATUS )
      ENDIF
*
*    End
*
      END
