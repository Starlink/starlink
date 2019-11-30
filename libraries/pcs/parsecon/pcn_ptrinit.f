      SUBROUTINE PARSECON_PTRINIT ( STATUS )
*+
*  Name:
*     PARSECON_PTRINIT

*  Purpose:
*     Initialise pointers etc. before parsing IFL.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PTRINIT ( STATUS )

*  Description:
*     Initialises pointers to the various storage lists
*     and various names before the parsing of an interface file begins.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Set the various pointers to zero.

*  Implementation Deficiencies:
*     Strictly, all the pointer arrays should be zeroed - eg PARDEF,
*     PARDYN, PARASSOC, NEEDOB, NEEDCAN.....

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1993 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984:  Original (REVAD::BDK)
*     27.02.1985:  initialize PROGNAME and EXEPATH (REVAD::BDK)
*     23.08.1985:  initialize MONOLITH (REVAD::BDK)
*     16.08.1990:  initialize ACNAME and PRNAME for error reports (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      PARPTR = 0
      ACTPTR = 0
      NEEDPTR = 0
      INTPTR = 0
      INT64PTR = 0
      REALPTR = 0
      DOUBLEPTR = 0
      CHARPTR = 0
      LOGPTR = 0
      PROGNAME = ' '
      EXEPATH = ' '
      MONOLITH = .FALSE.

*   Names in error reporting common block
      ACNAME = ' '
      PRNAME = ' '

      END
