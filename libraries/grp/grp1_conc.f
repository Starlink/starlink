      SUBROUTINE GRP1_CONC( SLOT, INDEX, CH, OK, STATUS )
*+
*  Name:
*     GRP1_CONC

*  Purpose:
*     Returns a group control character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_CONC( SLOT, INDEX, CH, OK, STATUS )

*  Description:
*     Each group has associated with it several "control characters"
*     which are the characters used to indicate various items of syntax
*     within a group expression. These control characters can be
*     changed at any time by calling GRP_SETCC. This routine returns
*     the current value of a single requested control character.  The
*     individual characters are described in GRP__SETCC.
*
*     All the control characters for each group are stored in a single
*     string in a common array. Each control character has an
*     associated integer parameter of the form GRP__PxxxC, where "xxx"
*     indicates the particular control character. These parameters
*     specify the index of the corresponding control character within
*     the string held in common.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group with which the required
*        control character is associated.
*     INDEX = INTEGER (Given)
*        The index of the required control character within the string
*        CMN_CHAR( SLOT ) held in common. The calling routine should
*        specify one of the parameters GRP__PxxxC (see GRP_CONST) for
*        this argument.
*     CH = CHARACTER * ( * ) (Returned)
*        The required control character is returned as the first
*        character in this string. The rest of the string is unchanged.
*     OK = LOGICAL (Returned)
*        OK is returned .TRUE. if the requested control character does
*        not have the same value as the NULL control character, and is
*        returned .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants
      INCLUDE 'GRP_CONST'        ! GRP private constants
      INCLUDE 'GRP_ERR'          ! GRP errors

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CHARS( GRP__MAXG ) = CHARACTER*(GRP__NCHAR) (Read)
*           The control characters used to define the syntax of group
*           expressions. A set of characters stored in a single string
*           for each group. The order is defined by global constants
*           GRP__PINDC, GRP__PDELC, GRP__PCOMC, etc (see GRP_CONST).

*  Arguments Given:
      INTEGER SLOT
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER CH*(*)
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  IF the index is invalid report an error.
      IF( INDEX .LE. 0 .OR. INDEX .GT. GRP__NCHAR ) THEN
         STATUS = GRP__BADCC
         CALL MSG_SETI( 'INDEX', INDEX )
         CALL ERR_REP( 'GRP1_CONC_ERR1',
     :  'GRP1_CONC: Illegal control character index (^INDEX) supplied.',
     :                 STATUS )

*  Otherwise, extract the required control character.
      ELSE
         CH( 1 : 1 ) = CMN_CHARS( SLOT )( INDEX : INDEX )

*  Compare it against the NULL control character.
         IF( CH( 1 : 1 ) .EQ.
     :       CMN_CHARS( SLOT )( GRP__PNULC : GRP__PNULC ) ) THEN
            OK = .FALSE.
         ELSE
            OK = .TRUE.
         END IF

      END IF

*  If the escape character has been requested, but no escape character is
*  defined, use a "safe" value.
      IF( INDEX .EQ. GRP__PESCC .AND. .NOT. OK ) THEN
         CH( 1 : 1 ) = CHAR( GRP__DFESC )
         OK = .TRUE.
      END IF

      END
