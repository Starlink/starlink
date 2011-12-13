      SUBROUTINE KPG1_AGFND( NAME, PICID, STATUS )
*+
*  Name:
*     KPG1_AGFND

*  Purpose:
*     Selects the highest picture of a given name within the current AGI
*     picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AGFND( NAME, PICID, STATUS )

*  Description:
*     This routine searches forwards through the AGI database for a
*     picture of a given name that lies within the current picture,
*     including the current picture itself.  If one is found it
*     becomes the new current picture.  If it could not be found a
*     bad status will be returned, and the current picture is unchanged.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the picture to be searched for in the graphics
*        database.
*     PICID = INTEGER (Returned)
*        The picture identifier of the most-recent picture named NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 7 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * )
     :  NAME

*  Arguments Returned:
      INTEGER
     :  PICID                    ! Input picture identifier

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  PICIDC,                  ! Data image picture identifier
     :  PICIDT                   ! Work picture identifier

      CHARACTER * ( DAT__SZNAM )
     :  PNAME                    ! Name of the current picture.

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise the temporary and image picture identifications.

      PICIDT = -1
      PICID = -1

*    It could be the input picture.

      CALL AGI_INAME( PNAME, STATUS )
      CALL CHR_UCASE( PNAME )
      IF ( PNAME .NE. NAME ) THEN

*       Try to get the last related named picture from the database.

         CALL AGI_RCL( NAME, PICIDT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*          There is no picture of the chosen name in the current frame
*          so report the error context.

            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'KPG1_AGFND_NOTPRE',
     :        'The current database picture is not of name ^NAME, or '/
     :        /'it does not contain a ^NAME picture within itself.',
     :        STATUS )
         ELSE
            PICID = PICIDT
         END IF

      ELSE

*       Obtain the current picture identifier.

         CALL AGI_ICURP( PICIDC, STATUS )

*       Use the current picture.

         PICID = PICIDC
      END IF

*    Select the picture of the chosen name to be the current
*    database picture.

      CALL AGI_SELP( PICID, STATUS )

      END
