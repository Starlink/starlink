      SUBROUTINE ASTGETREFPOS( STATUS )
*+
*  Name:
*     ASTGETREFPOS

*  Purpose:
*     Get the reference position for a SpecFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETREFPOS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns the reference position (specified by
*     attributes RefRA and RefDec) converted to the celestial coordinate
*     system represented by a supplied SkyFrame. The formated celestial
*     longitude and latitude values are displayed.

*  Usage:
*     astgetrefpos this frm

*  ADAM Parameters:
*     FRM = LITERAL (Read)
*        An NDF or text file holding the SkyFrame that describes the
*        coordinate system in which the reference longitude and latitude
*        values should be displayed. If an NDF is supplied, the current
*        Frame of its WCS FrameSet will be used. If a null (!) value is
*        supplied, the reference position will be displayed as FK5 J2000
*        right ascension and declination.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the SpecFrame. If an NDF is supplied,
*        the current Frame in the WCS FrameSet will be used.

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JAN-2003 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  External References:
      EXTERNAL AST_ISASPECFRAME
      EXTERNAL AST_ISASKYFRAME
      EXTERNAL AST_ISAFRAMESET

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER ATTR*20
      DOUBLE PRECISION LON
      DOUBLE PRECISION LAT
      INTEGER SFRM
      INTEGER CFRM
      INTEGER FRM
      INTEGER IAT
      INTEGER ILAT
      INTEGER ILON
      INTEGER RESULT
      INTEGER THIS
      INTEGER UFRM
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the SpecFrame, check its class.
      CALL KPG1_GTOBJ( 'THIS', ' ', AST_NULL, THIS, STATUS )
      IF( AST_ISAFRAMESET( THIS, STATUS ) ) THEN
         SFRM = AST_GETFRAME( THIS, AST__CURRENT, STATUS )
      ELSE
         SFRM = AST_CLONE( THIS, STATUS )
      END IF

      IF( .NOT. AST_ISASPECFRAME( SFRM, STATUS ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'CLASS', AST_GETC( SFRM, 'Class', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'An AST ^CLASS was provided for parameter'//
     :                 ' THIS, but a SpecFrame was needed.', STATUS )
         GO TO 999
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the SkyFrame, check its class.
      CALL KPG1_GTOBJ( 'FRM', ' ', AST_NULL, FRM, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         UFRM = AST_SKYFRAME( ' ', STATUS )
      ELSE
         IF( AST_ISAFRAMESET( FRM, STATUS ) ) THEN
            UFRM = AST_GETFRAME( FRM, AST__CURRENT, STATUS )
         ELSE
            UFRM = AST_CLONE( FRM, STATUS )
         END IF

         IF( .NOT. AST_ISASKYFRAME( UFRM, STATUS ) .AND.
     :        STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'CLASS', AST_GETC( UFRM, 'Class', STATUS ) )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'An AST ^CLASS was provided for '//
     :                    'parameter FRM, but a SkyFrame was needed.',
     :                    STATUS )
            GO TO 999
         END IF

      END IF

*  Get the reference position.
      CALL AST_GETREFPOS( SFRM, FRM, LON, LAT, STATUS )

*  Format and display the reference position
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'SpecFrame reference position is:', STATUS )

      ILON = AST_GETI( UFRM, 'LonAxis', STATUS )
      CALL MSG_SETC( 'LON', AST_FORMAT( UFRM, ILON, LON, STATUS ) )
      ATTR = 'Label('
      IAT = 6
      CALL CHR_PUTI( ILON, ATTR, IAT )
      CALL CHR_PUTC( ')', ATTR, IAT )
      CALL MSG_SETC( 'LAB', AST_GETC( UFRM, ATTR( : IAT ), STATUS ) )
      CALL MSG_OUT( ' ', '  ^LAB : ^LON', STATUS )

      ILAT = AST_GETI( UFRM, 'LatAxis', STATUS )
      CALL MSG_SETC( 'LAT', AST_FORMAT( UFRM, ILAT, LAT, STATUS ) )
      ATTR = 'Label('
      IAT = 6
      CALL CHR_PUTI( ILAT, ATTR, IAT )
      CALL CHR_PUTC( ')', ATTR, IAT )
      CALL MSG_SETC( 'LAB', AST_GETC( UFRM, ATTR( : IAT ), STATUS ) )
      CALL MSG_OUT( ' ', '  ^LAB : ^LAT', STATUS )

      CALL MSG_BLANK( STATUS )
      CALL MSG_SETC( 'TTL', AST_GETC( UFRM, 'Title', STATUS ) )
      CALL MSG_OUT( ' ', '  (^ttl)', STATUS )
      CALL MSG_BLANK( STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTGETREFPOS_ERR', 'Error getting the '//
     :                 'reference position of a SpecFrame.', STATUS )
      END IF

      END
