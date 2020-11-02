      SUBROUTINE ASTSETREFPOS( STATUS )
*+
*  Name:
*     ASTSETREFPOS

*  Purpose:
*     Set the reference position for a SpecFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSETREFPOS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application sets the reference position of a SpecFrame
*     (specified by attributes RefRA and RefDec) using axis values
*     (in radians) supplied within the celestial coordinate system
*     represented by a supplied SkyFrame.

*  Usage:
*     astsetrefpos this frm lon lat result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRM = LITERAL (Read)
*        An NDF or text file holding the SkyFrame that describes the
*        coordinate system to which the LON and LAT parameter values refer.
*        If an NDF is supplied, the current Frame of its WCS FrameSet will
*        be used. If a null (!) value is supplied, LON and LAT will be
*        assumed to be FK5 J2000 right ascension and declination.
*     LAT = LITERAL (Read)
*        The formatted sky latitude value of the reference position, in
*        the system specified by FRM.
*     LON = _DOUBLE (Read)
*        The formatted sky longitude value of the reference position, in
*        the system specified by FRM.
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified SpecFrame. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new Object if possible (if it is a FrameSet in which the base
*        Frame has Domain GRID and has 1 axis for each NDF dimension).
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
      CHARACTER LAT*50
      CHARACTER LON*50
      DOUBLE PRECISION DLON
      DOUBLE PRECISION DLAT
      INTEGER SFRM
      INTEGER UFRM
      INTEGER ILAT
      INTEGER ILON
      INTEGER FRM
      INTEGER NC
      INTEGER RESULT
      INTEGER THIS
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
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CLASS', AST_GETC( SFRM, 'Class', STATUS ) )
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
     :       STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'CLASS', AST_GETC( UFRM, 'Class', STATUS ) )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'An AST ^CLASS was provided for '//
     :                    'parameter FRM, but a SkyFrame was needed.',
     :                    STATUS )
            GO TO 999
         END IF
      END IF

*  Get the string longitude and latitude values
      CALL PAR_GET0C( 'LON', LON, STATUS )
      CALL PAR_GET0C( 'LAT', LAT, STATUS )

*  Unformat them.
      ILON = AST_GETI( UFRM, 'LONAXIS', STATUS )
      NC = AST_UNFORMAT( UFRM, ILON, LON, DLON, STATUS )
      IF( NC .NE. LEN( LON ) .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'L', LON )
         CALL ERR_REP( ' ', 'Invalid longitude value given for '//
     :                 'parameter LON: ''^L''.', STATUS )
         GO TO 999
      END IF

      ILAT = AST_GETI( UFRM, 'LATAXIS', STATUS )
      NC = AST_UNFORMAT( UFRM, ILAT, LAT, DLAT, STATUS )
      IF( NC .NE. LEN( LAT ) .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'L', LAT )
         CALL ERR_REP( ' ', 'Invalid latitude value given for '//
     :                 'parameter LAT: ''^L''.', STATUS )
         GO TO 999
      END IF

*  Set the reference position.
      CALL AST_SETREFPOS( SFRM, FRM, DLON, DLAT, STATUS )

*  Write the modified Object out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSETREFPOS_ERR', 'Error setting the '//
     :                 'reference position of a SpecFrame.', STATUS )
      END IF

      END
