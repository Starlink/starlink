      SUBROUTINE ASTCONVERT( STATUS )
*+
*  Name:
*     ASTCONVERT

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCONVERT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application compares two FrameSets (or Frames) and determines
*     whether it is possible to convert between the coordinate systems which
*     they represent. If conversion is possible, it returns a FrameSet
*     which describes the conversion and which may be used (as a
*     Mapping) to transform coordinate values in either direction.

*  Usage:
*     astconvert from to domainlist result

*  ADAM Parameters:
*     DOMAINLIST = LITERAL (Read)
*        A string containing a comma-separated list of Frame domains.
*        This may be used to define a priority order for the different
*        intermediate coordinate systems that might be used to perform
*        the conversion. If a blank or null (!) value indicates that all
*        coordinate systems should be considered, regardless of their
*        domains.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FROM = LITERAL (Read)
*        An NDF or text file holding a Frame or FrameSet. If an NDF is
*        supplied, the WCS FrameSet will be used. It represents the
*        "source" coordinate system.  This is the coordinate system in
*        which you already have coordinates available. If a FrameSet is
*        given, its current Frame is taken to describe the source
*        coordinate system.
*     TO = LITERAL (Read)
*        An NDF or text file holding a Frame or FrameSet. If an NDF is
*        supplied, the WCS FrameSet will be used. It represents the
*        "destination" coordinate system. This is the coordinate system
*        into which you wish to convert your coordinates. If a FrameSet
*        is given, its current Frame is taken to describe the destination
*        coordinate system.
*     RESULT = LITERAL (Read)
*        If the requested coordinate conversion is possible, a FrameSet is
*        written to the specified text file. Otherwise, a warning message
*        is displayed. If created, the FrameSet will contain two Frames.
*        Frame number 1 (its base Frame) will describe the source coordinate
*        system, corresponding to the FROM parameter. Frame number 2
*        (its current Frame) will describe the destination coordinate
*        system, corresponding to the TO parameter. The Mapping
*        which inter-relates these two Frames will perform the
*        required conversion between their respective coordinate
*        systems.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     12-JAN-2001 (DSB):
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
      EXTERNAL AST_ISAFRAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER BFD*50
      CHARACTER DOMLST*255
      INTEGER BFI
      INTEGER FROM
      INTEGER RESULT
      INTEGER TO
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the source Frame.
      CALL KPG1_GTOBJ( 'FROM', 'Frame or FrameSet', AST_ISAFRAME,
     :                 FROM, STATUS )

*  Get the destination Frame.
      CALL KPG1_GTOBJ( 'TO', 'Frame or FrameSet', AST_ISAFRAME,
     :                 TO, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the domain list.
      CALL PAR_GET0C( 'DOMAINLIST', DOMLST, STATUS )

*  IF a null value was supplied, annul the error and use a blank domain
*  list.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DOMLST = ' '
      END IF

*  Get the required FrameSet.
      RESULT = AST_CONVERT( FROM, TO, DOMLST, STATUS )

*  Issue a warning if no conversion was possible.
      IF( RESULT .EQ. AST__NULL ) THEN
         CALL MSG_OUT( 'ASTCONVERT_MSG1', 'No Mapping could be found '//
     :                 'between the two supplied coordinate systems.',
     :                 STATUS )

*  Otherwise, tell the user which Base Frames were used.
      ELSE

         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'ASTCONVERT_MSG2', 'Conversion was achieved '//
     :                 'by aligning the following Frames:', STATUS )

         IF( AST_ISAFRAMESET( FROM, STATUS ) ) THEN
            BFI = AST_GETI( FROM, 'BASE', STATUS )
            BFD = AST_GETC( AST_GETFRAME( FROM, AST__BASE, STATUS ),
     :                     'Domain', STATUS )

            CALL MSG_SETI( 'BFI', BFI )
            IF( BFD .NE. ' ' ) THEN
               CALL MSG_SETC( 'BFD', '(' )
               CALL MSG_SETC( 'BFD', BFD )
               CALL MSG_SETC( 'BFD', ')' )
            ELSE
               CALL MSG_SETC( 'BFD', ' ' )
            ENDIF

            CALL MSG_OUT( 'ASTCONVERT_MSG3', '   Frame ^BFI ^BFD in '//
     :                    'the ''FROM'' FrameSet.', STATUS )
         ELSE
            CALL MSG_OUT( 'ASTCONVERT_MSG4', '   The supplied '//
     :                    '''FROM'' Frame.', STATUS )
         END IF

         IF( AST_ISAFRAMESET( TO, STATUS ) ) THEN
            BFI = AST_GETI( TO, 'BASE', STATUS )
            BFD = AST_GETC( AST_GETFRAME( TO, AST__BASE, STATUS ),
     :                     'Domain', STATUS )

            CALL MSG_SETI( 'BFI', BFI )
            IF( BFD .NE. ' ' ) THEN
               CALL MSG_SETC( 'BFD', '(' )
               CALL MSG_SETC( 'BFD', BFD )
               CALL MSG_SETC( 'BFD', ')' )
            ELSE
               CALL MSG_SETC( 'BFD', ' ' )
            ENDIF

            CALL MSG_OUT( 'AST CONVERT_MSG5', '   Frame ^BFI ^BFD in '//
     :                    'the ''TO'' FrameSet.', STATUS )
         ELSE
            CALL MSG_OUT( 'ASTCONVERT_MSG6', '   The supplied ''TO'' '//
     :                    'Frame.', STATUS )
         END IF

         CALL MSG_BLANK( STATUS )

*  Write the FrameSet out to a text file.
         CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )
      END IF

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCONVERT_ERR', 'Error finding a conversion '//
     :                 'between two FrameSets.', STATUS )
      END IF

      END
