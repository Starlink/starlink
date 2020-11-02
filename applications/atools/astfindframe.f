      SUBROUTINE ASTFINDFRAME( STATUS )
*+
*  Name:
*     ASTFINDFRAME

*  Purpose:
*     Find a coordinate system with specified characteristics.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTFINDFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application uses a "template" Frame to search another Frame
*     (or FrameSet) to identify a coordinate system which has a
*     specified set of characteristics. If a suitable coordinate
*     system can be found, the function returns a pointer to a
*     FrameSet which describes the required coordinate system and how
*     to convert coordinates to and from it.
*
*     The index of the closest matching Frame in the target FrameSet
*     is displayed on the screen and returned in output parameter
*     IFRAME.

*  Usage:
*     astfindframe target template domainlist result

*  ADAM Parameters:
*     DOMAINLIST = LITERAL (Read)
*        A string containing a comma-separated list of Frame domains.
*        This may be used to establish a priority order for the different
*        types of coordinate system that might be found.
*
*        The function will first try to find a suitable coordinate
*        system whose Domain attribute equals the first domain in this
*        list. If this fails, the second domain in the list will be
*        used, and so on, until a result is obtained. A blank domain
*        (e.g. two consecutive commas) indicates that any coordinate
*        system is acceptable (subject to the template) regardless of
*        its domain.
*
*        This list is case-insensitive and all white space is ignored.
*        If you do not wish to restrict the domain in this way,
*        you should supply a blank string or null (!) value.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     IFRAME = INTEGER (Write)
*        On exit, this holds the index of the closest matching Frame in the
*        target FrameSet, or zero if no matching Frame was found. If the
*        Target is a Frame instead of a FrameSet, then a value of 1 is
*        returned if a match is found, and zero otherwise.
*     RESULT = LITERAL (Read)
*        If the search is successful, a FrameSet is written to the specified
*        text file or NDF. Otherwise, a warning message is displayed. If
*        created, the FrameSet will contain two Frames. Frame number 1 (its
*        base Frame) represents the target coordinate system and will be the
*        same as the (base Frame of the) target. Frame number 2 (its current
*        Frame) will be a Frame representing the coordinate system which
*        the function found. The Mapping which inter-relates these two Frames
*        will describe how to convert between their respective coordinate
*        systems.
*     TARGET = LITERAL (Read)
*        An NDF or text file holding a Frame or FrameSet. If an NDF is
*        supplied, the WCS FrameSet will be used.
*     TEMPLATE = LITERAL (Read)
*        An NDF or text file holding a Frame. If an NDF is supplied, the
*        current Frame of the WCS FrameSet will be used. The Frame should
*        be an instance of the type of Frame you wish to find. If you wanted
*        to find a Frame describing a celestial coordinate system, for example,
*        then you might use a SkyFrame here.

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
*     14-FEB-2001 (DSB):
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
      CHARACTER CFD*50
      CHARACTER DOMLST*255
      INTEGER CFI
      INTEGER RESULT
      INTEGER TARGET
      INTEGER TEMPLT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the target Frame.
      CALL KPG1_GTOBJ( 'TARGET', 'Frame or FrameSet', AST_ISAFRAME,
     :                 TARGET, STATUS )

*  Get the template Frame.
      CALL KPG1_GTOBJ( 'TEMPLATE', 'Frame', AST_ISAFRAME, TEMPLT,
     :                 STATUS )

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
      RESULT = AST_FINDFRAME( TARGET, TEMPLT, DOMLST, STATUS )

*  Issue a warning if no Frame was found
      IF( RESULT .EQ. AST__NULL ) THEN
         CALL MSG_OUT( 'ASTFINDFRAME_MSG1', 'No Frame matching the '//
     :                 'supplied template could be found in the '//
     :                 'supplied target.', STATUS )
         CALL PAR_PUT0I( 'IFRAME', 0, STATUS )

*  Otherwise, tell the user which Frame was used.
      ELSE

         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'ASTFINDFRAME_MSG2', 'The closest matching '//
     :                 'Frame was:', STATUS )

         IF( AST_ISAFRAMESET( TARGET, STATUS ) ) THEN
            CFI = AST_GETI( TARGET, 'CURRENT', STATUS )
            CFD = AST_GETC( AST_GETFRAME( TARGET, AST__CURRENT,
     :                                    STATUS ),
     :                     'Domain', STATUS )

            CALL MSG_SETI( 'CFI', CFI )
            IF( CFD .NE. ' ' ) THEN
               CALL MSG_SETC( 'CFD', '(' )
               CALL MSG_SETC( 'CFD', CFD )
               CALL MSG_SETC( 'CFD', ')' )
            ELSE
               CALL MSG_SETC( 'CFD', ' ' )
            ENDIF

            CALL MSG_OUT( 'ASTFINDFRAME_MSG3', '   Frame ^CFI ^CFD in'//
     :                    ' the ''TARGET'' FrameSet.', STATUS )
            CALL PAR_PUT0I( 'IFRAME', CFI, STATUS )

         ELSE
            CALL MSG_OUT( 'ASTFINDFRAME_MSG4', '   The supplied '//
     :                    '''TARGET'' Frame.', STATUS )
            CALL PAR_PUT0I( 'IFRAME', 1, STATUS )
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
         CALL ERR_REP( 'ASTFINDFRAME_ERR', 'Error finding a matching '//
     :                 'Frame in a FrameSet.', STATUS )
      END IF

      END
