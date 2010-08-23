      SUBROUTINE KPG1_SRCTA( STATUS )
*+
*  Name:
*     KPG1_SRCTA

*  Purpose:
*     Reads AST_ data as text from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SRCTA( STATUS )

*  Description:
*     This is a service routine to be provided as a "source" routine
*     for the AST_CHANNEL function. It reads data from a GRP group
*     (in response to reading from an AST_ Channel) and delivers it to
*     the AST_ library for interpretation.
*
*     This routine has only a STATUS argument, so it communicates with
*     other KPG routines via global variables stored in the KPG_AST common
*     blocks. These are described below under "Global Variables used as
*     Arguments".

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Global Variables used as Arguments:
*     ASTGRP = INTEGER (Given)
*        A GRP identifier for the group which holds the data.
*     ASTLN = INTEGER (Given and Returned)
*        This must initially be set to the value 1, to indicate that
*        data will be read starting at the first element of the group
*        (note the routine will not operate correctly unless 1 is
*        the initial value - you cannot start reading at another point
*        in the group if you have previously read from a different
*        group). On exit it will be incremented by the number of
*        elements used to obtain data, so that it identifies the first
*        element to be used on the next invocation.
*     ASTTSZ = INTEGER (Given)
*        The number of characters to use from each GRP element.

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1998 (DSB):
*        Original version, based on NDF1_RDAST.
*     26-MAY-1999 (DSB):
*        Only read the required length from each GRP element.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants (needed by KPG_AST)
      INCLUDE 'GRP_PAR'          ! GRP_ public constants
      INCLUDE 'KPG_PAR'          ! KPG_ constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTGRP = INTEGER (Read)
*           GRP identifier for group holding AST_ data.
*        ASTLN = INTEGER (Read and Write)
*           Next element to use in group holding AST_ data.
*        ASTTSZ = INTEGER (Read)
*           The length to read from each GRP element.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER EMPTY              ! Empty character buffer
      PARAMETER ( EMPTY = -1 )
      INTEGER SZTEXT             ! Size of text buffer
      PARAMETER ( SZTEXT = ( GRP__SZNAM - 1 ) * ( KPG__MXACL + 1 ) )

*  Local Variables:
      CHARACTER * ( SZTEXT ) NEXT ! Buffer for input text
      CHARACTER * ( SZTEXT ) TEXT ! Buffer for AST_ text
      INTEGER DIM                 ! Size of GRP group
      INTEGER L                   ! Number of characters in AST_ text
      LOGICAL AGAIN               ! Loop to read another line?

      SAVE DIM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Before reading the first line, obtain the number of elements in the
*  group being read from.
      IF ( ASTLN .EQ. 1 ) CALL GRP_GRPSZ( ASTGRP, DIM, STATUS )

*  Loop to extract lines (including continuation lines) from the
*  group and to re-assemble them into a single line.
      L = EMPTY
      IF ( STATUS .EQ. SAI__OK ) THEN
         AGAIN = .TRUE.
 1       CONTINUE                ! Start of "DO WHILE" loop
         IF ( AGAIN .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Check that we have not reached the end of the group. If not, obtain
*  the contents of the next element.
            IF ( ASTLN .LE. DIM ) THEN
               CALL GRP_GET( ASTGRP, ASTLN, 1, NEXT( : GRP__SZNAM ),
     :                       STATUS )

*  If this is the first element read, insert it at the start of the
*  text buffer (minus the first character, which is a flag) and update
*  the count of characters in this buffer. Also update the group
*  element number.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( L .EQ. EMPTY ) THEN
                     TEXT( : ASTTSZ - 1 ) = NEXT( 2 : ASTTSZ )
                     L = ASTTSZ - 1
                     ASTLN = ASTLN + 1

*  If it is a continuation line, check whether its contents can be
*  appended to the text buffer without exceeding its length. If not,
*  then report an error.
                  ELSE IF ( NEXT( 1 : 1 ) .EQ. '+' ) THEN
                     IF ( ( L + ASTTSZ - 1 ) .GT. LEN( TEXT ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'LEN', LEN( TEXT ) )
                        CALL ERR_REP( 'KPG1_SRCTA_CONT',
     :                       'Too many input continuation lines; ' //
     :                       'internal buffer length of ^LEN ' //
     :                       'characters exceeded.', STATUS )

*  Otherwise, append its contents to the text buffer and advance the
*  input group element.
                     ELSE
                        TEXT( L + 1 : L + ASTTSZ - 1 ) =
     :                     NEXT( 2 : ASTTSZ )
                        L = L + ASTTSZ - 1
                        ASTLN = ASTLN + 1
                     END IF

*  Quit looping if we have read all the continuation lines.
                  ELSE
                     AGAIN = .FALSE.
                  END IF
               END IF

*  Also quit looping if the input group is exhausted.
            ELSE
               AGAIN = .FALSE.
            END IF
            GO TO 1              ! End of "DO WHILE" loop
         END IF
      END IF

*  Remove any trailing blanks from the text buffer.
      IF ( L .GT. 0 ) L = CHR_LEN( TEXT( : L ) )

*  If there has been an error, set the number of characters to EMPTY
*  (-1) to indicate no more data. If no text has been read (e.g. the
*  input group is exhausted), then this value will still be
*  set to EMPTY anyway.
      IF ( STATUS .NE. SAI__OK ) L = EMPTY

*  Send the text to the AST_ library for interpretation.
      CALL AST_PUTLINE( TEXT, L, STATUS )

      END
