      SUBROUTINE KPG1_ASGRP( PARAM, FRM, IGRP, NP, NAX, OUT, STATUS )
*+
*  Name:
*     KPG1_ASGRP

*  Purpose:
*     Reads spatial positions from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGRP( PARAM, FRM, IGRP, NP, NAX, OUT, STATUS )

*  Description:
*     This routine reads formatted positions from a GRP group. The
*     positions are assumed to be in the supplied Frame. Each element
*     in the group should contain 1 position per line. Each position is
*     given by a set of strings delimited by comma, space or tab (the
*     first gives the value for Axis 1, the second for Axis 2, etc.). The
*     number of strings per element in the group should equal the number
*     of axes in the Base Frame of the supplied FrameSet.
*
*     An error is reported if any unreadable elements are found.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an environment parameter to use to get the indices of
*        the columns within the text file which are to be used. If blank,
*        the file must contain exactly NAX columns, all of which are used.
*        If a null value is supplied, the dynamic default values will be
*        used, which is [1,2,3... NAX].
*     FRM = INTEGER (Given)
*        A pointer to an AST Frame.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group to read.
*     NP = INTEGER (Given)
*        The number of positions which can be stored in the returned array.
*     NAX = INTEGER (Given)
*        The number of axes in the supplied Frame.
*     OUT( NP, NAX ) = DOUBLE PRECISION (Returned)
*        The array to hold the returned co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     17-SEP-1999 (DSB):
*        Added argument PARAM.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRM
      INTEGER IGRP
      INTEGER NP
      INTEGER NAX

*  Arguments Returned:
      DOUBLE PRECISION OUT( NP, NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER CHR_NTH          ! Returns "st", "nd", "rd", etc.

*  Local Constants:
      INTEGER MXCOL              ! Max no of columns in a text file
      PARAMETER ( MXCOL = 200 )

*  Local Variables:
      CHARACTER MESS*30          ! Error message phrase
      CHARACTER TEXT*(GRP__SZNAM)! Text of current element
      CHARACTER WORDS( MXCOL )*50! Words extracted from current element
      INTEGER I                  ! Element index
      INTEGER J                  ! Column index
      INTEGER K                  ! Axis index
      INTEGER LSTAT              ! CHR status
      INTEGER NC                 ! No. of characters used by AST_UNFORMAT
      INTEGER NCW                ! No. of characters in current word
      INTEGER NWRD               ! No. of words in current element
      INTEGER SIZE               ! No. of elements in group
      INTEGER WSTART( MXCOL )    ! Index of start of each word
      INTEGER WSTOP( MXCOL )     ! Index of end of each word
      LOGICAL OK                 ! Can element be read?
      INTEGER POSMAX             ! Largest required column index
      INTEGER POSDEF( MXCOL )    ! Default column indices
      INTEGER POSCOD( MXCOL )    ! Required column indices
      LOGICAL POSDUP             ! Duplicate column indices supplied?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If no parameter has been supplied, use columns 1 to NAX.
      IF( PARAM .EQ. ' ' ) THEN
         DO I = 1, NAX
            POSCOD( I ) = I
         END DO

*  Otherwise, ask the user which columns to use.
      ELSE

*  Set dynamic defaults for position columns.
         DO I = 1, NAX
            POSDEF( I ) = I
         END DO

*  Loop until we have a unique set of columns.
         POSDUP = .TRUE.
         DO WHILE ( POSDUP .AND. STATUS .EQ. SAI__OK )

*  Get the co-ordinates.
            CALL PAR_GDR1I( PARAM, NAX, POSDEF, 1, VAL__MAXI, .TRUE.,
     :                      POSCOD, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN

*  Assume no duplication for the moment.
               POSDUP = .FALSE.

*  Check for duplication of the  co-ordinate columns.
               DO I = 1, NAX - 1
                  DO J = I + 1, NAX
                     IF( POSCOD( I ) .EQ. POSCOD( J ) .AND.
     :                   STATUS .EQ. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', I )
                        CALL MSG_SETC( 'I', CHR_NTH( I ) )
                        CALL MSG_SETI( 'J', J )
                        CALL MSG_SETC( 'J', CHR_NTH( J ) )
                        CALL MSG_SETC( 'P', PARAM )
                        CALL ERR_REP( 'KPG1_ASGRP_ERR1', '^I and ^J '//
     :                           'columns specified by parameter %^P '//
     :                           'are equal.', STATUS )
                     END IF
                  END DO
               END DO

*  Report the error immediately, and reset the status to OK.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )

*  There is duplication so try again.
                  POSDUP = .TRUE.

*  Cancel the parameters so that the user can be re-prompted.
                  CALL PAR_CANCL( PARAM, STATUS )
               END IF

            END IF

         END DO

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END IF

*  Find the largest required column index.
      POSMAX = 0
      DO I = 1, NAX
         POSMAX = MAX( POSMAX, POSCOD( I ) )
      END DO

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Loop round each element in the group.
      DO I = 1, MIN( SIZE, NP )
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

*  Assume the element cannot be read.
         OK = .FALSE.

*  Replace commas and tabs by spaces.
         LSTAT = SAI__OK
         CALL CHR_TRCHR( ',	', '  ', TEXT, LSTAT )

*  Split the element up in to words delimited by spaces.
         CALL CHR_DCWRD( TEXT, MXCOL, NWRD, WSTART, WSTOP, WORDS,
     :                   LSTAT )

*  If this element has too many or too few fields, store a message.
         IF( NWRD .LT. POSMAX ) THEN
            MESS = 'too few fields'

         ELSE IF( PARAM .EQ. ' ' .AND. NWRD .GT. NAX ) THEN
            MESS = 'too many fields'

*  Otherwise...
         ELSE

*  Assume the element can be read OK.
            OK = .TRUE.

*  Loop round each required column.
            DO K = 1, NAX

*  Get the word index.
               J = POSCOD( K )

*  Store number of characters in this word.
               NCW = WSTOP( J ) - WSTART( J ) + 1

*  Attempt to read an axis value from the current word.
               NC = AST_UNFORMAT( FRM, J, WORDS( J )( : NCW ),
     :                            OUT( I, J ), STATUS )

*  If the word could not be decoded, or if not all of the word was used,
*  we cannot use this group element.
               IF( NC .NE. NCW ) THEN
                  OK = .FALSE.
                  MESS = ' '
                  GO TO 10
               END IF

            END DO

 10         CONTINUE

         END IF

*  If this element could not be used, report an error.
         IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'D', AST_GETC( FRM, 'DOMAIN', STATUS ) )
            CALL MSG_SETI( 'N', NAX )
            CALL MSG_SETC( 'TEXT', TEXT )
            STATUS = SAI__ERROR

            IF( MESS .EQ. ' ' ) THEN
               CALL ERR_REP( 'KPG1_ASGRP_1', 'The following string '//
     :                       'could not  be interpreted as a ^N '//
     :                       'dimensional ^D position: ''^TEXT''.',
     :                       STATUS )
            ELSE
               CALL MSG_SETC( 'M', MESS )
               IF( PARAM .EQ. ' ' ) THEN
                  CALL ERR_REP( 'KPG1_ASGRP_2', 'The following string'//
     :                          ' could not  be interpreted as a ^N '//
     :                          'dimensional ^D position (^M): '//
     :                          '''^TEXT''.', STATUS )
               ELSE
                  CALL MSG_SETC( 'P', PARAM )
                  CALL ERR_REP( 'KPG1_ASGRP_3', 'The following string'//
     :                          ' could not  be interpreted as a ^N '//
     :                          'dimensional ^D position using the '//
     :                          'columns given by parameter %^P (^M)'//
     :                          ': ''^TEXT''.', STATUS )
               END IF

            END IF

         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Fill any un-used room in the array with bad values.
      DO I = MIN( SIZE, NP ) + 1, NP
         DO J = 1, NAX
            OUT( I, J ) = AST__BAD
         END DO
      END DO

 999  CONTINUE

      END
