      SUBROUTINE KPS1_ZPDEC( FNAME, FRM, TEXT, POS, NP, REGION, LINE,
     :                       OBJECT, OK, STATUS )
*+
*  Name:
*     KPS1_ZPDEC

*  Purpose:
*     Decodes a record from a ZAPLIN steering file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ZPDEC( FNAME, FRM, TEXT, POS, NP, REGION, LINE, OBJECT,
*                      OK, STATUS )

*  Description:
*     This routine extracts field from a line of text read from a ZAPLIN
*     steering file, classifies the type of region described by the
*     line, and returns the corresponding axis values in the supplied
*     Frame.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        The name of the text file from which the supplied text was
*        read.  Used only in error messages.
*     FRM = INTEGER (Given)
*        A pointer to the AST Frame to be used to unformat the axis
*        values.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text read from the file.  It should contain no leading
*        spaces.
*     POS( 2, 2 ) = DOUBLE PRECISION (Given and Returned)
*        On entry, this array should hold two typical positions on the
*        image.  On exit, it holds the positions read from the text.
*        The first index is position number, and the second index is
*        axis number.
*     NP = INTEGER (Returned)
*        The number of positions read from the line.  Allowed values
*        are 0, 1 or 2.
*     REGION = LOGICAL (Returned)
*        Does the text represent a region?
*     LINE = LOGICAL (Returned)
*        Does the text represent a set of image lines?
*     OBJECT = CHARACTER * ( * ) (Returned)
*        A string describing the type of region to be zapped.
*     OK = LOGICAL (Returned)
*        Was this a comment line or blank line?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - No error is reported if an illegally formatted line is supplied,
*     but NP is returned as 0 (OK is still returned .TRUE.).

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JAN-2000 (DSB):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations

*  Arguments Given:
      CHARACTER FNAME*(*)
      INTEGER FRM
      CHARACTER TEXT*(*)

*  Arguments Given and Returned:
      DOUBLE PRECISION POS( 2, 2 )

*  Arguments Returned:
      INTEGER NP
      LOGICAL REGION
      LOGICAL LINE
      CHARACTER OBJECT*(*)
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*( 10 )      ! Axis label
      CHARACTER LC*( 1 )         ! First character in 3rd word
      DOUBLE PRECISION AXVAL( 2 )! Axis values
      INTEGER AX( 2 )            ! Axis indices for first two values
      INTEGER F                  ! Index of first character
      INTEGER I                  ! Loop index
      INTEGER IAX                ! Axis index to use
      INTEGER L                  ! Index of last character
      INTEGER NC                 ! Number of characters used
*.

*  Initialise
      NP = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First check for comment or blank line.
      IF( TEXT .EQ. ' ' .OR. TEXT( 1 : 1 ) .EQ. '#' .OR.
     :    TEXT( 1 : 1 ) .EQ. '!' ) THEN
         OK = .FALSE.

      ELSE
         OK = .TRUE.

*  Find the third word in the line.
         L = -1
         DO I = 1, 3
            F = L + 1
            CALL CHR_FIWS( TEXT, F, STATUS )
            L = F
            CALL CHR_FIWE( TEXT, L, STATUS )
         END DO

*  Classify the line according to the first character in the third word.
         LC = TEXT( F : F )
         CALL CHR_UCASE( LC )

         IF( LC .EQ. 'L' ) THEN
            LINE = .TRUE.
            REGION = .FALSE.
            OBJECT = 'range of lines'
            AX( 1 ) = 2
            AX( 2 ) = 2

         ELSE IF( LC .EQ. 'C' ) THEN
            LINE = .FALSE.
            REGION = .FALSE.
            OBJECT = 'range of columns'
            AX( 1 ) = 1
            AX( 2 ) = 1

         ELSE
            LINE = .FALSE.
            REGION = .TRUE.
            OBJECT = 'rectangular region'
            AX( 1 ) = 1
            AX( 2 ) = 2
         END IF

*  Unformat the first field in the text.
         F = 1
         IAX = AX( 1 )
         NC = AST_UNFORMAT( FRM, IAX, TEXT, AXVAL( 1 ), STATUS )

*  If succesful, store the index of the next character to be read.
         IF( NC .GT. 0 ) THEN
            F = F + NC

*  Unformat the second field in the text.
            IAX = AX( 2 )
            NC = AST_UNFORMAT( FRM, IAX, TEXT( F : ), AXVAL( 2 ),
     :                         STATUS )

*  If succesful, store the index of the next character to be read.
            IF( NC .GT. 0 ) THEN
               F = F + NC

*  If the text specifies a region, store the axis values obtained
*  above in the correct place, and get the remaining axis values.
               IF( REGION ) THEN
                  POS( 1, 1 ) = AXVAL( 1 )
                  POS( 1, 2 ) = AXVAL( 2 )

*  Unformat the third field in the text.
                  IAX = 1
                  NC = AST_UNFORMAT( FRM, 1, TEXT( F : ), POS( 2, 1 ),
     :                               STATUS )

*  If succesful, store the index of the next character to be read.
                  IF( NC .GT. 0 ) THEN
                     F = F + NC

*  Unformat the fourth field in the text.
                     IAX = 2
                     NC = AST_UNFORMAT( FRM, 2, TEXT( F : ),
     :                                  POS( 2, 2 ), STATUS )

                  END IF

*  If the text specifies a set of lines, store the axis values obtained
*  above in the correct place (the other axis retains its suppled
*  values).
               ELSE IF( LINE ) THEN
                  POS( 1, 2 ) = AXVAL( 1 )
                  POS( 2, 2 ) = AXVAL( 2 )

*  If the text specifies a set of columns, store the axis values
*  obtained above in the correct place (the other axis retains its
*  supplied values).
               ELSE
                  POS( 1, 1 ) = AXVAL( 1 )
                  POS( 2, 1 ) = AXVAL( 2 )
               END IF

            END IF

         END IF

*  If a field could not be read, report an warning.
         IF( NC .EQ. 0 ) THEN
            ATTR = 'Label('
            NC = 6
            CALL CHR_PUTI( IAX, ATTR, NC )
            CALL CHR_APPND( ')', ATTR, NC )
            CALL MSG_SETC( 'L', AST_GETC( FRM, ATTR( : NC ), STATUS ) )
            CALL MSG_SETI( 'I', IAX )
            CALL MSG_SETC( 'T', TEXT( F : ) )
            CALL MSG_OUT( 'KPS1_ZPDEC_MSG1', 'Failed to read a '//
     :                    'value for axis ^I (^L), from the '//
     :                    'start of the text string ''^T''.',
     :                    STATUS )
         ELSE
            NP = 2
         END IF

      END IF

      END
