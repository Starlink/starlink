      SUBROUTINE KPG1_NMCOL( NAME, R, G, B, STATUS )
*+
*  Name:
*     KPG1_NMCOL

*  Purpose:
*     Finds the RGB intensities of a named colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NMCOL( NAME, R, G, B, STATUS )

*  Description:
*     Given the name of a colour this routine searches the standard
*     colour set for it, and if it exists returns its R-G-B intensities.
*     An error is returned if the named colour is not in the colour set.
*     All comparisons are performed in uppercase with the blanks
*     removed.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the nearest colour in the named colour set to the
*        input RGB colour.  Note at least eighteen characters are
*        required to avoid truncation. This string may also be an HTML
*        code of the form "#aabbcc" (or "@aabbcc" - for use in contexts
*        where "#" is a comment character, e.g. KAPPA style files) where
*        a, b and c are hexadecimal digits, and "aa", "bb" and "cc" give
*        red, blue and green intensities normalised to a maximum of "ff"
*        (256).
*     R = REAL (Returned)
*        The red intensity of the named colour to be identified.  It is
*        in the range 0.0 to 1.0.
*     G = REAL (Returned)
*        The green intensity of the named colour to be identified.  It
*        is in the range 0.0 to 1.0.
*     B = REAL (Returned)
*        The blue intensity of the named colour to be identified.  It is
*        in the range 0.0 to 1.0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     1992 March 27 (MJC):
*        Used MIT colour set with new common-variable names.
*     19-NOV-2001 (DSB):
*        Allow specification of colour by RGB triples and HTML code.
*     25-MAY-2009 (DSB):
*        Document HTML code option.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management definitions
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! definitions

*  Global Variables:
      INCLUDE 'CTM_COM'          ! RGBs and names of colours
*        CTM_RGB( 3, CTM__NAMED ) = REAL (Read)
*           The normalised RGB intensities of the named colours.
*        CTM_NAM( CTM__NAMED ) = CHARACTER * 20 (Read)
*           The names of each of the predefined colours.

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      REAL R
      REAL G
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CTM_XCOLS         ! Initialize the CTM block data
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CNAME*( 24 )     ! Colour name in uppercase and sans
                                 ! blanks
      DOUBLE PRECISION DVAL      ! Unformatted value
      INTEGER FRM                ! AST Frame used for unformatting
      INTEGER HX( 6 )            ! Offsets of characters from "0"
      INTEGER I                  ! Loop counter
      INTEGER IAT                ! Index of next character to read
      INTEGER J                  ! Offset of character from "0"
      INTEGER LEN                ! Number of characters in the colour's
                                 ! name
      INTEGER NC                 ! Number of characters read
      INTEGER PC                 ! Percentage for grey level
      LOGICAL MATCH              ! Has a colour match been found?
      REAL GLEVEL                ! A grey level
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a version of the name in uppercase less leading blanks.
      CNAME = NAME
      CALL CHR_UCASE( CNAME )
      CALL CHR_LDBLK( CNAME )

*  Save the used length of the supplied string.
      LEN = CHR_LEN( CNAME )

*  No match found
      MATCH = .FALSE.

*  Can the supplied string be interpreted as a triple of floating point
*  values? If so, use these as the RGB values. Use the unformatting
*  facilities of the AST Frame class.
      FRM = AST_FRAME( 1, ' ', STATUS )

      NC = AST_UNFORMAT( FRM, 1, CNAME, DVAL, STATUS )
      IAT = 1 + NC
      IF ( NC .GT. 0 .AND. IAT .LE. LEN ) THEN
         R = MIN( 1.0, MAX( 0.0, REAL( DVAL ) ) )

         NC = AST_UNFORMAT( FRM, 1, CNAME( IAT: ), DVAL, STATUS )
         IAT = IAT + NC
         IF ( NC .GT. 0 .AND. IAT .LE. LEN ) THEN
            G = MIN( 1.0, MAX( 0.0, REAL( DVAL ) ) )

            NC = AST_UNFORMAT( FRM, 1, CNAME( IAT: ), DVAL, STATUS )
            IAT = IAT + NC
            IF ( NC .GT. 0 .AND. IAT .GT. LEN ) THEN
               B = MIN( 1.0, MAX( 0.0, REAL( DVAL ) ) )
               MATCH = .TRUE.
            END IF

         END IF
      END IF

*  Release AST resources.
      CALL AST_ANNUL( FRM, STATUS )

*  If no match was found, attempt to interpret the colour as an HTML
*  code of the form "#aabbcc" (or "@aabbcc") where a, b and c are
*  hexadecimal digits.
      IF ( .NOT. MATCH .AND. ( CNAME( 1 : 1 ) .EQ. '#' .OR.
     :    CNAME( 1 : 1 ) .EQ. '@' ) .AND. LEN .EQ. 7 ) THEN

         MATCH = .TRUE.
         DO I = 2, 7
            J = ICHAR( CNAME( I : I ) ) - ICHAR( '0' )
            IF ( J .GT. 9 ) J = J - 7
            IF ( J .LT. 0 .OR. J .GT. 15 ) THEN
               MATCH = .FALSE.
            ELSE
               HX( I - 1 ) = J
            END IF
         END DO

         IF ( MATCH ) THEN
            R = REAL( 16*HX( 1 ) + HX( 2 ) )/255.0
            G = REAL( 16*HX( 3 ) + HX( 4 ) )/255.0
            B = REAL( 16*HX( 5 ) + HX( 6 ) )/255.0
         END IF

      END IF

*  If no match has been found, test for a grey level.  These have
*  percentage suffices except for just Grey or Gray.
      IF ( .NOT. MATCH .AND. ( CNAME( 1:4 ) .EQ. 'GREY' .OR.
     :                        CNAME( 1:4 ) .EQ. 'GRAY' ) ) THEN
         MATCH = .TRUE.

*  Look for the grey without a percentage.
         IF ( LEN .EQ. 4 ) THEN
            R = 0.75
            G = 0.75
            B = 0.75
         ELSE

*  Find percentage suffix.
            CALL CHR_CTOI( CNAME( 5:MIN( LEN, 7 ) ), PC, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'CNAME', CNAME )
               CALL ERR_REP( 'KPG1_NMCOL_NSGREY', 'An unknown grey '/
     :                       /'level ^CNAME has been specified.',
     :                       STATUS )
            ELSE
               GLEVEL = REAL( PC ) / 100.0
               R = GLEVEL
               G = GLEVEL
               B = GLEVEL
            END IF
         END IF
      END IF

*  If still no match, look for the named colour in the colour set.
      IF ( .NOT. MATCH ) THEN

*  Loop to find the colour and hence its RGB.
*  ==========================================
         MATCH = .FALSE.
         I = 1
         DO WHILE ( .NOT. MATCH .AND. I .LE. CTM__NAMED )

*  Look for the match. The colour set names are stored in uppercase
*  with no blanks.  Read the corresponding RGB values, otherwise just
*  increment the colour counter.
            MATCH = CNAME( :LEN ) .EQ. CTM_NAM( I )( :LEN )
            IF ( MATCH ) THEN
               R = CTM_RGB( 1, I )
               G = CTM_RGB( 2, I )
               B = CTM_RGB( 3, I )
            ELSE
               I = I + 1
            END IF
         END DO

*  Report an error if there was no match.
         IF ( .NOT. MATCH .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CNAME', CNAME )
            CALL ERR_REP( 'KPG1_NMCOL_UNKNOWN', 'An unknown colour '/
     :                    /'^CNAME has been specified.', STATUS )
         END IF

      END IF

      END
