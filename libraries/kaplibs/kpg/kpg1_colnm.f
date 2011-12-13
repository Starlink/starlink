      SUBROUTINE KPG1_COLNM( R, G, B, NAME, STATUS )
*+
*  Name:
*     KPG1_COLNM

*  Purpose:
*     Finds the named colour nearest to an RGB triple.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_COLNM( R, G, B, NAME, STATUS )

*  Description:
*     This returns the name of a colour in the standard colour set that
*     matches an input R-G-B colour, or failing that, that is nearest
*     to the input colour.  A city-block metric is used.

*  Arguments:
*     R = REAL (Given)
*        The red intensity of the colour to be identified.  It should
*        be in the range 0.0 to 1.0.
*     G = REAL (Given)
*        The green intensity of the colour to be identified.  It should
*        be in the range 0.0 to 1.0.
*     B = REAL (Given)
*        The blue intensity of the colour to be identified.  It should
*        be in the range 0.0 to 1.0.
*     NAME = CHARACTER * ( * ) (Returned)
*        The name of the nearest colour in the named colour set to the
*        input RGB colour.  Note at least eighteen characters are
*        required to avoid truncation.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Notes:
*     -  The actual R-G-B colours used will be constrained to lie in the
*     range 0.0--1.0.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     1991 July 19 (MJC):
*        Original version.
*     1992 March 27 (MJC):
*        Used MIT colour set with new common-variable names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management definitions

*  Global Variables:
      INCLUDE 'CTM_COM'          ! RGBs and names of colours
*        CTM_RGB( 3, CTM__NAMED ) = REAL (Read)
*           The normalised RGB intensities of the named colours.
*        CTM_NAM( CTM__NAMED ) = CHARACTER * 20 (Read)
*           The names of each of the predefined colours.

*  Arguments Given:
      REAL R
      REAL G
      REAL B

*  Arguments Returned:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CTM_XCOLS         ! Needed on some linkers to acces the
                                 ! CTM block data

*  Local Constants:
      REAL COPREC                ! Colour precision (for colours to be
                                 ! identical)
      PARAMETER ( COPREC = 0.001 )

*  Local Variables:
      INTEGER
     :  I,                       ! Loop counter
     :  NC,                      ! Number of characters
     :  PC                       ! Percentage for grey level

      REAL
     :  BB,                      ! Bounded blue intensity
     :  CLOMET,                  ! Closest metric
     :  GB,                      ! Bounded green intensity
     :  METRIC,                  ! Metric for a colour in the colour set
     :  RB                       ! Bounded red intensity

      LOGICAL                    ! True if:
     :  MATCH                    ! A colour match is found.

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make sure the RGBs are within bounds.

      RB = MIN( 1.0, MAX( 0.0, R ) )
      GB = MIN( 1.0, MAX( 0.0, G ) )
      BB = MIN( 1.0, MAX( 0.0, B ) )

*    First test for a grey level.

      IF ( ABS( RB - GB ) .LT. COPREC .AND.
     :     ABS( GB - BB ) .LT. COPREC ) THEN

*       It is grey.  The greys have a percentage unless one of a few
*       nominated intensities including black or white.

         IF ( ABS( RB - 1.0 ) .LT. COPREC ) THEN
            NAME = 'White'

         ELSE IF ( ABS( RB - 0.0 ) .LT. COPREC ) THEN
            NAME = 'Black'

         ELSE IF ( ABS( RB - 0.667 ) .LT. COPREC ) THEN
            NAME = 'Light Grey'

         ELSE IF ( ABS( RB - 0.333 ) .LT. COPREC ) THEN
            NAME = 'Dim Grey'

         ELSE IF ( ABS( RB - 0.75 ) .LT. COPREC ) THEN
            NAME = 'Grey'

*       Use percentage suffix.

         ELSE
            PC = MIN( 99, MAX( 1, NINT( 100 * RB ) ) )
            NC = 0
            NAME = ' '
            CALL CHR_PUTC( 'Grey', NAME, NC )
            CALL CHR_PUTI( PC, NAME, NC )
         END IF

*    It is a colour that is to be identified.
      ELSE

*       Loop to find a colour corresponding to the input RGB.
*       =====================================================

*       If a match is found within the colour precision then the loop
*       can be exited.  For efficiency use a city-block metric to find
*       the closest colour.  The closest match must therefore be less
*       than three.

         MATCH = .FALSE.
         I = 1
         CLOMET = 3.0
         DO WHILE ( .NOT. MATCH .AND. I .LE. CTM__NAMED )
            METRIC = ABS( RB - CTM_RGB( 1, I ) ) +
     :               ABS( GB - CTM_RGB( 2, I ) ) +
     :               ABS( BB - CTM_RGB( 3, I ) )

            IF ( METRIC .LT. 3 * COPREC ) THEN

*             A match is found.

               MATCH = .TRUE.
               NAME = CTM_NAM( I )

            ELSE

*             Look to see whether or not the latest colour is nearer
*             than the previous nearest.

               IF ( METRIC .LT. CLOMET ) THEN
                  CLOMET = METRIC
                  NAME = CTM_NAM( I )
               END IF

               I = I + 1
            END IF
         END DO

*       Convert to lowercase with an uppercase first character.

         CALL CHR_LCASE( NAME( 2: ) )
      END IF

      END
