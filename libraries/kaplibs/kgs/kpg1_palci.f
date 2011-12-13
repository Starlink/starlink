      SUBROUTINE KPG1_PALCI( COLOUR, COLIND, STATUS )
*+
*  Name:
*     KPG1_PALCI

*  Purpose:
*     Finds the nearest colour in the palette to a named colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PALCI( COLOUR, COLIND, STATUS )

*  Description:
*     This routine finds the colour index within the palette of a
*     named colour.  The required colour must be in the standard colour
*     set, and SAI__ERROR status is returned if it is not.  If the
*     named colour is not present the index of the colour nearest to
*     the requested colour is returned.  A city-block metric is used.

*  Arguments:
*     COLOUR = CHARACTER * ( * ) (Given)
*        The name of the colour whose colour index is required.
*        Note at least eighteen characters are required to avoid
*        truncation.  The name may be abbreviated.  If there is any
*        ambiguity, the first match (in alphabetical order) is selected.
*     COLIND = INTEGER (Returned)
*        The colour index within the palette of the colour or its
*        nearest equivalent.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  A GKS image-display workstation must be open and active.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management definitions

*  Arguments Given:
      CHARACTER * ( * ) COLOUR

*  Arguments Returned:
      INTEGER COLIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      REAL COPREC                ! Colour precision (for colours to be
                                 ! identical)
      PARAMETER ( COPREC = 0.001 )

*  Local Variables:
      INTEGER
     :  GSTAT,                   ! Graphics status
     :  I,                       ! Loop counter
     :  WKID                     ! Work station identification

      REAL
     :  B,                       ! Blue intensity
     :  CLOMET,                  ! Closest metric
     :  G,                       ! Green intensity
     :  METRIC,                  ! Metric for a colour in the colour set
     :  PALETT( NPRICL, 0:CTM__RSVPN - 1 ),
                                 ! Palette colours
     :  R                        ! Red intensity

      LOGICAL                    ! True if:
     :  MATCH                    ! A colour match is found.

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the RGB intensities of the named colour.

      CALL KPG1_NMCOL( COLOUR, R, G, B, STATUS )

*    Find the workstation identifier.

      CALL SGS_ICURW( WKID )

*    Inquire the palette colour indices.

      DO  I = 0, CTM__RSVPN - 1
         CALL GQCR( WKID, I, 1, GSTAT, PALETT( 1, I ), PALETT( 2, I ),
     :              PALETT( 3, I ) )
      END DO

*    See if within GKS an error has occurred.

      CALL GKS_GSTAT( STATUS )

*    Loop to find a colour corresponding to the input colour's RGB.
*    ==============================================================

*    If a match is found within the colour precision then the loop can
*    be exited.  For efficiency use a city-block metric to find the
*    closest colour.  The closest match must therefore be less than
*    three.

      MATCH = .FALSE.
      I = 0
      CLOMET = 3.0
      DO WHILE ( .NOT. MATCH .AND. I .LE. CTM__RSVPN - 1 )
         METRIC = ABS( R - PALETT( 1, I ) ) +
     :            ABS( G - PALETT( 2, I ) ) +
     :            ABS( B - PALETT( 3, I ) )

         IF ( METRIC .LT. 3 * COPREC ) THEN

*          A match is found so exit and record the colour index.

            MATCH = .TRUE.
            COLIND = I

         ELSE

*          Look to see whether or not the latest colour is nearer
*          than the previous nearest.

            IF ( METRIC .LT. CLOMET ) THEN
               CLOMET = METRIC
               COLIND = I
            END IF

            I = I + 1
         END IF
      END DO

      END
