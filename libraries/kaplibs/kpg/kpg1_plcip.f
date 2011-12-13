      SUBROUTINE KPG1_PLCIP( COLOUR, COLIND, STATUS )
*+
*  Name:
*     KPG1_PLCIP

*  Purpose:
*     Finds the nearest colour in the palette to a named colour
*     (PGPLOT version of KPG1_PALCI).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLCIP( COLOUR, COLIND, STATUS )

*  Description:
*     This routine finds the PGPLOT colour index within the palette of a
*     named colour.  The required colour must be in the standard colour
*     set, and SAI__ERROR status is returned if it is not.  If the
*     named colour is not present the index of the colour nearest to
*     the requested colour is returned.  A city-block metric is used.
*
*     A close colour is only accepted if the close colour is not equal
*     to the background colour.  This avoids pens becoming invisible.

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
*     -  A PGPLOT image-display workstation must be open and active.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1998, 1999 Central Laboratory of the Research
*                   Councils.
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
*     27-FEB-1998 (DSB):
*        Changed for use with PGPLOT instead of GKS.  Re-formatted to
*        modern style.
*     9-JUN-1999 (DSB):
*        Avoid accepting close colours which are equal to the background
*        colour, and thus invisible.
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
      CHARACTER COLOUR*(*)

*  Arguments Returned:
      INTEGER COLIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      REAL COPREC                ! Colour precision for colours to be
      PARAMETER ( COPREC = 0.001 ) ! identical

*  Local Variables:
      INTEGER CI1                ! Lowest available colour index
      INTEGER CI2                ! Highest available colour index
      INTEGER HI                 ! Highest palette colour index
      INTEGER I                  ! Loop counter
      LOGICAL BACKG              ! Is this the background colour?
      LOGICAL MATCH              ! A colour match found?
      REAL B                     ! Blue intensity
      REAL B0                    ! Blue intensity of background
      REAL CLOMET                ! Closest metric
      REAL G                     ! Green intensity
      REAL G0                    ! Green intensity of background
      REAL METRIC                ! Metric for a colour in the colour set
      REAL PALETT( NPRICL, 0:CTM__RSVPN - 1 ) ! Palette colours
      REAL R                     ! Red intensity
      REAL R0                    ! Red intensity of background
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the RGB intensities of the named colour.
      CALL KPG1_NMCOL( COLOUR, R, G, B, STATUS )

*  Find the range of colour indices available on the cureent graphics
*  device.
      CALL PGQCOL( CI1, CI2 )

*  For Xoverlay devices, the above call returns CI1=0 implying that
*  the background can be written to.  This is technically correct
*  because writing to the background causes any foreground graphics to
*  be "rubbed out".  However, the pen representation for the background
*  gives rgb=0,0,0 (i.e. it thinks the background is a black pen).  This
*  is not correct, therefore do not allow pen 0 to be used if CI1 is
*  zero and CI2 = 1.
      IF( CI1 .EQ. 0 .AND. CI2 .EQ. 1 ) THEN
         COLIND = 1

*  Otherwise...
      ELSE

*  Note the highest colour index in the palete.
         HI = MIN( CI2, CTM__RSVPN - 1 )

*  Inquire the palette colour indices.
         DO  I = 0, HI
            CALL PGQCR( I, PALETT( 1, I ), PALETT( 2, I ),
     :                  PALETT( 3, I ) )
         END DO

*  Note the background colour.
         R0 = PALETT( 1, 0 )
         G0 = PALETT( 2, 0 )
         B0 = PALETT( 3, 0 )

*  Loop to find a colour corresponding to the input colour's RGB.
*  ==============================================================

*  If a match is found within the colour precision then the loop can
*  be exited.  For efficiency use a city-block metric to find the
*  closest colour.  The closest match must therefore be less than
*  three.
         MATCH = .FALSE.
         I = 0
         CLOMET = 3.0
         COLIND = HI

         DO WHILE ( .NOT. MATCH .AND. I .LE. HI )
            METRIC = ABS( R - PALETT( 1, I ) ) +
     :               ABS( G - PALETT( 2, I ) ) +
     :               ABS( B - PALETT( 3, I ) )

*  Set a flag if this colour is close to the background colour, and thus
*  would be difficult to see if used.
            BACKG = ( ABS( R0 - PALETT( 1, I ) ) +
     :                ABS( G0 - PALETT( 2, I ) ) +
     :                ABS( B0 - PALETT( 3, I ) ) .LE. 0.5 )

*  Accept this colour if it matches the requested colour exactly, or if
*  it is close to the requested colour and is not close to the
*  background colour.
            IF ( METRIC .EQ. 0 .OR.
     :           METRIC .LT. 3 * COPREC .AND. .NOT. BACKG ) THEN

*  A match is found so exit and record the colour index.
               MATCH = .TRUE.
               COLIND = I

            ELSE

*  Look to see whether or not the latest colour is nearer than the
*  previous nearest.
               IF ( METRIC .LT. CLOMET .AND. .NOT. BACKG ) THEN
                  CLOMET = METRIC
                  COLIND = I
               END IF

               I = I + 1
            END IF
         END DO
      END IF

      END
