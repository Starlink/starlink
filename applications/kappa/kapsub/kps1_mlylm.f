      SUBROUTINE KPS1_MLYLM( NLINE, YMX, YMN, YLOG, PYLMT, YLMT,
     :                       STATUS )
*+
*  Name:
*     KPS1_MLYLM

*  Purpose:
*     Get vertical limits of a multi-line display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLYLM( NLINE, YMX, YMN, YLOG, PYLMT, YLMT, STATUS )

*  Description:
*     This routine gets the upper and lower vertical limits of a
*     multi-line display. The suggested default lower limit is the
*     smallest value of the bottom line, and the upper limit is such
*     that no lines ever overlap after application of the offsets.
*     When the vertical axis is logarithmic, the lines will not be
*     offset, so the default upper limit is the max. value of the
*     lines, and the default lower limit is the smallest value of the
*     lines.

*  Arguments:
*     NLINE = INTEGER (Given)
*        Number of the lines to be plotted.
*     DATA( NSMP, NLINE ) = REAL (Given)
*        The data array to be plotted.
*     YMX( NLINE ) = REAL (Given)
*        The maximum data value of each line.
*     YMN( NLINE ) = REAL (Given)
*        The minimum data value of each line.
*     YLOG = LOGICAL (Given)
*        If true the vertical axis will be logarithmic, otherwise
*        linear.
*     PYLMT = CHARACTER*(*) (Given)
*        The name of the parameter used to get the values for lower and
*        upper vertical limits.
*     YLMT( 2 ) = REAL (Returned)
*        The lower and upper vertical limits.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-APR-1991 (WG):
*        Original version.
*     1991 June 18 (MJC):
*        Renamed from GTYMLT, and revised the error reporting.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic value definition

*  Arguments Given:
      INTEGER NLINE
      REAL YMX( NLINE )
      REAL YMN( NLINE )
      LOGICAL YLOG
      CHARACTER*( * ) PYLMT

*  Arguments Returned:
      REAL YLMT( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DFYLMT( 2 )           ! Default lower and upper y limits
      LOGICAL GOT                ! Flag for finding min at bottom trace
      INTEGER I                  ! Do-loop index
      REAL SPAN                  ! Maximum trace span in the display
      REAL YMIN                  ! Minimum value in the default display
      REAL YMAX                  ! Maximum value in the default display
      REAL YTEMP                 ! A temporary buffer.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the vertical axis is linear, get the minimum value of the bottom
*  line.
      IF ( .NOT. YLOG ) THEN
         I = 1
         GOT = .FALSE.
         DO WHILE ( .NOT. GOT .AND. I .LE. NLINE )

*  If ith line has a valid minimum, the minimum value of the bottom is
*  obtained.
            IF ( YMN( I ) .NE. VAL__BADR ) THEN
               DFYLMT( 1 ) = YMN( I )
               GOT = .TRUE.

*  Otherwise, the ith line contains no valid samples, consider next
*  line.
            ELSE
               I = I + 1
            END IF
         END DO

*  If no minimum value obtained from all lines, set status, report
*  an error and exit.
         IF ( .NOT. GOT ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_MLYLM_NOVALSMP',
     :        'No valid data to display', STATUS )
            GOTO 999
         END IF

*  Find the line which has the maximum span, and note down the span.
         SPAN = 0.0
         DO I = 1, NLINE
            IF ( YMX( I ) .NE. VAL__BADR .AND.
     :           YMN( I ) .NE. VAL__BADR ) THEN
               IF ( YMX( I ) - YMN( I ) .GT. SPAN )
     :           SPAN = YMX( I ) - YMN( I )
            END IF
         END DO

*  Set maximum y display such that no trace ever overlaps.
         DFYLMT( 2 ) = NLINE * SPAN + DFYLMT( 1 )

*  If the vertical axis is logarithmic, find the largest and smallest
*  values of the lines.
      ELSE
         DFYLMT( 1 ) = VAL__MAXR
         DFYLMT( 2 ) = VAL__MINR
         DO I = 1, NLINE
            IF ( YMN( I ) .NE. VAL__BADR .AND.
     :           DFYLMT( 1 ) .GT. YMN( I ) ) DFYLMT( 1 ) = YMN( I )
            IF ( YMX( I ) .NE. VAL__BADR .AND.
     :           DFYLMT( 2 ) .LT. YMX( I ) ) DFYLMT( 2 ) = YMX( I )
         END DO
      END IF

*  Set the selectable range for the lower and upper limits.
      IF ( YLOG ) THEN
         YMIN = VAL__SMLR
         YMAX = VAL__MAXR
      ELSE
         YMIN = VAL__MINR
         YMAX = VAL__MAXR
      END IF

*  Start a new error context.
      CALL ERR_MARK

*  Enter a do loop to get the vertical limits from the environment.
      GOT = .FALSE.
      DO WHILE ( .NOT. GOT )
         CALL PAR_GDR1R( PYLMT, 2, DFYLMT, YMIN, YMAX, .FALSE.,
     :                   YLMT, STATUS )

*  Check status, and exit if there was an error.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_RLSE
            GOTO 999
         END IF

*  If the given limits are in the wrong order, swap them.
         IF ( YLMT( 2 ) .LT. YLMT( 1 ) ) THEN
            YTEMP = YLMT( 2 )
            YLMT( 2 ) = YLMT( 1 )
            YLMT( 1 ) = YTEMP
         END IF

*  If Y axis does not have sufficient extent to display, report and
*  cancel the parameter to re-gain the limits.
         IF ( YLMT( 2 ) - YLMT( 1 ) .LE. VAL__SMLR ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_MLYLM_INSUFY',
     :        'The extent of Y axis is too small.', STATUS )
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( PYLMT, STATUS )
         ELSE
            GOT = .TRUE.
         END IF
      END DO

*  Release the new error context.
      CALL ERR_RLSE

 999  CONTINUE

      END
