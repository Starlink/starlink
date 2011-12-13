      SUBROUTINE KPS1_MLCOF( OFMTHD, NDISP, YLMT, AVERAG, PARAM,
     :                       OFFSET, STATUS )
*+
*  Name:
*     KPS1_MLCOF

*  Purpose:
*     Calculate an offset for each line in a multi-line plot to
*     separate them vertically.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLCOF( OFMTHD, NDISP, YLMT, AVERAG, OFFSET, PARAM,
*                      STATUS )

*  Description:
*     This routine uses one of the three possible methods to calculate
*     offsets required to vertically separate the displayed lines in
*     a multi-line plot.
*
*     The three methods are:
*       'FREE'     -- In this method nothing need be calculated,
*                     get the offset value from the user.
*       'CONSTANT' -- In this method the lines are evenly spaced
*                     between upper and lower vertical limits.
*       'AVERAGE'  -- In this method the 'averages' of the lines
*                     are equally spaced between upper and lower
*                     vertical limits.

*  Arguments:
*     OFMTHD = INTEGER (Given)
*        Defines the method to offset the traces. It can take one of
*        the following values:
*
*         0 :'FREE' offset
*
*         1 :'CONSTANT' offset
*
*         2 :'AVERAGE' offset
*     NDISP = INTEGER (Given)
*        The number of lines to be displayed.
*     YLMT( 2 ) = REAL (Given)
*        The lower and upper limits of the vertical axis.
*     AVERAG( NDISP ) = REAL (Given)
*        The average value of each line.
*     PARAM = CHARACTER*( * )
*        The name of the ADAM parameter used to get the offset values
*        for each line from the user when the method is 'FREE'.
*     OFFSET( NDISP ) = REAL (Returned)
*        The offset for each scaled data trace.
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
*     26-FEB-1991 (WG):
*        Original version (Based on INTERIM version CALOFF by DSB)
*     1991 June 18 (MJC):
*        Renamed from CALOFF.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic value Definition

*  Arguments Given:
      INTEGER OFMTHD
      INTEGER NDISP
      REAL YLMT( 2 )
      REAL AVERAG( NDISP )
      CHARACTER*( * ) PARAM

*  Arguments Returned:
      REAL OFFSET( NDISP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do-loop index
      LOGICAL GOT                ! Flag showing bottom average got
      REAL AVEBTM                ! Average value of bottom trace

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If method is 'FREE', get the offsets from the user.
      IF ( OFMTHD .EQ. 0 ) THEN
         CALL PAR_EXACR( PARAM, NDISP, OFFSET, STATUS )

*  If method is 'CONSTANT', calculate evenly spaced offsets for each
*  line.
      ELSE IF ( OFMTHD .EQ. 1 ) THEN
         DO I = 1, NDISP
            OFFSET( I ) = REAL( I - 1 ) * ( YLMT( 2 ) - YLMT( 1 ) )
     :                                     / REAL( NDISP )
         END DO

*  If method is 'AVERAGE', calculate offsets which cause the averages
*  of the lines to be evenly spaced.
      ELSE IF ( OFMTHD .EQ. 2 ) THEN

*  Get the average value of the bottom trace.
         I = 1
         GOT = .FALSE.
         DO WHILE ( .NOT. GOT .AND. I .LE. NDISP )
            IF ( AVERAG( I ) .NE. VAL__BADR ) THEN
               AVEBTM = AVERAG( I )
               GOT = .TRUE.

*  If the average is invalid, this line contains no valid samples and so
*  will not appear in the plot, consider the next line.
            ELSE
               I = I + 1
            END IF
         END DO

*  Enter a do loop to calculate the offset for each trace.
         DO I = 1, NDISP

*  If the trace is valid, calculate its offset.
            IF ( AVERAG( I ) .NE. VAL__BADR ) THEN
               OFFSET( I ) = AVEBTM - AVERAG( I )
     :                       + REAL( I - 1 ) * ( YLMT( 2 ) - AVEBTM )
     :                                        / REAL( NDISP )

*  Otherwise set its offset as bad.
            ELSE
               OFFSET( I ) = VAL__BADR
            END IF
         END DO
      END IF

 999  CONTINUE

      END
