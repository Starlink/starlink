      SUBROUTINE KPS1_MLPUT( DIM1, DIM2, DATIN, NSMP, NLINE, LININD,
     :                       ABSAXS, YLOG, DATOUT, STATUS )
*+
*  Name:
*     KPS1_MLPUT

*  Purpose:
*     Puts the data to draw in a multi-line plot in the output array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPUT( DIM1, DIM2, DATIN, NSMP, NLINE, LININD, ABSAXS,
*                      YLOG, DATOUT, STATUS )

*  Description:
*     This routine put the specified lines from the input data array
*     to the output data array for plotting. The argument ABSAXS
*     specifies which dimension of the input array will be taken as
*     abscissa of the display, while the first dimension of the
*     output data array will always be taken as abscissa of the display.
*     When the vertical axis of the display is logarithmic, the negative
*     and zero values will be set to the bad value when put into the
*     output array.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The size of the first dimension of the input array.
*     DIM2 = INTEGER (Given)
*        The size of the second dimension of the input array.
*     DATIN( DIM1, DIM2 ) = REAL (Given)
*        The input data array.
*     NSMP = INTEGER (Given)
*        The number of samples on abscissa.
*     NLINE = INTEGER (Given)
*        The number of lines to plot.
*     LININD( NLINE ) = INTEGER (Given)
*        The pixel indices of the lines to plot.
*     ABSAXS = INTEGER (Given)
*        If it is 1, the first dimension of the input array will be
*        taken as abscissa of the plot.  Otherwise the second dimension
*        becomes the abscissa.
*     YLOG = LOGICAL (Given)
*        If it is true, the vertical axis of the display is logarithmic;
*        otherwise, it is linear.
*     DATOUT( NSMP, NLINE ) = REAL (Returned)
*        The output array containing the extracted data from the input
*        array.
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
*     9-MAY-1991 (WG):
*        Original version.
*     1991 June 18 (MJC):
*        Renamed from PUTDAT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT definitions

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      REAL DATIN( DIM1, DIM2 )
      INTEGER NSMP
      INTEGER NLINE
      INTEGER LININD( NLINE )
      INTEGER ABSAXS
      LOGICAL YLOG

*  Arguments Returned:
      REAL DATOUT( NSMP, NLINE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! The do-loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the first dimension of the input array is taken as abscissa,...
      IF ( ABSAXS .EQ. 1 ) THEN
         DO J = 1, NLINE
            DO I = 1, NSMP

*  If the vertical axis is logarithmic, and the value is zero or
*  negative, set it as bad value.
               IF ( YLOG .AND.
     :              DATIN( I, LININD( J ) ) .LT. VAL__SMLR ) THEN
                  DATOUT( I, J ) = VAL__BADR

*  Otherwise, pass the data as it is.
               ELSE
                  DATOUT( I, J ) = DATIN( I, LININD( J ) )
               END IF
            END DO
         END DO

*  If the second dimension ot the input array is taken as abscissa, ...
      ELSE
         DO J = 1, NLINE
            DO I = 1, NSMP

*  If the vertical axis is logarithmic, and the value is zero or
*  negative, set it as bad value.
               IF ( YLOG .AND.
     :              DATIN( LININD( J ), I ) .LT. VAL__SMLR ) THEN
                  DATOUT( I, J ) = VAL__BADR

*  Otherwise, pass the data as it is.
               ELSE
                  DATOUT( I, J ) = DATIN( LININD( J ), I )
               END IF
            END DO
         END DO
      END IF

      END
