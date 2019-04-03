      SUBROUTINE NDF1_P2A( N, IPIX, LBND, UBND, HAVCEN, HAVWID, HAVVAR,
     :                     CENTRE, WIDTH, VARIAN, CEN, WID, VAR,
     :                     STATUS )
*+
*  Name:
*     NDF1_P2A

*  Purpose:
*     Convert pixel indices into axis centre, width and variance
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_P2A( N, IPIX, LBND, UBND, HAVCEN, HAVWID, HAVVAR,
*                    CENTRE, WIDTH, VARIAN, CEN, WID, VAR, STATUS )

*  Description:
*     This routine converts a series of pixel indices for a single NDF
*     dimension into the corresponding axis coordinate system values,
*     namely: the pixel centre, width and variance. The routine will
*     provide the correct default values if the appropriate axis arrays
*     are not available, and will also extrapolate the axis coordinate
*     system in either direction if necessary.

*  Arguments:
*     N = INTEGER (Given)
*        Number of pixel index values to be converted.
*     IPIX( N ) = INTEGER (Given)
*        Array of pixel indices to be converted.
*     LBND = INTEGER (Given)
*        Lower pixel index bound for the NDF dimension.
*     UBND = INTEGER (Given)
*        Upper pixel index bound for the NDF dimension.
*     HAVCEN = LOGICAL (Given)
*        Whether an array of pixel centre positions is available.
*     HAVWID = LOGICAL (Given)
*        Whether an array of pixel width values is available.
*     HAVVAR = LOGICAL (Given)
*        Whether an array of pixel variance values is available.
*     CENTRE( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel centre positions. This is only used if HAVCEN is
*        .TRUE..
*     WIDTH( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel width values. This is only used if both HAVCEN
*        and HAVWID are .TRUE..
*     VARIAN( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of pixel variance values. This is only used if both
*        HAVCEN and HAVVAR are .TRUE..
*     CEN( N ) = DOUBLE PRECISION (Returned)
*        Array of centre positions for the selected pixels.
*     WID( N ) = DOUBLE PRECISION (Returned)
*        Array of width values for the selected pixels.
*     VAR( N ) = DOUBLE PRECISION (Returned)
*        Array of variance values for the selected pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine performs checks on the validity of the centre,
*     width and variance values it returns and will report an error if
*     they are invalid.

*  Algorithm:
*     -  If no pixel centre coordinates are provided, then loop to
*     calculate all the centre positions, widths and variances
*     directly.
*     -  Otherwise loop to calculate these values from the array(s)
*     provided.
*     -  First test to see if the pixel index lies below the lower
*     bound of the pixel centre array.
*     -  If so, then obtain the pixel spacing to use for extrapolation
*     from the spacing of the two nearest pixel centres, if available.
*     -  Check that the spacing is not zero; this indicates invalid
*     centre values.
*     -  Use a spacing of unity if only one centre value is available.
*     -  Extrapolate to obtain the pixel centre position.
*     -  If pixel width values have been provided, then use the width
*     of the nearest pixel, checking it for validity.
*     -  Otherwise, use the pixel spacing to generate a width value.
*     -  The extrapolated variance value is zero.
*     -  If the pixel index lies above the upper bound of the pixel
*     centre array, then obtain the pixel spacing to use for
*     extrapolation from the spacing of the two nearest pixel centres.
*     -  Check that the spacing is not zero; this indicates invalid
*     centre values.
*     -  Use a spacing of unity if only one centre value is available.
*     -  Extrapolate to obtain the pixel centre position.
*     -  If pixel width values have been provided, then use the width
*     of the nearest pixel, checking it for validity.
*     -  Otherwise, use the pixel spacing to generate a width value.
*     -  The extrapolated variance value is zero.
*     -  If the pixel index lies within the bounds of the pixel centre
*     array, then extract the appropriate centre position.
*     -  If pixel width values have been provided, then extract the
*     matching pixel width, checking it for validity.
*     -  Otherwise, calculate the width from the centre spacing of
*     neighbouring pixels. Use both neighbours if available.
*     -  Use only one neighbour if the other lies outside the bounds of
*     the pixel centre array.
*     -  Use a width value of unity if only one centre position has
*     been supplied.
*     -  If an array of variance values has been provided, then extract
*     the appropriate value, checking it for validity.
*     -  Otherwise return a variance value of zero.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAR-1991 (RFWS):
*        Original version.
*     12-MAR-1991 (RFWS):
*        Added checks for invalid returned values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER N
      INTEGER IPIX( N )
      INTEGER LBND
      INTEGER UBND
      LOGICAL HAVCEN
      LOGICAL HAVWID
      LOGICAL HAVVAR
      DOUBLE PRECISION CENTRE( LBND : UBND )
      DOUBLE PRECISION WIDTH( LBND : UBND )
      DOUBLE PRECISION VARIAN( LBND : UBND )

*  Arguments Returned:
      DOUBLE PRECISION CEN( N )
      DOUBLE PRECISION WID( N )
      DOUBLE PRECISION VAR( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION SPACE     ! Pixel spacing for extrapolation
      INTEGER I                  ! Loop counter for pixel indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If no pixel centre coordinates are provided, then loop to calculate
*  all the centre positions, widths and variances directly.
      IF ( .NOT. HAVCEN ) THEN
         DO 1 I = 1, N
            CEN( I ) = DBLE( IPIX( I ) ) - 0.5D0
            WID( I ) = 1.0D0
            VAR( I ) = 0.0D0
 1       CONTINUE

*  Otherwise loop to calculate these values from the array(s) provided.
      ELSE
         DO 2 I = 1, N

*  First test to see if the pixel index lies below the lower bound of
*  the pixel centre array.
            IF ( IPIX( I ) .LT. LBND ) THEN

*  If so, then obtain the pixel spacing to use for extrapolation from
*  the spacing of the two nearest pixel centres, if available.
               IF ( UBND .GT. LBND ) THEN
                  SPACE = CENTRE( LBND + 1 ) - CENTRE( LBND )

*  Check that the spacing is not zero; this indicates invalid centre
*  values.
                  IF ( SPACE .EQ. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR1',
     :                             'Axis CENTRE values do not ' //
     :                             'increase or decrease ' //
     :                             'monotonically.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Use a spacing of unity if only one centre value is available.
               ELSE
                  SPACE = 1.0D0
               END IF

*  Extrapolate to obtain the pixel centre position.
               CEN( I ) =
     :            CENTRE( LBND ) - SPACE * DBLE( LBND - IPIX( I ) )

*  If pixel width values have been provided, then use the width of the
*  nearest pixel, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( LBND )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR2',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, use the pixel spacing to generate a width value.
               ELSE
                  WID( I ) = ABS( SPACE )
               END IF

*  The extrapolated variance value is zero.
               VAR( I ) = 0.0D0

*  If the pixel index lies above the upper bound of the pixel centre
*  array, then obtain the pixel spacing to use for extrapolation from
*  the spacing of the two nearest pixel centres.
            ELSE IF ( IPIX( I ) .GT. UBND ) THEN
               IF ( UBND .GT. LBND ) THEN
                  SPACE = CENTRE( UBND ) - CENTRE( UBND - 1 )

*  Check that the spacing is not zero; this indicates invalid centre
*  values.
                  IF ( SPACE .EQ. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR3',
     :                             'Axis CENTRE values do not ' //
     :                             'increase or decrease ' //
     :                             'monotonically.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Use a spacing of unity if only one centre value is available.
               ELSE
                  SPACE = 1.0D0
               END IF

*  Extrapolate to obtain the pixel centre position.
               CEN( I ) =
     :            CENTRE( UBND ) + SPACE * DBLE( IPIX( I ) - UBND )

*  If pixel width values have been provided, then use the width of the
*  nearest pixel, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( UBND )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR4',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, use the pixel spacing to generate a width value.
               ELSE
                  WID( I ) = ABS( SPACE )
               END IF

*  The extrapolated variance value is zero.
               VAR( I ) = 0.0D0

*  If the pixel index lies within the bounds of the pixel centre array,
*  then extract the appropriate centre position.
            ELSE
               CEN( I ) = CENTRE( IPIX( I ) )

*  If pixel width values have been provided, then extract the matching
*  pixel width, checking it for validity.
               IF ( HAVWID ) THEN
                  WID( I ) = WIDTH( IPIX( I ) )
                  IF ( WID( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR5',
     :                             'Invalid negative axis WIDTH ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise, calculate the width from the centre spacing of
*  neighbouring pixels. Use both neighbours if available.
               ELSE
                  IF ( ( IPIX( I ) .GT. LBND ) .AND.
     :                 ( IPIX( I ) .LT. UBND ) ) THEN
                     WID( I ) = 0.5D0 *
     :                  ABS ( CENTRE( IPIX( I ) + 1 ) -
     :                        CENTRE( IPIX( I ) - 1 ) )

*  Use only one neighbour if the other lies outside the bounds of the
*  pixel centre array.
                  ELSE IF ( IPIX( I ) .GT. LBND ) THEN
                     WID( I ) = ABS( CENTRE( IPIX( I ) ) -
     :                               CENTRE( IPIX( I ) - 1 ) )
                  ELSE IF ( IPIX( I ) .LT. UBND ) THEN
                     WID( I ) = ABS( CENTRE( IPIX( I ) + 1 ) -
     :                               CENTRE( IPIX( I ) ) )

*  Use a width value of unity if only one centre position has been
*  supplied.
                  ELSE
                     WID( I ) = 1.0D0
                  END IF
               END IF

*  If an array of variance values has been provided, then extract the
*  appropriate value, checking it for validity.
               IF ( HAVVAR ) THEN
                  VAR( I ) = VARIAN( IPIX( I ) )
                  IF ( VAR( I ) .LT. 0.0D0 ) THEN
                     STATUS = NDF__AXVIN
                     CALL ERR_REP( 'NDF1_P2A_ERR6',
     :                             'Invalid negative axis VARIANCE ' //
     :                             'value encountered.',
     :                             STATUS )
                     GO TO 99
                  END IF

*  Otherwise return a variance value of zero.
               ELSE
                  VAR( I ) = 0.0D0
               END IF
            END IF
 2       CONTINUE
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_P2A', STATUS )

      END
