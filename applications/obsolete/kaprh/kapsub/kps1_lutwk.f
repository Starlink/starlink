      SUBROUTINE KPS1_LUTWK( NINTS, INLUT, ORIGIN, SQUSTR, WHITE,
     :                       OUTLUT, STATUS )
*+
*  Name:
*     KPS1_LUTWK

*  Purpose:
*     Routine to adjust image-display lookup table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUTWK( NINTS, INLUT, ORIGIN, SQUSTR, WHITE, OUTLUT,
*                      STATUS )

*  Description:
*     This routine adjusts the image-display lookup table.  The origin
*     may be displaced and/or the lookup table may be stretched or
*     squashed.  If the squash/stretch factor is negative the lookup
*     table will be flipped as well.  When the output lookup table is
*     squashed the remaining colour indices are filled with either
*     white or the first and last colours of the input lookup table,
*     the sense depending on the sign of the squash/stretch factor.
*     Should the lookup table begin to the left of the lookup table
*     then the section that 'overhangs' is just ignored. Equally if the
*     lookup table 'overhangs' at the right-hand end the extra is just
*     ignored.  In the squash/stretch operation the colour indices in
*     the part of the output lookup table that are been modified
*     are allocated the nearest value from the input lookup table.

*  Arguments:
*     NINTS = INTEGER (Given)
*        The number of colour indices available in the lookup table
*        excluding the reserved pens.
*     INLUT( 3, 0:NINTS-1 ) = REAL (Given)
*        The lookup table before alteration.
*     ORIGIN = REAL (Given)
*        The origin of the colour table, as taken from some measure of
*        the locator's x displacement.
*     SQUSTR = REAL (Given)
*        The compression of the colour table as obtained from the
*        locator's y displacement.  It acts as a pointer to a lookup
*        table of compression factors, where index zero corresponds to
*        unit magnification.  The value is constrained to lie
*        between -486 and 238.  These yield scaling from 0.012 to 32.0,
*        both positive and negative.  The scale factor increments for
*        unit displacement increase as the absolute scale factor
*        increases.
*     WHITE = LOGICAL (Given)
*        If true when the lookup table is squashed the remainder of the
*        output lookup table is padded in white, otherwise the first
*        and last colours in the input lookup table are used.
*     OUTLUT( 3, 0:NINTS-1 ) = REAL (Returned)
*        Resulting lookup table.
*     STATUS = INTEGER (Given and Returned )
*        Global status parameter.

*  Algorithm:
*     When fitting the lookup table to the pens the compression
*     is carried out using the equaton :
*     PEN = OFFSET + [ RANGE * COLOUR NUMBER / ( NO. OF PENS - 1 ) ]

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Apr 19 (MJC):
*        Original version.
*     1991 February (MJC):
*        Twice increased the number of compression factors to give
*        finer control.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard ADAM constants

*  Arguments Given:
      INTEGER
     :  NINTS

      REAL
     :  ORIGIN,
     :  SQUSTR

      REAL
     :  INLUT( 3, 0:NINTS-1 )

      LOGICAL
     :  WHITE

*  Arguments Returned:
      REAL
     :  OUTLUT( 3, 0:NINTS-1 )

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL DELORG               ! Factor in offset calculation
      PARAMETER ( DELORG = 1.0 )

*  Local Variables:
      INTEGER
     :  I, J,                   ! General variables
     :  IMAG,                   ! Magnification index
     :  COLOUR,                 ! Colour index---used to point to
                                ! colour in the input LUT
     :  LOWER,                  ! Lower limit of range of pens into
                                ! which the lookup table is to be placed
     :  PADHIG,                 ! Colour index of the padding above the
                                ! the squashed LUT
     :  PADLOW,                 ! Colour index of the padding below the
                                ! the squashed LUT
     :  UPPER                   ! Last pen in range containing used part
                                ! of the lookup table.

      REAL
     :  ORIG,                   ! Offset of pens
     :  RANGE,                  ! Range of pens into which lookup table
                                ! has to be put
     :  RNGFCT( 0:119 ),        ! Factor in range calculation,
                                ! enlargement
     :  RNGFNE( -243:-124 ),    ! Factor in range calculation, negative
                                ! enlargement
     :  RNGFRD( -123:-1 ),      ! Factor in range calculation, reduction
     :  RNINTS                  ! Scaling factor from pen to colour
                                ! index

*   Local data:
      DATA RNGFNE/ -32.0, -30.6, -29.4, -28.2, -27.0, -25.9, -24.8,
     :  -23.8, -22.8, -21.9, -21.0, -20.2, -19.4, -18.6, -17.8, -17.0,
     :  -16.3, -15.6, -14.9, -14.3, -13.7, -13.1, -12.6, -12.1, -11.6,
     :  -11.2, -10.8, -10.4, -9.80, -9.44, -9.09, -8.74, -8.40, -7.96,
     :  -7.63, -7.30, -7.08, -6.87, -6.66, -6.45, -6.25, -6.05, -5.85,
     :  -5.66, -5.47, -5.29, -5.11, -4.93, -4.76, -4.59, -4.43, -4.27,
     :  -4.12, -3.97, -3.83, -3.69, -3.56, -3.43, -3.31, -3.19, -3.08,
     :  -2.97, -2.87, -2.77, -2.67, -2.58, -2.49, -2.41, -2.33, -2.26,
     :  -2.19, -2.12, -2.06, -2.00, -1.94, -1.88, -1.83, -1.78, -1.73,
     :  -1.68, -1.63, -1.59, -1.55, -1.51, -1.47, -1.43, -1.39, -1.36,
     :  -1.33, -1.30, -1.27, -1.25, -1.23, -1.21, -1.19, -1.17, -1.153,
     :  -1.134, -1.118, -1.104, -1.092, -1.086, -1.078, -1.064, -1.057,
     :  -1.051, -1.045, -1.04, -1.035, -1.03, -1.026, -1.022, -1.018,
     :  -1.015, -1.012, -1.009, -1.006, -1.004, -1.002, -1.0 /

      DATA RNGFRD/ -0.99, -0.98, -0.96, -0.94, -0.92, -0.88, -0.86,
     :  -0.84, -0.82, -0.80, -0.78, -0.76, -0.74, -0.72, -0.70, -0.68,
     :  -0.66, -0.64, -0.62, -0.60, -0.58, -0.56, -0.54, -0.52, -0.50,
     :  -0.48, -0.46, -0.44, -0.42, -0.40, -0.38, -0.36, -0.34, -0.32,
     :  -0.30, -0.28, -0.265, -0.25, -0.235, -0.22, -0.21, -0.20, -0.19,
     :  -0.18, -0.17, -0.16, -0.15, -0.14, -0.13, -0.12, -0.11, -0.10,
     :  -0.09, -0.08, -0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.012,
     :  0.012, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10,
     :  0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.20,
     :  0.21, 0.22, 0.235, 0.25, 0.265, 0.28, 0.30, 0.32, 0.34, 0.36,
     :  0.38, 0.40, 0.42, 0.44, 0.46, 0.48, 0.50, 0.52, 0.54, 0.56,
     :  0.58, 0.60, 0.62, 0.64, 0.66, 0.68, 0.70, 0.72, 0.74, 0.76,
     :  0.78, 0.80, 0.82, 0.84, 0.86, 0.88, 0.90, 0.92, 0.94, 0.96,
     :  0.98, 0.99 /

      DATA RNGFCT/ 1.0, 1.002, 1.004, 1.006, 1.009, 1.012, 1.015, 1.018,
     :  1.022, 1.026, 1.03, 1.035, 1.04, 1.045, 1.051, 1.057, 1.064,
     :  1.078, 1.086, 1.092, 1.104, 1.118, 1.134, 1.153, 1.17, 1.19,
     :  1.21, 1.23, 1.25, 1.27, 1.30, 1.33, 1.36, 1.39, 1.43, 1.47,
     :  1.51, 1.55, 1.59, 1.63, 1.68, 1.73, 1.78, 1.83, 1.88, 1.94,
     :  2.00, 2.06, 2.12, 2.19, 2.26, 2.33, 2.41, 2.49, 2.58, 2.67,
     :  2.77, 2.87, 2.97, 3.08, 3.19, 3.31, 3.43, 3.56, 3.69, 3.83,
     :  3.97, 4.12, 4.27, 4.43, 4.59, 4.76, 4.93, 5.11, 5.29, 5.47,
     :  5.66, 5.85, 6.05, 6.25, 6.45, 6.66, 6.87, 7.08, 7.30, 7.63,
     :  7.96, 8.40, 8.74, 9.09, 9.44, 9.80, 10.4, 10.8, 11.2, 11.6,
     :  12.1, 12.6, 13.1, 13.7, 14.3, 14.9, 15.6, 16.3, 17.0, 17.8,
     :  18.6, 19.4, 20.2, 21.0, 21.9, 22.8, 23.8, 24.8, 25.9, 27.0,
     :  28.2, 29.4, 30.6, 32.0 /

*.

*    If the status is bad on entry, then return to the main program.

      IF ( STATUS .NE. SAI__OK ) RETURN

      RNINTS = REAL( NINTS ) - 1.0

*    If the locator has been moved in the x direction then the
*    point in the lookup table corresponding to the beginning of
*    the lookup table is shifted.

      ORIG = ORIGIN * DELORG

*    As the cursor has been moved in the y direction the range of
*    into which the lookup table is to be fitted is changed.  Use a
*    predefined table.

      IMAG = NINT( SQUSTR * 0.5 )
      IF ( IMAG .LT. -123 ) THEN
         RANGE = RNGFNE( MAX( -243, IMAG ) ) * REAL( NINTS )
      ELSE IF ( IMAG .LT. 0 ) THEN
         RANGE = RNGFRD( MAX( -123, IMAG ) ) * REAL( NINTS )
      ELSE
         RANGE = RNGFCT( MIN( 119, IMAG ) ) * REAL( NINTS )
      END IF

*    The lower and upper limits of the lookup table part of the
*    lookup table are set.
*    ========================================================

      IF ( RANGE. GT. 0 ) THEN

*       The lower point is set to the point in the pen set where the
*       lookup table starts, unless this is outside the region 0 to
*       NINTS-1 when it is set to the appropriate limit.

         LOWER = NINT( MIN( MAX( ORIG, 0.0 ), RNINTS ) )

*       The upper point is set to the point in the pen set where the
*       end of the lookup table lies, unless this is outside the region
*       0 to NINTS-1 when it is set to the appropriate limit.

         UPPER = NINT( MAX( MIN( ORIG + RANGE, RNINTS ), 0.0 ) )
      ELSE

*       However, if the y displacement causes a negative scaling of
*       the lookup table the lower and upper points are reversed.

         UPPER = NINT( MIN( MAX( ORIG, 0.0 ), RNINTS ) )

*       The upper point is set to the point in the pen set where the
*       end of the lookup table lies, unless this is outside the region
*       0 to NINTS-1 when it is set to the appropriate limit.

         LOWER = NINT( MAX( MIN( ORIG + RANGE, RNINTS ), 0.0 ) )
      END IF

*    Fill in padding colour.
*    =======================

*    Fill in white.

      IF ( WHITE ) THEN

*       Are there any unfilled colour indices below the squashed colour
*       table?

        IF ( LOWER .GT. 0 ) THEN

*          Set all pens which appear before the colour table to white.

            DO  I = 0, LOWER-1, 1
               DO  J = 1, 3, 1
                  OUTLUT( J, I ) = 1.0
               END DO
            END DO

         END IF

*       Are there any unfilled colour indices above the squashed colour
*       table?

         IF ( UPPER .LT. NINTS-1 ) THEN

*          Set all pens after the lookup table are set to white.

            DO  I = UPPER+1, NINTS-1, 1
               DO  J = 1, 3, 1
                  OUTLUT( J, I ) = 1.0
               END DO
            END DO

         END IF

*    Pad with the end colours.

      ELSE

*       Determine the colour indices of the padding.  The sign of RANGE
*       will determine the polarity of the lookup-table mapping.

         IF ( RANGE .LT. 0.0 ) THEN
            PADLOW = NINTS - 1
            PADHIG = 0
         ELSE
            PADLOW = 0
            PADHIG = NINTS - 1
         END IF

*       Are there any unfilled colour indices below the squashed colour
*       table?

         IF ( LOWER .GT. 0 ) THEN

*          Set all pens which appear before the colour table to the
*          first value in the table.

            DO  I = 0, LOWER-1, 1
               DO  J = 1, 3, 1
                  OUTLUT( J, I ) = INLUT( J, PADLOW )
               END DO
            END DO

         END IF

*       Are there any unfilled colour indices above the squashed colour
*       table?

         IF ( UPPER .LT. NINTS-1 ) THEN

*          Set all pens after the lookup table are set to the last
*          entry in the lookup table.

            DO  I = UPPER+1, NINTS-1, 1
               DO  J = 1, 3, 1
                  OUTLUT( J, I ) = INLUT( J, PADHIG )
               END DO
            END DO

         END IF
      END IF

*    Squash or stretch the lookup table.
*    ===================================

*    Is the new lookup table just padding?

      IF ( LOWER .NE. UPPER ) THEN

*       Compress and store the lookup table in the lookup table.

         DO  I = LOWER, UPPER, 1
            DO  J = 1, 3, 1
               COLOUR = MAX( 0, MIN( NINTS-1, NINT(
     :                  ( REAL( I ) - ORIG ) / RANGE * RNINTS ) ))
               OUTLUT( J, I ) = INLUT( J, COLOUR )
            END DO
         END DO
      END IF

  999 CONTINUE

      END
