      SUBROUTINE KPS1_ZPRGR( DIM1, DIM2, LBND, UBND, NOISE,
     :                         VAR, ARRAY, STATUS )
*+
*  Name:
*     KPS1_ZPRGx

*  Purpose:
*     Linearly interpolates over a region in a 2-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ZPRGx( DIM1, DIM2, LBND, UBND, NOISE, VAR, ARRAY,
*                      STATUS )

*  Description:
*     This routine bi-linearly interpolates across regions of a 2-d
*     array an associated variance array.  If any of the lines or
*     columns to be replaced are edge lines or columns, interpolation
*     is not possible, and duplication of the nearest good line or
*     column is used.  If there are no good lines or columns, such as
*     in a corner, bad pixels are substituted in the region.  Random
*     Normal-distribution noise may be added to the data using the
*     variance for cosmetic effect.  Where the variance is bad no noise
*     is added.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the 2-d array to be processed.
*     DIM2 = INTEGER (Given)
*        The second dimension of the 2-d array to be processed.
*     LBND( 2 ) = INTEGER (Given)
*        Lower bounds of the region to be modified.  Note that these are
*        within the array whose bounds are 1 to DIM1 and 1 to DIM2.
*     UBND( 2 ) = INTEGER (Given)
*        Upper bounds of the region to be modified.  Note that these are
*        within the array whose bounds are 1 to DIM1 and 1 to DIM2.
*     NOISE = LOGICAL (Given)
*        True if Normal-distribution noise is to be added to the data
*        array after replacement of values.  Noise will not be added to
*        a bad data value.
*     VAR( DIM1, DIM2 ) = ? (Given and Returned)
*        Array containing data to be processed.
*     ARRAY( DIM1, DIM2 ) = ? (Given and Returned)
*        Array containing data to be processed.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     arrays supplied to the routine must have the data type specified.

*  Algorithm:
*     The routine takes as input a data array and its dimensions,
*     and also the start and finish lines and columns that are to be
*     modified. First interpolation across lines is calculated followed
*     by interpolation across columns and then the average is the final
*     result. If the start or finish of the columns and lines are at an
*     edge, no interpolation can take place, and the 'bad' data are
*     replaced with the most adjacent line or column of 'good' data.
*     Interpolation is used if the lines or columns concerned are
*     flanked on either side by at least one line or column pixel of
*     'good' and valid data. (If a flanking pixel is invalid then the
*     next adjacent pixel is used until a valid pixel or an edge of the
*     array is encountered. Should the latter occur the 'bad' data are
*     replaced by the most adjacent line or column of 'good' data.
*     If the NOISE argument is set to true on entry, then
*     pseudo-Poissonian noise is added to the interpolated data. After
*     modification, the same array is returned.

*  Implementation Deficiencies:
*     No bad-pixel flag to improve efficiency.

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
*     MJC: Malcolm Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 28 (MJC):
*        Original based on ZPLNSB in pre-0.8 KAPPA.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Standard ADAM constants
      INCLUDE  'PRM_PAR'         ! PRIMDAT public constants

*  Arguments Given:
      INTEGER
     :  DIM1, DIM2,
     :  LBND( 2 ),
     :  UBND( 2 )

      LOGICAL
     :  NOISE

*  Arguments Given and Returned:
      REAL
     :  VAR( DIM1, DIM2 ),
     :  ARRAY( DIM1, DIM2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  RLBND( 2 ),              ! Region lower bound
     :  RUBND( 2 ),              ! Region upper bound
     :  STARTM,                  ! Actual start position for
                                 ! interpolation
     :  FINSHP,                  ! Actual finish position for
                                 ! interpolation
     :  I, J, II, JJ             ! General counter variables

      REAL
     :  TEMARR,                  ! A column-interpolated array value
     :  TEMVAR                   ! A column-interpolated variance

      DOUBLE PRECISION
     :  DELARR,                  ! Increment in data value
     :  DELVAR,                  ! Increment in variance value
     :  DIFARR,                  ! Increment in data value per
                                 ! line/column
     :  DIFVAR,                  ! Increment in variance value per
                                 ! line/column
     :  SEPAR                    ! Separation of the interpolation
                                 ! bounds

      LOGICAL                    ! True if:
     :  LINES,                   ! Interpolate across lines
     :  SEDGE,                   ! Start pixel for interpolation lies
                                 ! beyond the array
     :  FEDGE                    ! Finish pixel for interpolation lies
                                 ! beyond the array
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'     ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'     ! NUM definitions for conversions

*.

*    Check the inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make sure that the bounds are in the correct order otherwise swap
*    them.

      RLBND( 1 ) = MIN( LBND( 1 ), UBND( 1 ) )
      RLBND( 2 ) = MIN( LBND( 2 ), UBND( 2 ) )
      RUBND( 1 ) = MAX( LBND( 1 ), UBND( 1 ) )
      RUBND( 2 ) = MAX( LBND( 2 ), UBND( 2 ) )

*    Determine if interpolation across lines is required.  See whether
*    the line bounds span the whole array.

      LINES = .NOT. ( RLBND( 2 ) .LE. 1 .OR. RUBND( 2 ) .GE. DIM2 )

*    First interpolate across lines:
*    ===============================

      IF ( LINES ) THEN
         DO  I =  RLBND( 1 ), RUBND( 1 )

*          Initialise STARTM and FINSHP.

            STARTM = RLBND( 2 ) - 1
            FINSHP = RUBND( 2 ) + 1

*          See where the interpolation will start from.

            SEDGE = .FALSE.
            IF ( STARTM .LE. 1 ) SEDGE = .TRUE.

*          The start position cannot be an invalid pixel.

            DO WHILE ( ARRAY( I, STARTM ) .EQ. VAL__BADR .AND.
     :                .NOT. SEDGE )

*             Move start position back and check whether the bottom
*             edge of the image has been encountered.

               STARTM = STARTM - 1
               IF ( STARTM .LT. 1 ) THEN
                  SEDGE = .TRUE.
                  STARTM = 1
               END IF
            END DO

*          See where the interpolation will finish.

            FEDGE = .FALSE.
            IF ( FINSHP .LE. 1 ) FEDGE = .TRUE.

*          The finish position cannot be an invalid pixel.

            DO WHILE ( ARRAY( I, FINSHP ) .EQ. VAL__BADR .AND.
     :                 .NOT. FEDGE )

*             Move the finish position forwards and check whether the
*             top edge of the image has been encountered.

               FINSHP = FINSHP + 1
               IF ( FINSHP .GT. DIM2 ) THEN
                  FEDGE = .TRUE.
                  FINSHP = DIM2
               END IF
            END DO

*          If the line adjacent to the lines to be substituted is bad
*          then make all pixels in the specified range bad.  This may
*          overturned when looking in the other direction.  Note the
*          variance is unchanged since no new value has been
*          substituted.

            IF ( SEDGE .AND. FEDGE ) THEN
               DO  J = RLBND( 2 ), RUBND( 2 )
                  ARRAY( I, J ) = VAL__BADR
               END DO

*          At the bottom edge of the image.
*          ================================

            ELSE IF ( SEDGE ) THEN

               DO  J = RLBND( 2 ), RUBND( 2 )

*                Duplicate the data and variance.

                  ARRAY( I, J ) = ARRAY( I, FINSHP )
                  VAR( I, J ) = VAR( I, FINSHP )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. VAR( I, J ) .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, VAR( I, J ),
     :                                 ARRAY( I, J ), STATUS )
               END DO

*          At the top edge of the image.
*          =============================

            ELSE IF ( FEDGE ) THEN

               DO  J = RLBND( 2 ), RUBND( 2 )

*                Duplicate the data and variance.

                  ARRAY( I, J ) = ARRAY( I, STARTM )
                  VAR( I, J ) = VAR( I, STARTM )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. VAR( I, J ) .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, VAR( I, J ),
     :                                 ARRAY( I, J ), STATUS )
               END DO

*          The line bounds are at neither edge of the image.
*          =================================================

            ELSE

*             Find the separation of the bounds.

               SEPAR = DBLE( FINSHP - STARTM )

*             Find the intensity step per pixel over the gap.  Note the
*             lack of factorisation to reduce the risk of an overflow.

               DIFARR = ( NUM_RTOD( ARRAY( I, FINSHP ) ) / SEPAR ) -
     :                  ( NUM_RTOD( ARRAY( I, STARTM ) ) / SEPAR )

*             Similarly for the variance.  This is strictly not correct
*             but should make little difference.

               DIFVAR = ( NUM_RTOD( VAR( I, FINSHP ) ) / SEPAR ) -
     :                  ( NUM_RTOD( VAR( I, STARTM ) ) / SEPAR )

               DO  J = RLBND( 2 ), RUBND( 2 )

*               Work out how far across the gap we are.

                  JJ = J - STARTM

*                Find the shift in value and variance.

                  DELARR = DIFARR * DBLE( JJ )
                  DELVAR = DIFVAR * DBLE( JJ )

*                Now add the step per pixel times the number of steps
*                to the starting value to interpolate into the gap.

                  ARRAY( I, J ) = ARRAY( I, STARTM ) +
     :                            NUM_DTOR( DELARR )
                  VAR( I, J ) = VAR( I, STARTM ) + NUM_DTOR( DELVAR )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. VAR( I, J ) .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, VAR( I, J ),
     :                                 ARRAY( I, J ), STATUS )
               END DO
            END IF

*       End of the loop for the columns.

         END DO

*    End of the check for line interpolation.

      END IF

*    Now repeat for the columns to be zapped:
*    ========================================

      IF ( .NOT. ( RLBND( 1 ) .LE. 1 .OR. RUBND( 1 ) .GE. DIM1 ) ) THEN
         DO  J = RLBND( 2 ), RUBND( 2 )

*          Initialise STARTM and FINSHP.

            STARTM = RLBND( 1 ) - 1
            FINSHP = RUBND( 1 ) + 1

*          See where the interpolation will start from.

            SEDGE = .FALSE.
            IF ( STARTM .LE. 1 ) SEDGE = .TRUE.

*          The start position cannot be an invalid pixel.

            DO WHILE ( ARRAY( STARTM, J ) .EQ. VAL__BADR .AND.
     :                 .NOT. SEDGE )

*             Move the start position back and check whether the left
*             edge of the image has been encountered.

               STARTM = STARTM - 1
               IF ( STARTM .LT. 1 ) THEN
                  SEDGE = .TRUE.
                  STARTM = 1
               END IF
            END DO

*         See where the interpolation will finish.

            FEDGE = .FALSE.
            IF ( FINSHP .LE. 1 ) FEDGE = .TRUE.

*          The finish position cannot be an invalid pixel.

            DO WHILE ( ARRAY( FINSHP, J ) .EQ. VAL__BADR .AND.
     :                 .NOT. FEDGE )

*             Move the finish position forwards and check whether the
*             right edge of the image has been encountered.

               FINSHP = FINSHP + 1
               IF ( FINSHP .GT. DIM1 ) THEN
                  FEDGE = .TRUE.
                  FINSHP = DIM1
               END IF
            END DO

*          At the left edge of the image.
*          ==============================

            IF ( SEDGE ) THEN

               DO  I = RLBND( 1 ), RUBND( 1 )

*                Duplicate the data so that the average of the two
*                possible interpolations may be evaluated.

                  TEMARR = ARRAY( STARTM, J )
                  TEMVAR = VAR( STARTM, J )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. TEMVAR .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, TEMVAR, TEMARR,
     :                                 STATUS )

*                If the interpolation across lines gave an undefined
*                result or never happened, just use the value from
*                interpolation across columns.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     ARRAY( I, J ) = TEMARR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     ARRAY( I, J ) = ( TEMARR / 2.0E0 ) +
     :                               ( ARRAY( I, J ) / 2.0E0 )
                  END IF

*                Repeat for the variance.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     VAR( I, J ) = TEMVAR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     VAR( I, J ) = ( TEMVAR / 2.0E0 ) +
     :                             ( VAR( I, J ) / 2.0E0 )
                  END IF
               END DO

*          At the right edge of the image.
*          ===============================

            ELSE IF ( FEDGE ) THEN

               DO  I = RLBND( 1 ), RUBND( 1 )

*                Duplicate the data so that the average of the two
*                possible interpolations may be evaluated.

                  TEMARR = ARRAY( FINSHP, J )
                  TEMVAR = VAR( FINSHP, J )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. TEMVAR .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, TEMVAR, TEMARR,
     :                                 STATUS )

*                If the interpolation across lines gave an undefined
*                result or never happened, just use the value from
*                interpolation across columns.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     ARRAY( I, J ) = TEMARR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     ARRAY( I, J ) = ( TEMARR / 2.0E0 ) +
     :                               ( ARRAY( I, J ) / 2.0E0 )
                  END IF

*                Repeat for the variance.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     VAR( I, J ) = TEMVAR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     VAR( I, J ) = ( TEMVAR / 2.0E0 ) +
     :                             ( VAR( I, J ) / 2.0E0 )
                  END IF
               END DO

*          The column bounds are at neither edge of the image.
*          ===================================================

*          If the column adjacent to the bad columns is bad there is no
*          need to make all pixels in the specified range bad because
*          the line interpolation may have produced defined values.

            ELSE IF ( .NOT. ( SEDGE .AND. FEDGE ) ) THEN

*             So no edge effects to worry about.

*             Find the separation of the bounds.

               SEPAR = DBLE( FINSHP - STARTM )

*             Find the intensity step per pixel over the gap.  Note the
*             lack of factorisation to reduce the risk of an overflow.

               DIFARR = ( NUM_RTOD( ARRAY( FINSHP, J ) ) / SEPAR ) -
     :                  ( NUM_RTOD( ARRAY( STARTM, J ) ) / SEPAR )

*             Similarly for the variance.  This is strictly not correct
*             but should make little difference.

               DIFVAR = ( NUM_RTOD( VAR( FINSHP, J ) ) / SEPAR ) -
     :                  ( NUM_RTOD( VAR( STARTM, J ) ) / SEPAR )

               DO  I = RLBND( 1 ), RUBND( 1 )

*               Work out how far across the gap we are.

                  II = I - STARTM

*                Find the shift in value and variance.

                  DELARR = DIFARR * DBLE( II )
                  DELVAR = DIFVAR * DBLE( II )

*                Now add the step per pixel times the number of steps
*                to the starting value to interpolate into the gap.

                  TEMARR = ARRAY( STARTM, J ) + NUM_DTOR( DELARR )
                  TEMVAR = VAR( STARTM, J ) + NUM_DTOR( DELVAR )

*                Add random Normal-distribution noise and bad pixels
*                may be present.

                  IF ( NOISE .AND. TEMVAR .NE. VAL__BADR )
     :              CALL KPG1_NOISR( .TRUE., 1, TEMVAR, TEMARR,
     :                                 STATUS )

*                If the interpolation across lines gave an undefined
*                result or never happened, just use the value from
*                interpolation across columns.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     ARRAY( I, J ) = TEMARR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     ARRAY( I, J ) = ( TEMARR / 2.0E0 ) +
     :                               ( ARRAY( I, J ) / 2.0E0 )
                  END IF

*                Repeat for the variance.

                  IF ( ARRAY( I, J ) .EQ. VAL__BADR .OR.
     :                 .NOT. LINES ) THEN
                     VAR( I, J ) = TEMVAR

*                Otherwise average the two interpolations.  Lack of
*                factorisation is to prevent overflows.

                  ELSE
                     VAR( I, J ) = ( TEMVAR / 2.0E0 ) +
     :                             ( VAR( I, J ) / 2.0E0 )
                  END IF
               END DO
            END IF

*       End of the loop for the lines.

         END DO

*    End of the check for column interpolation.

      END IF

*    End and return

      END
