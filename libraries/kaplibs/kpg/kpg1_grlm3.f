      SUBROUTINE KPG1_GRLM3( PARAM, N, D1, D2, USEBAR, BAR, LIM1,
     :                       LIM2, STATUS )
*+
*  Name:
*     KPG1_GRLM3

*  Purpose:
*     Finds the default limits for a graph axis (single precision)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GRLM3( PARAM, N, D1, D2, USEBAR, BAR, LIM1, LIM2, STATUS )

*  Description:
*     This routine is exactly like KPG1_GRLM2,except that it uses
*     single-precision data rather than double precision. Ideally, this
*     should be handled using GENERIC, but this would involve renaming
*     the long-standing KPG1_GRLM2 routine, which is not a good idea.
*
*     It returns the default limits for a graph axis. The way in which the
*     limits are chosen is specified by the user through the parameter
*     specified by PARAM. This parameter can take the following values:
*
*        - "Range" -- LIM1 and LIM2 are returned equal to the lowest and
*        highest supplied data values (including error bars).
*
*        - "Extended" -- LIM1 and LIM2 are returned equal to the lowest and
*        highest supplied data values (including error bars), extended to
*        give a margin of 2.5% of the total data range at each end.
*
*        - "Extended,10,5" -- Like "Extended", except the margins at the
*        two ends are specified as a pair of numerical value in the second
*        and third elements of the array. These values are percentages of
*        the total data range. So, "Extended,10,5" includes a margin of 10%
*        of the total data range in LIM1, and 5% in LIM2. If only one
*        numerical value is given, the same value is used for both limits. If
*        no value is given, both limits default to 2.5. "Range" is equivalent
*        to "Extended,0,0".
*
*        - "Percentiles,5,95" -- The second and third elements of the array
*        are interpreted as percentiles. For instance, "Perc,5,95" causes 5%
*        of the data points (ignoring error bars) to be below LIM1, and 10%
*        to be above the LIM2. If only 1 value (p1) is supplied, the other
*        one, p2, defaults to (100 - p1). If no values are supplied, p1 and
*        p2 default to 5 and 95.
*
*        - "Sigma,2,3" -- The second and third elements of the array are
*        interpreted as multiples of the standard deviation of the data
*        values (ignoring error bars). For instance, "S,2,3" causes the
*        LIM1 to be the mean of the data values, minus two sigma, and LIM2
*        to be the mean plus three sigma. If only 1 value is supplied, the
*        same value is used for both limits. If no values are supplied, both
*        values default to 3.0.
*
*     The above strings can be abbreviated to one character.
*
*     If the parameter name is supplied as blank, then "Extended" is
*     assumed (i.e. LIM1 and LIM2 are chosen so that the axis encompasses
*     the entire data range including error bars, with 2.5% margin at each
*     end).
*
*     If only 1 limit is required (i.e. if LIM1 or LIM2 are supplied not
*     equal to VAL__BADR), then only 1 numerical value can be supplied
*     in the above limit descriptions.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use to get the method for chosing
*        the default limits for the axis. May be blank.
*     N = INTEGER (Given)
*        No. of points
*     D1( N ) = REAL (Given)
*        The central data value at each point.
*     D2( N ) = REAL (Given)
*        An associated mask array. D1( I ) is only used if both D1( I ) and
*        D2( I ) are not equal to VAL__BADR.
*     USEBAR = LOGICAL (Given)
*        Should BAR be used?
*     BAR( N,2 ) = REAL (Given)
*        The upper and lower ends of each error bar. Assumed equal to D1
*        if USEBAR is .FALSE. (i.e. no error bars).
*     LIM1 = REAL (Given and Returned)
*        The chosen low data limit. Only returned if a value of VAL__BADR
*        is supplied, Otherwise, the supplied value is returned unchanged.
*     LIM2 = REAL (Given and Returned)
*        The chosen high data limit. Only returned if a value of VAL__BADR
*        is supplied, Otherwise, the supplied value is returned unchanged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC; Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1999 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER N
      REAL D1( N )
      REAL D2( N )
      LOGICAL USEBAR
      REAL BAR( N, 2 )

*  Arguments Given and Returned:
      REAL LIM1
      REAL LIM2

*  Status:
      INTEGER STATUS          ! Global status

*  Local Constants:
      INTEGER HISTSZ          ! Size of histogram used for percentiles
      PARAMETER( HISTSZ = 2048 )

*  Local Variables:
      CHARACTER PVALS( 3 )*20 ! Array of parameter values
      REAL DMAX               ! Max data value
      REAL DMIN               ! Min data value
      REAL DUMMY              ! Temporary storage
      REAL MEAN               ! Mean of data values
      REAL PERVAL( 2 )        ! Data values at corresponding percentiles
      REAL RANGE              ! Range of data values
      REAL S1                 ! Sum of data values
      REAL S2                 ! Sum of squared data values
      REAL S3                 ! No. of data values summed
      REAL SIG                ! Standard deviation of data values
      REAL V1                 ! First supplied numerical parameter value
      REAL V2                 ! Second supplied numerical parameter value
      INTEGER HIST( HISTSZ )  ! Array containing histogram
      INTEGER I               ! Loop count
      INTEGER IPW1            ! Pointer to work space
      INTEGER MAXPOS          ! Position of the maximum (not used)
      INTEGER MINPOS          ! Position of the minimum (not used)
      INTEGER NINVAL          ! No. of bad values found
      INTEGER NLIM            ! No. of limits reqired.
      INTEGER NVAL            ! Number of parameter values supplied
      REAL FRAC( 2 )          ! Percentiles as fractions
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  See how many limits are required.
      NLIM = 0
      IF( LIM1 .EQ. VAL__BADR ) NLIM = NLIM + 1
      IF( LIM2 .EQ. VAL__BADR ) NLIM = NLIM + 1

*  Do nothing if both limits are already defined.
      IF( NLIM .GT. 0 ) THEN

*  If no parameter has been geven, pretend we get the value "Extended"
*  from the user.
         IF( PARAM .EQ. ' ' ) THEN
            PVALS( 1 ) = 'EXTENDED'
            NVAL = 1

*  Otherwise, get an array containing strings or numerical values.
         ELSE
            CALL KPG1_MIXVR( PARAM, NLIM + 1, VAL__MINR, VAL__MAXR,
     :                      'Extended,Percentiles,Sigmas,Range', PVALS,
     :                      NVAL, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

         END IF

*  Convert "Range" to "Extended,0".
         IF( PVALS( 1 ) .EQ. 'RANGE' ) THEN

*  Report an error if any extra values were supplied.
            IF( NVAL .EQ. 2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'V', PVALS( 2 ) )
               CALL ERR_REP( 'KPG1_GRLM3_ERR1', 'Extra value (^V) '//
     :                       'supplied after ''RANGE'' for parameter'//
     :                       ' %^P.', STATUS )
               GO TO 999

            ELSE IF( NVAL .GT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'V', PVALS( 2 ) )

               DO I = 2, NVAL
                  CALL MSG_SETC( 'V', ',' )
                  CALL MSG_SETC( 'V', PVALS( I ) )
               END DO

               CALL ERR_REP( 'KPG1_GRLM3_ERR2', 'Extra values (^V) '//
     :                       'supplied after ''RANGE'' for parameter '//
     :                       '%^P.', STATUS )
               GO TO 999
            END IF

*  Replace the supplied parameter value.
            PVALS( 1 ) = 'EXTENDED'
            PVALS( 2 ) = '0.0'
            NVAL = 2

         END IF

*  Convert any numerical values to floating point form. Report an error
*  if either the second or third values are not numerical.
         V1 = VAL__BADR
         V2 = VAL__BADR

         IF( NVAL .GE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOR( PVALS( 2 ), V1, STATUS )

            IF( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'V', PVALS( 1 ) )
               CALL MSG_SETC( 'V1', PVALS( 2 ) )

               CALL ERR_REP( 'KPG1_GRLM3_ERR3', 'Non-numeric value '//
     :                       '(^V1) supplied after ''^V'' for '//
     :                       'parameter %^P.', STATUS )
               GO TO 999
            END IF
         END IF

         IF( NVAL .GE. 3 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOR( PVALS( 3 ), V2, STATUS )

            IF( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'P', PARAM )
               CALL MSG_SETC( 'V', PVALS( 1 ) )
               CALL MSG_SETC( 'V2', PVALS( 3 ) )

               CALL ERR_REP( 'KPG1_GRLM3_ERR4', 'Non-numeric value '//
     :                       '(^V2) supplied after ''^V'' for '//
     :                       'parameter %^P.', STATUS )
               GO TO 999
            END IF
         END IF

*  Check the first value against the special strings...

*  Limits given as percentage points in the data histogram.
*  ========================================================
         IF( PVALS( 1 ) .EQ. 'PERCENTILES' ) THEN

*  Allocate work space for the masked data values.
            CALL PSX_CALLOC( N, '_REAL', IPW1, STATUS )

*  Copy the data values to the work array, applying the bad value mask
*  supplied in D2 at the same time.
            CALL KPG1_CPBDR( N, D1, D2, %VAL( CNF_PVAL( IPW1 ) ),
     :                       STATUS )

*  Implement defaults for any missing numerical parameter values.
            IF( V1 .EQ. VAL__BADR ) V1 = 5.0
            IF( V2 .EQ. VAL__BADR ) V2 = 100.0 - V1

*  Convert percentiles to fractions.
            FRAC( 1 ) = REAL( V1 )*0.01
            FRAC( 2 ) = REAL( V2 )*0.01

*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.
            CALL KPG1_MXMNR( .TRUE., N, %VAL( CNF_PVAL( IPW1 ) ),
     :                       NINVAL, DMAX,
     :                       DMIN, MAXPOS, MINPOS, STATUS )

*  Generate the histogram between those bounds. The number of bad pixels
*  has been counted so it might be possible to save future processing.
            CALL KPG1_GHSTR( ( NINVAL .EQ. 0 ), N,
     :                       %VAL( CNF_PVAL( IPW1 ) ), 0.0, 0.0D0,
     :                       HISTSZ, .FALSE., DMAX, DMIN, HIST, STATUS )

*  Estimate the values at the percentiles. On exit, the values in FRAC
*  are re-arranged into ascending order.
            CALL KPG1_HSTFR( HISTSZ, HIST, DMAX, DMIN, 2, FRAC, PERVAL,
     :                       STATUS )

*  Swap the percentiles back if they were flipped.
            IF ( V2 .LT. V1 ) THEN
               DUMMY = PERVAL( 2 )
               PERVAL( 2 ) = PERVAL( 1 )
               PERVAL( 1 ) = DUMMY
            END IF

*  Return the required values. First deal with cases where only 1 limit
*  is required (e.g. HISTOGRAM always sets the lower limit to zero, so
*  only the upper limit is required to be found by this routine).
            IF( NLIM .EQ. 1 ) THEN

*  If the user supplied a percentile value, use it (i.e. PERVAL( 1 )).
               IF( NVAL .EQ. 2 ) THEN
                  IF( LIM1 .EQ. VAL__BADR ) THEN
                     LIM1 = PERVAL( 1 )
                  ELSE
                     LIM2 = PERVAL( 1 )
                  END IF

*  Otherwise (i.e. if the defaults of 5,95 have been used), use the default
*  appropriate to the required limit ( PERVAL( 1 ) for low limits,
*  PERVAL( 2 ) for high limits).
               ELSE
                  IF( LIM1 .EQ. VAL__BADR ) THEN
                     LIM1 = PERVAL( 1 )
                  ELSE
                     LIM2 = PERVAL( 2 )
                  END IF
               END IF

*  Now deal with cases where both limits are required.
            ELSE
               LIM1 = PERVAL( 1 )
               LIM2 = PERVAL( 2 )
            END IF

*  Free the work array.
            CALL PSX_FREE( IPW1, STATUS )

*  Limits given as percentage points as a range of standard deviations.
*  ====================================================================
         ELSE IF( PVALS( 1 ) .EQ. 'SIGMAS' ) THEN

*  Implement defaults for any missing numerical parameter values.
            IF( V1 .EQ. VAL__BADR ) V1 = 3.0
            IF( V2 .EQ. VAL__BADR ) V2 = V1

*  Initialise the sums.
            S1 = 0.0
            S2 = 0.0
            S3 = 0.0

*  Find the required sums so that we can calculate the mean and standard
*  deviation of the data.
            DO I = 1, N
               IF( D1( I ) .NE. VAL__BADR .AND.
     :          D2( I ) .NE. VAL__BADR ) THEN
                  S1 = S1 + D1( I )
                  S2 = S2 + D1( I )**2
                  S3 = S3 + 1.0
               END IF
            END DO

*  Report an error if if no good data was found.
            IF( S3 .LE. 1.0 ) THEN

               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPG1_GRLM3_ERR5', 'No good data to '//
     :                          'plot.', STATUS )
               END IF

               GO TO 999

*  Find the required limits.
            ELSE
               MEAN = S1/S3
               SIG = SQRT( MAX( 0.0, S2/S3 - MEAN**2 ) )

*  Return the required values.
               IF( LIM1 .EQ. VAL__BADR ) LIM1 = MEAN - V1*SIG
               IF( LIM2 .EQ. VAL__BADR ) LIM2 = MEAN + V2*SIG

            END IF

*  Limits given as an extended data range.
*  =======================================
         ELSE

*  Initialise the extreme data values.
            DMIN = VAL__MAXR
            DMAX = VAL__MINR

*  Find the data limits, including error bars if supplied.
            IF( USEBAR ) THEN

               DO I = 1, N
                  IF( D1( I ) .NE. VAL__BADR .AND.
     :                D2( I ) .NE. VAL__BADR .AND.
     :                BAR( I, 1 ) .NE. VAL__BADR .AND.
     :                BAR( I, 2 ) .NE. VAL__BADR ) THEN

                     IF( BAR( I, 1 ) .GT. BAR( I, 2 ) ) THEN
                        DMIN = MIN( DMIN, BAR( I, 2 ) )
                        DMAX = MAX( DMAX, BAR( I, 1 ) )
                     ELSE
                        DMIN = MIN( DMIN, BAR( I, 1 ) )
                        DMAX = MAX( DMAX, BAR( I, 2 ) )
                     END IF

                     DMAX = MAX( DMAX, D1( I ) )
                     DMIN = MIN( DMIN, D1( I ) )

                  END IF
               END DO

            ELSE

               DO I = 1, N
                  IF( D1( I ) .NE. VAL__BADR .AND.
     :                D2( I ) .NE. VAL__BADR ) THEN
                     DMIN = MIN( DMIN, D1( I ) )
                     DMAX = MAX( DMAX, D1( I ) )
                  END IF
               END DO

            END IF

*  Report an error if if no good data was found.
            IF( DMAX .LT. DMIN ) THEN

               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPG1_GRLM3_ERR6', 'No good data to '//
     :                          'plot.', STATUS )
               END IF

               GO TO 999

*  Find the data range.
            ELSE
               RANGE = DMAX - DMIN

*  Implement defaults for any missing numerical values.
               IF( V1 .EQ. VAL__BADR ) V1 = 2.5
               IF( V2 .EQ. VAL__BADR ) V2 = V1

*  Extend the range by the required amount at each end.
               IF( LIM1 .EQ. VAL__BADR ) LIM1 = DMIN - RANGE*V1*0.01
               IF( LIM2 .EQ. VAL__BADR ) LIM2 = DMAX + RANGE*V2*0.01

            END IF

         END IF

      END IF

 999  CONTINUE

      END
