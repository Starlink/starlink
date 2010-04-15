      SUBROUTINE SPD_WZCAR( BIAS, NELM, XDATA, YDATA, VAR,
     :   MOMENT, MVAR, STATUS )
*+
*  Name:
*     SPD_WZCA{DR}

*  Purpose:
*     Moments of a spectrum (use variance).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZCAR( BIAS, NELM, XDATA, YDATA, VAR,
*        MOMENT, MVAR, STATUS )

*  Description:
*     This routine works out the moments of a spectrum as required by
*     the MOMENTS application. It works on one spectrum of given length
*     and uses a variance array. Remember that y(x) is assumed to be a
*     probability distribution, i.e. y is proportional to the probability
*     of measuring a value of x.
*
*     Actually, y minus the bias value is the measure of probability, to
*     allow for a non-zero continuum or baseline level.

*  Arguments:
*     BIAS = REAL (Given)
*        YDATA-BIAS is used instead of YDATA. This is useful for spectra
*        with a constant non-zero baseline or continuum level.
*     NELM = INTEGER (Given)
*        The length of the spectrum.
*     XDATA( NELM ) = REAL (Given)
*        The vector of x values.
*     YDATA( NELM ) = REAL (Given)
*        The vector of y values. Bad values will be recognised and
*        handled properly.
*     VAR( NELM ) = REAL (Given)
*        The vector of variances of the y values. Bad values will be
*        recognised and handled properly.
*     MOMENT( 15 ) = REAL (Returned)
*        The vector containing the moments of the given spectrum:
*         1 minimum pos.         2 maximum pos.
*         3 data minimum         4 data maximum
*         5 sum of data          6 pos. of minimum
*         7 pos. of maximum      8 median
*         9 centroid            10 variance
*        11 mean abs. dev.      12 skewness
*        13 kurtosis            14 momentum
*        15 energy
*        The returned values may be the bad value where appropriate.
*     MVAR( 15 ) = REAL (Returned)
*        The vector containing the variances of the moments. The
*        returned values may be the bad value where appropriate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation status:
*     The median is not calculated and always returned with the bad
*     value.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Mar 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRM constants

*  Arguments Given:
      REAL BIAS
      INTEGER NELM
      REAL XDATA( NELM )
      REAL YDATA( NELM )
      REAL   VAR( NELM )

*  Arguments Returned:
      REAL MOMENT( 15 )
      REAL   MVAR( 15 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Vector index
      INTEGER IERR, NERR         ! Returned by VEC_ routines
      DOUBLE PRECISION SUMY      ! sum(y)
      DOUBLE PRECISION SUMV      ! sum[var(y)]
      DOUBLE PRECISION SUMXY     ! sum(x*y)
      DOUBLE PRECISION SUMXV     ! sum[x*var(y)]
      DOUBLE PRECISION SUMYA     ! sum(y*|x-centroid|)
      DOUBLE PRECISION SUMYD     ! sum[y*(x-centroid)]
      DOUBLE PRECISION SUMYDD    ! sum[y*(x-centroid)**2]
      DOUBLE PRECISION SUMYD3    ! sum[y*(x-centroid)**3]
      DOUBLE PRECISION SUMYD4    ! sum[y*(x-centroid)**4]
      DOUBLE PRECISION SIGMA     ! sqrt(variance)
      DOUBLE PRECISION LMOMEN( 15 ) ! Local versions of returned values
      DOUBLE PRECISION LMVAR(  15 ) ! Local versions of returned variances

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      LMOMEN(1)  = VAL__BADD
      LMOMEN(2)  = VAL__BADD
      LMOMEN(3)  = VAL__BADD
      LMOMEN(4)  = VAL__BADD
      LMOMEN(5)  = 0D0
      LMOMEN(6)  = VAL__BADD
      LMOMEN(7)  = VAL__BADD
      LMOMEN(8)  = VAL__BADD
      LMOMEN(9)  = VAL__BADD
      LMOMEN(10) = VAL__BADD
      LMOMEN(11) = VAL__BADD
      LMOMEN(12) = VAL__BADD
      LMOMEN(13) = VAL__BADD
      LMOMEN(14) = 0D0
      LMOMEN(15) = 0D0
      LMVAR(1)  = VAL__BADD
      LMVAR(2)  = VAL__BADD
      LMVAR(3)  = VAL__BADD
      LMVAR(4)  = VAL__BADD
      LMVAR(5)  = 0D0
      LMVAR(6)  = VAL__BADD
      LMVAR(7)  = VAL__BADD
      LMVAR(8)  = VAL__BADD
      LMVAR(9)  = VAL__BADD
      LMVAR(10) = VAL__BADD
      LMVAR(11) = VAL__BADD
      LMVAR(12) = VAL__BADD
      LMVAR(13) = VAL__BADD
      LMVAR(14) = 0D0
      LMVAR(15) = 0D0
      SUMY   = 0D0
      SUMV   = 0D0
      SUMXY  = 0D0
      SUMXV  = 0D0
      SUMYA  = 0D0
      SUMYD  = 0D0
      SUMYDD = 0D0
      SUMYD3 = 0D0
      SUMYD4 = 0D0

*  First pass:
      DO 1 I = 1, NELM

*     minimum pos. = min{x}
*     ignore x with bad y or bad var(y)
         IF ( LMOMEN(1) .EQ. VAL__BADD ) THEN
            LMOMEN(1) = XDATA(I)
         ELSE IF ( YDATA(I) .NE. VAL__BADR .AND.
     :               VAR(I) .NE. VAL__BADR       ) THEN
            LMOMEN(1) = MIN( LMOMEN(1), DBLE(XDATA(I)) )
         END IF

*     maximum pos. = max{x}
*     ignore x with bad y or bad var(y)
         IF ( LMOMEN(2) .EQ. VAL__BADD ) THEN
            LMOMEN(2) = XDATA(I)
         ELSE IF ( YDATA(I) .NE. VAL__BADR .AND.
     :               VAR(I) .NE. VAL__BADR       ) THEN
            LMOMEN(2) = MAX( LMOMEN(2), DBLE(XDATA(I)) )
         END IF

*     data minimum = min{y}
*     use IF and update pos. of minimum and var of minimum
*     ignore bad y or y with bad var(y)
*     pos. of minimum is spin-off from data minimum
         IF ( LMOMEN(3) .EQ. VAL__BADD ) THEN
            LMOMEN(3) = YDATA(I) - BIAS
            LMOMEN(6) = XDATA(I)
            LMVAR(3)  = VAR(I)
         ELSE IF ( YDATA(I) .NE. VAL__BADR .AND.
     :               VAR(I) .NE. VAL__BADR       ) THEN
            IF ( LMOMEN(3) .GT. DBLE( YDATA(I) ) ) THEN
               LMOMEN(3) = YDATA(I) - BIAS
               LMOMEN(6) = XDATA(I)
               LMVAR(3)  = VAR(I)
            END IF
         END IF

*     data maximum = max{y}
*     use IF and update pos. of maximum and var of minimum
*     ignore bad y or y with bad var(y)
*     pos. of maximum is spin-off from data maximum
         IF ( LMOMEN(4) .EQ. VAL__BADD ) THEN
            LMOMEN(4) = YDATA(I) - BIAS
            LMOMEN(7) = XDATA(I)
            LMVAR(4)  = VAR(I)
         ELSE IF ( YDATA(I) .NE. VAL__BADR .AND.
     :               VAR(I) .NE. VAL__BADR       ) THEN
            IF ( LMOMEN(4) .LT. DBLE( YDATA(I) ) ) THEN
               LMOMEN(4) = YDATA(I) - BIAS
               LMOMEN(7) = XDATA(I)
               LMVAR(4)  = VAR(I)
            END IF
         END IF

*     sum of data = sum(y)
*     one or more bad y or bad var(y) and the sum is bad, too
*     variance of sum is sum of variances
*     momentum = sum(x*y)
*     one or more bad y or bad var(y) and the sum is bad, too
*     variance is sum[x*var(y)]
*     energy = sum(x*x*y)
*     one or more bad y or bad var(y) and the sum is bad, too
*     variance is sum[x*x*var(y)]
         IF ( LMOMEN(5) .NE. VAL__BADD   .AND.
     :         YDATA(I) .NE. VAL__BADR .AND.
     :           VAR(I) .NE. VAL__BADR       ) THEN
            LMOMEN(5)  = LMOMEN(5)  + YDATA(I) - BIAS
            LMVAR(5)   = LMVAR(5)   +   VAR(I)
            LMOMEN(14) = LMOMEN(14) + XDATA(I) * (YDATA(I)-BIAS)
            LMVAR(14)  = LMVAR(14)  + XDATA(I) * VAR(I)
            LMOMEN(15) = LMOMEN(15) + XDATA(I)*XDATA(I)*(YDATA(I)-BIAS)
            LMVAR(15)  = LMVAR(15)  + XDATA(I)*XDATA(I)*VAR(I)
         ELSE
            LMOMEN(5)  = VAL__BADD
            LMVAR(5)   = VAL__BADD
            LMOMEN(14) = VAL__BADD
            LMVAR(14)  = VAL__BADD
            LMOMEN(15) = VAL__BADD
            LMVAR(15)  = VAL__BADD
         END IF

*     centroid = sum(x*y) / sum(y)
*     ignore bad y or y with bad var(y)
*     var(centroid) = sum[x*var(y)] / [sum(y)]^2
*        + sum[var(y)] * [sum(x*y)]^2 / [sum(y)]^4
*     the sum of data and momentum cannot be used here, because of
*     different bad-value propagation
         IF ( YDATA(I) .NE. VAL__BADR .AND.
     :          VAR(I) .NE. VAL__BADR       ) THEN
            SUMXY = SUMXY + XDATA(I) * ( YDATA(I) - BIAS )
            SUMY  = SUMY  + YDATA(I) - BIAS
            SUMXV = SUMXV + XDATA(I) * VAR(I)
            SUMV  = SUMV  + VAR(I)
         END IF
 1    CONTINUE

*  Complete work on centroid.
      IF ( SUMY*SUMY*SUMY*SUMY .NE. 0D0 ) THEN
         LMOMEN(9) = SUMXY / SUMY
         LMVAR(9) = ( SUMXV + SUMV*SUMXY*SUMXY/SUMY/SUMY ) / SUMY/SUMY
      END IF

*  The second and third pass make sense only because we know the
*  centroid now, or do we?
      IF ( LMOMEN(9) .NE. VAL__BADD ) THEN

*     Second pass:
         DO 2 I = 1, NELM
            IF ( YDATA(I) .NE. VAL__BADR .AND.
     :             VAR(I) .NE. VAL__BADR       ) THEN

*           variance = { sum(y*(x-centroid)^2)
*                      - [sum(y*(x-centroid))]^2 / sum(y) } / sum(y)
*           the second addend reduces the rounding error of the first
*           ignore bad y or y with bad var(y)
*           don't bother with var(variance), set it bad
               SUMYDD = SUMYDD + (YDATA(I)-BIAS)*(XDATA(I)-LMOMEN(9))**2
               SUMYD  = SUMYD  + (YDATA(I)-BIAS)*(XDATA(I)-LMOMEN(9))

*           mean abs. dev. = sum(y*|x-centroid|)/sum(y)
*           ignore bad y or y with bad var(y)
*           don't bother with var(mean abs. dev.), set it bad
               SUMYA = SUMYA + (YDATA(I)-BIAS) * ABS(XDATA(I)-LMOMEN(9))
            END IF
 2       CONTINUE

*     Complete work on variance and mean absolute deviation.
*     Negative variance is made zero.
         LMOMEN(10) = ( SUMYDD - SUMYD/SUMY ) / SUMY
         LMOMEN(10) = MAX( 0D0, LMOMEN(10) )
         LMOMEN(11) = SUMYA / SUMY
         SIGMA = SQRT( LMOMEN(10) )

*     The third pass makes sense only if sigma is positive.
         IF ( SIGMA .GT. 0D0 ) THEN

*     Third pass:
            DO 3 I = 1, NELM
               IF ( YDATA(I) .NE. VAL__BADR .AND.
     :                VAR(I) .NE. VAL__BADR       ) THEN

*              skewness = sum{y*[(x-centroid)/sigma]^3} / sum(y)
*              ignore bad y or y with bad var(y)
*              don't bother with var(skewness), set it bad
                  SUMYD3 = SUMYD3
     :               + (YDATA(I)-BIAS) * ((XDATA(I)-LMOMEN(9))/SIGMA)**3

*              kurtosis = sum{y*[(x-centroid)/sigma]^4} / sum(y) - 3
*              ignore bad y or y with bad var(y)
*              don't bother with var(kurtosis), set it bad
                  SUMYD4 = SUMYD4
     :               + (YDATA(I)-BIAS) * ((XDATA(I)-LMOMEN(9))/SIGMA)**4
               END IF
 3          CONTINUE

*        Complete work on skewness and kurtosis.
            LMOMEN(12) = SUMYD3 / SUMY
            LMOMEN(13) = SUMYD4 / SUMY - 3D0
         END IF
      END IF

*  Copy all results from local double precision to required precision,
*  retaining bad values.
      CALL VEC_DTOR( .TRUE., 15, LMOMEN, MOMENT, IERR, NERR, STATUS )
      CALL VEC_DTOR( .TRUE., 15, LMVAR,  MVAR,   IERR, NERR, STATUS )

*  Return.
      END
