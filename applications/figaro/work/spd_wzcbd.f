      SUBROUTINE SPD_WZCBD( BIAS, NELM, XDATA, YDATA, MOMENT, STATUS )
*+
*  Name:
*     SPD_WZCB{DR}

*  Purpose:
*     Moments of a spectrum (ignore variance).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZCBD( BIAS, NELM, XDATA, YDATA, MOMENT, STATUS )

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
*     BIAS = DOUBLE PRECISION (Given)
*        YDATA-BIAS is used instead of YDATA. This is useful for spectra
*        with a constant non-zero baseline or continuum level.
*     NELM = INTEGER (Given)
*        The length of the spectrum.
*     XDATA( NELM ) = DOUBLE PRECISION (Given)
*        The vector of x values.
*     YDATA( NELM ) = DOUBLE PRECISION (Given)
*        The vector of y values. Bad values will be recognised and
*        handled properly.
*     MOMENT( 15 ) = DOUBLE PRECISION (Returned)
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
*     03 Oct 1994 (hme):
*        Refuse third pass if variance turns out negative. This can
*        happen if the far wings are on average negative.
*        Also, the good-data check was wrong for min/max{x}, min/max{y}
*        and the position of min/max{y}.
*        Also, if the mean absolute deviation turns out to be negative,
*        set it bad.
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
      DOUBLE PRECISION BIAS
      INTEGER NELM
      DOUBLE PRECISION XDATA( NELM )
      DOUBLE PRECISION YDATA( NELM )

*  Arguments Returned:
      DOUBLE PRECISION MOMENT( 15 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Vector index
      INTEGER IERR, NERR         ! Returned by VEC_ routines
      DOUBLE PRECISION SUMY      ! sum(y)
      DOUBLE PRECISION SUMXY     ! sum(x*y)
      DOUBLE PRECISION SUMYA     ! sum(y*|x-centroid|)
      DOUBLE PRECISION SUMYD     ! sum[y*(x-centroid)]
      DOUBLE PRECISION SUMYDD    ! sum[y*(x-centroid)**2]
      DOUBLE PRECISION SUMYD3    ! sum[y*(x-centroid)**3]
      DOUBLE PRECISION SUMYD4    ! sum[y*(x-centroid)**4]
      DOUBLE PRECISION SIGMA     ! sqrt(variance)
      DOUBLE PRECISION LMOMEN( 15 ) ! Local versions of returned values

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
      SUMY   = 0D0
      SUMXY  = 0D0
      SUMYA  = 0D0
      SUMYD  = 0D0
      SUMYDD = 0D0
      SUMYD3 = 0D0
      SUMYD4 = 0D0

*  First pass:
      DO 1 I = 1, NELM

*     minimum pos. = min{x}
*     ignore x with bad y
         IF ( YDATA(I) .NE. VAL__BADD ) THEN
            IF ( LMOMEN(1) .EQ. VAL__BADD ) THEN
               LMOMEN(1) = XDATA(I)
            ELSE
               LMOMEN(1) = MIN( LMOMEN(1), DBLE(XDATA(I)) )
            END IF
         END IF

*     maximum pos. = max{x}
*     ignore x with bad y
         IF ( YDATA(I) .NE. VAL__BADD ) THEN
            IF ( LMOMEN(2) .EQ. VAL__BADD ) THEN
               LMOMEN(2) = XDATA(I)
            ELSE
               LMOMEN(2) = MAX( LMOMEN(2), DBLE(XDATA(I)) )
            END IF
         END IF

*     data minimum = min{y}
*     use IF and update pos. of minimum
*     ignore bad y
*     pos. of minimum is spin-off from data minimum
         IF ( YDATA(I) .NE. VAL__BADD ) THEN
            IF ( LMOMEN(3) .EQ. VAL__BADD .OR.
     :           LMOMEN(3) .GT. DBLE( YDATA(I) ) ) THEN
               LMOMEN(3) = YDATA(I) - BIAS
               LMOMEN(6) = XDATA(I)
            END IF
         END IF

*     data maximum = max{y}
*     use IF and update pos. of maximum
*     ignore bad y
*     pos. of maximum is spin-off from data maximum
         IF ( YDATA(I) .NE. VAL__BADD ) THEN
            IF ( LMOMEN(4) .EQ. VAL__BADD .OR.
     :           LMOMEN(4) .LT. DBLE( YDATA(I) ) ) THEN
               LMOMEN(4) = YDATA(I) - BIAS
               LMOMEN(7) = XDATA(I)
            END IF
         END IF

*     sum of data = sum(y)
*     one or more bad y and the sum is bad, too
         IF ( LMOMEN(5) .NE. VAL__BADD   .AND.
     :         YDATA(I) .NE. VAL__BADD       ) THEN
            LMOMEN(5) = LMOMEN(5) + YDATA(I) - BIAS
         ELSE
            LMOMEN(5) = VAL__BADD
         END IF

*     momentum = sum(x*y)
*     one or more bad y and the sum is bad, too
         IF ( LMOMEN(14) .NE. VAL__BADD   .AND.
     :          YDATA(I) .NE. VAL__BADD       ) THEN
            LMOMEN(14) = LMOMEN(14) + XDATA(I) * (YDATA(I)-BIAS)
         ELSE
            LMOMEN(14) = VAL__BADD
         END IF

*     energy = sum(x*x*y)
*     one or more bad y and the sum is bad, too
         IF ( LMOMEN(15) .NE. VAL__BADD   .AND.
     :          YDATA(I) .NE. VAL__BADD       ) THEN
            LMOMEN(15) = LMOMEN(15) + XDATA(I)*XDATA(I)*(YDATA(I)-BIAS)
         ELSE
            LMOMEN(15) = VAL__BADD
         END IF

*     centroid = sum(x*y) / sum(y)
*     ignore bad y
*     the sum of data and momentum cannot be used here, because of
*     different bad-value propagation
         IF ( YDATA(I) .NE. VAL__BADD ) THEN
            SUMXY = SUMXY + XDATA(I) * ( YDATA(I) - BIAS )
            SUMY  = SUMY  + YDATA(I) - BIAS
         END IF
 1    CONTINUE

*  Complete work on centroid.
      IF ( SUMY*SUMY*SUMY*SUMY .NE. 0D0 ) THEN
         LMOMEN(9) = SUMXY / SUMY
      END IF

*  The second and third pass make sense only because we know the
*  centroid now, or do we?
      IF ( LMOMEN(9) .NE. VAL__BADD ) THEN

*     Second pass:
         DO 2 I = 1, NELM
            IF ( YDATA(I) .NE. VAL__BADD ) THEN

*           variance = { sum(y*(x-centroid)^2)
*                      - [sum(y*(x-centroid))]^2 / sum(y) } / sum(y)
*           the second addend reduces the rounding error of the first
*           ignore bad y
               SUMYDD = SUMYDD + (YDATA(I)-BIAS)*(XDATA(I)-LMOMEN(9))**2
               SUMYD  = SUMYD  + (YDATA(I)-BIAS)*(XDATA(I)-LMOMEN(9))

*           mean abs. dev. = sum(y*|x-centroid|)/sum(y)
*           ignore bad y
               SUMYA = SUMYA + (YDATA(I)-BIAS) * ABS(XDATA(I)-LMOMEN(9))
            END IF
 2       CONTINUE

*     Complete work on variance and mean absolute deviation.
         LMOMEN(10) = ( SUMYDD - SUMYD/SUMY ) / SUMY
         LMOMEN(11) = SUMYA / SUMY
         IF ( LMOMEN(11) .LT. 0D0 ) LMOMEN(11) = VAL__BADD

*     Third pass:
*     skewness = sum{y*[(x-centroid)/sigma]^3} / sum(y)
*     ignore bad y
*     kurtosis = sum{y*[(x-centroid)/sigma]^4} / sum(y) - 3
*     ignore bad y

*     If the far wings are on average negative, then the variance may
*     have turned out negative, too. In that case we must set it bad and
*     refrain from the third pass.
         IF ( LMOMEN(10) .LE. 0D0 ) THEN
            LMOMEN(10) = VAL__BADD
         ELSE
            SIGMA = SQRT( LMOMEN(10) )
            DO 3 I = 1, NELM
               IF ( YDATA(I) .NE. VAL__BADD ) THEN
                  SUMYD3 = SUMYD3 + (YDATA(I)-BIAS)
     :               * ((XDATA(I)-LMOMEN(9))/SIGMA) ** 3
                  SUMYD4 = SUMYD4 + (YDATA(I)-BIAS)
     :               * ((XDATA(I)-LMOMEN(9))/SIGMA) ** 4
               END IF
 3          CONTINUE
            LMOMEN(12) = SUMYD3 / SUMY
            LMOMEN(13) = SUMYD4 / SUMY - 3D0
         END IF
      END IF

*  Copy all results from local double precision to required precision,
*  retaining bad values.
      CALL VEC_DTOD( .TRUE., 15, LMOMEN, MOMENT, IERR, NERR, STATUS )

*  Return.
      END
