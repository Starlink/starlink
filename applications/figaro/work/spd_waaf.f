      SUBROUTINE SPD_WAAF( VARUSE, BADCHK, NX, X, Y, SY,
     :   NS, A0, A1, R, DA0, DA1, SIGMA, CHISQR, STATUS )
*+
*  Name:
*     SPD_WAAF

*  Purpose:
*     Variance-weighted linear regression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAF( VARUSE, BADCHK, NX, X, Y, SY,
*        A0, A1, R, DA0, DA1, SIGMA, CHISQR, STATUS )

*  Description:
*     This routine performs a linear regression of
*        y(x) = a0 + a1 * x
*     The routine optionally receives the y-variances and uses their
*     reciprocal as statistical weights.

*  Arguments:
*     VARUSE = LOGICAL (Given)
*        True if SY is to be used as variances of Y. Data points with
*        SY.LE.0 will be rejected.
*     BADCHK = LOGICAL (Given)
*        True if X, Y, SY are to be checked for bad values. If true
*        these data points will be rejected.
*     NX = INTEGER (Given)
*        Number of elements in X, Y, SY.
*     X( NX ) = REAL (Given)
*        The values of the independent error-free variable.
*     Y( NX ) = REAL (Given)
*        The values of the dependent variable.
*     SY( NX ) = REAL (Given)
*        The variances for Y. This array is not used if .NOT.VARUSE.
*     NS = INTEGER (Returned)
*        Number of valid points in the sample. This differs from NX if
*        VARUSE and a variance=0 was found. It also differs from NX if
*        BADCHK and bad values were found.
*     A0 = REAL (Returned)
*        The fitted ordinate interception value.
*     A1 = REAL (Returned)
*        The fitted slope.
*     R = REAL (Returned)
*        The regression coefficient. This may assume the bad value, if
*        the square root of a negative number would be needed to
*        calculate it.
*     DA0 = REAL (Returned)
*        The error of A0. This may assume the bad value, if the square
*        root of a negative number would be needed to calculate it.
*     DA1 = REAL (Returned)
*        The error of A1. This may assume the bad value, if the square
*        root of a negative number would be needed to calculate it.
*     SIGMA = REAL (Returned)
*        Estimated variance. (Unweighted average deviation from fit.)
*     CHISQR = REAL (Returned)
*        The normalised chi-squared. Set to VAL__BADR if .NOT.VARUSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status. If less than two points are valid, this
*        routine reports an error and returns unpredictable fit values.

*  References:
*     Mettig, Richter, 1983, Statistik zweier Messgroessen,
*     Astronomie und Raumfahrt, 21, Heft 3
*
*     Bevington, P.R., 1969, Data reduction and error analysis for
*     the physical sciences, McGraw-Hill Book Company, New York, San
*     Francisco, St. Louis, Toronto, London, Sidney

*  Implementation Status:
*     This might become a routine in the Public Domain Algorithms'
*     library. Then it should accept the bad value as a given argument
*     rather than VAL__BADR from PRM_PAR. It should also not use SAI__OK
*     or SAI__ERROR from SAE_PAR, but 0 and a set of documented status
*     values for the different error conditions. It should also no longer
*     issue error reports, but leave that to the caller.

*  Authors:
*     hme: Horst Meyerdierks (RAIUB, UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Aug 1991 (hme):
*        Original version.
*     30 Sep 1991 (hme):
*        Last version at RAIUB.
*     13 Jul 1991 (hme):
*        SPECDRE version. Receive variance. Switches for using variance
*        and for checking for bad values. Internal sums DOUBLE.
*     28 Oct 1991 (hme):
*        Allow DA0, DA1, R to assume bad value, if the square roots are
*        undefined. Use blank parameter and hard-wired message for
*        ERR_REP.
*     27 Nov 1991 (hme):
*        Use a unique message name for ERR_REP.
*     25 Jan 1995 (hme):
*        Renamed from RGRSS1.
*     27 Nov 1995 (hme):
*        Better error checking. Now spots the cases where only two points
*        given, where sum of weights zero, or where all x identical.
*     07 Dec 1995 (hme):
*        Correct calculation of chi-squared: The addends have the variance
*        in the denominator, not the square of the variance.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad value

*  Arguments Given:
      LOGICAL VARUSE
      LOGICAL BADCHK
      INTEGER NX
      REAL X( NX )
      REAL Y( NX )
      REAL SY( NX )

*  Arguments Returned:
      INTEGER NS
      REAL A0
      REAL A1
      REAL R
      REAL DA0
      REAL DA1
      REAL SIGMA
      REAL CHISQR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      DOUBLE PRECISION S,
     :   SP, SPX, SPY,           ! Weighted sums of 1, x, y
     :   SPXY, SPX2, SPY2,       ! Weighted subs of xy, xx, yy
     :   SDEV, SCHI              ! Sums for SIGMA, CHISQR

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Sum up various moments.
*  =======================

*  Initialise weighted sums.
      NS   = 0
      SP   = 0D0
      SPX  = 0D0
      SPY  = 0D0
      SPXY = 0D0
      SPX2 = 0D0
      SPY2 = 0D0

*  Do the weighted sums.
      IF ( VARUSE .AND. BADCHK ) THEN

*     Full checking. Use variances.
         DO 1 I = 1, NX
            IF ( SY(I) .GT. 0. .AND. SY(I) .NE. VAL__BADR .AND.
     :           X(I) .NE. VAL__BADR .AND. Y(I) .NE. VAL__BADR ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0       / SY(I)
               SPX  = SPX  + X(I)      / SY(I)
               SPY  = SPY  + Y(I)      / SY(I)
               SPXY = SPXY + X(I)*Y(I) / SY(I)
               SPX2 = SPX2 + X(I)*X(I) / SY(I)
               SPY2 = SPY2 + Y(I)*Y(I) / SY(I)
            END IF
 1       CONTINUE
      ELSE IF ( BADCHK ) THEN

*     All weights 1, check only for bad values.
         DO 2 I = 1, NX
            IF ( X(I) .NE. VAL__BADR .AND. Y(I) .NE. VAL__BADR ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0
               SPX  = SPX  + X(I)
               SPY  = SPY  + Y(I)
               SPXY = SPXY + X(I)*Y(I)
               SPX2 = SPX2 + X(I)*X(I)
               SPY2 = SPY2 + Y(I)*Y(I)
            END IF
 2       CONTINUE
      ELSE IF ( VARUSE ) THEN

*     Weight with 1/variance, check only for variance=0.
         DO 3 I = 1, NX
            IF ( SY(I) .GT. 0. ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0       / SY(I)
               SPX  = SPX  + X(I)      / SY(I)
               SPY  = SPY  + Y(I)      / SY(I)
               SPXY = SPXY + X(I)*Y(I) / SY(I)
               SPX2 = SPX2 + X(I)*X(I) / SY(I)
               SPY2 = SPY2 + Y(I)*Y(I) / SY(I)
            END IF
 3       CONTINUE
      ELSE

*     No checks, all weights 1.
         DO 4 I = 1, NX
            SP   = SP   + 1D0
            SPX  = SPX  + X(I)
            SPY  = SPY  + Y(I)
            SPXY = SPXY + X(I)*Y(I)
            SPX2 = SPX2 + X(I)*X(I)
            SPY2 = SPY2 + Y(I)*Y(I)
 4       CONTINUE
         NS = NX
      END IF


*  Check that regression makes sense.
*  ==================================

*  Any degrees of freedom?
      IF ( NS .LE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAF_E01', 'SPD_WAAF: No degrees of ' //
     :      'freedom in regression.', STATUS )
         GO TO 500
      END IF

*  Was x variable?
      IF ( SPX2 * SP .EQ. SPX * SPX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAF_E02', 'SPD_WAAF: No variation on ' //
     :      'abscissa in regression.', STATUS )
         GO TO 500
      END IF

*  Did we accummulate any weight?
      IF ( SP .LE. 0D0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAF_E03', 'SPD_WAAF: Sum of weights ' //
     :      'not positive in regression.', STATUS )
         GO TO 500
      END IF


*  Regression results.
*  ===================

*  Slope and intercept.
      A1 = ( SPXY * SP - SPY * SPX ) / ( SPX2 * SP - SPX * SPX )
      A0 = ( SPY - A1 * SPX ) / SP

*  Errors thereof.
      S = ( A0*A0 * SP + 2D0 * A0 * A1 * SPX + A1*A1 * SPX2
     :   - 2D0 * A1 * SPXY - 2D0 * A0 * SPY + SPY2 ) / FLOAT(NS-2)
      DA0 = S * SPX2 / ( SP * SPX2 - SPX * SPX )
*     DA1 = DA0 * SP / SPX2
      DA1 = S * SP / ( SP * SPX2 - SPX * SPX )
      IF ( DA0 .GT. 0. ) THEN
         DA0 = SQRT(DA0)
      ELSE
         DA0 = VAL__BADR
      END IF
      IF ( DA1 .GT. 0. ) THEN
         DA1 = SQRT(DA1)
      ELSE
         DA1 = VAL__BADR
      END IF

*  Regression coefficient.
      R = ( SPX2 - SPX * SPX / SP) * ( SPY2 - SPY * SPY / SP )
      IF ( R .GT. 0. ) THEN
         R = ( SPXY - (SPX*SPY) / SP ) / SQRT(R)
      ELSE
         R = VAL__BADR
      END IF


*  Sum up deviation squares after fit known.
*  =========================================

*  Initialise sigma and chi-squared.
      NS   = 0
      SDEV = 0D0
      SCHI = 0D0

*  Do the sums and final division for sigma and chi-squared.
      IF ( VARUSE .AND. BADCHK ) THEN

*     Full checking.
         DO 5 I = 1, NX
            IF ( SY(I) .GT. 0. .AND. SY(I) .NE. VAL__BADR .AND.
     :           X(I) .NE. VAL__BADR .AND. Y(I) .NE. VAL__BADR ) THEN
               NS = NS + 1
               SDEV = SDEV + (Y(I)-A0-A1*X(I))**2
               SCHI = SCHI + (Y(I)-A0-A1*X(I))**2 / SY(I)
            END IF
 5       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-2) )
         CHISQR = SCHI / FLOAT(NS-2)
      ELSE IF ( BADCHK ) THEN

*     Check for bad values only. CHISQR undefined.
         DO 6 I = 1, NX
            IF ( X(I) .NE. VAL__BADR .AND. Y(I) .NE. VAL__BADR ) THEN
               NS = NS + 1
               SDEV  = SDEV + (Y(I)-A0-A1*X(I))**2
            END IF
 6       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-2) )
         CHISQR = VAL__BADR
      ELSE IF ( VARUSE ) THEN

*     Check for variance=0 only.
         DO 7 I = 1, NX
            IF ( SY(I) .GT. 0. ) THEN
               NS = NS + 1
               SDEV = SDEV + (Y(I)-A0-A1*X(I))**2
               SCHI = SCHI + (Y(I)-A0-A1*X(I))**2 / SY(I)
            END IF
 7       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-2) )
         CHISQR = SCHI / FLOAT(NS-2)
      ELSE

*     No checking. CHISQR undefined.
         DO 8 I = 1, NX
            SDEV = SDEV + (Y(I)-A0-A1*X(I))**2
 8       CONTINUE
         NS = NX
         SIGMA  = SQRT( SDEV / FLOAT(NS-2) )
         CHISQR = VAL__BADR
      END IF


*  Return.
*  =======
 500  CONTINUE

      END
