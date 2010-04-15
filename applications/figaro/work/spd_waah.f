      SUBROUTINE SPD_WAAH( VARUSE, BADCHK, NX, X1, X2, Y, SY,
     :   NS, A0, A1, A2, DA0, DA1, DA2, SIGMA, CHISQR, STATUS )
*+
*  Name:
*     SPD_WAAH

*  Purpose:
*     Variance-weighted two-parameter linear fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAH( VARUSE, BADCHK, NX, X1, X2, Y, SY,
*        NS, A0, A1, A2, DA0, DA1, DA2, SIGMA, CHISQR, STATUS )

*  Description:
*     This routine performs a two-parameter linear fit of
*     y(x1,x2) = a0 + a1 * x1 + a2 * x2
*     The routine optionally receives the z-variances and uses their
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
*     X1( NX ) = REAL (Given)
*        The values of the first independent error-free variable.
*     X2( NX ) = REAL (Given)
*        The values of the second independent error-free variable.
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
*        The fitted slope with respect to X1.
*     A2 = REAL (Returned)
*        The fitted slope with respect to X2.
*     DA0 = REAL (Returned)
*        The error of A0. This may assume the bad value, if the square
*        root of a negative number would be needed to calculate it.
*     DA1 = REAL (Returned)
*        The error of A1. This may assume the bad value, if the square
*        root of a negative number would be needed to calculate it.
*     DA2 = REAL (Returned)
*        The error of A2. This may assume the bad value, if the square
*        root of a negative number would be needed to calculate it.
*     SIGMA = REAL (Returned)
*        Estimated variance. (Unweighted average deviation from fit.)
*     CHISQR = REAL (Returned)
*        The normalised chi-squared. Set to VAL__BADR if .NOT.VARUSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status. If less than two points are valid, this
*        routine reports an error and returns unpredictable fit values.

*  References:
*     Mettig, Richter, 1983, Statistik zweier Messgroessen, Astronomie
*     und Raumfahrt, 21, Heft 3

*  Authors:
*     hme: Horst Meyerdierks (RAIUB)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Oct 1986 (hme):
*        Original version.
*     31 Jul 1987 (hme):
*        Last version at RAIUB.
*     13 Jul 1991 (hme):
*        SPECDRE version. Receive variance. Switches for using variance
*        and for checking for bad values. Internal sums DOUBLE.
*     28 Oct 1991 (hme):
*        Allow DA0, DA1, DA2 to assume bad value, if the square roots
*        are undefined. Use blank parameter and hard-wired message for
*        ERR_REP.
*     27 Nov 1991 (hme):
*        Use unique message name for ERR_REP.
*     25 Jan 1995 (hme):
*        Renamed from RGRSS2.
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
      REAL X1( NX )
      REAL X2( NX )
      REAL Y( NX )
      REAL SY( NX )

*  Arguments Returned:
      INTEGER NS
      REAL A0
      REAL A1
      REAL A2
      REAL DA0
      REAL DA1
      REAL DA2
      REAL SIGMA
      REAL CHISQR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      DOUBLE PRECISION S, DETA,
     :   SP, SP1, SP2, SPY,      ! Weighted sums of 1, x1, x2, y
     :   SP11, SP12, SP22,       ! Weighted sums of x1**2, x1*x2, x2**2
     :   SP1Y, SP2Y, SPYY,       ! Weighted sums of x1*y, x2*y, y**2
     :   SDEV, SCHI              ! Sums for SIGMA, CHISQR

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise weighted sums.
      NS   = 0
      SP   = 0D0
      SP1  = 0D0
      SP2  = 0D0
      SPY  = 0D0
      SP11 = 0D0
      SP12 = 0D0
      SP22 = 0D0
      SP1Y = 0D0
      SP2Y = 0D0
      SPYY = 0D0

*  Do the weighted sums.
      IF ( VARUSE .AND. BADCHK ) THEN

*     Full checking. Use variances.
         DO 1 I = 1, NX
            IF ( SY(I) .GT. 0. .AND. SY(I) .NE. VAL__BADR .AND.
     :           X1(I) .NE. VAL__BADR .AND. X2(I) .NE. VAL__BADR .AND.
     :            Y(I) .NE. VAL__BADR ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0         / SY(I)
               SP1  = SP1  + X1(I)       / SY(I)
               SP2  = SP2  + X2(I)       / SY(I)
               SPY  = SPY  + Y(I)        / SY(I)
               SP11 = SP11 + X1(I)*X1(I) / SY(I)
               SP12 = SP12 + X1(I)*X2(I) / SY(I)
               SP22 = SP22 + X2(I)*X2(I) / SY(I)
               SP1Y = SP1Y + X1(I)*Y(I)  / SY(I)
               SP2Y = SP2Y + X2(I)*Y(I)  / SY(I)
               SPYY = SPYY + Y(I) *Y(I)  / SY(I)
            END IF
 1       CONTINUE
      ELSE IF ( BADCHK ) THEN

*     All weights 1, check only for bad values.
         DO 2 I = 1, NX
            IF ( X1(I) .NE. VAL__BADR .AND. X2(I) .NE. VAL__BADR .AND.
     :            Y(I) .NE. VAL__BADR ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0
               SP1  = SP1  + X1(I)
               SP2  = SP2  + X2(I)
               SPY  = SPY  + Y(I)
               SP11 = SP11 + X1(I)*X1(I)
               SP12 = SP12 + X1(I)*X2(I)
               SP22 = SP22 + X2(I)*X2(I)
               SP1Y = SP1Y + X1(I)*Y(I)
               SP2Y = SP2Y + X2(I)*Y(I)
               SPYY = SPYY + Y(I) *Y(I)
            END IF
 2       CONTINUE
      ELSE IF ( VARUSE ) THEN

*     Weight with 1/variance, check only for variance=0.
         DO 3 I = 1, NX
            IF ( SY(I) .GT. 0. ) THEN
               NS   = NS   + 1
               SP   = SP   + 1D0         / SY(I)
               SP1  = SP1  + X1(I)       / SY(I)
               SP2  = SP2  + X2(I)       / SY(I)
               SPY  = SPY  + Y(I)        / SY(I)
               SP11 = SP11 + X1(I)*X1(I) / SY(I)
               SP12 = SP12 + X1(I)*X2(I) / SY(I)
               SP22 = SP22 + X2(I)*X2(I) / SY(I)
               SP1Y = SP1Y + X1(I)*Y(I)  / SY(I)
               SP2Y = SP2Y + X2(I)*Y(I)  / SY(I)
               SPYY = SPYY + Y(I) *Y(I)  / SY(I)
            END IF
 3       CONTINUE
      ELSE

*     No checks, all weights 1.
         DO 4 I = 1, NX
            SP   = SP   + 1D0
            SP1  = SP1  + X1(I)
            SP2  = SP2  + X2(I)
            SPY  = SPY  + Y(I)
            SP11 = SP11 + X1(I)*X1(I)
            SP12 = SP12 + X1(I)*X2(I)
            SP22 = SP22 + X2(I)*X2(I)
            SP1Y = SP1Y + X1(I)*Y(I)
            SP2Y = SP2Y + X2(I)*Y(I)
            SPYY = SPYY + Y(I) *Y(I)
 4       CONTINUE
         NS = NX
      END IF

*  Enough data for a regression?
      IF ( NS .LT. 3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RGRSS_TOOFEW',
     :      'SPD_WAAH: Too few data.', STATUS )
         GO TO 500
      END IF

*  Slopes and intercept.
      A2 = SP  * ( SP11 * SP2Y - SP12 * SP1Y )
     :   + SP1 * (  SP2 * SP1Y + SP12 * SPY )
     :   - ( SP1 * SP1 * SP2Y + SP11 * SP2 * SPY )
      A2 = A2 / ( SP * ( SP11 * SP22 - SP12 * SP12 )
     :           + 2. * SP1 * SP2 * SP12
     :           - SP1 * SP1 * SP22 - SP11 * SP2 * SP2 )
      A1 = ( SP * SP1Y - SP1 * SPY + A2 * ( SP1 * SP2 - SP * SP12 ) )
     :   / ( SP * SP11 - SP1 * SP1 )
      A0 = ( SPY - A2 * SP2 - A1 * SP1 ) / SP

*  Errors thereof.
      S = SPYY + A0 * A0 * SP + A1 * A1 * SP11 + A2 * A2 * SP22
     :  + 2D0 * ( A0 * A1 * SP1 + A0 * A2 * SP2 + A1 * A2 * SP12
     :          - ( A0 * SPY + A1 * SP1Y + A2 * SP2Y ) )
      S = S / FLOAT(NS-3)
      DETA = SP * ( SP11 * SP22 - SP12 * SP12 )
     ;     - SP11 * SP2 * SP2  - SP22 * SP1 * SP1
     :     + 2D0 * SP1 * SP2 * SP12
      DA0 = S * ( SP11 * SP22 - SP12 * SP12 ) / DETA
      DA1 = S * ( SP * SP22 - SP2 * SP2 ) / DETA
      DA2 = S * ( SP * SP11 - SP1 * SP1 ) / DETA
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
      IF ( DA2 .GT. 0. ) THEN
         DA2 = SQRT(DA2)
      ELSE
         DA2 = VAL__BADR
      END IF

*  Initialise sigma and chi-squared.
      NS   = 0
      SDEV = 0D0
      SCHI = 0D0

*  Do the sums and final division for sigma and chi-squared.
      IF ( VARUSE .AND. BADCHK ) THEN

*     Full checking.
         DO 5 I = 1, NX
            IF ( SY(I) .GT. 0. .AND. SY(I) .NE. VAL__BADR .AND.
     :           X1(I) .NE. VAL__BADR .AND. X2(I) .NE. VAL__BADR .AND.
     :            Y(I) .NE. VAL__BADR ) THEN
               NS = NS + 1
               SDEV = SDEV + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2
               SCHI = SCHI + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2 / SY(I)
            END IF
 5       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-3) )
         CHISQR = SCHI / FLOAT(NS-3)
      ELSE IF ( BADCHK ) THEN

*     Check for bad values only. CHISQR undefined.
         DO 6 I = 1, NX
            IF ( X1(I) .NE. VAL__BADR .AND. X2(I) .NE. VAL__BADR .AND.
     :            Y(I) .NE. VAL__BADR ) THEN
               NS = NS + 1
               SDEV = SDEV + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2
            END IF
 6       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-3) )
         CHISQR = VAL__BADR
      ELSE IF ( VARUSE ) THEN

*     Check for variance=0 only.
         DO 7 I = 1, NX
            IF ( SY(I) .GT. 0. ) THEN
               NS = NS + 1
               SDEV = SDEV + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2
               SCHI = SCHI + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2 / SY(I)
            END IF
 7       CONTINUE
         SIGMA  = SQRT( SDEV / FLOAT(NS-3) )
         CHISQR = SCHI / FLOAT(NS-3)
      ELSE

*     No checking. CHISQR undefined.
         DO 8 I = 1, NX
            SDEV  = SDEV + (Y(I)-A0-A1*X1(I)-A2*X2(I))**2
 8       CONTINUE
         NS = NX
         SIGMA  = SQRT( SDEV / FLOAT(NS-3) )
         CHISQR = VAL__BADR
      END IF

*  Return.
 500  CONTINUE

      END
