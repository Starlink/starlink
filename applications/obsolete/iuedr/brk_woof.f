      SUBROUTINE BRK_WOOF( ALPHA, M1, N1, W1, F1, Q1, WCUT1,
     :                     M2, N2, W2, F2, Q2, WCUT2, KM, OK )
*+
*  Name:
*     SUBROUTINE BRK_WOOF

*  Purpose:
*    Use Barker's method for finding K between two adjacent orders.

*  Description:
*    This routine accepts the net spectra from two adjacent orders,
*    which Echelle numbers M1 and M2, and finds an improved
*    ripple constant, KM, which fits the data in the region of
*    overlap.   The value of ALPHA and an initial value of KM
*    are supplied.   The spectra for order Mn are defined by
*    Calibrated Vacuum Wavelengths, Wn(Nn), and Net Fluxes,
*    Fn(Nn), and Data Quality, Qn(Nn).   If successful, the
*    routine returns OK=.TRUE. and a better value in KM.

*  Method:
*    Note that M1 < M2 so that M1 includes longer wavelengths than M2.
*    The data values with Qn=0 are "perfect";  others may also be
*    usable, but it requires a bit more effort to investigate the
*    reason why they are marked "bad".   The FULL extracted spectrum
*    is provided;  no attempt has been made to remove the ends.
*    It is assumed that the fitting algorithm will reject bits which
*    have too low a signal?
*    <all yours>.
*
*    The numerical algorithm comes from P.K. Barker (1984, Ap.J.,
*    Vol. 89, 899-903).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     IDH: Ian Howarth (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     28-AUG-84 (JRG):
*       IUEDR Vn. 1.3
*       Skeletal form.
*     ??-AUG-84 (IDH):
*       IUEDR Vn. 1.3
*     30-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     16-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Local Constants:
      REAL*8 FLXMIN       ! Minimum mean flux allowable.
      REAL*8 KTOLER       ! Convergence criterion for Barker's method.
      INTEGER MAXIT       ! Maximum number of iterations alowable.
      PARAMETER ( FLXMIN = 1000.0, MAXIT  = 10, KTOLER = 3.0 )

*  Arguement Given:
      REAL*8 ALPHA        ! Ripple "Alpha" value.
      INTEGER M1          ! Lower Echelle order number.
      INTEGER N1          ! Number of wavelengths in Lower order.
      REAL*8 W1( N1 )     ! Vacuum wavelengths for Lower order.
      REAL*8 F1( N1 )     ! Net fluxes for Lower order.
      INTEGER Q1( N1 )    ! Data Quality for Lower order.
      REAL*8 WCUT1( 1 : 2 ) ! Wavelength cutoffs for lower order.
      INTEGER M2          ! Upper Echelle order number.
      INTEGER N2          ! Number of wavelengths in Upper order.
      REAL*8 W2( N2 )     ! Vacuum wavelengths for Upper order.
      REAL*8 F2( N2 )     ! Net fluxes for Upper order.
      INTEGER Q2( N2 )    ! Data Quality for Upper order.
      REAL*8 WCUT2( 1 : 2 ) ! Wavelength cutoffs for upper order.

*  Arguments Given and Returned:
      REAL*8 KM           ! Ripple "K" value between orders.

*  Status:
      LOGICAL OK          ! Whether KM is any good.

*  External references:
      REAL*8 BRK_FLXMEAN

*  Local variables:
      REAL*8 FLXVAL
      REAL*8 FMEAN1
      REAL*8 FMEAN2
      REAL*8 KCORR
      REAL*8 TEST
      REAL*8 WMEAN1
      REAL*8 WMEAN2

      LOGICAL FINISH

      INTEGER I
      INTEGER IM1W1
      INTEGER IM1W2
      INTEGER IM2W1
      INTEGER IM2W2
*.

*  Initialise.
      OK = .TRUE.
      FINISH= .FALSE.

*  Find overlap range, returning corresponding array indexes
*  Use all of overlap; if none present, use 1A bands
*  nearest ends of orders.
      CALL BRK_OVLAP( M1, N1, W1, F1, Q1, WCUT1,
     :                M2, N2, W2, F2, Q2, WCUT2,
     :                IM1W1, IM1W2, IM2W1, IM2W2 )

*  Check that sufficient nett flux is available in each region
*  Mean flux for order 1.
      FLXVAL = BRK_FLXMEAN( IM1W1, IM1W2, N1, F1, W1, Q1, OK )
      IF ( .NOT. OK ) THEN
         GO TO 999
      END IF

*  Test against minimum flux.
      TEST = FLXVAL / FLXMIN
      IF ( TEST .LT. 1.0 ) THEN
         OK = .FALSE.
         GO TO 999
      END IF

*  Mean flux for order 2.
      FLXVAL = BRK_FLXMEAN( IM2W1, IM2W2, N2, F2, W2, Q2, OK )
      IF ( .NOT. OK ) THEN
         GO TO 999
      END IF

*  Test against minimum flux.
      TEST = FLXVAL / FLXMIN
      IF ( TEST .LT. 1.0 ) THEN
         OK = .FALSE.
         GO TO 999
      END IF

*  OK to here - then begin Barker's method.
*  Iterate to optimum KM for order.
      I = 0

      DO WHILE ( FINISH .EQV. .FALSE. )
         I = I + 1

*     Test for maximum number of iterations exceeded.
         IF ( I .GE. MAXIT ) THEN
            OK = .FALSE.
            GO TO 100
         END IF

*     Evaluate averaged ripple corrected fluxes
*     For order 1.
         CALL BRK_RIPVAL( M1, IM1W1, IM1W2, N1, F1, W1, Q1, KM, ALPHA,
     :                    FMEAN1, WMEAN1, OK )

*     Test for errors in BRK_RIPVAL.
         IF ( .NOT. OK ) THEN
            GO TO 100
         END IF

*     For order 2
         CALL BRK_RIPVAL( M2, IM2W1, IM2W2, N2, F2, W2, Q2, KM, ALPHA,
     :                    FMEAN2, WMEAN2, OK )

*     Test for errors in BRK_RIPVAL.
         IF ( .NOT. OK ) THEN
            GO TO 100
         END IF

*     Calculate correction to 'K'.
         CALL BRK_KCORR( M1, M2, FMEAN1, FMEAN2, WMEAN1, WMEAN2, KM,
     :                   ALPHA, KCORR, OK )

*     Test for errors in BRK_KCORR.
         IF ( .NOT. OK ) THEN
            GO TO 100
         END IF

*     Apply correction to KM.
         KM = KM - KCORR

*     Test convergence
         IF ( ABS( KCORR ) .LT. KTOLER ) THEN
            FINISH = .TRUE.
         END IF

*     Check for abort condition.
 100     CONTINUE
         IF ( .NOT. OK ) THEN
            FINISH = .TRUE.
         END IF
      ENDDO

 999  CONTINUE

      END
