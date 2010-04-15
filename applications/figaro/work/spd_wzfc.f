      SUBROUTINE SPD_WZFC( FITTED, FILE, FU, IN, VARUSE, NELM,
     :   MSKDIM, MSKUSE, MSKELM, MASK, XMIN, XMAX,
     :   MAXPAR, ORDER, COEFF, CHISQR, STATUS )
*+
*  Name:
*     SPD_WZFC

*  Purpose:
*     FITPOLY reporting.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZFC( FITTED, FILE, FU, IN, VARUSE, NELM,
*        MSKDIM, MSKUSE, MSKELM, MASK, XMIN, XMAX,
*        MAXPAR, ORDER, COEFF, CHISQR, STATUS )

*  Description:
*     This routine reports the current settings of FITPOLY to the
*     standard output device (usually the screen) or to a log file.

*  Arguments:
*     FITTED = LOGICAL (Given)
*        True if fit parameters, degrees of freedom and chi-squared are
*        to be reported.
*     FILE = LOGICAL (Given)
*        If true, report goes to file with unit number FU. If false,
*        report goes to the standard output device (usually the screen).
*     FU = INTEGER (Given)
*        The Fortran file unit number.
*     IN = CHARACTER * ( * ) (Given)
*        The input NDF name as to be reported.
*     VARUSE = LOGICAL (Given)
*        The flag for usage of variance, as to be reported. The value of
*        VARUSE also determines how CHISQR is intepreted and reported.
*     NELM = INTEGER (Given)
*        The size of the NDF as to be reported.
*     MSKDIM = INTEGER (Given)
*        The size of the MASK array. Should be even. This is not
*        reported but used for dimensioning the MASK array and for
*        finding the interval bounds therein.
*     MSKUSE = INTEGER (Given)
*        The number of used mask intervals. If given as zero, no mask
*        will be reported. This number is reported and determines how
*        many intervals are reported.
*     MSKELM = INTEGER (Given)
*        The number of abscissa values that lie within the mask, as to
*        be reported. This value is also used to calculate the number of
*        degrees of freedom. If MSKUSE is false and FITTED is true, this
*        value should be given as equal to NELM.
*     MASK( MSKDIM ) = REAL (Given)
*        The lower (1...MSKDIM/2) and upper (MSKDIM/2+1 ... MSKDIM)
*        bounds of the mask intervals. The mask intervals are reported
*        as: [MASK(1);MASK(1+MSKDIM/2)],
*            [MASK(2);MASK(2+MSKDIM/2)],
*            ...,
*            [MASK(MSKUSE);MASK(MSKUSE+MSKDIM/2)].
*     XMIN = REAL (Given)
*        The minimum abscissa value as to be reported.
*     XMAX = REAL (Given)
*        The maximum abscissa value as to be reported.
*     MAXPAR = INTEGER (Given)
*        The size of the COEFF array. Only ORDER+1 elements are
*        reported.
*     ORDER = INTEGER (Given)
*        The order of the polynomial. This is reported and determines
*        how many COEFF elements are reported.
*     COEFF( MAXPAR ) = REAL (Given)
*        Polynomial coefficients. Only elements 1 to ORDER+1
*        are used.
*     CHISQR = REAL (Given)
*        The chi-squared, or the square of r.m.s. times the degrees of
*        freedom, depending on VARUSE.
*        If VARUSE is true then CHISQR is reported as being chi-squared.
*        If VARUSE is false then the square root of
*        CHISQR/(MSKELM-ORDER-1) is reported as being the r.m.s.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 May 1991 (hme):
*        Original version (REPORT).
*     03 May 1991 (hme):
*        Rearrange MASK elements so that first MSKDIM/2 are interval
*        starts, second MSKDIM/2 are interval ends.
*        Report only used coefficients.
*     08 May 1991 (hme):
*        Use FWHM instead of sigma.
*     19 Jun 1991 (hme):
*        Get SIGMA from calling routine, but report FWHM.
*        Get covariance matrix from calling routine and report it
*        as parameter errors and correlation matrix.
*        Report line integral (and its error).
*     22 Jul 1991 (hme):
*        FITDIM. Report row coordinates.
*     30 Oct 1991 (hme):
*        Order of free parameters in SPFHSS had been changed. This makes
*        calculation of line integral errors difficult, set them 0 now.
*     01 Nov 1991 (hme):
*        Write also the covariance matrix.
*     20 Nov 1991 (hme):
*        Fix the case where more numbers are written than fit into the
*        message string. Calculate proper line integral variances.
*        Scrap the covariance matrix. Beautify file output.
*     03 Apr 1992 (hme):
*        Adapt SPABS from SPFREP. Reduce to reporting Chebyshev series.
*        Report only things that have meaning, e.g. no masks before
*        masking done.
*     27 Jan 1995 (hme):
*        Renamed from SPABS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL FITTED
      LOGICAL FILE
      INTEGER FU
      CHARACTER * ( * ) IN
      LOGICAL VARUSE
      INTEGER NELM
      INTEGER MSKDIM
      INTEGER MSKUSE
      INTEGER MSKELM
      REAL MASK( MSKDIM )
      REAL XMIN
      REAL XMAX
      INTEGER MAXPAR
      INTEGER ORDER
      REAL COEFF( MAXPAR )
      REAL CHISQR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) STRING  ! Message string
      INTEGER I, J               ! Loop indices

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats: 1xx Logfile output.
*  Formats: 2xx Screen output.
*  Formats: x0x General.
 101  FORMAT ( /
     :   ' SPECDRE: FitPoly results (v. 1.1).' )
 102  FORMAT ( ' Input NDF:  ', A )
 107  FORMAT ( ' Varuse parameter:             ', L1 )
 108  FORMAT ( ' No. of data points:       ', I5 )
 201  FORMAT ( ' SPECDRE: FitPoly results (v. 1.1).' )

*  Formats: x1x Mask.
 111  FORMAT ( ' No. of mask intervals:        ', I1 )
 112  FORMAT ( ' No. of valid data points: ', I5 )
 113  FORMAT ( ' Mask:' )
 114  FORMAT ( ' ', 2G15.7E2 )
 115  FORMAT ( ' Actual abscissa range:' )

*  Formats: x4x Polynomial.
 141  FORMAT ( ' Order of polynomial: ', I1 )
 142  FORMAT ( ' Coefficients of polynomial:' )
 143  FORMAT ( ' ', 8G15.7E2 )
 243  FORMAT ( ' ', 4G15.7E2 )

*  Formats: x5x Chi-squared or rms.
 151  FORMAT ( ' Degrees of freedom:       ', I5 )
 152  FORMAT ( ' Chi-squared:              ', G15.7E2 )
 153  FORMAT ( ' rms:                      ', G15.7E2 )
 154  FORMAT ( ' rms is undefined.' )

*  Logfile output. Do not mix *-formats with proper formats.
      IF ( FILE ) THEN

*     General: Title, files.
         WRITE( FU, 101 )
         WRITE( FU, 102 ) IN(:MIN(LEN(IN), 115))

*     General: Use of variance, total number of data points.
         WRITE ( FU, 107 ) VARUSE
         WRITE ( FU, 108 ) NELM

*     Mask.
         IF ( MSKUSE .GT. 0 ) THEN
            WRITE( FU, 111 ) MSKUSE
            WRITE( FU, 112 ) MSKELM
            WRITE( FU, 113 )
            WRITE( FU, 114 )
     :         ( MASK(I), MASK(I+MSKDIM/2), I = 1, MSKUSE )
         END IF

*     Abscissa range.
         WRITE( FU, 115 )
         WRITE( FU, 114 ) XMIN, XMAX

*     Fit result.
         IF ( FITTED ) THEN

*        Polynomial.
            WRITE( FU, 141 ) ORDER
            WRITE( FU, 142 )
            WRITE( FU, 143 ) ( COEFF(I), I = 1, ORDER+1 )

*        R.m.s. or chi squared.
            WRITE( FU, 151 ) MSKELM - ORDER - 1
            IF ( VARUSE ) THEN
               WRITE( FU, 152 ) CHISQR
            ELSE IF ( MSKELM - ORDER - 1 .GT. 0 ) THEN
               WRITE( FU, 153 ) SQRT( CHISQR / ( MSKELM - ORDER - 1 ) )
            ELSE
               WRITE( FU, 154 )
            END IF
         END IF

*  Screen output.
      ELSE

*     General: Title, files.
         CALL MSG_OUT ( 'SPD_WZFC_OUT', ' ', STATUS )
         WRITE( STRING, 102 ) IN(:MIN(LEN(IN), 65))
         CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING , STATUS )

*     General: Use of variance, total number of data points.
         WRITE ( STRING, 107 ) VARUSE
         CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
         WRITE ( STRING, 108 ) NELM
         CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )

*     Mask.
         IF ( MSKUSE .GT. 0 ) THEN
            WRITE( STRING, 111 ) MSKUSE
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            WRITE( STRING, 112 ) MSKELM
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            WRITE( STRING, 113 )
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            DO 1 I = 1, MSKUSE
               WRITE( STRING, 114 ) MASK(I), MASK(I+MSKDIM/2)
               CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
 1          CONTINUE
         END IF

*     Abscissa range.
         WRITE( STRING, 115 )
         CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
         WRITE( STRING, 114 ) XMIN, XMAX
         CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )

*     Fit result.
         IF ( FITTED ) THEN

*        Polynomial.
            WRITE( STRING, 141 ) ORDER
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            WRITE( STRING, 142 )
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            DO 2 I = 1, ORDER+1, 4
               WRITE( STRING, 243 )
     :            ( COEFF(J), J = I, MIN( I+3, ORDER+1 ) )
               CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
 2          CONTINUE

*        R.m.s. or chi squared.
            WRITE( STRING, 151 ) MSKELM - ORDER - 1
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
            IF ( VARUSE ) THEN
               WRITE( STRING, 152 ) CHISQR
            ELSE IF ( MSKELM - ORDER - 1 .GT. 0 ) THEN
               WRITE( STRING, 153 )
     :            SQRT( CHISQR / ( MSKELM - ORDER - 1 ) )
            ELSE
               WRITE( STRING, 154 )
            END IF
            CALL MSG_OUT ( 'SPD_WZFC_OUT', STRING, STATUS )
         END IF

*     Final blank line.
         CALL MSG_OUT ( 'SPD_WZFC_OUT', ' ', STATUS )
      END IF

*  Return.
      END
