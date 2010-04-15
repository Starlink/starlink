      SUBROUTINE SPD_WZGC( LOGUSE, FILENO, MSKDIM, FITPAR, FITDIM,
     :   IN, NELM,
     :   VARUSE, MSKUSE, MSKELM, XMIN, XMAX, MASK,
     :   NCOMP, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
     :   COVAR, CHISQR, STATUS )
*+
*  Name:
*     SPD_WZGC

*  Purpose:
*     Report FITBB settings/results to screen or file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZGC( LOGUSE, FILENO, MSKDIM, FITPAR, FITDIM,
*        IN, NELM,
*        VARUSE, MSKUSE, MSKELM, XMIN, XMAX, MASK,
*        NCOMP, CFLAGS, PFLAGS, SFLAGS, THETA, ALPHA, TEMPE,
*        COVAR, CHISQR, STATUS )

*  Description:
*     Report current settings of FITBB to screen or log file.
*     The covariance matrix is reported in
*     the form of a correlation matrix and of errors to the free
*     parameters:
*                               covar(i,j)
*        corr(i,j) = -------------------------------
*                     SQRT( covar(i,i) covar(j,j) )
*
*        sigma(i) = SQRT( covar(i,i) )         if VARUSE
*
*        sigma(i) = SQRT( covar(i,i) ) * rms   if .NOT.VARUSE

*  Arguments:
*     LOGUSE = LOGICAL (Given)
*        If true, report goes to file with unit number FILENO.
*        If false, report goes to screen.
*     FILENO = INTEGER (Given)
*        File unit number.
*     MSKDIM = INTEGER (Given)
*        Size of MASK array. Must be even.
*     FITPAR = INTEGER (Given)
*        Number of free parameters.
*     FITDIM = INTEGER (Given)
*        max( 1, FITPAR ). Used for size of arrays.
*     IN = CHARACTER * ( * ) (Given)
*        The input file name.
*     NELM = INTEGER (Given)
*        The number of data points.
*     VARUSE = LOGICAL (Given)
*        True if errors available and to be used.
*     MSKUSE = INTEGER (Given)
*        Number of valid mask intervals.
*     MSKELM = INTEGER (Given)
*        Number of x values that lie within the mask.
*     XMIN = REAL (Given)
*        The minimum x value of the masked data.
*     XMAX = REAL (Given)
*        The maximum x value of the masked data.
*     MASK( MSKDIM ) = REAL (Given)
*        The mask is put together from up to MSKDIM/2 intervals:
*           complex mask = [MASK(1);MASK(1+MSKDIM/2)]
*                        U [MASK(2);MASK(2+MSKDIM/2)]
*                        U ...
*                        U [MASK(MSKUSE);MASK(MSKUSE+MSKDIM/2)].
*     NCOMP = INTEGER (Given)
*        The value of NCOMP is the number of diluted Planck components.
*        If given as zero, no components will be reported.
*     CFLAGS( MAXBBS ) = INTEGER (Given)
*        Theta fit flags.
*     PFLAGS( MAXBBS ) = INTEGER (Given)
*        Alpha fit flags.
*     SFLAGS( MAXBBS ) = INTEGER (Given)
*        Temperature fit flags.
*     THETA( MAXBBS ) = REAL (Given)
*        Vertical scaling constant for each component.
*     ALPHA( MAXBBS ) = REAL (Given)
*        Emissivity exponent for each component.
*     TEMPE ( MAXBBS ) = REAL (Given)
*        Colour temperature for each component.
*     COVAR( FITDIM, FITDIM ) = DOUBLE PRECISION (Given)
*        Covariance matrix. If the fit was by minimising r.m.s. (i.e.
*        VARUSE is false), then the covariances are actually this matrix
*        times the square of r.m.s.
*     CHISQR = REAL (Given)
*        The normalised chi-squared or the square of r.m.s. depending on
*        whether errors are available or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Jan 1993 (hme):
*        Adapt from SPAAM.
*     27 Jan 1995 (hme):
*        Renamed from SPACP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Constants:
      INTEGER MAXBBS             ! Maximum no. of Gauss components
      PARAMETER ( MAXBBS = 6 )

*  Arguments Given:
      LOGICAL LOGUSE
      INTEGER FILENO
      INTEGER MSKDIM
      INTEGER FITPAR
      INTEGER FITDIM
      CHARACTER * ( * ) IN
      INTEGER NELM
      LOGICAL VARUSE
      INTEGER MSKUSE
      INTEGER MSKELM
      REAL XMIN
      REAL XMAX
      REAL MASK( MSKDIM )
      INTEGER NCOMP
      INTEGER CFLAGS( MAXBBS )
      INTEGER PFLAGS( MAXBBS )
      INTEGER SFLAGS( MAXBBS )
      REAL THETA( MAXBBS )
      REAL ALPHA( MAXBBS )
      REAL TEMPE( MAXBBS )
      DOUBLE PRECISION COVAR( FITDIM, FITDIM )
      REAL CHISQR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) STRING  ! Message string
      INTEGER I                  ! Counters for components
      INTEGER J, J1, J2          ! Counters for parameters
      REAL VARIAN( 3*MAXBBS )    ! Variances of the parameters
      REAL VARSCL                ! CHISQR or 1.
      CHARACTER * ( 10 )
     :   PARNAM( 3*MAXBBS )      ! Names of the free parameters

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats: 1xx Logfile output.
*  Formats: 2xx Screen output.
*  Formats: x0x General.
 101  FORMAT ( /
     :   ' SPECDRE: FitBB results (v. 0.7).' )
 102  FORMAT ( ' Input NDF:  ', A )
 107  FORMAT ( ' Varuse parameter:             ', L1 )
 108  FORMAT ( ' No. of data points:       ', I5 )
 201  FORMAT ( ' SPECDRE: FitBB results (v. 0.7).' )

*  Formats: x1x Mask.
 111  FORMAT ( ' No. of mask intervals:        ', I1 )
 112  FORMAT ( ' No. of valid data points: ', I5 )
 113  FORMAT ( ' Mask:' )
 114  FORMAT ( ' ', 2G15.7E2 )
 115  FORMAT ( ' Actual abscissa range:' )

*  Formats: x2x Plancks.
 122  FORMAT ( ' No. of diluted Planck comps:  ', I1 )
 123  FORMAT ( ' Fit flags:')
 124  FORMAT ( ' #     Theta   alpha   log10(T)' )
 125  FORMAT ( ' ', I1, 3I8 )
 126  FORMAT ( ' List of free parameters:' )
 127  FORMAT ( ' ', I2, ' ', A10 )
 128  FORMAT ( ' Diluted Planck components:' )
 129  FORMAT ( ' #   Theta               (+-)',
     :   '      alpha               (+-)',
     :   '      log10(T)            (+-)' )
 229  FORMAT ( '  #    Theta          alpha      ',
     :   '     log10(T)' )

*  Formats: x3x Plancks.
 130  FORMAT ( ' ', I1, ' ', 6G15.7E2 )
 230  FORMAT ( '  ', I1, '  ', 3G15.7E2 )
 231  FORMAT ( ' +/- ', 3G15.7E2 )
 132  FORMAT ( ' Correlation between free parameters:' )
 133  FORMAT ( ' ', 18F6.2 )
 233  FORMAT ( ' ', 12F6.2 )
 234  FORMAT ( '    ', 6F6.2 )

*  Formats: x5x Chi-squared or rms.
 151  FORMAT ( ' Degrees of freedom:       ', I5 )
 152  FORMAT ( ' Chi-squared:              ', G15.7E2 )
 153  FORMAT ( ' rms:                      ', G15.7E2 )
 154  FORMAT ( ' rms is undefined.' )

*  This part only if there are Plancks to be reported.
      IF ( NCOMP .GT. 0 ) THEN

*     In case of no errors available the covariance matrix as given has
*     to be multiplied by rms**2 to contain the variances as diagonal
*     elements and the covariances off the diagonal.
*     This scaling factor happens to be in the variable CHISQR.
         VARSCL = CHISQR
         IF ( VARUSE ) VARSCL = 1.

*     J counts free parameters, I counts components. The relation
*     between the two is not simple because of fixed and tied
*     parameters.
         J = 0

*     Variance of I-th theta.
         DO 1 I = 1, NCOMP
            IF ( CFLAGS(I) .EQ. 0 ) THEN
               J = J+1
               VARIAN(3*I-2) = REAL(COVAR(J,J)) * VARSCL
               WRITE( PARNAM(J), '('' Theta #'',I1,'' '')' ) I
            ELSE IF ( CFLAGS(I) .EQ. I ) THEN
               VARIAN(3*I-2) = 0.
            ELSE
               VARIAN(3*I-2) = VARIAN(3*CFLAGS(I)-2)
            END IF
 1       CONTINUE

*     Variance of I-th alpha.
         DO 2 I = 1, NCOMP
            IF ( PFLAGS(I) .EQ. 0 ) THEN
               J = J+1
               VARIAN(3*I-1) = REAL(COVAR(J,J)) * VARSCL
               WRITE( PARNAM(J), '('' alpha #'',I1,'' '')' ) I
            ELSE IF ( PFLAGS(I) .EQ. I ) THEN
               VARIAN(3*I-1) = 0.
            ELSE
               VARIAN(3*I-1) = VARIAN(3*PFLAGS(I)-1)
     :            * ( ALPHA(I) / ALPHA(PFLAGS(I)) ) ** 2
            END IF
 2       CONTINUE

*     Variance of I-th temperature.
         DO 3 I = 1, NCOMP
            IF ( SFLAGS(I) .EQ. 0 ) THEN
               J = J+1
               VARIAN(3*I) = REAL(COVAR(J,J)) * VARSCL
               WRITE( PARNAM(J), '('' lg(T) #'',I1,'' '')' ) I
            ELSE IF ( SFLAGS(I) .EQ. I ) THEN
               VARIAN(3*I) = 0.
            ELSE
               VARIAN(3*I) = VARIAN(3*SFLAGS(I))
     :            * ( TEMPE(I) / TEMPE(SFLAGS(I)) ) ** 2
            END IF
 3       CONTINUE
      END IF

*  Logfile output. Do not mix *-formats with proper formats.
      IF ( LOGUSE ) THEN

*     General: Title, files, coordinates.
         WRITE( FILENO, 101 )
         WRITE( FILENO, 102 ) IN(:MIN(LEN(IN), 115))

*     General: use of variance, total number of data points.
         WRITE( FILENO, 107 ) VARUSE
         WRITE( FILENO, 108 ) NELM

*     Mask.
         WRITE( FILENO, 111 ) MSKUSE
         WRITE( FILENO, 112 ) MSKELM
         WRITE( FILENO, 113 )
         WRITE( FILENO, 114 )
     :      ( MASK(I), MASK(I+MSKDIM/2), I = 1, MSKUSE )
         WRITE( FILENO, 115 )
         WRITE( FILENO, 114 ) XMIN, XMAX

*     This part only if there are components to be reported.
         IF ( NCOMP .GT. 0 ) THEN

*        No. of comp's, flags.
            WRITE( FILENO, 122 ) NCOMP
            WRITE( FILENO, 123 )
            WRITE( FILENO, 124 )
            WRITE( FILENO, 125 )
     :         ( I, CFLAGS(I), PFLAGS(I), SFLAGS(I), I = 1, NCOMP )
            WRITE( FILENO, 126 )
            WRITE( FILENO, 127 ) ( J, PARNAM(J)(:10), J = 1, FITPAR )

*        Fit results.
            WRITE( FILENO, 128 )
            WRITE( FILENO, 129 )
            WRITE( FILENO, 130 )
     :         ( I, THETA(I), SQRT(VARIAN(3*I-2)),
     :              ALPHA(I), SQRT(VARIAN(3*I-1)),
     :              TEMPE(I), SQRT(VARIAN(3*I)),
     :           I = 1, NCOMP )

*        Correlation matrix if it exists.
            IF ( COVAR(1,1) .NE. 0D0 ) THEN
               WRITE( FILENO, 132 )
               DO 6 J2 = 1, FITPAR
                  WRITE( FILENO, 133 ) ( COVAR(J1,J2)
     :               / SQRT( COVAR(J1,J1) ) / SQRT( COVAR(J2,J2) ),
     :               J1 = 1, FITPAR )
 6             CONTINUE
            END IF

*        R.m.s. or chi squared.
            WRITE( FILENO, 151 ) MSKELM - FITPAR
            IF ( VARUSE ) THEN
               WRITE( FILENO, 152 ) CHISQR * ( MSKELM - FITPAR )
            ELSE IF ( MSKELM - FITPAR .GT. 0 ) THEN
               WRITE( FILENO, 153 ) SQRT(CHISQR)
            ELSE
               WRITE( FILENO, 154 )
            END IF
         END IF

*  Screen output.
      ELSE

*     General: Title, files.
         CALL MSG_SETC( 'MESS', ' ' )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         WRITE( STRING, 102 )     IN(:MIN(LEN(IN), 65))
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS' , STATUS )

*     General: use of variance, total number of data points.
         WRITE ( STRING, 107 ) VARUSE
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         WRITE ( STRING, 108 ) NELM
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )

*     Mask.
         WRITE( STRING, 111 ) MSKUSE
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         WRITE( STRING, 112 ) MSKELM
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         WRITE( STRING, 113 )
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         DO 7 I = 1, MSKUSE
            WRITE( STRING, 114 ) MASK(I), MASK(I+MSKDIM/2)
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
 7       CONTINUE
         WRITE( STRING, 115 )
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         WRITE( STRING, 114 ) XMIN, XMAX
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )

*     This part only if there are components to be reported.
         IF ( NCOMP .GT. 0 ) THEN

*        No. of comp's.
            WRITE( STRING, 122 ) NCOMP
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )

*        Flags.
            WRITE( STRING, 123 )
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            WRITE( STRING, 124 )
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            DO 8 I = 1, NCOMP
               WRITE( STRING, 125 ) I, CFLAGS(I), PFLAGS(I), SFLAGS(I)
               CALL MSG_SETC( 'MESS', STRING )
               CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
 8          CONTINUE
            WRITE( STRING, 126 )
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            DO 9 J = 1, FITPAR
               WRITE( STRING, 127 ) J, PARNAM(J)(:10)
               CALL MSG_SETC( 'MESS', STRING )
               CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
 9          CONTINUE

*        Fit results.
            WRITE( STRING, 128 )
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            WRITE( STRING, 229 )
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            DO 10 I = 1, NCOMP
               WRITE( STRING, 230 ) I, THETA(I), ALPHA(I), TEMPE(I)
               CALL MSG_SETC( 'MESS', STRING )
               CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
               WRITE( STRING, 231 ) SQRT(VARIAN(3*I-2)),
     :            SQRT(VARIAN(3*I-1)), SQRT(VARIAN(3*I))
               CALL MSG_SETC( 'MESS', STRING )
               CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
 10         CONTINUE

*        Correlation matrix if it exists.
            IF ( COVAR(1,1) .NE. 0D0 ) THEN
               WRITE( STRING, 132 )
               CALL MSG_SETC( 'MESS', STRING )
               CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
               DO 11 J2 = 1, FITPAR
                  WRITE( STRING, 233 )    ( COVAR(J1,J2)
     :                  / SQRT( COVAR(J1,J1) ) / SQRT( COVAR(J2,J2) ),
     :                  J1 = 1, MIN(12,FITPAR) )
                  CALL MSG_SETC( 'MESS', STRING )
                  CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
                  IF ( FITPAR .GT. 12 ) THEN
                     WRITE( STRING, 234 ) ( COVAR(J1,J2)
     :                  / SQRT( COVAR(J1,J1) ) / SQRT( COVAR(J2,J2) ),
     :                  J1 = 13, FITPAR )
                     STRING = '   ' // STRING(:77)
                     CALL MSG_SETC( 'MESS', STRING )
                     CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
                  END IF
 11            CONTINUE
            END IF

*        R.m.s. or chi squared.
            WRITE( STRING, 151 ) MSKELM - FITPAR
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
            IF ( VARUSE ) THEN
               WRITE( STRING, 152 ) CHISQR * ( MSKELM - FITPAR )
            ELSE IF ( MSKELM - FITPAR .GT. 0 ) THEN
               WRITE( STRING, 153 ) SQRT(CHISQR)
            ELSE
               WRITE( STRING, 154 )
            END IF
            CALL MSG_SETC( 'MESS', STRING )
            CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
         END IF

*     Final blank line.
         STRING = ' '
         CALL MSG_SETC( 'MESS', STRING )
         CALL MSG_OUT ( 'FITBB_REPORT', '^MESS', STATUS )
      END IF

      END
