      SUBROUTINE SPD_WAAJ( FILENO, INFO, IN1, IN2, IN3,
     :   NS, A0, A1, A2, DA0, DA1, DA2, SIGMA, CHISQR, STATUS )
*+
*  Name:
*     SPD_WAAJ

*  Purpose:
*     Output SPD_WAAH results.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAJ( FILENO, INFO, IN1, IN2, IN3,
*        NS, A0, A1, A2, DA0, DA1, DA2, SIGMA, CHISQR, STATUS )

*  Description:
*     This routine writes the results from a two-parameter linear fit
*     to the terminal and to an ASCII file.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number of ASCII file. If negative, output only to
*        screen.
*     INFO = LOGICAL (Given)
*        True if screen output requested.
*     IN1 = CHARACTER * ( * ) (Given)
*        Name of first input file.
*     IN2 = CHARACTER * ( * ) (Given)
*        Name of second input file.
*     IN3 = CHARACTER * ( * ) (Given)
*        Name of third input file.
*     NS = INTEGER (Given)
*        Number of valid points in the sample. This differs from NX if
*        VARUSE and a variance=0 was found. It also differs from NX if
*        BADCHK and bad values were found.
*     A0 = REAL (Given)
*        The fitted ordinate interception value.
*     A1 = REAL (Given)
*        The fitted slope with respect to X1.
*     A2 = REAL (Given)
*        The fitted slope with respect to X2.
*     DA0 = REAL (Given)
*        The error of A0.
*     DA1 = REAL (Given)
*        The error of A1.
*     DA2 = REAL (Given)
*        The error of A2.
*     SIGMA = REAL (Given)
*        Estimated variance. (Unweighted average deviation from fit.)
*     CHISQR = REAL (Given)
*        The normalised chi-squared. Set to VAL__BADR if .NOT.VARUSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1991 (hme):
*        Original version.
*     27 Nov 1991 (hme):
*        INFO keyword. Change message reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FILENO
      LOGICAL INFO
      CHARACTER * ( * ) IN1
      CHARACTER * ( * ) IN2
      CHARACTER * ( * ) IN3
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
      CHARACTER * ( 79 ) STRNG   ! Line to be written

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats used.
 101  FORMAT('      N = ',I10,24X,'Points used')
 102  FORMAT('     A0 = ',G14.7E2,' +-',G14.7E2)
 103  FORMAT('     A1 = ',G14.7E2,' +-',G14.7E2,'  =  1 / ',G14.7E2)
 104  FORMAT('     A2 = ',G14.7E2,' +-',G14.7E2,'  =  1 / ',G14.7E2)
 105  FORMAT('      S = ',G14.7E2,20X,'Standard deviation')
 106  FORMAT(' chi**2 = ',G14.7E2,20X,'Normalised chi-squared')

*  Write to screen.
      IF ( INFO ) THEN
         CALL MSG_SETC( 'MESS', ' ' )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         CALL MSG_SETC( 'MESS',
     :      ' Two-parameter linear fit' //
     :      '   y = A0 + A1 * x1 + A2 * x2   from files' )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         CALL MSG_SETC( 'MESS', ' ' )
         CALL MSG_SETC( 'MESS', IN1 )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         CALL MSG_SETC( 'MESS', ' ' )
         CALL MSG_SETC( 'MESS', IN2 )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         CALL MSG_SETC( 'MESS', ' ' )
         CALL MSG_SETC( 'MESS', IN3 )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 101 ) NS
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 102 ) A0, DA0
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 103 ) A1, DA1, 1/A1
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 104 ) A2, DA2, 1/A2
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 105 ) SIGMA
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
         WRITE ( STRNG, 106 ) CHISQR
         CALL MSG_SETC( 'MESS', STRNG )
         CALL MSG_OUT( 'CORREL_REPORT', '^MESS', STATUS )
      END IF

*  Write to file.
      IF ( FILENO .GT. 0 ) THEN
         WRITE ( FILENO, '(A1)' ) ' '
         STRNG =
     :      ' Two-parameter linear fit' //
     :      '   y = A0 + A1 * x1 + A2 * x2   from files'
         WRITE ( FILENO, '(A79)' ) STRNG
         STRNG = ' '//IN1
         WRITE ( FILENO, '(A79)' ) STRNG
         STRNG = ' '//IN2
         WRITE ( FILENO, '(A79)' ) STRNG
         STRNG = ' '//IN3
         WRITE ( FILENO, '(A79)' ) STRNG
         WRITE ( FILENO, 101 ) NS
         WRITE ( FILENO, 102 ) A0, DA0
         WRITE ( FILENO, 103 ) A1, DA1, 1/A1
         WRITE ( FILENO, 104 ) A2, DA2, 1/A2
         WRITE ( FILENO, 105 ) SIGMA
         WRITE ( FILENO, 106 ) CHISQR
      END IF

      END
