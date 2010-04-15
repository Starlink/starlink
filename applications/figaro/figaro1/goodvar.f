      SUBROUTINE GOODVAR( STATUS )
*+
*  Name:
*     GOODVAR

*  Purpose:
*     Replace negative, zero and bad variance values.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GOODVAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine checks the variance component of an NDF for values that
*     are bad, negative, or zero and replaces them by values specified
*     by the user. The specified value can be the null value ("!") which
*     is translated into the bad value.

*  Usage:
*     goodvar in out bad neg zero

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF.
*     OUT = NDF (Read)
*        The output NDF.
*     BAD = _REAL (Read)
*        The value which replaces bad values. Enter an exclamation mark
*        to keep bad values.
*        Bad values in VARIANCE or ERRORS are not allowed by Figaro. If
*        DSA has to convert these arrays while mapping them, floating
*        overflows or square roots of negative numbers may occur, and
*        the application is liable to crash. [!]
*     NEG = _REAL (Read)
*        The value which replaces negative values. Enter an exclamation
*        mark to replace negative values with the bad value. Negative
*        errors or variances are nonsense. Negative variances often will
*        cause an application to crash because it takes the square root
*        to calculate the error. [!]
*     ZERO = _REAL (Read)
*        The value which replaces zeroes. Enter an exclamation mark
*        to replace zeroes with the bad value.
*        Errors of zero sometimes are reasonable or necessary for error
*        propagation. In other instances they cause problems, because
*        statistical weights often are the reciprocal of the variance.
*        [!]

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1991 (HME):
*        Original version.
*     27-NOV-1991 (HME):
*        Proper status annulling.
*     15-DEC-1991 (HME):
*        Suppress Starlink error messages arising from DSA-calls.
*     05-JUN-1992 (HME):
*        Port to NDF and Unix.
*     07-OCT-1992 (HME):
*        Fix the bug, i.e. use types _REAL or _DOUBLE.
*        Move the routine to Figaro.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! Status returned by PAR_
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
*     INTEGER MAXDIM             ! Maximum no. of axes
*     PARAMETER ( MAXDIM = 10 )

*  Local Variables:
      REAL BAD
      REAL NEG
      REAL ZERO
      LOGICAL BADBAD             ! True if bad values to be kept
      LOGICAL NEGBAD             ! True if negat. values to be made bad
      LOGICAL ZERBAD             ! True if zeroes to be made bad
      INTEGER NDF( 2 )           ! NDF identifiers
      INTEGER NELM               ! NDF size
      INTEGER PNTR               ! Array pointer
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Data type to be used

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Access files.
*  OUT is a copy of IN, we will work on it in situ.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_PROP( NDF(1), 'UNITS,DATA,VARIANCE,QUALITY,AXIS',
     :   'OUT', NDF(2), STATUS )

*  Access array in question.
      CALL NDF_TYPE( NDF(2), 'VARIANCE', DTYPE, STATUS )
      IF ( DTYPE .NE. '_DOUBLE' ) DTYPE = '_REAL'
      CALL NDF_MAP(  NDF(2), 'VARIANCE', DTYPE, 'UPDATE', PNTR, NELM,
     :   STATUS )

*  Get other parameters.
      BADBAD = .FALSE.
      NEGBAD = .FALSE.
      ZERBAD = .FALSE.
      CALL PAR_GET0R( 'BAD', BAD, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         BADBAD = .TRUE.
         BAD = 0.
      END IF
      CALL PAR_GET0R( 'NEG', NEG, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NEGBAD = .TRUE.
         NEG = 0.
      END IF
      CALL PAR_GET0R( 'ZERO', ZERO, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ZERBAD = .TRUE.
         ZERO = 0.
      END IF

*  Do the action.
      IF ( DTYPE .EQ. 'DOUBLE' ) THEN
         CALL FIG_GVRDOD( BADBAD, NEGBAD, ZERBAD, NELM,
     :                    DBLE(BAD), DBLE(NEG), DBLE(ZERO),
     :                    %VAL( CNF_PVAL(PNTR) ), STATUS )
      ELSE
         CALL FIG_GVRDOR( BADBAD, NEGBAD, ZERBAD, NELM,
     :      BAD, NEG, ZERO, %VAL( CNF_PVAL(PNTR) ), STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      CALL NDF_END( STATUS )

      END
