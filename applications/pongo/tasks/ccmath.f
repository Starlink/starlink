      SUBROUTINE CCMATH( STATUS )
*+
*  Name:
*     CCMATH

*  Purpose:
*     Perform inter-column maths.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Perform inter-column maths (using TRANSFORM, SUN/61). The
*     expressions and functions recognised have Fortran types and
*     syntax.  Any construct that is legal in TRANSFORM is legal in
*     this subroutine, with the additional function INDEX for filling
*     the array with an increasing sequence of integers. See SUN/61 for
*     further details.
*
*     The names used for the data areas are as follows:
*
*        - "X" -- the XCOL data area,
*        - "Y" -- the YCOL data area,
*        - "Z" -- the ZCOL data area,
*        - "EX" -- the EXCOL error area,
*        - "EY" -- the EYCOL error area.

*  Usage:
*     ccmath

*  ADAM Parameters:
*     X = _CHAR (Read)
*        The transformation to perform on the contents of the XCOL data
*        area.
*
*        ["X" -- i.e. will cause the contents of the data area to remain
*        unchanged.]
*     Y = _CHAR (Read)
*        The transformation to perform on contents of the YCOL data
*        area.
*
*        ["Y" -- i.e. will cause the contents of the data area to remain
*        unchanged.]
*     Z = _CHAR (Read)
*        The transformation to perform on contents of the ZCOL data
*        area.
*
*        ["Z" -- i.e. will cause the contents of the data area to remain
*        unchanged.]
*     EX = _CHAR (Read)
*        The transformation to perform on contents of the EXCOL data
*        area.
*
*        ["EX" -- i.e. will cause the contents of the data area to
*        remain unchanged.]
*     EY = _CHAR (Read)
*        The transformation to perform on contents of the EYCOL data
*        area.
*
*        ["EY" -- i.e. will cause the contents of the data area to
*        remain unchanged.]

*  Examples:
*     ICL> CCMATH X=2*Y
*
*        will fill each element of the XCOL data area with twice the
*        corresponding element of the YCOL data area.
*

*  Notes:
*     - More than one array may be manipulated with a single command.
*     - The INDEX function cannot be combined with any other function.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Added DAT_PAR include file and changed string assignments to
*        use CHR_PREFX.
*     9-JUN-1994 (PDRAPER):
*        Tidied error reports from TRN.
*     24-APR-1997 (PDRAPER):
*        Fixed the INDEX section for Y (now works)!
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 132 ) FORWARD( 5 )
      CHARACTER * ( 20 ) INVERSE( 5 )
      CHARACTER * ( DAT__SZLOC ) LOCTR

      INTEGER IAT                ! Position in string
      INTEGER ID
      INTEGER J
      INTEGER LENFOR
      INTEGER NCIN
      INTEGER NCOUT

      REAL INDATA( NDATMAX, 5 )
      REAL RESULT( NDATMAX, 5 )

*  Internal References:
      INTEGER CHR_LEN            ! Length of character string

      DATA NCIN / 5 /
      DATA NCOUT / 5 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Put the values into the array (could be done by redeclaring the data
*  columns in PONGO).
      DO J = 1, NDAT
         INDATA( J, 1 ) = REAL( XDATA( J ) )
         INDATA( J, 2 ) = REAL( YDATA( J ) )
         INDATA( J, 3 ) = ERRX( J )
         INDATA( J, 4 ) = ERRY( J )
         INDATA( J, 5 ) = ZDATA( J )
      END DO

      INVERSE( 1 ) = 'X'
      INVERSE( 2 ) = 'Y'
      INVERSE( 3 ) = 'EX'
      INVERSE( 4 ) = 'EY'
      INVERSE( 5 ) = 'Z'

      CALL PAR_GET0C( 'X', FORWARD( 1 ), STATUS )
      LENFOR = CHR_LEN( FORWARD( 1 ) )
      CALL CHR_UCASE( FORWARD( 1 )( : LENFOR ) )

      IF ( FORWARD( 1 )( : LENFOR ) .EQ. 'INDEX' ) THEN

         DO J = 1, NDAT
            INDATA( J, 1 ) = J
         END DO

         FORWARD( 1 ) = 'NX=X'
      ELSE
         CALL CHR_PREFX( 'NX=', FORWARD( 1 ), IAT )
      END IF

      CALL PAR_GET0C( 'Y', FORWARD( 2 ), STATUS )
      LENFOR = CHR_LEN( FORWARD( 2 ) )
      CALL CHR_UCASE( FORWARD( 2 )( : LENFOR ) )

      IF ( FORWARD( 2 )( : LENFOR ) .EQ. 'INDEX' ) THEN

         DO J = 1, NDAT
            INDATA( J, 2 ) = J
         END DO

         FORWARD( 2 ) = 'NY=Y'
      ELSE
         CALL CHR_PREFX( 'NY=', FORWARD(2 ), IAT )
      END IF

      CALL PAR_GET0C( 'EX', FORWARD( 3 ), STATUS )
      LENFOR = CHR_LEN( FORWARD( 3 ) )
      CALL CHR_UCASE( FORWARD( 3 )( : LENFOR ) )
      CALL CHR_PREFX( 'NEX=', FORWARD( 3 ), IAT )

      CALL PAR_GET0C( 'EY', FORWARD( 4 ), STATUS )
      LENFOR = CHR_LEN( FORWARD( 4 ) )
      CALL CHR_UCASE( FORWARD( 4 )( : LENFOR ) )
      CALL CHR_PREFX( 'NEY=', FORWARD( 4 ), IAT )

      CALL PAR_GET0C( 'Z', FORWARD( 5 ), STATUS )
      LENFOR = CHR_LEN( FORWARD( 5 ) )
      CALL CHR_UCASE( FORWARD( 5 )( : LENFOR ) )
      CALL CHR_PREFX( 'NZ=', FORWARD( 5 ), IAT )

*  Trap any errors from TRN.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL TRN_NEW( NCIN, NCOUT, FORWARD, INVERSE,
     :                 '_REAL:', 'X -->f(X)', ' ', ' ', LOCTR, STATUS )
         CALL TRN_COMP( LOCTR, .TRUE., ID, STATUS )
         CALL TRN_TRNR( .FALSE., NDATMAX, NCIN, NDAT, INDATA, ID,
     :                  NDATMAX, NCOUT, RESULT, STATUS )
         CALL DAT_ANNUL( LOCTR, STATUS )
         CALL TRN_ANNUL( ID, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Failed to transform data using:' ,
     :                    STATUS )
            CALL MSG_SETC( 'F1', FORWARD( 1 ) )
            CALL ERR_REP( ' ', '  ^F1', STATUS )
            CALL MSG_SETC( 'F2', FORWARD( 2 ) )
            CALL ERR_REP( ' ', '  ^F2', STATUS )
            CALL MSG_SETC( 'F3', FORWARD( 3 ) )
            CALL ERR_REP( ' ', '  ^F3', STATUS )
            CALL MSG_SETC( 'F4', FORWARD( 4 ) )
            CALL ERR_REP( ' ', '  ^F4', STATUS )
            CALL MSG_SETC( 'F5', FORWARD( 5 ) )
            CALL ERR_REP( ' ', '  ^F5', STATUS )
         END IF
         CALL ERR_RLSE
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO J = 1, NDAT
            XDATA( J ) = DBLE( RESULT( J, 1 ) )
            YDATA( J ) = DBLE( RESULT( J, 2 ) )
            ERRX( J ) = RESULT( J, 3 )
            ERRY( J ) = RESULT( J, 4 )
            ZDATA( J ) = RESULT( J, 5 )
         END DO
      END IF

      CALL TRN_CLOSE( STATUS )

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'CCMATH_END',
     :                              'CCMATH: Unable to perform ' //
     :                              'inter-column maths.', STATUS )

      END
* $Id$
