      SUBROUTINE CLLOG( STATUS )
*+
*  Name:
*     CLLOG

*  Purpose:
*     Take the logarithm of a column.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CLLOG( STATUS )

*  Description:
*     Take the base 10 logarithm of a data column.  This application
*     should be used to take the logarithm of the data columns in
*     preference to doing it with CCMATH, because it can deal with the
*     associated error values consistently. It automatically adds the
*     "L" option to the PONGO_XOPT or PONGO_YOPT global parameters, as
*     appropriate.

*  ADAM Parameters:
*    ACTION = _CHAR (Read)
*       The data column to transform. It should be one of the
*       following:
*
*          - "X" -- XCOL
*          - "Y" -- YCOL
*          - "Z" -- ZCOL
*
*       [The value will be prompted for.]
*    XOPT = _CHAR (Write)
*       The PGPLOT X-axis options string.  The global parameter will be
*       updated to include the PGPLOT "L" axis option at the start.
*       This option means that logarithmic style axis labels and tick
*       marks will be plotted. The READF command will automatically
*       remove any "L" characters at the start of this string since it
*       assumes that they have been put there by CLLOG, and fresh data
*       read in will not be logarithmic. If data are naturally
*       logarithmic, the "L" should be placed other than at the start
*       of the string to make an "L" option that will not be modified
*       by PONGO.
*
*       It is not intended that this parameter be set by the user when
*       CLLOG is executed.
*
*       The value will be written to the global parameter PONGO_XOPT.
*    YOPT = _CHAR (Write)
*       The PGPLOT Y-axis options string. Its action is similar to the
*       XOPT parameter.
*       It is not intended that this parameter be set by the user when
*       CLLOG is executed.
*
*       The value will be written to the global parameter PONGO_YOPT.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     BKM: B.K. McIlwrath (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Changed character prefixing to use CHR_PREFX. Removed
*        unnecessary double precision extension to constant (ZDATA).
*        Removed reference to CHR_EQUAL (unused).
*     6-JUN-1994 (PDRAPER):
*        Changed DCV_PAR to PRM_PAR.
*     4-SEP-1996 (BKM):
*        Changed routine name from CLOG to CLLOG to avoid a clash with
*        Linux g77 intrinsic function of the same name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action to be taken
      CHARACTER * ( 20 ) AOPT    ! Axis option string

      INTEGER IAT                ! Position in string
      INTEGER IDAT               ! Counter
      INTEGER LENACT             ! Length of ACTION

*  Internal References:
      INTEGER CHR_LEN

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      LENACT = MAX( 1, CHR_LEN( ACTION ) )
      CALL CHR_UCASE( ACTION( : LENACT ) )

      IF ( ACTION( : LENACT ) .EQ. 'X' ) THEN
         CALL PAR_GET0C( 'XOPT', AOPT, STATUS )
         CALL CHR_UCASE( AOPT )

         IF ( ( XMIN .GT. 0.0 ) .OR. ( XMAX .GT. 0.0 ) ) THEN

            IF ( XMIN .LE. 0.0 ) CALL MSG_OUT( ' ',
     :         'There are negative values: LOG set to -999 for these.',
     :         STATUS )
            XMIN = VAL__MAXR

            DO IDAT = 1, NDAT
               IF ( XDATA( IDAT ) .GT. 0.0D+00 ) THEN
                  XDATA( IDAT ) = LOG10( XDATA( IDAT ) )
                  XMIN = MIN( XMIN, REAL( XDATA( IDAT ) ) )
               ELSE
                  XDATA( IDAT ) = -999.0D+00
               END IF
            END DO

            LXLOG = .TRUE.
            XMAX = LOG10( XMAX )

            IF ( INDEX( AOPT, 'L' ) .EQ. 0 ) THEN
               CALL CHR_PREFX( 'L', AOPT, IAT )
               CALL CHR_UCASE( AOPT )
               CALL PAR_PUT0C( 'XOPT', AOPT, STATUS )
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLOG_UNDEF', 'All X values are negative.',
     :                    STATUS )
         END IF
      ELSE IF ( ACTION( : LENACT ) .EQ. 'Y' ) THEN
         CALL PAR_GET0C( 'YOPT', AOPT, STATUS )
         CALL CHR_UCASE( AOPT )

         IF ( ( YMIN .GT. 0.0 ) .OR. ( YMAX .GT. 0.0 ) ) THEN
            IF ( YMIN .LE. 0.0 ) CALL MSG_OUT( ' ',
     :         'There are negative values: LOG set to -999 for these.',
     :         STATUS )
            YMIN = VAL__MAXR

            DO IDAT = 1, NDAT

               IF ( YDATA( IDAT ) .GT. 0.0D+00 ) THEN
                  YDATA( IDAT ) = LOG10( YDATA( IDAT ) )
                  YMIN = MIN( YMIN, REAL( YDATA( IDAT ) ) )
               ELSE
                  YDATA( IDAT ) = -999.0D+00
               END IF
            END DO

            LYLOG = .TRUE.
            YMAX = LOG10( YMAX )

            IF ( INDEX( AOPT, 'L' ) .EQ. 0 ) THEN
               CALL CHR_PREFX( 'L', AOPT, IAT )
               CALL CHR_UCASE( AOPT )
               CALL PAR_PUT0C( 'YOPT', AOPT, STATUS )
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CLOG_UNDEF', 'All Y values are negative.',
     :                    STATUS )
         END IF
      ELSE IF ( ACTION( : LENACT ) .EQ. 'Z' ) THEN

         DO IDAT = 1, NDAT

            IF ( ZDATA( IDAT ) .GT. 0.0D+00 ) THEN
               ZDATA( IDAT ) = LOG10( ZDATA( IDAT ) )
            ELSE
               ZDATA( IDAT ) = -999.0
            END IF
         END DO
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'CLOG_END',
     :                              'CLOG: Unable to take the ' //
     :                              'logarithm of the column.', STATUS )

      END
* $Id$
