      SUBROUTINE CHT_SETONE( STATUS )
*+
*  Name:
*     CHT_SETONE

*  Purpose:
*     Set a single CHART parameter

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_SETONE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task changes the value of a single CHART parameter in the
*     file CHARTPAR.DAT.

*  Usage:
*     SETONE PARAM VALUE

*  ADAM Parameters:
*     PARAM = _CHAR (Read)
*        The name of the CHART parameter
*        []
*     VALUE = _CHAR (Read)
*        The new value of the CHART parameter
*        []

*  Examples:
*     SETONE DEVICE XWINDOWS
*        Set the display device to use X-windows
*     SETONE MAGNITUD 8.0
*        Set the magnitude limit for catalogue searches to 8.0

*  Notes:
*     -  This routine has been extensively rewritten by PMA to ADAMize
*        it. It draws heavily on the KFH INTERIM version.

*  External Routines Used:
*     CHR:
*        CHR_LEN, CHR_UNAME
*     PAR:
*        PAR_CANCL, PAR_DEF0C, PAR_GET0C
*     ERR:
*        ERR_REP

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1991 (PMA):
*        Original ADAM version.
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_SETONE.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     7-MAY-1993 (AJJB):
*        Put in an IF statement so that only new values for parameters
*        other than filenames will be changed to uppercase, as UNIX is
*        case sensitive. Device names also are left in the case in which
*        they were entered.
*     12-MAY-1993 (AJJB):
*        Added another IF so that if parameters FIELDS and EXTRA are
*        set to TERMINAL or NONE, respectively, which are standard
*        values, they will be converted to uppercase so as not to
*        confuse programs which use these parameters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! The length of a character string

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! The current CHART parameters
      CHARACTER * ( 50 ) PAR     ! The name of a CHART parameter
      CHARACTER * ( 50 ) VALUE   ! The value of a CHART parameter
      INTEGER NVAL               ! The number of CHART parameters
      INTEGER NPOS               ! The position of a CHART parameter
      INTEGER L                  ! The length of the name of a parameter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the Current set of default parameters
      CALL GETPARAMS( PARAMS, NVAL, STATUS )

*  First ask for the parameter to be changed.
      CALL PAR_GET0C( 'PARAM', PAR, STATUS )
      CALL PAR_CANCL( 'PARAM', STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Convert the parameter name to upper case

            CALL CHR_UCASE( PAR )

*  Then get the current value of that parameter
         L = CHR_LEN( PAR )
         CALL GETDEFLT( PARAMS, NVAL, PAR(1:L), VALUE, NPOS,
     :      STATUS )

*  Check to see that a value was found
         IF( NPOS .GT. 0 ) THEN

*  Ask for the new value
            CALL PAR_DEF0C( 'VALUE', VALUE, STATUS )
            CALL PAR_GET0C( 'VALUE', VALUE, STATUS )
            CALL PAR_CANCL( 'VALUE', STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN

* Convert new value to uppercase if parameter is not a filename or a
* device name

               IF (( PAR(:5) .NE. 'INPUT' ) .AND.
     :             ( PAR(:6) .NE. 'FIELDS' ) .AND.
     :             ( PAR(:6) .NE. 'COORDS' ) .AND.
     :             ( PAR(:6) .NE. 'DEVICE' ) .AND.
     :             ( PAR(:5) .NE. 'EXTRA' )) THEN

                  CALL CHR_UCASE( VALUE )

               ENDIF

* FIELDS and EXTRA can be filenames or standard values - convert to
* uppercase only if they are standard values of 'terminal' or 'none',
* respectively

               IF (( PAR(:6) .EQ. 'FIELDS' .AND.
     :         VALUE(:8) .EQ. 'terminal' ) .OR.
     :            ( PAR(:5) .EQ. 'EXTRA' .AND.
     :             VALUE(:4) .EQ. 'none' )) THEN

                  CALL CHR_UCASE( VALUE )

               ENDIF

               PARAMS( NPOS )( 21:70 ) = VALUE
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PARAM', 'That parameter was not found',
     :         STATUS )
         END IF
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PARAM', 'No valid parameter entered', STATUS )
      END IF

*  The new set of parameters may now be stored on disk.
      CALL PUTPARAMS( PARAMS, NVAL, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETONE_ERR',
     :   'SETONE: Error setting a CHART parameter.',
     :   STATUS )
      END IF

      END
