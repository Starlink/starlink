*+  IO2RO - top level subroutine for A-task monolith

      SUBROUTINE IO2RO (STATUS)

*    Description :
*
*     This is the top level monolith subroutine for the ACVT suite
*     of A-tasks for converting IRCAM I and O files into RO files for
*     CLRED reduction. The value of NAME is input from the interface and
*     parsed, the requested A-task being called on successful matching
*     of the input string with a valid task name.
*
*    Invocation :
*
*     CALL ACVT( NAME, STATUS)
*
*    Method :
*
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.
*
*    Deficiencies :
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Sandy Leggett (SKL@JACH) - adapted from code by
*     Colin Aspin (UKTH::CAA) and
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*
*    History :
*
*     10-Aug-1994 First implementation (SKL@JACH)
*     19-AUG-1994 Changed style to new UNIX/VMS Atask monolith style (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_PAR'          ! necessary for non-VMS
      INCLUDE  'CHR_ERR'


      INTEGER  STATUS             ! global status parameter

      CHARACTER*(PAR__SZNAM)   NAME     ! action name

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      END IF


*    get action name

      CALL TASK_GET_NAME( NAME, STATUS )

*    force the input string to upper-case before testing

      CALL CHR_UCASE( NAME )

*    check the string against valid A-task names - if matched then
*    call the relevant A-task
*    (actually only one task currently)

      IF( NAME .EQ. 'REDUCE') THEN

*       do conversion of I and O files to RO

         CALL REDUCE ( STATUS )

      ELSE

*       no such option exists

        CALL MSG_OUT( 'ERR', 'No such IO2RO action exists',
     :                 STATUS )
        CALL MSG_SETC( 'NAME', NAME)
        CALL MSG_OUT( 'MESSAGE', 'Action requested was ^NAME',
     :                 STATUS)

      END IF


      END
