      SUBROUTINE CHT_LISTO( STATUS )
*+
*  Name:
*     CHT_LISTO

*  Purpose:
*     Create readable version of the intermediate CHART file

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_LISTO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program reads the intermediate data file and creates a
*     formatted file for output to a printer.  Most of the hardwork is
*     done by the existing PRINT subroutine in CHART.

*  Usage:
*     LISTO {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     ANO: A.N. Other (Somewhere)
*        The original version contained no comment to indicate its
*        author.
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (ANO):
*        Original version.
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task
*     23-JUN-1992 (PMA):
*        {changes}
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_LISTO.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     28-APR-1993 (AJJB):
*        Changed STATUS specifier where 'output.lis' file is OPENed to
*        UNKNOWN instead of NEW, so that old output.lis files get
*        overwritten instead of causing the program to crash.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHT_ERR'          ! CHART error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPARI              ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the common block CONVF
      CALL CON_FACTOR( STATUS )

*  Open the intermediate file.
      IPARI = 4
      OPEN( UNIT=IPARI, FILE='chartint.dat', FORM='UNFORMATTED',
     :      STATUS='OLD', ERR=1 )
*  If OK then
      GOTO 2
*  else
    1    CONTINUE
         STATUS = CHT__UNOIF
         CALL ERR_REP( 'LISTO_UNOPINF',
     :      'Unable to open the intermediate file', STATUS )
         GOTO 990

    2 CONTINUE

*  Open a file for the formatted output.
      OPEN( UNIT=7, FILE='output.lis', STATUS='UNKNOWN' )

*  There may be several sets of parameters, corresponding to several
*  fields.  So - keep restoring until an error occurs.
      CALL SUB_LISTO( IPARI, STATUS )

  990 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LISTO_ERR',
     :   'LISTO: Failed to list intermediate file.',
     :   STATUS )
      END IF

      END
