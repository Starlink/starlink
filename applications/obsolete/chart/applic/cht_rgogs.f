      SUBROUTINE CHT_RGOGS( STATUS )
*+
*  Name:
*     CHT_RGOGS

*  Purpose:
*     Output information for RGO guide stars

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_RGOGS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program reads the intermediate data file and creates a
*     formatted file for output to a printer.  The output contains
*     information relevant to the RGO 26 and 13 inch refracting
*     telescopes and the 1 metre camera.  Most of the work is done by
*     subroutines adapted from those in the much earlier ICL 1903T
*     version of Chart.

*  Usage:
*     RGOGS {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     ANO: A.N. Other (RGO)   The original author is not documented
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (ANO):
*        Original version.
*     10-DEC-1991: (PMA)
*        Changed from main program to subroutine to be an ADAM task
*     30-JUN-1992 (PMA):
*        Finished conversion to ADAM task.
*     26-FEB-1993 (PMA):
*        Changed the name of the routine to CHT_RGOGS.
*     1-MAR-1993 (AJJB):
*        STATUS arg added to SUB_RGS call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     22-MAR-1993 (AJJB):
*        Commented out declarations of unused local variables, to keep
*        Sun compiler quiet.
*     17-MAY-1993 (AJJB):
*        Changed the STATUS in the OPEN statement from 'NEW' to
*        'UNKNOWN'.
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

*  Local Variables:
*     CHARACTER * ( 3 ) FLDNUM   ! [local_variable_description]
*     CHARACTER * ( 60 ) TEXT    ! [local_variable_description]
      INTEGER IPARI              ! [local_variable_description]


*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C   Set up the common block CONVF
C
      CALL CON_FACTOR( STATUS )
C
C   Open the intermediate file.
C
      IPARI=4
      OPEN( UNIT=IPARI, FILE='chartint.dat', FORM='UNFORMATTED',
     :   STATUS='OLD', ERR=1 )

*  If all is well, skip the next block.
      GOTO 2

*  Else report an error.
    1 CONTINUE
         CALL ERR_REP( 'UNOPINF',
     :      'Unable to open the intermediate file', STATUS )
         GOTO 900
    2 CONTINUE

C
C   Open a file for the formatted output.
C
      OPEN( UNIT=7, FILE='rgogs.lis', STATUS='UNKNOWN' )
C
C   There may be several sets of parameters, corresponding to
C   several fields.
C
C   So - keep restoring until an error occurs.
C
      CALL SUB_RGS(IPARI, STATUS)

  900 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RGOGS_ERR',
     :   'RGOGS: Failed to output information on RGO guide stars.',
     :   STATUS )
      END IF

      END
