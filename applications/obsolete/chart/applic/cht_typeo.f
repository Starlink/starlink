      SUBROUTINE CHT_TYPEO( STATUS )
*+
*  Name:
*     CHT_TYPEO

*  Purpose:
*     Read the intermediate file and print it on the terminal

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_TYPEO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program reads the intermediate data file and writes a
*     formatted listing to the terminal.

*  Usage:
*     TYPEO

*  [ADAM_parameters]
*  [examples]
*  Algorithm:
*     -  The hard work is done by minor modificatins of the old CHART
*        routines HDNG (TTYHDNG) and OUTPUT (formerly PRINT, now
*        TTYOUT).

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     ANO: A.N. Other (Somewhere)
*     {enter_new_authors_here}

*  History:
*     Unknown (ANO):
*        Original version.
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task
*     16-NOV-1992 (PMA):
*        Finished conversion to ADAM A-task.
*     26-FEB-1993 (PMA):
*        Changed the name of the routine to CHT_TYPEO.
*     1-MAR-1993 (AJJB):
*        STATUS arg added to TTYOUT call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHT_ERR'          ! Chart error constants

*  Global Variables:
      INCLUDE 'MAIN'             ! Main CHART common block
*        {descriptions_of_global_variables_referenced}...

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL RESPONSE           ! Arg. for TTYHDNG and TTYOUT
      CHARACTER * ( 3 ) FLDNUM   ! [local_variable_description]
      CHARACTER * ( 60 ) TEXT    ! [local_variable_description]
      INTEGER IPARI              ! [local_variable_description]
      INTEGER IFLD               ! [local_variable_description]
      INTEGER IEND               ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the common block CONVF
      CALL CON_FACTOR( STATUS )

*  Open the intermediate file
      IPARI = 4
      OPEN( UNIT=IPARI, FILE='chartint.dat',FORM='UNFORMATTED',
     :      STATUS='OLD', ERR=800)

*  There may be several sets of parameters, corresponding to several
*  fields.  So - keep restoring until an error occurs.
      IFLD = 1
  100 CONTINUE
         CALL RESTORE( IEND, IPARI , STATUS )
         IF( IEND .GT. 0 ) GO TO 899
         WRITE(FLDNUM, '(I3)' ) IFLD
         TEXT = 'Field number ' // FLDNUM // ' processed'
         CALL MSG_OUT( 'TYPEO_MESS', TEXT, STATUS )
         IFLD = IFLD + 1
         CALL SORT( STATUS )
         CALL TTYHDNG( RESPONSE, STATUS )
         IF ( .NOT. RESPONSE ) GOTO 501 ! Fall thru if user responds NO
         CALL CONST( AO, DO , STATUS )
         CALL TTYOUT( RESPONSE, STATUS )
         IF ( .NOT. RESPONSE ) GOTO 501 ! Fall thru if user responds NO
         GO TO 100
  800 CONTINUE
      STATUS = CHT__NOFIL
      CALL ERR_REP( 'TYPEO_UNOPEN',
     :   'Unable to open the intermediate file', STATUS )
  899 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TYPEO_ERR',
     :   'TYPEO: Failed to print the data.',
     :   STATUS )
      END IF

 501  CONTINUE

      END


