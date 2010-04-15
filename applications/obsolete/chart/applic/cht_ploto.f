      SUBROUTINE CHT_PLOTO( STATUS )
*+
*  Name:
*     CHT_PLOTO

*  Purpose:
*     Read in data for making a plot

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_PLOTO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Reads in the parameter file and interprets the plotting
*     parameters then reads in the intermediate file to make up the
*     common blocks.

*  Usage:
*     PLOTO {parameter_usage}

*  [ADAM_parameters]
*  [examples]
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     MJV: {authors_name} (RGO)
*     TNW: Tim Wilkins (University of Manchester)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1983 (MJV):
*        Original version.
*      8-DEC-1988 (TNW, PMA):
*        Modified to use GKS 7.2 instead of GKS 6.2.
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task.
*     23-JUN-1992 (PMA):
*        Converted to full A-task.
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHT_PLOTO.
*     1-MAR-1993 (AJJB):
*        STATUS arg added to SUB_PLOT call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
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

*  Read the intermediate file.
      IPARI = 13
      OPEN( UNIT=IPARI, FILE='chartint.dat', FORM='UNFORMATTED',
     :      STATUS='OLD', ERR=1 )
*  If OK then
      GOTO 2
*  else
    1    CONTINUE
         STATUS = CHT__UNOIF
         CALL ERR_REP( 'PLOTO_UNOPINF',
     :      'Unable to open the intermediate file', STATUS )
         GOTO 990

    2 CONTINUE

*  There may be several sets of parameters, corresponding to several
*  fields.  So - keep restoring until an error occurs.
      CALL SUB_PLOT( IPARI, STATUS )

  990 CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PLOTO_ERR',
     :   'PLOTO: Failed to plot a chart.',
     :   STATUS )
      END IF

      END
