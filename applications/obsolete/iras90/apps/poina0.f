      SUBROUTINE POINA0( PAUTO, PLOGFI, 
     :                   AUTO, BANDLU, LOGFID, LOGREQ, STATUS )
*+
*  Name:
*     POINA0

*  Purpose:
*     Initialisation for pointcrdd

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA0( PAUTO, PLOGFI, PMSGFL, 
*                  AUTO, BANDLU, LOGFID, LOGREQ, STATUS )

*  Description:
*     This subroutine initaialises variables and parameters used by POINTCRDD 
*     It obtains the MSG_FILTER parameter.
*     It obtains the name of the log file and if the file is required, opens
*     it and writes a header to it.
*     It obtains the parameter Automatic
*     It sets up the value of the band last used variable to a value which
*     will trigger the reading of the profiles file.
*
*  Arguments:
*     PAUTO = CHARACTER (Given)
*         The name of the parameter used to obtain the Automatic flag.
*     PLOGFI = CHARACTER (Given)
*         The name of the parameter used to obtain the logfile identity.
*     AUTO = LOGICAL (Returned)
*        TRUE when application is required to run in automatic mode.
*     BANDLU = INTEGER (Returned)
*         The waveband used by the last NDF to be processed
*     LOGFID = INTEGER (Returned)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Returned)
*        TRUE when logging results to the logfile is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG system constants

*  Arguments Given:
      CHARACTER*( * ) PAUTO
      CHARACTER*( * ) PLOGFI
      CHARACTER*( * ) PMSGFL

*  Arguments Returned:
      LOGICAL AUTO
      INTEGER BANDLU
      INTEGER LOGFID
      LOGICAL LOGREQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level
      CALL MSG_IFGET( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRC system.
      CALL IRC_INIT( STATUS )

*  Set the band last used to an initial value which means that the profiles
*  will be read no matter what band is found in the ndf
      BANDLU = 0

*  Obtain the value of the Automatic flag
      CALL PAR_GET0L( PAUTO, AUTO, STATUS )

*  Ask user for name of logfile, if the logfile name is entered as null
*  the LOGREQ flag is set false, if a valid file name is given, the file
*  is opened and LOGREQ returned TRUE.
      CALL IRM_ASFIO( PLOGFI, 'WRITE', 'LIST', 80, LOGFID, LOGREQ, 
     :                STATUS )

*  If the logfile is required display name of the logfile to the user
      IF( LOGREQ ) THEN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'POINTCRDD_MSG1',
     :           '  Logging detection results to $LOGFILE', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Write a header to the log file.
         CALL FIO_WRITE( LOGFID, ' ', STATUS )
         CALL FIO_WRITE( LOGFID, '    *** POINTCRDD log file ***',
     :                   STATUS )
         CALL FIO_WRITE( LOGFID, ' ', STATUS )
      END IF

      END
