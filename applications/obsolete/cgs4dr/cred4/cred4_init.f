*+ CRED4_INIT - Initialises the CRED4 task
      SUBROUTINE CRED4_INIT( STATUS )
*    Description :
*     This routine carries out the initialisation required
*     before running the CRED4 CD-task.
*    Invocation :
*     CALL CRED4_INIT( VALUE, STATUS )
*    Authors :
*     J. Lightfoot (REVAD::JFL)
*     S.M. Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989: Original as DEVINIT for CRED4.            (JFL)
*     11-Oct-1989: History section added. Description changed.  (SMB)
*     17-Jan-1990: Directory spec removed from INCLUDE file.    (SMB)
*      8-Feb-1990: Code added to unset the CRED4_BUSY flag in
*                  P4. Note this will only work if P4 is already
*                  loaded.                                      (SMB)
*     28-Mar-1990: Initialisation of INTEGRATION_AVAILABLE
*                  and OBSERVATION_AVAILABLE added.             (SMB)
*      4-Jun-1990: Rewritten so a noticeboard is used
*                  to store flags and parameters, instead of
*                  using a common block and the P4 task.        (SMB)
*     15-Jun-1990: Phase 1 of major changes applied (see CACT). (SMB)
*     18-Jun-1990: Phase 2 of major changes applied (see CACT). (SMB)
*      2-Oct-1990: Initialisation of GROUP_AVAILABLE added.     (SMB)
*     22-Nov-1990: Initialisation of VERBOSE flag added.        (SMB)
*     14-Apr-1992: DRIMP/5.1 and 5.2 implemented. VALUE string
*                  removed. It is not used.                     (SMB)
*     15-Apr-1992: The CRED4 task was still not working under
*                  ADAM V2. The rescheduling mechanism was not
*                  working. I have concluded that the task needs
*                  a major revision to make it compatible with
*                  ADAM V2. Since a DEVINIT is no longer supported
*                  under ADAM V2, this routine now becomes
*                  CRED4_INIT.                                  (SMB)
*     11-Feb-1993: Conform to error strategy                    (PND)
*     28-Jul-1994: Initialise QMAN_OK and port to Unix          (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   No actions should be in progress in the RED4 or P4 tasks
      QMAN_OK     = .FALSE.
      RED4_ACTIVE = .FALSE.
      P4_ACTIVE   = .FALSE.

*   No data reduction sequence has been set up yet.
      SEQUENCE_SETUP = .FALSE.
      PAUSE_REDUCTION = .FALSE.

*   There are no integrations, observations or reduced groups available
      REDUCTION_OK          = .FALSE.
      INTEGRATION_AVAILABLE = .FALSE.
      OBSERVATION_AVAILABLE = .FALSE.
      GROUP_AVAILABLE       = .FALSE.
      NEWGROUP              = .FALSE.

*   Brief messages should be used by default.
      VERBOSE = .FALSE.

*   Get some environmental variables
      CALL CRED4_INIT_SYS( STATUS )

      END

