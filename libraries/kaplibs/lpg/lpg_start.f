      SUBROUTINE LPG_START( VERBO, DELAYO, DISABO, STATUS )
*+
*  Name:
*     LPG_START

*  Purpose:
*     Initialise the contents of the LPG common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_START( VERBO, DELAYO, DISABO, STATUS )

*  Description:
*     Initialises the global variables used by LPG. See LPG_AGAIN.

*  Arguments:
*     VERBO = LOGICAL (Given)
*        If .TRUE. then the name of the data file being used for each
*	 parameter will be displayed on each invocation of the
*	 application at the point where the parameter is accessed.
*	 Parameters which are not multi-valued (i.e. which are associated
*	 with the same data file on all invocations) are not displayed.
*	 In addition, a blank line will be displayed on the screen
*	 between each invocation of the application. No text is displayed
*	 if VERB is .FALSE.
*     DELAYO = REAL (Given)
*        Put a delay of DELAY seconds between invocations.
*     DISABO = LOGICAL (Given)
*        If .TRUE. the looping facilities are disabled. LPG_AGAIN returns
*        .TRUE. only on the first invocation, and LPG_ASSOC, LPG_CREAT,
*        LPG_PROP, LPG_CREP, LPG_CATASSOC and LPG_CATCREAT make simple
*        calls to the corresponding NDF or CAT routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants.
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        PNAME( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Write)
*           The names of the NDF parameters used by the application.
*        PNAME2( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Write)
*           The names of non-NDF parameters used by the application.
*        IGRP( LPG__MXPAR ) = INTEGER (Write)
*           The identifier for the GRP groups holding the NDF names 
*           supplied for each NDF parameter.
*        SIZE( LPG__MXPAR ) = INTEGER (Write)
*           The number of NDFs supplied for each NDF parameter.
*        NPAR = INTEGER (Write)
*           The number of NDF parameters used by the application.
*        NPAR2 = INTEGER (Write)
*           The number of non-NDF parameters used by the application.
*        NRUN = INTEGER (Write)
*           The number of times the application has been invoked so far.
*        OLD( LPG__MXPAR ) = LOGICAL (Write)
*           A flag for each NDF parameter indicating if the parameter is 
*           used to access existing (i.e. old) NDFs. If not, the parameter 
*           is used to access new NDFs to be created by the application.
*        REP( LPG__MXPAR ) = LOGICAL (Write)
*           A flag for each NDF parameter indicating if the parameter value 
*           has been reported yet by the current invocation of the
*           application.
*        STATE2( LPG__MXPAR ) = INTEGER (Write)
*           The original (i.e. before the first invocation was performed) 
*           PAR state of each parameter listed in array PNAME2.
*        VERB = LOGICAL (Write)
*           Run in verbose mode, displaying the name of each NDF as it is
*           used?
*        DELAY = REAL (Write)
*           Delay between invocations, in seconds.
*        DISAB = LOGICAL (Write)
*           Disable looping?

*  Arguments Given:
      LOGICAL VERBO
      REAL DELAYO
      LOGICAL DISABO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset all slots in the common arrays.
      DO I = 1, LPG__MXPAR
         PNAME( I ) = ' '
         PNAME2( I ) = ' '
         IGRP( I ) = GRP__NOID
         SIZE( I ) = 0
         OLD( I ) = .TRUE.
         REP( I ) = .FALSE.
         STATE2( I ) = SUBPAR__GROUND
      END DO

*  No parameters are currently known.
      NPAR = 0
      NPAR2 = 0

*  The application has not yet ben invoked.
      NRUN = 0 

*  Record whether or not to report NDF names, etc.
      VERB = VERBO

*  Record the required delay between invocations.
      DELAY = DELAYO

*  Record whether looping should be disabled.
      DISAB = DISABO

      END
