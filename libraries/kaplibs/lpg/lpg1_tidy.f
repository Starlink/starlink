      SUBROUTINE LPG1_TIDY( STATUS )
*+
*  Name:
*     LPG1_TIDY

*  Purpose:
*     Release resources and reset common block entries used to store 
*     lists of data file names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG1_TIDY( STATUS )

*  Description:
*     Tidies the global variables used by LPG. See LPG_AGAIN.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if an error has already
*     occurred.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
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
*           The names of the data file parameters used by the application.
*        PNAME2( LPG__MXPAR ) = CHARACTER * ( DAT__SZNAM ) (Write)
*           The names of other parameters used by the application.
*        IGRP( LPG__MXPAR ) = INTEGER (Write)
*           The identifier for the GRP groups holding the data file names 
*           supplied for each data file parameter.
*        SIZE( LPG__MXPAR ) = INTEGER (Write)
*           The number of data files supplied for each data file parameter.
*        NPAR = INTEGER (Write)
*           The number of data file parameters used by the application.
*        NPAR2 = INTEGER (Write)
*           The number of non-data file parameters used by the application.
*        NRUN = INTEGER (Write)
*           The number of times the application has been invoked so far.
*        STATE2( LPG__MXPAR ) = INTEGER (Write)
*           The original (i.e. before the first invocation was performed) 
*           PAR state of each parameter listed in array PNAME2.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER ISTAT              ! Local status value
*.

*  Save the initial status value and set a new value for this routine.
      ISTAT = STATUS
      STATUS = SAI__OK

*  Create a new error context.
      CALL ERR_MARK

*  Loop round all used data file parameters.
      DO I = 1, NPAR

*  Reset the parameter name, index of next data file, and number of data files.
         PNAME( I ) = ' '
         SIZE( I ) = 0

*  Delete the GRP group, if it still exists.
         IF( IGRP( I ) .NE. GRP__NOID ) CALL GRP_DELET( IGRP( I ), 
     :                                                  STATUS )

      END DO

*  Loop round all used non-data file parameters.
      DO I = 1, NPAR2

*  Reset the parameter name and original state.
         PNAME2( I ) = ' '
         STATE2( I ) = SUBPAR__GROUND

      END DO

*  Indicate all parameters have been released.
      NPAR = 0
      NPAR2 = 0
      NRUN = 0

*  If the initial status was bad, then ignore all internal errors.
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
