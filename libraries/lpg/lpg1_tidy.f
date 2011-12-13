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

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     28-MAR-2004 (DSB):
*        Free GRP groups used to hold input and temp output NDF names.
*     20-MAY-2004 (DSB):
*        Bug fix to avoid "STATUS set without error report" errors from EMS.
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
*        TMPLST = INTEGER (Read and Write)
*           A GRP identifier for a group holding the full specification
*           for any temporary output NDFs created during the previous
*           invocation of the application. A temporary output NDF is
*           created if the output NDF requested by the user may already
*           be open by the NDF system. In this case the temporary NDF
*           is copied to the requested position once the application has
*           finished.  The TMPLST group holds adjacent pairs of file
*           specs; the first one in each pair is the spec of the temporary
*           output NDF, the second is the spec of the requested output NDF.
*        OPNLST = INTEGER (Read and Write)
*           A GRP identifier for a group holding the full specification
*           for any existing NDFs which have been opened for read-only
*           input by this invocation of the application.

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

*  Free the groups used to hold input NDF names and temporary output NDF
*  names.
      IF( TMPLST .NE. GRP__NOID ) CALL GRP_DELET( TMPLST, STATUS )
      IF( OPNLST .NE. GRP__NOID ) CALL GRP_DELET( OPNLST, STATUS )

*  If the initial status was bad, then ignore all internal errors.
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
