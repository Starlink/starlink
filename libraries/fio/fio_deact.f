      SUBROUTINE FIO_DEACT( STATUS )
*+
*  Name:
*     FIO_DEACT

*  Purpose:
*     Deactivate FIO

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_DEACT( STATUS )

*  Description:
*     The FIO stand-alone and environment levels are de-activated for
*     the end of an executable image.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is not SAI__OK on input, then the routine will still
*        attempt to execute, but will return with STATUS set to the
*        import value.
*     -  This routine is not normally needed as FIO is closed down by
*        normal program termination.

*  Algorithm:
*     Annul all file descriptors associated with parameters and cancel
*     the parameters.
*     Stop FIO.

*  Implementation Deficiencies:
*     The package closedown system isn't implemented. Commented out.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     AJC: A Chipperfield  (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-Feb-1988 (AJC):
*        Original version.
*     12-FEB-1992 (PMA):
*        Convert prologue to new style.
*     23-FEB-1992 (PMA):
*        Add INCLUDE 'PAR_PAR' for use on Unix systems.
*     12-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
*     1-JUL-1992 (PMA):
*        Replace saving the value of STATUS on entry and restoring it
*        on exit with EMS_BEGIN and EMS_END.
*     2-JUL-1992 (PMA):
*        Change the calls to EMS to calls to ERR.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*      2-AUG-2002 (AJC):
*        Split FIOGO_CMN/FIOGOPA_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_SYS'		 ! FIO Internal Constants
      INCLUDE 'FIO_ERR'		 ! FIO Error codes
      INCLUDE 'FIOPAR_SYS'	 ! FIOPAR Internal Constants

*  Global Variables:
      INCLUDE 'FIOGOPA_CMN'        ! FIO Initialisation Switch
*        FIOSLP = LOGICAL (Read)
*           Whether package is asleep

      INCLUDE 'FIOPA_CMN'        ! FIO Parameter Table
*        PFREE( FIO__MXPAR ) = LOGICAL (Read and Write)
*           Whether slot used
*        PDESC( FIO__MXPAR ) = INTEGER (Read and Write)
*           File descriptor for parameter
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM  )
*                                      (Read and Write)
*           Parameter names

*  External References:
      EXTERNAL FIOPA_BLK           ! Block data subprogram

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I			 ! Loop index
      INTEGER FD                 ! File descriptor
      INTEGER RFD                ! File descriptor

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
*  This also means that the handling of errors from FIO1_CHKFD does not
*  need to be enclosed in an ERR_MARK and ERR_RLSE pair.
      CALL ERR_BEGIN( STATUS )

*  Loop round all of the active parameters.
      DO I = 1, FIO__MXPAR
         IF( .NOT. PFREE( I ) ) THEN

*  Check if the file is still open.
            FD = PDESC( I )
            CALL FIO1_CHKFD( FD, RFD, STATUS )
            IF( STATUS .EQ. FIO__NTOPN ) THEN
*  It must have been closed elsewhere. Annul the error.
               CALL ERR_ANNUL( STATUS )
            ELSE
*  Close it
               CALL FIO_CLOSE( FD, STATUS )
            ENDIF

*  Cancel the parameter.
            CALL PAR_CANCL( PTNAME( I ), STATUS )

*  Clear the FIO parameter table entry
            PTNAME( I ) = ' '
            PDESC( I ) = 0
            PFREE( I ) = .TRUE.
         ENDIF
      END DO

*  Stop the FIO package.
      CALL FIO_STOP( STATUS )

*  Set FIO asleep.
      FIOSLP = .TRUE.

*  Return to the previous error reporting environment.
      CALL ERR_END( STATUS )

      END
