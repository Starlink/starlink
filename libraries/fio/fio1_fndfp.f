      SUBROUTINE FIO1_FNDFP( PARAM, FP, RFP, STATUS )
*+
*  Name:
*     FIO1_FNDFP

*  Purpose:
*     Get the file parameter descriptor for a named parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO1_FNDFP( PARAM, FP, RFP, STATUS )

*  Description:
*     Given the name of a file parameter, the corresponding file
*     parameter descriptor is found.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Contains the file parameter name
*     FP = INTEGER (Returned)
*        A variable to contain the file parameter descriptor
*     RFP = INTEGER (Returned)
*        A variable to contain the relative file parameter descriptor
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Algorithm:
*     The PARAM string is looked up in the FIO_PA common block.
*     If it is found, then the tape parameter decriptor corresponding
*     to its position in the table is returned.

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
*     03-Feb-1988 (AJC):
*        Original version.
*     29-Aug-1990 (AJC):
*        Compare using CHR_SIMLR.
*     21-JAN-1992 (PMA):
*        Changed the name from FIO_$FNDFP to FIO1_FNDFP.
*        Changed the prologue to the new style.
*     23-FEB-1992 (PMA):
*        Add INCLUDE 'PAR_PAR' for use on Unix systems.
*     12-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     2-JUL-1992 (PMA):
*        Change the calls to EMS to calls to ERR.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO Errors
      INCLUDE 'FIOPAR_SYS'       ! FIO Parameter system Constants
      INCLUDE 'FIO_SYS'          ! FIO internal constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO Parameter Table
*        PFREE( FIO__MXPAR ) = LOGICAL (Read)
*           Whether slot used
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Read)
*           Parameter names

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER FP
      INTEGER RFP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Caseless string equality

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for PARAM in the FIO PAR common block.
      DO I = 1, FIO__MXPAR
         IF ( .NOT. PFREE( I ) ) THEN
            IF ( CHR_SIMLR( PARAM, PTNAME( I ) ) ) THEN
*  Parameter name found
               RFP = I
               FP = RFP + FIO__BASE
               GOTO 1
            ENDIF
         ENDIF
      ENDDO

*  Parameter name not found
      STATUS = FIO__UNKPA
      CALL ERR_REP( 'FIO1_FNDFP_UNPA',
     :   'Unknown parameter', STATUS )

1     CONTINUE
      END
