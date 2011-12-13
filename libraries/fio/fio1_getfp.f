      SUBROUTINE FIO1_GETFP( PARAM, FP, RFP, STATUS )
*+
*  Name:
*     FIO1_GETFP

*  Purpose:
*     Get a file parameter descriptor

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_GETFP( PARAM, FP, RFP, STATUS )

*  Description:
*     Given the name of a file parameter, the corresponding parameter
*     descriptor is found.  If the parameter is not known return a free
*     parameter descriptor. If the parameter is already known, return
*     its parameter descriptor, set STATUS to FIO__ISACT, but do not
*     report an error.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The file parameter name
*     FP = INTEGER (Returned)
*        A variable to contain the file parameter descriptor.
*     RFP = INTEGER (Returned)
*        A variable to contain the relative file parameter descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Algorithm:
*     The PARAM string is looked up in the FIO_PA common block.
*     If it is found, then the parameter descriptor corresponding to
*     its position in the table is returned. Otherwise a new
*     parameter descriptor is returned. The common block is searched in
*     reverse order so that if a new parameter descriptor is returned,
*     it will be the lowest possible number.

*  External Routines Used:
*     CHR:
*        CHR_SIMLR

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
*     22-Feb-1988 (AJC):
*        Original version.
*     21-JAN-1992 (PMA):
*        Changed name from FIO_$GETFP to FIO1_GETFP.
*     23-FEB-1992 (PMA):
*        Add INCLUDE 'PAR_PAR' for use on Unix systems.
*     12-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     2-JUL-1992 (PMA):
*        Change the calls to EMS to calls to ERR.
*     28-JUL-1992 (PMA):
*        Change the logic so that this routine no longer calls
*        FIO1_FNDFP. This remove the need to handle errors from
*        FIO1_FNDFP in perfectly normal situations.
*        Do not report an error when setting STATUS to FIO__ISACT.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Implementation Deficiencies:
*     This routine does not report an error when setting STATUS to
*     FIO__ISACT. This is because it is only called from FIO_ASSOC,
*     which always annuls this error. It would be better if this
*     information were passed back by another argument. This has not
*     yet been done so that a check can be made to see if this will
*     affect anyone else.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO Errors
      INCLUDE 'FIOPAR_SYS'       ! FIOPAR System Constants
      INCLUDE 'FIO_SYS'          ! FIO internal constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO Parameter Table
*        PFREE( FIO__MXPAR ) = LOGICAL  (Read)
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
      LOGICAL CHR_SIMLR          ! Case insensitive comparison of strings

*  Local Variables:
      INTEGER I                  ! Loop index
      LOGICAL NEW                ! Has a new parameter descriptor been selected?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No new parameter descriptor has been found yet.
      NEW = .FALSE.

*  Look for a descriptor.
      DO I = FIO__MXPAR, 1, -1
         IF ( PFREE( I ) ) THEN
*  Free descriptor found.
            NEW = .TRUE.
            RFP = I
         ELSE
            IF ( CHR_SIMLR( PARAM, PTNAME( I ) ) ) THEN
*  Parameter already in table
               RFP = I
               STATUS = FIO__ISACT
               GOTO 1
            END IF
         END IF
      END DO

*  If no new descriptors were selected, set an invalid parameter
*  descriptor and report an error.
      IF ( .NOT. NEW ) THEN
         RFP = 0
         STATUS = FIO__TOOFP
         CALL ERR_REP( 'FIO1_GETFP_TOOF', 'Too many file parameters',
     :      STATUS )
      END IF

    1 CONTINUE
      FP = RFP + FIO__BASE

      END
