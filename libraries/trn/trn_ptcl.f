      SUBROUTINE TRN_PTCL( CLASS, LOCTR, STATUS )







*+
*  Name:
*     TRN_PTCL

*  Purpose:
*     put classification.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_PTCL( CLASS, LOCTR, STATUS )

*  Description:
*     The routine enters classification information into a
*     transformation structure passed by HDS locator.  The information
*     is supplied as a logical array and is validated before use.  If
*     the transformation already contains such information, it is
*     over-written by this routine.

*  Arguments:
*     CLASS( TRN__MXCLS ) = LOGICAL (given)
*        The logical classification array.
*     LOCTR = CHARACTER * ( * ) (given)
*        HDS locator to the transformation structure.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Validate the transformation structure.
*     - Obtain the definition status and the numbers of variables
*       information from the transformation structure.
*     - Validate the classification array supplied.
*     - Write the classification information into the structure.
*     - Update the software version number.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Arguments Given:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array

      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      LOGICAL TMPCL( TRN__MXCLS )
                                ! Temporary classification array

      INTEGER DFOR              ! Forward definition status

      INTEGER DINV              ! Inverse definition status

      INTEGER NVIN              ! Number of input variables

      INTEGER NVOUT             ! Number of output variables

      INTEGER I                 ! Loop counter for copying data


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the transformation structure supplied.
      CALL TRN1_VTR( LOCTR, STATUS )


*   Read the definition status information from the transformation.
      CALL TRN1_RDDST( LOCTR, DFOR, DINV, STATUS )


*   Obtain the numbers of input/output variables.
      CALL TRN1_GTNV( LOCTR, NVIN, NVOUT, STATUS )


*   Copy the classification information to a temporary array and
*   validate it.
      DO I = 1, TRN__MXCLS
        TMPCL( I ) = CLASS( I )
      ENDDO
      CALL TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, TMPCL, STATUS )


*   Enter the classification information into the transformation.
      CALL TRN1_WRCLS( LOCTR, TMPCL, STATUS )


*   Update the software version number.
      CALL TRN1_UPVSN( LOCTR, STATUS )


*   Exit routine.
      END
