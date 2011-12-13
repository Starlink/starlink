      SUBROUTINE TRN_GTNV( LOCTR, NVIN, NVOUT, STATUS )







*+
*  Name:
*     TRN_GTNV

*  Purpose:
*     get numbers of variables.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_GTNV( LOCTR, NVIN, NVOUT, STATUS )

*  Description:
*     The routine obtains the numbers of input and output variables from
*     a transformation structure passed by HDS locator.

*  Arguments:
*     LOCTR = CHARACTER * ( * ) (given)
*        HDS locator to transformation structure.
*     NVIN = INTEGER (returned)
*        Number of input variables.
*     NVOUT = INTEGER (returned)
*        Number of output variables.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Validate the transformation structure.
*     - Call TRN1_GTNV to obtain the numbers of variables information.

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


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables

      INTEGER NVOUT             ! Number of output variables


*  Status:
      INTEGER STATUS            ! Error status


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the transformation.
      CALL TRN1_VTR( LOCTR, STATUS )


*   Obtain the numbers of variables information.
      CALL TRN1_GTNV( LOCTR, NVIN, NVOUT, STATUS )


*   Exit routine.
      END
