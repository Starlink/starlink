      SUBROUTINE TRN_GTCL( LOCTR, FORWD, CLASS, STATUS )







*+
*  Name:
*     TRN_GTCL

*  Purpose:
*     get classification.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_GTCL( LOCTR, FORWD, CLASS, STATUS )

*  Description:
*     The routine obtains the logical classification array associated
*     with a transformation structure passed by HDS locator.  The
*     classification information is fully validated.

*  Arguments:
*     LOCTR = CHARACTER * ( * ) (given)
*        HDS locator to transformation structure.
*     FORWD = LOGICAL (given)
*        Whether the classification information is required for the
*        forward (as opposed to the inverse) transformation
*        direction.
*     CLASS( TRN__MXCLS ) = LOGICAL (returned)
*        Classification array.
*     STATUS = INTEGER
*        Inherited error status.

*  Algorithm:
*     - Validate the transformation structure.
*     - Obtain the definition status information and check that the
*       transformation is defined in the requested direction.
*     - Obtain the numbers of input and output variables, and the
*       classification information.
*     - Validate the classification information.
*     - Interchange the numbers of variables if the inverse
*       transformation was requested.
*     - Combine the classification and numbers of variables information
*       to yield the logical classification array for the transformation
*       in the requested direction.

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
*     17-AUG-1988:  Original version (DUVAD::RFWS)
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
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure

      LOGICAL FORWD             ! Whether the forward (as opposed to
                                ! inverse) transformation is required


*  Arguments Returned:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      LOGICAL OK                ! Whether the requested transformation
                                ! direction is defined

      INTEGER DFOR              ! Forward definition status

      INTEGER DINV              ! Inverse definition status

      INTEGER TSTAT             ! Temporary status variable

      INTEGER NVIN              ! Number of input variables

      INTEGER NVOUT             ! Number of output variables

      INTEGER NVTMP             ! Temporary variable for interchanging
                                ! NVIN and NVOUT


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the transformation structure.
      CALL TRN1_VTR( LOCTR, STATUS )


*   Obtain the definition status information.
      CALL TRN1_RDDST( LOCTR, DFOR, DINV, STATUS )


*   If there is no error, check the transformation is defined in the
*   requested direction.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( FORWD ) THEN
          OK = ( DFOR .NE. TRN_DS_UDEF )
        ELSE
          OK = ( DINV .NE. TRN_DS_UDEF )
        ENDIF


*   If the requested direction is not available, report an error.
        IF( .NOT. OK ) THEN
          STATUS = TRN__TRNUD   ! transformation undefined
          CALL TRN1_ERRL( 'TRN_GTCL', LOCTR, STATUS )
        ENDIF
      ENDIF


*   Obtain the numbers of input and output variables and the
*   classification information.
      CALL TRN1_GTNV( LOCTR, NVIN, NVOUT, STATUS )
      CALL TRN1_RDCLS( LOCTR, CLASS, STATUS )


*   If there is no error, mark the error stack and validate the
*   classification information.  If it is not valid, make a new error
*   report which cites the offending data structure.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL ERR_MARK
        CALL TRN1_VALCL( NVIN, NVOUT, DFOR, DINV, CLASS, STATUS )
        IF( STATUS .NE. SAI__OK ) THEN
          TSTAT = STATUS
          CALL ERR_ANNUL( TSTAT )
          CALL TRN1_ERRC( 'TRN_GTCL', LOCTR, 'CLASSIFICATION', STATUS )
        ENDIF
        CALL ERR_RLSE
      ENDIF


*   If there is no error, then if the inverse transformation was
*   selected, interchange the numbers of input and output variables.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( .NOT. FORWD ) THEN
          NVTMP = NVIN
          NVIN = NVOUT
          NVOUT = NVTMP
        ENDIF
      ENDIF


*   Combine the numbers of variables information with the classification
*   array.
      CALL TRN1_NVCLS( NVIN, NVOUT, CLASS, STATUS )


*   Exit routine.
      END
