      SUBROUTINE TRN_APND( LOCTR1, LOCTR2, STATUS )







*+
*  Name:
*     TRN_APND

*  Purpose:
*     append transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_APND( LOCTR1, LOCTR2, STATUS )

*  Description:
*     The routine concatenates two transformation structures passed by
*     HDS locator.  The first transformation is modified by appending
*     the second one to it.  The second transformation is not altered.

*  Arguments:
*     LOCTR1 = CHARACTER * ( * ) (given)
*        HDS locator to first transformation structure, which is to
*        be altered.
*     LOCTR2 = CHARACTER * ( * ) (given)
*        HDS locator to second transformation structure.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Validate the two transformation structures.
*     - Read the definition status, number of variables and
*       classification information from the two transformation
*       structures.
*     - Validate the classification information.
*     - Combine the information from the two transformations.  This
*       process also checks that the two transformations are compatible.
*     - Update the software version number in the first transformation
*       structure.
*     - Modify the first transformation's definition status information.
*     - Expand the MODULE_ARRAY component in the first structure to
*       accommodate the extra modules from the second structure.
*     - Copy the extra MODULE_ARRAY components across.
*     - Tidy up.
*     - Insert the combined classification information into the modified
*       transformation structure.

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
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTR1  ! Locator to first transformation

      CHARACTER * ( * ) LOCTR2  ! Locator to second transformation


*  Status:
      INTEGER STATUS            ! Error status


*  Local Variables:
      LOGICAL CLASS1( TRN__MXCLS )
                                ! Classification array for
                                ! transformation 1

      LOGICAL CLASS2( TRN__MXCLS )
                                ! Classification array for
                                ! transformaton 2

      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array for the
                                ! combined transformation

      INTEGER DFOR1             ! Forward definition status for
                                ! transformation 1

      INTEGER DFOR2             ! Forward definition status for
                                ! transformation 2

      INTEGER DFOR              ! Forward definition status for
                                ! combined transformation

      INTEGER DINV1             ! Inverse definition status for
                                ! transformation 1

      INTEGER DINV2             ! Inverse definition status for
                                ! transformation 2

      INTEGER DINV              ! Inverse definition status for
                                ! combined transformation

      INTEGER NVIN1             ! Number of input variables for
                                ! transformation 1

      INTEGER NVOUT1            ! Number of output variables for
                                ! transformation 1

      INTEGER NVIN2             ! Number of input variables for
                                ! transformation 2

      INTEGER NVOUT2            ! Number of output variables for
                                ! transformation 2

      INTEGER NVIN              ! Number of input variables for the
                                ! combined transformation

      INTEGER NVOUT             ! Number of output variables for the
                                ! combined transformation

      INTEGER TSTAT             ! Temporary status variable

      INTEGER NMOD1             ! Number of modules in transformation 1

      INTEGER NMOD2             ! Number of modules in transformation 2

      INTEGER IMOD1             ! Module counter for transformation 1

      INTEGER IMOD2             ! Module counter for transformation 2

      CHARACTER * ( DAT__SZLOC ) LOCMA1
                                ! Locator to MODULE_ARRAY component in
                                ! transformation 1

      CHARACTER * ( DAT__SZLOC ) LOCMA2
                                ! Locator to MODULE_ARRAY component in
                                ! transformation 2

      CHARACTER * ( DAT__SZLOC ) LOCC1
                                ! Locator to cell of MODULE_ARRAY in
                                ! transformation 1

      CHARACTER * ( DAT__SZLOC ) LOCC2
                                ! Locator to cell of MODULE_ARRAY in
                                ! transformation 2


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Validate the two transformation structures supplied.
      CALL TRN1_VTR( LOCTR1, STATUS )
      CALL TRN1_VTR( LOCTR2, STATUS )


*   Read the definition status information for each of the
*   transformations.
      CALL TRN1_RDDST( LOCTR1, DFOR1, DINV1, STATUS )
      CALL TRN1_RDDST( LOCTR2, DFOR2, DINV2, STATUS )


*   Read the number of input and output variables for each of the
*   transformations.
      CALL TRN1_GTNV( LOCTR1, NVIN1, NVOUT1, STATUS )
      CALL TRN1_GTNV( LOCTR2, NVIN2, NVOUT2, STATUS )


*   Read the classification information for each of the transformations.
      CALL TRN1_RDCLS( LOCTR1, CLASS1, STATUS )
      CALL TRN1_RDCLS( LOCTR2, CLASS2, STATUS )


*   If there is no error, validate the classification information
*   associated with the first transformation.  If it is invalid, make a
*   new error report citing the offending data structure.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL ERR_MARK
        CALL TRN1_VALCL( NVIN1, NVOUT1, DFOR1, DINV1, CLASS1, STATUS )
        IF( STATUS .NE. SAI__OK ) THEN
          TSTAT = STATUS
          CALL ERR_ANNUL( TSTAT )
          CALL TRN1_ERRC( 'TRN_APND', LOCTR1, 'CLASSIFICATION', STATUS )
        ENDIF
        CALL ERR_RLSE
      ENDIF


*   If there is no error, make a similar check (and error report) on
*   the classification information for the second transformation.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL ERR_MARK
        CALL TRN1_VALCL( NVIN2, NVOUT2, DFOR2, DINV2, CLASS2, STATUS )
        IF( STATUS .NE. SAI__OK ) THEN
          TSTAT = STATUS
          CALL ERR_ANNUL( TSTAT )
          CALL TRN1_ERRC( 'TRN_APND', LOCTR2, 'CLASSIFICATION', STATUS )
        ENDIF
        CALL ERR_RLSE
      ENDIF


*   Combine the information from the two transformations to yield that
*   for the combined transformation.
      CALL TRN1_CBCLI( NVIN1, NVOUT1, DFOR1, DINV1, CLASS1,
     :                 NVIN2, NVOUT2, DFOR2, DINV2, CLASS2,
     :                 NVIN, NVOUT, DFOR, DINV, CLASS, STATUS )


*   Update the software version number in the first transformation,
*   which is being modified.
      CALL TRN1_UPVSN( LOCTR1, STATUS )


*   Write the definition status information for the combined
*   transformation.
      CALL TRN1_WRDST( LOCTR1, DFOR, DINV, STATUS )


*   Locate the MODULE_ARRAY component in both transformation structures
*   and obtain the number of modules in each transformation from the
*   sizes of these arrays.
      NMOD1 = 0
      NMOD2 = 0
      CALL DAT_FIND( LOCTR1, 'MODULE_ARRAY', LOCMA1, STATUS )
      CALL DAT_SIZE( LOCMA1, NMOD1, STATUS )
      CALL DAT_FIND( LOCTR2, 'MODULE_ARRAY', LOCMA2, STATUS )
      CALL DAT_SIZE( LOCMA2, NMOD2, STATUS )


*   Expand the size of the MODULE_ARRAY in the first transformation
*   to accommodate the extra modules from the second transformation.
      CALL DAT_ALTER( LOCMA1, 1, NMOD1 + NMOD2, STATUS )


*   Step through the new elements in this array, obtaining locators to
*   each new cell in transformation 1 and the corresponding cell of
*   transformation 2.
      IMOD2 = 0
      DO WHILE ( ( IMOD2 .LT. NMOD2 ) .AND. ( STATUS .EQ. SAI__OK ) )
        IMOD2 = IMOD2 + 1
        IMOD1 = IMOD2 + NMOD1
        CALL DAT_CELL( LOCMA1, 1, IMOD1, LOCC1, STATUS )
        CALL DAT_CELL( LOCMA2, 1, IMOD2, LOCC2, STATUS )


*   Copy the cell contents across from transformation 1 to
*   transformation 2, then annul the cell locators.
        CALL TRN1_CPYST( LOCC2, LOCC1, STATUS )
        CALL DAT_ANNUL( LOCC1, STATUS )
        CALL DAT_ANNUL( LOCC2, STATUS )
      ENDDO


*   Annul the locators to the MODULE_ARRAY components.
      CALL DAT_ANNUL( LOCMA1, STATUS )
      CALL DAT_ANNUL( LOCMA2, STATUS )


*   Enter the new classification information into the combined
*   transformaton.
      CALL TRN1_WRCLS( LOCTR1, CLASS, STATUS )


*   Exit routine.
      END
