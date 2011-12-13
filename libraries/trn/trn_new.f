      SUBROUTINE TRN_NEW( NVIN, NVOUT, FOR, INV, PREC, COMM, ELOC, NAME,
     :                    LOCTR, STATUS )
*+
*  Name:
*     TRN_NEW

*  Purpose:
*     Create new transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_NEW( NVIN, NVOUT, FOR, INV, PREC, COMM, ELOC, NAME,
*                   LOCTR, STATUS )

*  Description:
*     The routine creates a new named component of type TRN_TRANSFORM
*     in an existing HDS structure and uses it to store a standard
*     transformation structure.  The transformation function
*     definitions supplied are fully validated before use.  If the
*     enclosing structure locator (ELOC) is supplied as a blank
*     character string, then a temporary object is created instead.  In
*     this latter case, the NAME argument is not used and may also be
*     blank.

*  Arguments:
*     NVIN = INTEGER (Given)
*        Number of transformation input variables.
*     NVOUT = INTEGER (Given)
*        Number of transformation output variables.
*     FOR( NVOUT ) = CHARACTER * ( * ) (Given)
*        Array of forward transformation function definitions.
*     INV( NVIN ) = CHARACTER * ( * ) (Given)
*        Array of inverse transformation function definitions.
*     PREC = CHARACTER * ( * ) (Given)
*        Transformation precision string.
*     COMM = CHARACTER * ( * ) (Given)
*        Comment string.
*     ELOC = CHARACTER * ( * ) (Given)
*        HDS locator to an enclosing structure to contain the new
*        object (or a blank string if a temporary object is
*        required).
*     NAME = CHARACTER * ( * ) (Given)
*        HDS name of the structure component to contain the new
*        transformation structure.
*     LOCTR = CHARACTER * ( * ) (Geturned)
*        HDS locator to the new transformation structure.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Obtain workspace for validating the transformation functions.
*     - Call TRN1_VFUNC to validate the transformation function
*       definitions supplied and to derive definition status
*       information.
*     - Release the workspace.
*     - Check that the transformation is defined in at least one
*       direction (forward and/or inverse).
*     - Find the character string lengths required to store the
*       transformation functions.
*     - Validate the precision string supplied.
*     - Create an HDS object to contain the new transformation
*       structure (a temporary object is created if required).
*     - Enter the software version number and the definition status
*       information.
*     - Create a single element MODULE_ARRAY component.
*     - Enter the numbers of variables information, (optionally) the
*       comment string, the precision string and the transformation
*       function definitions into appropriate objects created within
*       the (only cell of) the MODULE_ARRAY component.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Added handling of character string length when passing mapped
*        values (for Unix compatibility).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      INTEGER NVIN              ! Number of transformation input
                                ! variables

      INTEGER NVOUT             ! Number of transformation output
                                ! variables

      CHARACTER * ( * ) FOR( NVOUT )
                                ! Array of forward function definitions

      CHARACTER * ( * ) INV( NVIN )
                                ! Array of inverse function definitions

      CHARACTER * ( * ) PREC    ! Transformation precision string

      CHARACTER * ( * ) COMM    ! Comment string

      CHARACTER * ( * ) ELOC    ! Locator to enclosing structure

      CHARACTER * ( * ) NAME    ! Name of new structure component


*  Arguments Returned:
      CHARACTER * ( * ) LOCTR   ! Locator to new transformation


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      INTEGER CHR_LEN           ! Length of a character string with
                                ! trailing blanks removed


*  Local Variables:
      INTEGER IPW1              ! Pointer to mapped workspace object 1

      INTEGER IPW2              ! Pointer to mapped workspace object 2

      INTEGER DFOR              ! Forward definition status

      INTEGER DINV              ! Inverse definition status

      INTEGER CHSZF             ! String size needed for forward
                                ! function definitions

      INTEGER CHSZI             ! String size needed for inverse
                                ! function definitions

      INTEGER IDEF              ! Counter for indexing elements of
                                ! function definition arrays

      INTEGER IPRC              ! Transformation precision code

      CHARACTER * ( DAT__SZTYP ) TYPE
                                ! Type string for creating temporary
                                ! _CHAR objects

      CHARACTER * ( DAT__SZLOC ) LOCW1
                                ! Locator to workspace object 1

      CHARACTER * ( DAT__SZLOC ) LOCW2
                                ! Locator to workspace object 2

      CHARACTER * ( TRN__SZPRC ) VPREC
                                ! Validated precision string

      CHARACTER * ( DAT__SZLOC ) LOCA
                                ! Locator to MODULE_ARRAY component

      CHARACTER * ( DAT__SZLOC ) LOCC
                                ! Locator to first cell of MODULE_ARRAY
                                ! component


*.



*   Check error status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Create and map temporary character array objects to provide
*   workspace for validating the function definitions.
      CALL DAT_CCTYP( LEN( FOR( 1 ) ), TYPE )
      CALL TRN1_TEMP( TYPE, 1, NVOUT, LOCW1, STATUS )
      CALL DAT_MAP( LOCW1, '_CHAR', 'WRITE', 1, NVOUT, IPW1, STATUS )
      CALL DAT_CCTYP( LEN( INV( 1 ) ), TYPE )
      CALL TRN1_TEMP( TYPE, 1, NVIN, LOCW2, STATUS )
      CALL DAT_MAP( LOCW2, '_CHAR', 'WRITE', 1, NVIN, IPW2, STATUS )


*   Validate the function definitions, deriving the definition status
*   information.
      CALL TRN1_XVFUNC( NVIN, NVOUT, %VAL( CNF_PVAL( IPW1 ) ),
     :                  %VAL( CNF_PVAL( IPW2 ) ),
     :                  DFOR, DINV, FOR, INV, STATUS,
     :                  %VAL( LEN( FOR( 1 ) ) ),
     :                  %VAL( LEN( INV( 1 ) ) ) )

*   Unmap the workspace and delete the temporary objects.
      CALL TRN1_ANTMP( LOCW1, STATUS )
      CALL TRN1_ANTMP( LOCW2, STATUS )


*   If there is no error, check that the transformation is fully
*   defined in at least one direction.  Report an error if it is not.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( .NOT. ( ( DFOR .EQ. TRN_DS_DEF ) .OR.
     :              ( DINV .EQ. TRN_DS_DEF ) ) ) THEN
          STATUS = TRN__TRNUD   ! transformation undefined
          CALL TRN1_ERROR( 'TRN_NEW', ' ', STATUS )
        ENDIF
      ENDIF


*   If there is no error, examine the function definitions supplied to
*   determine the string lengths required to store them (CHSZF & CHSZI).
      IF( STATUS .EQ. SAI__OK ) THEN

*   ...the forward direction:
        CHSZF = 1
        DO IDEF = 1, NVOUT
          CHSZF = MAX( CHSZF, CHR_LEN( FOR( IDEF ) ) )
        ENDDO

*   ...the inverse direction:
        CHSZI = 1
        DO IDEF = 1, NVIN
          CHSZI = MAX( CHSZI, CHR_LEN( INV( IDEF ) ) )
        ENDDO
      ENDIF


*   Validate the precision string supplied.
      CALL TRN1_VPREC( PREC, VPREC, IPRC, STATUS )


*   Create a new structure component with the required name and a type
*   of TRN_TRANSFORM to hold the transformation.  If the enclosing
*   structure locator is a blank string, then create a temporary object
*   instead.
      IF( ELOC .NE. ' ' ) THEN
        CALL DAT_NEW( ELOC, NAME, 'TRN_TRANSFORM', 0, 0, STATUS )
        CALL DAT_FIND( ELOC, NAME, LOCTR, STATUS )
      ELSE
        CALL TRN1_TEMP( 'TRN_TRANSFORM', 0, 0, LOCTR, STATUS )
      ENDIF


*   Create a scalar _REAL component called TRN_VERSION inside the
*   transformation structure and enter the software version number.
      CALL DAT_NEW( LOCTR, 'TRN_VERSION', '_REAL', 0, 0, STATUS )
      CALL CMP_PUT0R( LOCTR, 'TRN_VERSION', TRN__VERSN, STATUS )


*   Write the definition status information into the structure.
      CALL TRN1_WRDST( LOCTR, DFOR, DINV, STATUS )


*   Create a single-element vector structure component MODULE_ARRAY of
*   type TRN_MODULE to hold the single module which the transformation
*   will contain.  Obtain a locator to this vector structure.
      CALL DAT_NEW( LOCTR , 'MODULE_ARRAY', 'TRN_MODULE', 1, 1,
     :                 STATUS )
      CALL DAT_FIND( LOCTR , 'MODULE_ARRAY', LOCA, STATUS)


*   Obtain a locator to the first (only) cell of MODULE_ARRAY and annul
*   the locator to the entire vector.
      CALL DAT_CELL( LOCA, 1, 1, LOCC, STATUS )
      CALL DAT_ANNUL( LOCA, STATUS )


*   Create integer objects NVAR_IN and NVAR_OUT inside this cell to
*   hold the number of input and output variables for the module and
*   enter the information.
      CALL DAT_NEW( LOCC, 'NVAR_IN', '_INTEGER', 0, 0, STATUS )
      CALL CMP_PUT0I( LOCC, 'NVAR_IN', NVIN, STATUS )
      CALL DAT_NEW( LOCC, 'NVAR_OUT','_INTEGER', 0, 0, STATUS )
      CALL CMP_PUT0I( LOCC, 'NVAR_OUT', NVOUT, STATUS )


*   If a non-blank comment was supplied, create a scaler _CHAR component
*   called COMMENT and enter the comment data.
      IF( COMM .NE. ' ' ) THEN
        CALL DAT_NEWC( LOCC, 'COMMENT', CHR_LEN( COMM ), 0, 0,
     :                    STATUS )
        CALL CMP_PUT0C( LOCC, 'COMMENT', COMM, STATUS )
      ENDIF


*   Create a PRECISION component and enter the validated precision
*   string into the module.
      CALL DAT_NEWC( LOCC, 'PRECISION', CHR_LEN( VPREC ), 0, 0,
     :                  STATUS )
      CALL CMP_PUT0C( LOCC, 'PRECISION', VPREC, STATUS )


*   Create _CHAR arrays FORWARD_FUNC and INVERSE_FUNC to hold the
*   forward and inverse transformation function definitions and enter
*   the information.
      CALL DAT_NEWC( LOCC, 'FORWARD_FUNC', CHSZF, 1, NVOUT, STATUS )
      CALL CMP_PUT1C( LOCC, 'FORWARD_FUNC', NVOUT, FOR, STATUS )
      CALL DAT_NEWC( LOCC, 'INVERSE_FUNC', CHSZI, 1, NVIN, STATUS)
      CALL CMP_PUT1C( LOCC, 'INVERSE_FUNC', NVIN, INV, STATUS )


*   Annul the locator to the MODULE_ARRAY cell.
          CALL DAT_ANNUL( LOCC, STATUS )


*   Exit routine.
      END

*  Dummy routine to permute the argument order when passing mapped
*  character values to TRN1_VFUNC.
      SUBROUTINE TRN1_XVFUNC( NVIN, NVOUT, WRK1, WRK2, DFOR, DINV,
     :                        FOR, INV, STATUS )

      IMPLICIT NONE

      INTEGER NVIN
      INTEGER NVOUT
      CHARACTER * ( * ) WRK1( NVOUT )
      CHARACTER * ( * ) WRK2( NVIN )
      INTEGER DFOR
      INTEGER DINV
      CHARACTER * ( * ) FOR( NVOUT )
      CHARACTER * ( * ) INV( NVIN )
      INTEGER STATUS

      CALL TRN1_VFUNC( NVIN, NVOUT, FOR, INV, DFOR, DINV, WRK1, WRK2,
     :                 STATUS )
      END
