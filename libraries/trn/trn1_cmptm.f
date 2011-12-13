      SUBROUTINE TRN1_CMPTM( LOCTM, FORWD, LOCCM, PINDX, NVIN, NVOUT,
     :                       IPRC, MXWRK, STATUS )
*+
*  Name:
*     TRN1_CMPTM

*  Purpose:
*     Compile a transformation module.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CMPTM( LOCTM, FORWD, LOCCM, PINDX, NVIN, NVOUT,
*                      IPRC, MXWRK, STATUS )

*  Description:
*     The routine checks and compiles a standard transformation module
*     passed by HDS locator and constructs a compiled module in a
*     temporary HDS object.  The data in the compiled module are mapped
*     into memory ready for use.  The routine returns an HDS locator to
*     the compiled module and a memory pointer into a mapped index
*     array which may be used to access the compiled information (with
*     the TRN1_EVTMx routines).  The routine also returns information
*     about the number of input and output variables for the
*     transformation defined by the module and about the workspace
*     which will be required when the transformation is evaluated.

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
*     4-FEB-1988 (RFWS):
*        Original version.
*     1-MAR-1988 (RFWS):
*        Removed expression compilation to TRN1_CMPFN.
*     9-MAY-1988 (RFWS):
*        Added precision handling.
*     13-MAY-1988 (RFWS):
*        Completely revised and dynamic memory allocation installed.
*     13-FEB-1992 (RFWS):
*        Added handling of character string lengths when passing mapped
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
      INCLUDE 'TRN_CONST_STM'    ! TRN_ private constants for use by
                                 ! routines which process standard
                                 ! transformation modules
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOCTM   ! Locator to transformation module
      LOGICAL FORWD             ! Select forward/inverse transformation


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      CHARACTER * ( * ) LOCCM   ! Locator to compiled module
      INTEGER PINDX             ! Pointer to mapped index array
      INTEGER NVIN              ! Number of input variables
      INTEGER NVOUT             ! Number of output variables
      INTEGER IPRC              ! Module precision code
      INTEGER MXWRK             ! Workspace required per data point when
                                ! evaluating the transformation


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
      INTEGER L_NITEM           ! Number of items in active locator list
      PARAMETER ( L_NITEM = 3 )
      INTEGER L_CODE            ! Location of locator to CODE object
      PARAMETER ( L_CODE = 1 )
      INTEGER L_CONST           ! Location of locator to CONST object
      PARAMETER ( L_CONST = 2 )
      INTEGER L_INDEX           ! Location of locator to INDEX object
      PARAMETER ( L_INDEX = 3 )


*  Local Variables:
      LOGICAL THERE             ! Whether a component exists
      INTEGER NVTMP             ! Temporary storage for number of
                                ! coordinate variables
      INTEGER LENF              ! Character string length of "forward"
                                ! definitions
      INTEGER LENI              ! Character string length of "inverse"
                                ! definitions
      INTEGER IPF               ! Pointer to mapped "forward"
                                ! definitions
      INTEGER IPI               ! Pointer to mapped "inverse"
                                ! definitions
      INTEGER IPEXP             ! Pointer to mapped array of function
                                ! expressions
      INTEGER IPVAR             ! Pointer to mapped array of variable
                                ! names
      INTEGER DEF               ! Transformation definition status in
                                ! requested direction
      INTEGER IPWRK             ! Pointer to mapped workspace array for
                                ! use during compilation
      INTEGER IERR              ! Function definition element
                                ! responsible for a compilation error.
      INTEGER TSTAT             ! Temporary status variable
      INTEGER INDEX( TRN_IN_NITEM )
                                ! Compiled module index array
      INTEGER DUMMYI( 1 )       ! Integer array (junk variable)
      INTEGER NCODE             ! Number of code items generated
      INTEGER NCON              ! Number of constants generated
      INTEGER ICODE             ! Number of code items (junk variable)
      INTEGER ICON              ! Number of constants (junk variable)
      DOUBLE PRECISION DUMMYD( 1 )
                                ! Double precision array (junk variable)
      CHARACTER * ( TRN__SZPRC ) PREC
                                ! Module precision string
      CHARACTER * ( TRN__SZPRC ) VPREC
                                ! Validated precision string
      CHARACTER * 7 FDIR        ! Direction name for the "forward"
                                ! transformation
      CHARACTER * 7 IDIR        ! Direction name for the "inverse"
                                ! transformation
      CHARACTER * ( DAT__SZLOC ) LOCF
                                ! Locator to "forward" definition
                                ! component
      CHARACTER * ( DAT__SZLOC ) LOCI
                                ! Locator to "inverse" definition
                                ! component
      CHARACTER * ( DAT__SZTYP ) TYPEF
                                ! HDS type of "forward" definition array
      CHARACTER * ( DAT__SZTYP ) TYPEI
                                ! HDS type of "inverse" definition array
      CHARACTER * ( DAT__SZLOC ) LOCEXP
                                ! Locator to array of function
                                ! expressions
      CHARACTER * ( DAT__SZLOC ) LOCVAR
                                ! Locator to array of variable names
      CHARACTER * ( DAT__SZLOC ) LOCWRK
                                ! Locator to workspace for use during
                                ! compilation
      CHARACTER * ( DAT__SZLOC ) LOCLST( L_NITEM )
                                ! List of active locators for the
                                ! compiled module

                                ! NOTE: "forward" and "inverse" should
                                ! be interchanged above if
                                ! FORWD = .FALSE.


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Obtain the numbers of input and output variables for the module.
      CALL CMP_GET0I( LOCTM, 'NVAR_IN', NVIN, STATUS )
      CALL CMP_GET0I( LOCTM, 'NVAR_OUT', NVOUT, STATUS )


*   If there is no error, check that the values are valid and report an
*   error if they are not.
      IF( STATUS .EQ. SAI__OK ) THEN

*   ...number of input variables:
        IF( NVIN .LE. 0 ) THEN
          STATUS = TRN__NVRIN   ! number of variables invalid
          CALL TRN1_ERRC( 'TRN1_CMPTM', LOCTM, 'NVAR_IN', STATUS )

*   ...number of output variables:
        ELSE IF( NVOUT .LE. 0 ) THEN
          STATUS = TRN__NVRIN   ! number of variables invalid
          CALL TRN1_ERRC( 'TRN1_CMPTM', LOCTM, 'NVAR_OUT', STATUS )
        ENDIF
      ENDIF


*   If there is no error, assign character strings which will be used to
*   find the transformation definitions appropriate to the direction
*   required.  Also swap the numbers of input and output variables if
*   the inverse transformation is required.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( FORWD ) THEN
          FDIR = 'FORWARD'
          IDIR = 'INVERSE'
        ELSE
          FDIR = 'INVERSE'
          IDIR = 'FORWARD'
          NVTMP = NVIN
          NVIN = NVOUT
          NVOUT = NVTMP
        ENDIF
      ENDIF


*   Set the default precision, then determine if the module has a
*   PRECISION component.  If so, read its value.
      PREC = ' '
      THERE = .FALSE.
      CALL DAT_THERE( LOCTM, 'PRECISION', THERE, STATUS )
      IF( THERE ) CALL CMP_GET0C( LOCTM, 'PRECISION', PREC, STATUS )


*   If there is no error, validate the precision string.  If it is not
*   valid, then make a new error report citing the offending data
*   object.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL ERR_MARK
        CALL TRN1_VPREC( PREC, VPREC, IPRC, STATUS )
        IF( STATUS .NE. SAI__OK ) THEN
          TSTAT = STATUS
          CALL ERR_ANNUL( TSTAT )
          CALL TRN1_ERRC( 'TRN1_CMPTM', LOCTM, 'PRECISION', STATUS )
        ENDIF
        CALL ERR_RLSE
      ENDIF


*   Find the "forward" and "inverse" transformation definition arrays.
      CALL DAT_FIND( LOCTM, FDIR // '_FUNC', LOCF, STATUS )
      CALL DAT_FIND( LOCTM, IDIR // '_FUNC', LOCI, STATUS )


*   Obtain the character string lengths of these arrays and create HDS
*   type strings for them.
      LENF = 1
      LENI = 1
      CALL DAT_LEN( LOCF, LENF, STATUS )
      CALL DAT_LEN( LOCI, LENI, STATUS )
      CALL DAT_CCTYP( LENF, TYPEF )
      CALL DAT_CCTYP( LENI, TYPEI )


*   Map the definition arrays.
      CALL DAT_MAP( LOCF, TYPEF, 'READ', 1, NVOUT, IPF, STATUS )
      CALL DAT_MAP( LOCI, TYPEI, 'READ', 1, NVIN, IPI, STATUS )


*   Create temporary character array objects with sizes and lengths to
*   match the function definitions and map them to provide workspace.
      CALL TRN1_TEMP( TYPEF, 1, NVOUT, LOCEXP, STATUS )
      CALL DAT_MAP( LOCEXP, TYPEF, 'WRITE', 1, NVOUT, IPEXP, STATUS )
      CALL TRN1_TEMP( TYPEI, 1, NVIN, LOCVAR, STATUS )
      CALL DAT_MAP( LOCVAR, TYPEI, 'WRITE', 1, NVIN, IPVAR, STATUS )


*   Extract the list of variable names from the left hand sides of the
*   inverse definitions, and the transformation expressions from the
*   right hand sides of the forward definitions, putting the results
*   into the workspace arrays.  This process also validates the
*   variable names and determines if the transformation is defined.
      CALL TRN1_CLVAR( NVIN, %VAL( CNF_PVAL( IPI ) ),
     :                 %VAL( CNF_PVAL( IPVAR ) ), STATUS,
     :                 %VAL( CNF_CVAL( LENI ) ),
     :                 %VAL( CNF_CVAL( LENI ) ) )
      CALL TRN1_CLEXP( NVOUT, %VAL( CNF_PVAL( IPF ) ),
     :                 %VAL( CNF_PVAL( IPEXP ) ), DEF, STATUS,
     :                 %VAL( CNF_CVAL( LENF ) ),
     :                 %VAL( CNF_CVAL( LENF ) ) )

*   Annul (and unmap) the function definition arrays.
      CALL DAT_ANNUL( LOCF, STATUS )
      CALL DAT_ANNUL( LOCI, STATUS )


*   If there is no error, check that the required mapping is defined.
*   Report an error if it is not.
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( DEF .EQ. TRN_DS_UDEF ) THEN
          STATUS = TRN__MAPUD   ! mapping undefined
          CALL TRN1_ERRC( 'TRN1_CMPTM', LOCTM, FDIR // '_FUNC', STATUS )
        ENDIF
      ENDIF


*   Create and map a temporary integer array to act as workspace during
*   expression compilation.  The size of this array is determined by the
*   maximum number of symbols which can appear in an expression, which
*   in turn is determined by the expression length.
      CALL TRN1_TEMP( '_INTEGER', 1, ( LENF * 3 ), LOCWRK, STATUS )
      CALL DAT_MAP( LOCWRK, '_INTEGER', 'WRITE', 1, ( LENF * 3 ),
     :                 IPWRK, STATUS )


*   If there is no error, make an initial compilation of the
*   expressions to check their syntax and determine the number of code
*   items and constants they will produce.
      NCODE = 0
      NCON = 0
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL TRN1_CMPFN( NVOUT, %VAL( CNF_PVAL( IPEXP ) ), NVIN,
     :                   %VAL( CNF_PVAL( IPVAR ) ),
     :                   LENF, .FALSE., 1, 1,
     :                   DUMMYI, NCODE, DUMMYD, NCON,
     :                   MXWRK, IERR, %VAL( CNF_PVAL( IPWRK ) ), STATUS,
     :                   %VAL( CNF_CVAL( LENF ) ),
     :                   %VAL( CNF_CVAL( LENI ) ) )

*   If there was a compilation error, make a further error report citing
*   the offending data object.
        IF( STATUS .NE. SAI__OK ) THEN
          STATUS = TRN__CMPER   ! compilation error
          CALL DAT_MSG( 'OBJECT', LOCTM )
          CALL MSG_SETC( 'COMPONENT', FDIR // '_FUNC' )
          CALL MSG_SETI( 'CELL', IERR )
          CALL TRN1_ERROR( 'TRN1_CMPTM',
     :                     '^OBJECT.^COMPONENT(^CELL)', STATUS )
        ENDIF
      ENDIF


*   Create a temporary structure to contain the compiled information and
*   create objects within it to contain index pointers, the code, any
*   constants and a locator list.
      NCODE = MAX( 1, NCODE )
      NCON = MAX( 1, NCON )
      CALL TRN1_TEMP( 'TRN_CMP_MODULE', 0, 0, LOCCM, STATUS )
      CALL DAT_NEW( LOCCM, 'INDEX', '_INTEGER', 1, TRN_IN_NITEM,
     :                 STATUS )
      CALL DAT_NEW( LOCCM, 'CODE', '_INTEGER', 1, NCODE, STATUS )
      CALL DAT_NEW( LOCCM, 'CONSTANTS', '_DOUBLE', 1, NCON, STATUS )
      CALL DAT_NEWC( LOCCM, 'LOCATORS', DAT__SZLOC, 1, L_NITEM,
     :                  STATUS )


*   Locate and map the new temporary CODE and CONSTANTS objects,
*   storing the locators in the LOCLST array and the pointers in the
*   INDEX array.
      CALL DAT_FIND( LOCCM, 'CODE', LOCLST( L_CODE ), STATUS )
      CALL DAT_MAP( LOCLST( L_CODE ), '_INTEGER', 'WRITE', 1, NCODE,
     :                 INDEX( TRN_IN_CODE ), STATUS )
      CALL DAT_FIND( LOCCM, 'CONSTANTS', LOCLST( L_CONST ), STATUS )
      CALL DAT_MAP( LOCLST( L_CONST ), '_DOUBLE', 'WRITE', 1, NCON,
     :                 INDEX( TRN_IN_CONST ), STATUS )


*   Store the pointers in the INDEX object, then map this object,
*   storing its locator in the LOCLST array and returning its pointer
*   as one of the routine arguments.
      CALL CMP_PUT1I( LOCCM, 'INDEX', TRN_IN_NITEM, INDEX, STATUS )
      CALL DAT_FIND( LOCCM, 'INDEX', LOCLST( L_INDEX ), STATUS )
      CALL DAT_MAP( LOCLST( L_INDEX ), '_INTEGER', 'READ', 1,
     :                 TRN_IN_NITEM, PINDX, STATUS )


*   Store the list of active locators associated with the compiled
*   module in the LOCATORS object within the module itself.
      CALL CMP_PUT1C( LOCCM, 'LOCATORS', L_NITEM, LOCLST, STATUS )


*   Compile the expressions again, this time generating lists of
*   operation codes and constants.
      CALL TRN1_CMPFN( NVOUT, %VAL( CNF_PVAL( IPEXP ) ), NVIN,
     :                 %VAL( CNF_PVAL( IPVAR ) ), LENF,
     :                 .TRUE., NCODE, NCON,
     :                 %VAL( CNF_PVAL( INDEX( TRN_IN_CODE ) ) ), ICODE,
     :                 %VAL( CNF_PVAL( INDEX( TRN_IN_CONST ) ) ), ICON,
     :                 MXWRK, IERR, %VAL( CNF_PVAL( IPWRK ) ), STATUS,
     :                 %VAL( CNF_CVAL( LENF ) ),
     :                 %VAL( CNF_CVAL( LENI ) ) )

*   If an error was detected, attempt to delete any parts of the
*   temporary structure which may have been created.
      IF( STATUS .NE. SAI__OK ) CALL TRN1_RELTS( 1, LOCCM, STATUS )


*   Release the workspace arrays.
      CALL TRN1_ANTMP( LOCEXP, STATUS )
      CALL TRN1_ANTMP( LOCVAR, STATUS )
      CALL TRN1_ANTMP( LOCWRK, STATUS )


*   Exit routine.
      END
