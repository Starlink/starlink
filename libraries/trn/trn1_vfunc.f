      SUBROUTINE TRN1_VFUNC( NVIN, NVOUT, FOR, INV, DFOR, DINV, WRK1,
     :                       WRK2, STATUS )
*+
*  Name:
*     TRN1_VFUNC

*  Purpose:
*     Validate a set of transformation function definitions.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VFUNC( NVIN, NVOUT, FOR, INV, DFOR, DINV, WRK1,
*                      WRK2, STATUS )

*  Description:
*     The routine checks the form of a set of matching forward and
*     inverse transformation function definitions.  It checks for the
*     presence, validity and uniqueness of the variable names defined
*     by the left hand sides of the function definitions.  It also
*     checks for the presence/absence of expressions on the right hand
*     sides (RHS) and derives definition status information for the
*     forward/inverse transformations described by the functions.
*     Finally, the expressions on the RHS are compiled to fully check
*     their syntax.
*
*     If anything is wrong, STATUS is set and an error is reported.

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
*     12-MAY-1988 (RFWS):
*        Original version.
*     2-JUN-1988 (RFWS):
*        Added compilation check on expression syntax.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
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
      INCLUDE 'TRN_CONST'        ! TRN_ private constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      INTEGER NVIN              ! Number of input variables
      INTEGER NVOUT             ! Number of output variables
      CHARACTER * ( * ) FOR( NVOUT )
                                ! Forward function definitions
      CHARACTER * ( * ) INV( NVIN )
                                ! Inverse function definitions


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER DFOR              ! Forward transformation definition
                                ! status
      INTEGER DINV              ! Inverse transformation definition
                                ! status
      CHARACTER * ( * ) WRK1( NVOUT )
                                ! Workspace array - its length must
                                ! match FOR
      CHARACTER * ( * ) WRK2( NVIN )
                                ! Workspace array - its length must
                                ! match INV


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      INTEGER CHR_LEN           ! Length of a character string with
                                ! trailing blanks removed


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
      INTEGER MXSYM0		! Maximum expression length which can be
      PARAMETER ( MXSYM0 = 80 ) ! compiled using static workspace


*  Local Variables:
      LOGICAL EXTRA             ! Whether extra (dynamic) workspace is
                                ! required
      INTEGER I                 ! Loop counter for indexing workspace
                                ! array elements
      INTEGER J                 ! Loop counter for indexing workspace
                                ! array elements
      INTEGER MXSYM             ! Maximum non-blank length of an
                                ! expression - determines workspace
                                ! requirement for compilation
      INTEGER WRK( MXSYM0, 3 )  ! Static workspace array for compiling
                                ! expressions
      INTEGER DUMMYI( 1 )       ! Junk integer array
      INTEGER NCODE		! Junk variable for TRN1_CMPEX call
      INTEGER NCON              ! Junk variable for TRN1_CMPEX call
      INTEGER MXSTK             ! Junk variable for TRN1_CMPEX call
      INTEGER IPWRK             ! Pointer to mapped workspace array
      DOUBLE PRECISION DUMMYD( 1 )
                                ! Junk double precision array
      CHARACTER * ( DAT__SZLOC ) LOCWRK
                                ! Locator to object mapped as extra
                                ! workspace


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Extract the variable names from the left hand sides of each set of
*   function definitions, putting them into the workspace arrays.  This
*   process validates their names and checks them for uniqueness within
*   each set of definitions.
      CALL TRN1_CLVAR( NVOUT, FOR, WRK1, STATUS )
      CALL TRN1_CLVAR( NVIN, INV, WRK2, STATUS )


*   If there is no error, intercompare the two sets of names, checking
*   for duplication.
      IF( STATUS .EQ. SAI__OK ) THEN
        DO I = 1, NVOUT
          DO J = 1, NVIN


*   If a duplicate name is found, report an error and quit the loops.
            IF( WRK1( I ) .EQ. WRK2( J ) ) THEN
              STATUS = TRN__DUVAR
              CALL MSG_SETC( 'VAR', WRK1( I ) )
              CALL ERR_REP( 'TRN1_VFUNC_DUPE',
     :                      'Duplicate variable name ''^VAR'' ' //
     :                      'encountered (possible programming ' //
     :                      'error).', STATUS )
              GO TO 1
            ENDIF


*   End of "intercompare the two sets of names" loops.
          ENDDO
        ENDDO
    1   CONTINUE


*   End of "no error extracting the variable names" condition.
      ENDIF


*   Extract the expressions from the right hand sides of the two sets
*   of function definitions.  This process validates the pattern of
*   expression presence/absence and derives definition status
*   information.
      CALL TRN1_CLEXP( NVOUT, FOR, WRK1, DFOR, STATUS )
      CALL TRN1_CLEXP( NVIN, INV, WRK2, DINV, STATUS )


*   If there is no error, determine the amount of workspace required
*   for compiling the expressions.  This is determined by the maximum
*   (non-blank) expression length.  Take account of undefined
*   expressions.
      MXSYM = 0
      IF( STATUS .EQ. SAI__OK ) THEN

*   ...forward transformation expressions:
        IF( DFOR .NE. TRN_DS_UDEF ) THEN
          DO I = 1, NVOUT
            MXSYM = MAX( MXSYM, CHR_LEN( FOR( I ) ) )
          ENDDO
        ENDIF

*   ...inverse transformation expressions:
        IF( DINV .NE. TRN_DS_UDEF ) THEN
          DO I = 1, NVIN
            MXSYM = MAX( MXSYM, CHR_LEN( INV( I ) ) )
          ENDDO
        ENDIF
      ENDIF


*   If the static workspace allocation is not sufficient, create and map
*   a temporary object to provide space.
      EXTRA = ( MXSYM .GT. MXSYM0 )
      IF( EXTRA ) THEN
        CALL TRN1_TEMP( '_INTEGER', 1, MXSYM * 3, LOCWRK, STATUS )
        CALL DAT_MAP( LOCWRK, '_INTEGER', 'WRITE', 1, MXSYM * 3,
     :                   IPWRK, STATUS )
      ENDIF


*   If the forward transformation is defined, extract the list of input
*   variable names again and loop to compile all the forward
*   transformation expressions as a check on their syntax.
      IF( DFOR .NE. TRN_DS_UDEF ) THEN
        CALL TRN1_CLVAR( NVIN, INV, WRK2, STATUS )
        I = 0
        DO WHILE ( ( I .LT. NVOUT ) .AND. ( STATUS .EQ. SAI__OK ) )
          I = I + 1

*   ...use extra workspace if necessary:
          IF( EXTRA ) THEN
            CALL TRN1_CMPEX( WRK1( I ), NVIN, WRK2, MXSYM, .FALSE.,
     :                       1, 1, DUMMYI, NCODE, DUMMYD, NCON,
     :                       MXSTK, %VAL( CNF_PVAL( IPWRK ) ), STATUS )
          ELSE
            CALL TRN1_CMPEX( WRK1( I ), NVIN, WRK2, MXSYM, .FALSE.,
     :                       1, 1, DUMMYI, NCODE, DUMMYD, NCON,
     :                       MXSTK, WRK, STATUS )
          ENDIF
        ENDDO
      ENDIF


*   If the inverse transformation is defined, extract the list of
*   output variable names and the inverse transformation expressions
*   again and repeat the above process to check their syntax.
      IF( DINV .NE. TRN_DS_UDEF ) THEN
        CALL TRN1_CLVAR( NVOUT, FOR, WRK1, STATUS )
        CALL TRN1_CLEXP( NVIN, INV, WRK2, DINV, STATUS )
        I = 0
        DO WHILE ( ( I .LT. NVIN ) .AND. ( STATUS .EQ. SAI__OK ) )
          I = I + 1
          IF( EXTRA ) THEN
            CALL TRN1_CMPEX( WRK2( I ), NVOUT, WRK1, MXSYM, .FALSE.,
     :                       1, 1, DUMMYI, NCODE, DUMMYD, NCON,
     :                       MXSTK, %VAL( CNF_PVAL( IPWRK ) ), STATUS )
          ELSE
            CALL TRN1_CMPEX( WRK2( I ), NVOUT, WRK1, MXSYM, .FALSE.,
     :                       1, 1, DUMMYI, NCODE, DUMMYD, NCON,
     :                       MXSTK, WRK, STATUS )
          ENDIF
        ENDDO
      ENDIF

*   Release any extra workspace which was required.
      IF( EXTRA ) CALL TRN1_ANTMP( LOCWRK, STATUS )


*   Exit routine.
      END
