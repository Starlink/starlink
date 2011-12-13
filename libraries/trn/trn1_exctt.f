      SUBROUTINE TRN1_EXCTT( STATUS )
*+
*  Name:
*     TRN1_EXCTT

*  Purpose:
*     extend the compiled transformation table (CTT).

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EXCTT( STATUS )

*  Description:
*     The routine extends the size of the compiled transformation table
*     (CTT) by a fixed amount, thus creating new free slots.  The number
*     of new slots is determined by the constant TRN_CT_SZINC in the
*     include file TRN_CONST.  If the CTT size is initially zero
*     (implying the table does not exist), then a new one is created.

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
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     9-MAY-1988:  Added CTT classification array handling (DUVAD::RFWS)
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
*     <declarations and descriptions for imported arguments>


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Constants:
      INTEGER L_NITEM
      PARAMETER ( L_NITEM = 3 ) ! Number of items in active locator list
      INTEGER L_CTTI
      PARAMETER ( L_CTTI = 1 )  ! Location of CTTI locator in LOCLST
      INTEGER L_CTTL
      PARAMETER ( L_CTTL = 2 )  ! Location of CTTL locator in LOCLST
      INTEGER L_CTTC
      PARAMETER ( L_CTTC = 3 )  ! Location of CTTC locator in LOCLST


*  Local Variables:
      INTEGER NDCTTI( 2 )       ! Dimensions of CTT integer array
      INTEGER NDCTTL( 1 )       ! Dimension of CTT locator list
      INTEGER NDCTTC( 2 )       ! Dimensions of CTT classification list
      INTEGER NELE              ! Number of elements read
      CHARACTER * ( DAT__SZLOC ) LOCLST( L_NITEM )
                                ! List of active locators


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN



*   If the current size of the CTT is zero, a new table must be created.
*   --------------------------------------------------------------------
      IF( TRN_SZCTT .EQ. 0 ) THEN


*   Create the top level temporary structure to hold the compiled
*   transformation table, storing the structure locator in the common
*   block.
        CALL TRN1_TEMP( 'TRN_CTT', 0, 0, TRN_LCTT, STATUS )


*   Create objects within this structure to contain the compiled
*   transformation table integers, locator list and classification list.
*   The initial size of the table is the same as the increment size
*   used for extending it subsequently.
        NDCTTI( 1 ) = TRN_CT_NITEM
        NDCTTI( 2 ) = TRN_CT_SZINC
        CALL DAT_NEW( TRN_LCTT, 'CTTI', '_INTEGER', 2, NDCTTI,
     :                   STATUS )
        NDCTTL( 1 ) = TRN_CT_SZINC
        CALL DAT_NEWC( TRN_LCTT, 'CTTL', DAT__SZLOC, 1, NDCTTL,
     :                    STATUS )
        NDCTTC( 1 ) = TRN__MXCLS
        NDCTTC( 2 ) = TRN_CT_SZINC
        CALL DAT_NEW( TRN_LCTT, 'CTTC', '_LOGICAL', 2, NDCTTC,
     :                   STATUS )


*   Put locators to these objects into the LOCLST array.
        CALL DAT_FIND( TRN_LCTT, 'CTTI', LOCLST( L_CTTI ), STATUS )
        CALL DAT_FIND( TRN_LCTT, 'CTTL', LOCLST( L_CTTL ), STATUS )
        CALL DAT_FIND( TRN_LCTT, 'CTTC', LOCLST( L_CTTC ), STATUS )


*   Map the table arrays, putting the pointers into the common block.
        CALL DAT_MAP( LOCLST( L_CTTI ), '_INTEGER', 'WRITE', 2,
     :                   NDCTTI, TRN_PCTTI, STATUS )
        CALL DAT_MAP( LOCLST( L_CTTL ), '_CHAR', 'WRITE', 1, NDCTTL,
     :                   TRN_PCTTL, STATUS )
        CALL DAT_MAP( LOCLST( L_CTTC ), '_LOGICAL', 'WRITE', 2,
     :                   NDCTTC, TRN_PCTTC, STATUS )


*   Create a 'LOCATORS' object and store the active locators in it.
        CALL DAT_NEWC( TRN_LCTT, 'LOCATORS', DAT__SZLOC, 1, L_NITEM,
     :                    STATUS )
        CALL CMP_PUT1C( TRN_LCTT, 'LOCATORS', L_NITEM, LOCLST, STATUS )


*   If there is no error, enter the table size in the common block and
*   initialise the new slots.
        IF( STATUS .EQ. SAI__OK ) THEN
          TRN_SZCTT = TRN_CT_SZINC
          CALL TRN1_INITL( 1, TRN_CT_SZINC,
     :                     %VAL( CNF_PVAL( TRN_PCTTI ) ), STATUS )
        ENDIF

*   If an error is detected at this point, then it is unlikely that the
*   TRN_ facility will be able to operate usefully (this should only
*   be the result of an external cause such as exceeding a system quota
*   or a protection violation).  Delete any temporary structure which
*   might have been produced before the error occurred.
        IF( STATUS .NE. SAI__OK ) CALL TRN1_RELTS( 1, TRN_LCTT, STATUS )



*   If the CTT already exists, it must be extended.
*   -----------------------------------------------
      ELSE


*   Obtain the list of active locators associated with the mapped CTT
*   data by reading the LOCATORS object.
        CALL CMP_GET1C( TRN_LCTT, 'LOCATORS', L_NITEM, LOCLST, NELE,
     :                  STATUS )


*   Un-map the associated arrays.
        CALL DAT_UNMAP( LOCLST( L_CTTI ), STATUS )
        CALL DAT_UNMAP( LOCLST( L_CTTL ), STATUS )
        CALL DAT_UNMAP( LOCLST( L_CTTC ), STATUS )


*   Increase the size of the objects which were mapped.
        NDCTTI( 1 ) = TRN_CT_NITEM
        NDCTTI( 2 ) = TRN_SZCTT + TRN_CT_SZINC
        CALL DAT_ALTER( LOCLST( L_CTTI ), 2, NDCTTI, STATUS )
        NDCTTL( 1 ) = TRN_SZCTT + TRN_CT_SZINC
        CALL DAT_ALTER( LOCLST( L_CTTL ), 1, NDCTTL, STATUS )
        NDCTTC( 1 ) = TRN__MXCLS
        NDCTTC( 2 ) = TRN_SZCTT + TRN_CT_SZINC
        CALL DAT_ALTER( LOCLST( L_CTTC ), 2, NDCTTC, STATUS )


*   Re-map the arrays, putting the pointers into the common block.
        CALL DAT_MAP( LOCLST( L_CTTI ), '_INTEGER', 'UPDATE', 2,
     :                   NDCTTI, TRN_PCTTI, STATUS )
        CALL DAT_MAP( LOCLST( L_CTTL ), '_CHAR', 'UPDATE', 1,
     :                   NDCTTL, TRN_PCTTL, STATUS )
        CALL DAT_MAP( LOCLST( L_CTTC ), '_LOGICAL', 'UPDATE', 2,
     :                   NDCTTC, TRN_PCTTC, STATUS )


*   If there is no error, enter the new table size in the common block
*   and initialise the new slots.
        IF( STATUS .EQ. SAI__OK ) THEN
          TRN_SZCTT = TRN_SZCTT + TRN_CT_SZINC
          CALL TRN1_INITL( TRN_SZCTT - TRN_CT_SZINC + 1, TRN_SZCTT,
     :                     %VAL( CNF_PVAL( TRN_PCTTI ) ), STATUS )
        ENDIF

*   If an error is detected, the CTT is likely to be un-mapped, so its
*   contents will be lost and it is unlikely that the TRN_ facility will
*   be able to continue useful operation (this should only be the result
*   of external causes such as exceeding a system quota).  There is
*   little hope of recoverng the resources associated with the lost
*   table.  Simply delete the table itself, and set its size to zero in
*   the common block (indicating the CTT does not exist).  When the CTT
*   is next accessed, a new table will then be created (if possible),
*   although it will be empty.
        IF( STATUS .NE. SAI__OK ) THEN
          CALL TRN1_RELTS( 1, TRN_LCTT, STATUS )
          TRN_SZCTT = 0
        ENDIF


*   End of "CTT already exists" condition.
      ENDIF


*   Exit routine.
      END
