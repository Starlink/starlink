      SUBROUTINE TRN1_EXCML( NEWSIZ, SCTTI, SCTTL, SCTTC, STATUS )








*+
*  Name:
*     TRN1_EXCML

*  Purpose:
*     extend a compiled module list.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EXCML( NEWSIZ, SCTTI, SCTTL, SCTTC, STATUS )

*  Description:
*     The routine extends the compiled module list (CML) associated
*     with a compiled transformation.  If the initial list size is
*     zero (indicating that it does not exist), then a new temporary
*     structure is created to hold it and its compiled transformation
*     table (CTT) entries are initialised.

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
*     16-FEB-1988:  Original version (DUVAD::RFWS)
*     9-MAY-1988:  Added classification array handling (DUVAD::RFWS)
*     9-MAY-1988:  Added precision handling (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Arguments Given:
      INTEGER NEWSIZ            ! The new size required for the CML
                                ! (i.e. the number of modules it is
                                ! to contain).


*  Arguments Given and Returned:
      INTEGER SCTTI( TRN_CT_NITEM )
                                ! Slice through the CTT integer array
                                ! for the slot whose CML is to be
                                ! extended
      CHARACTER * ( * ) SCTTL   ! CTT locator to the temporary structure
                                ! containing the CML (this is a one
                                ! element slice through the CTT locator
                                ! list).
      LOGICAL SCTTC( TRN__MXCLS )
                                ! Classification array for the CTT slot


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
      INTEGER L_NITEM           ! Number of items in active locator list
      PARAMETER ( L_NITEM = 4 )
      INTEGER L_LOCCM           ! Location of locator to LOCCM object
      PARAMETER ( L_LOCCM = 1 )
      INTEGER L_PINDX           ! Location of locator to PINDX object
      PARAMETER ( L_PINDX = 2 )
      INTEGER L_NVAR            ! Location of locator to NVAR object
      PARAMETER ( L_NVAR = 3 )
      INTEGER L_IPRC            ! Location of locator to IPRC object
      PARAMETER ( L_IPRC = 4 )


*  Local Variables:
      INTEGER ICLS              ! Loop counter for indexing
                                ! classification array
      CHARACTER * (DAT__SZLOC ) LOCLST( L_NITEM )
                                ! Array of active locators for mapping
      CHARACTER * ( DAT__SZMOD ) MODE
                                ! Mode to use for mapping data


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN



*   If the number of modules in the CML is currently zero.
*   ------------------------------------------------------
*
*   It will have no mapped information associated with it.  In this
*   case, create a new temporary scalar structure to contain this
*   information.  Return the locator associated with this temporary
*   structure for storage in the CTT locator list.
      IF( SCTTI( TRN_CT_NMOD ) .EQ. 0 ) THEN
        CALL TRN1_TEMP( 'TRN_CML', 0, 0, SCTTL, STATUS )


*   Create primitive objects within this temporary structure of the
*   correct size to contain information about the compiled modules.

*   ...Object to hold locators for the compiled module structures.
        CALL DAT_NEWC( SCTTL, 'LOCCM', DAT__SZLOC, 1, NEWSIZ,
     :                    STATUS )

*   ...Object to hold pointers to the module mapped index arrays:
        CALL DAT_NEW( SCTTL, 'PINDX', '_INTEGER', 1, NEWSIZ, STATUS )

*   ...Object to hold number of coordinate variables in the data stream
*      between each pair of modules:
        CALL DAT_NEW( SCTTL, 'NVAR', '_INTEGER', 1, NEWSIZ + 1,
     :                   STATUS )

*   ...Object to hold precision codes for each module.
        CALL DAT_NEW( SCTTL, 'IPRC', '_INTEGER', 1, NEWSIZ, STATUS )


*   Set the mode to be used for mapping the information in these
*   objects.
        MODE = 'WRITE'


*   Store the active locators associated with the new objects in the
*   LOCLST array.
        CALL DAT_FIND( SCTTL, 'LOCCM', LOCLST( L_LOCCM ), STATUS )
        CALL DAT_FIND( SCTTL, 'PINDX', LOCLST( L_PINDX ), STATUS )
        CALL DAT_FIND( SCTTL, 'NVAR', LOCLST( L_NVAR ), STATUS )
        CALL DAT_FIND( SCTTL, 'IPRC', LOCLST( L_IPRC ), STATUS )


*   Create a further object called 'LOCATORS' and store this list of
*   active locators in it.
        CALL DAT_NEWC( SCTTL, 'LOCATORS', DAT__SZLOC, 1, L_NITEM,
     :                    STATUS )
        CALL CMP_PUT1C( SCTTL, 'LOCATORS', L_NITEM, LOCLST, STATUS )


*   Initialise the workspace requirement entry for the transformation
*   in the CTT.
        SCTTI( TRN_CT_MXWRK ) = 0


*   Initialise the classification array for the CCT slot to correspond
*   with an identity transformation.
        DO ICLS = 1, TRN__MXCLS
          SCTTC( ICLS ) = .TRUE.
        ENDDO
        SCTTC( TRN__NEGDT ) = .FALSE.


*   If the CML already exists and contains modules.
*   -----------------------------------------------
*
*   The arrays containing the compiled information will need extending
*   to accommodate the new information about the compiled modules.
*   Obtain a list of the active locators associated with the mapped
*   objects by reading the 'LOCATORS' object.
      ELSE
        CALL CMP_GET1C( SCTTL, 'LOCATORS', L_NITEM, LOCLST, STATUS )


*   Unmap the mapped objects.
        CALL DAT_UNMAP( LOCLST( L_LOCCM ), STATUS )
        CALL DAT_UNMAP( LOCLST( L_PINDX ), STATUS )
        CALL DAT_UNMAP( LOCLST( L_NVAR ), STATUS )
        CALL DAT_UNMAP( LOCLST( L_IPRC ), STATUS )


*   Then extend the object sizes.
        CALL DAT_ALTER( LOCLST( L_LOCCM ), 1, NEWSIZ, STATUS )
        CALL DAT_ALTER( LOCLST( L_PINDX ), 1, NEWSIZ, STATUS )
        CALL DAT_ALTER( LOCLST( L_NVAR ), 1, NEWSIZ + 1, STATUS )
        CALL DAT_ALTER( LOCLST( L_IPRC ), 1, NEWSIZ, STATUS )


*   Set the mode for re-mapping the objects.
        MODE = 'UPDATE'
      ENDIF


*   Map (or re-map) the objects, putting the new pointers into the CTT.
      CALL DAT_MAP( LOCLST( L_LOCCM ), '_CHAR', MODE, 1, NEWSIZ,
     :                 SCTTI( TRN_CT_PLOCM ), STATUS )
      CALL DAT_MAP( LOCLST( L_PINDX ), '_INTEGER', MODE, 1, NEWSIZ,
     :                 SCTTI( TRN_CT_PPIND ), STATUS )
      CALL DAT_MAP( LOCLST( L_NVAR ), '_INTEGER', MODE, 1,
     :                 NEWSIZ + 1, SCTTI( TRN_CT_PNVAR ), STATUS )
      CALL DAT_MAP( LOCLST( L_IPRC ), '_INTEGER', MODE, 1, NEWSIZ,
     :                 SCTTI( TRN_CT_PIPRC ), STATUS )


*   Exit routine.
      END
