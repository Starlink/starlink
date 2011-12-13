      SUBROUTINE TRN_COMP( LOCTR, FORWD, IDT, STATUS )
*+
*  Name:
*     TRN_COMP

*  Purpose:
*     Compile transformation.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_COMP( LOCTR, FORWD, IDT, STATUS )

*  Description:
*     The routine compiles a transformation structure passed by HDS
*     locator and returns an integer identifier for the compiled
*     transformation.

*  Arguments:
*     LOCTR = CHARACTER * ( * ) (Given)
*        HDS locator to transformation structure.
*     FORWD = LOGICAL (Given)
*        Select forward/inverse transformation direction.
*     IDT = INTEGER (Returned)
*        ID for the compiled transformation.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Ensure the TRANSFORM facility is active.
*     - Validate the transformation structure.
*     - Obtain a free slot (and associated ID) from the compiled
*       transformation table (CTT), extending the table if necessary.
*     - Compile the transformation.
*     - Reset the ID if the compilation failed.

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
*     17-AUG-1988 (RFWS):
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


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure

      LOGICAL FORWD             ! Select forward/inverse transformation


*  Arguments Returned:
      INTEGER IDT               ! ID for compiled transformation


*  Status:
      INTEGER STATUS            ! Error status


*  Global Variables:
      INCLUDE 'TRN_CMN'         ! TRN_ common blocks


*  Local Variables:
      INTEGER SLOT              ! CTT slot number


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Ensure the TRANSFORM facility is active.
      CALL TRN1_SETUP( .TRUE., STATUS )


*   Validate the transformation structure.
      CALL TRN1_VTR( LOCTR, STATUS )


*   Get a slot number in the compiled transformation table (CTT).
      SLOT = 0
      CALL TRN1_GETSL( %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT, IDT,
     :                 STATUS )

      IF( SLOT .EQ. 0 ) THEN
        CALL TRN1_EXCTT( STATUS )
        CALL TRN1_GETSL( %VAL( CNF_PVAL( TRN_PCTTI ) ), SLOT, IDT,
     :                   STATUS )
      ENDIF

*   Compile the transformation.
      CALL TRN1_XCMPSL( %VAL( CNF_PVAL( TRN_PCTTL ) ), FORWD, SLOT,
     :                  %VAL( CNF_PVAL( TRN_PCTTI ) ),
     :                  LOCTR, %VAL( CNF_PVAL( TRN_PCTTC ) ), STATUS,
     :                  %VAL( CNF_CVAL( DAT__SZLOC ) ) )

*   If the compilation failed, reset the IDT value.
      IF( STATUS .NE. SAI__OK ) IDT = TRN__NOID


*   Exit routine.
      END

*  Dummy routine to permute the argument order of TRN1_CMPSL when
*  passing mapped character values.
      SUBROUTINE TRN1_XCMPSL( CTTL, FORWD, SLOT, CTTI, LOCTR, CTTC,
     :                        STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'TRN_PAR'
      INCLUDE 'TRN_CONST'
      INCLUDE 'TRN_CMN'

      CHARACTER * ( * ) CTTL( TRN_SZCTT )
      LOGICAL FORWD
      INTEGER SLOT
      INTEGER CTTI( TRN_CT_NITEM, TRN_SZCTT )
      CHARACTER * ( * ) LOCTR
      LOGICAL CTTC( TRN__MXCLS, TRN_SZCTT )
      INTEGER STATUS

      CALL TRN1_CMPSL( LOCTR, FORWD, SLOT, CTTI, CTTL, CTTC, STATUS )

      END
