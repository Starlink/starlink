      SUBROUTINE TRN1_SETUP( START, STATUS )
*+
*  Name:
*     TRN1_SETUP

*  Purpose:
*     Set up (and close down) the TRANSFORM facility.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_SETUP( START, STATUS )

*  Description:
*     The routine ensures the TRANSFORM facility is either active or
*     inactive by performing a startup or closedown sequence according
*     to the setting of the logical argument START.  If the facility is
*     already in the desired state, no action is taken.  If a closedown
*     sequence is specified (START=.FALSE.) the routine will attempt to
*     execute even if STATUS is set on entry, although no error report
*     will be made if it fails under these circumstances.

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
*     9-FEB-1988 (RFWS):
*        Original version.
*     10-FEB-1988 (RFWS):
*        Added TRN1_EXCTT call.
*     9-MAY-1988 (RFWS):
*        Altered workspace allocation to accommodate precision
*        handling.
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
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Arguments Given:
      LOGICAL START             ! Select startup/closedown sequence


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
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL ACTIVE            ! Whether TRN_ system is active
      SAVE ACTIVE               ! Remember system activity status
      INTEGER IDCNT             ! Number of transformation IDs issued
      SAVE IDCNT                ! Remember ID count
      INTEGER LSTAT             ! Local status variable


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
      DATA ACTIVE / .FALSE. /   ! System is initially inactive
      DATA IDCNT / 0 /          ! The count of IDs issued is initially
                                ! zero


*.




*   If performing a startup sequence...
*   --------------------------------
      IF( START ) THEN


*   Check the facility is inactive.
        IF( .NOT. ACTIVE ) THEN


*   Check status.
          IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise the CTT size to zero in the common block.
          TRN_SZCTT = 0


*   Extend the CTT (this causes it to be created with an initial size
*   allocation which is entered into the common block).
          CALL TRN1_EXCTT( STATUS )


*   Allocate workspace for use in transforming data points.  The number
*   of elements allocated of each data type is specified by the constant
*   TRN__NWRK in the include file TRN_CONST.  A locator to the workspace
*   structure and pointers to the mapped workspace arrays are stored in
*   the common blocks.
          CALL TRN1_ALWRK( TRN__NWRK, TRN_PWRKI, TRN_PWRKR, TRN_PWRKD,
     :                     TRN_LWRK, STATUS )


*   If there is no error, flag the system as active and set the count
*   of transformation IDs issued in the common block.
          IF( STATUS .EQ. SAI__OK ) THEN
            ACTIVE = .TRUE.
            TRN_IDCNT = IDCNT
          ENDIF


*   End of "the system is inactive" condition.
        ENDIF



*   If performing a closedown sequence...
*   ----------------------------------
      ELSE


*   Check the system is active.
        IF( ACTIVE ) THEN


*   Initialise the local status variable.
          LSTAT = STATUS


*   If the CTT exists (its size in not zero), release the resources
*   associated with all the slots in the CTT.
          IF( TRN_SZCTT .NE. 0 ) THEN
            CALL TRN1_RELSL( 1, TRN_SZCTT,
     :                       %VAL( CNF_PVAL( TRN_PCTTI ) ),
     :                       %VAL( CNF_PVAL( TRN_PCTTL ) ), LSTAT,
     :                       %VAL( CNF_CVAL( DAT__SZLOC ) ) )

*   Release the top level temporary structure containing the CTT,
*   freeing the resources used by it.
            CALL TRN1_RELTS( 1, TRN_LCTT, LSTAT )
          ENDIF


*   Release the workspace allocated at startup.
          CALL TRN1_RELTS( 1, TRN_LWRK, STATUS )


*   Remember the number of transformation IDs issued (this is not reset
*   at closedown, so the count continues at the next startup).
          IDCNT = TRN_IDCNT


*   Flag the system as inactive.
          ACTIVE = .FALSE.


*   If STATUS was not set on entry, return the local status value.
          IF( STATUS .EQ. SAI__OK ) STATUS = LSTAT


*   End of "the system is active" condition.
        ENDIF


*   End of "performing a closedown sequence" condition.
      ENDIF


*   Exit routine.
      END
