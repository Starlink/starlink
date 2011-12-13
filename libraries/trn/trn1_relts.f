      SUBROUTINE TRN1_RELTS( NSTR, LOCS, STATUS )
*+
*  Name:
*     TRN1_RELTS

*  Purpose:
*     Release temporary structures.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_RELTS( NSTR, LOCS, STATUS )

*  Description:
*     The routine releases a series of temporary structures passed by
*     HDS locator as elements of a character string array.  The
*     resources used by each structure are released and the structure
*     is deleted.  The routine assumes the structures to have a
*     standard form in which a list of active locators associated with
*     primitive components of the structure which are currently mapped
*     are held in a _CHAR component called LOCATORS.  This list is read
*     and the mapped objects are un-mapped before the structure
*     contents are erased.  The routine attempts to execute even if
*     STATUS is set on entry, although no error reports will be made if
*     it fails under these circumstances.  If more than one error
*     occurs (and STATUS was not set on entry), then a report will only
*     be made for the first error.

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
*     RFWS: R.F. Waren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988 (RFWS):
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


*  Arguments Given:
      INTEGER NSTR              ! Number of structures to release


*  Arguments Given and Returned:
      CHARACTER * ( * ) LOCS( * )
                                ! Locators to temporary structures


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER ISTR              ! Loop counter for structures
      INTEGER LP                ! Pointer to mapped list of active
                                ! locators
      INTEGER NLOC              ! Number of active locators
      INTEGER LSTAT             ! Local status variable
      INTEGER SSTAT             ! Status associated with a structure


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Initialise the local status variable and mark the error stack.
      LSTAT = SAI__OK
      CALL ERR_MARK


*   Release resources associated with each structure in turn.
      DO ISTR = 1, NSTR


*   Initialise the status for this structure and mark the error stack.
        SSTAT = SAI__OK
        CALL ERR_MARK


*   Map the list of active locators associated with the structure.
        CALL CMP_MAPV( LOCS( ISTR ), 'LOCATORS', '_CHAR', 'READ', LP,
     :                 NLOC, SSTAT )


*   If there is no error, annul all the active locators, which causes
*   the associated mapped arrays to be un-mapped.  Then un-map the
*   active locator array.
        IF( SSTAT .EQ. SAI__OK ) THEN
          CALL TRN1_ANLST( NLOC, %VAL( CNF_PVAL( LP ) ), SSTAT,
     :                     %VAL( CNF_CVAL( DAT__SZLOC ) ) )
          CALL CMP_UNMAP( LOCS( ISTR ), 'LOCATORS', SSTAT )
        ENDIF

*   Annul the structure locator, erasing the structure.
        CALL TRN1_ANTMP( LOCS( ISTR ), SSTAT )


*   If an error was detected while processing this structure but LSTAT
*   is already set, then ignore the error and annul the associated
*   error report.
        IF( SSTAT .NE. SAI__OK ) THEN
          IF( LSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( SSTAT )


*   Otherwise, transfer the status to LSTAT and allow the error report
*   to stand.
          ELSE
            LSTAT = SSTAT
          ENDIF
        ENDIF


*   Release the error stack.
        CALL ERR_RLSE


*   End of "release resources associated with each structure in turn"
*   loop.
      ENDDO


*   If an error was detected while processing any of the structures,
*   then LSTAT will be set.  However, if STATUS was set on entry, then
*   ignore LSTAT and annul the associated error report.
      IF( LSTAT .NE. SAI__OK ) THEN
        IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( LSTAT )


*   Otherwise, return the local status value and allow the error report
*   to stand.
        ELSE
          STATUS = LSTAT
        ENDIF
      ENDIF


*   Release the error stack.
      CALL ERR_RLSE


*   Exit routine.
      END
