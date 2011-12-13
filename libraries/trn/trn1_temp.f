      SUBROUTINE TRN1_TEMP( TYPE, NDIM, DIM, LOC, STATUS )
*+
*  Name:
*     TRN1_TEMP

*  Purpose:
*     Create temporary object.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_TEMP( TYPE, NDIM, DIM, LOC, STATUS )

*  Description:
*     This routine is a substitute for the DAT_TEMP routine: it
*     performs the same function, but in a different manner.  It also
*     implements error reporting.

*  Algorithm:
*     The first time the routine is called, it calls DAT_TEMP to create
*     a temporary structure.  It then creates a temporary object inside
*     this structure and returns a locator to it.  On subsequent calls,
*     further temporary objects are created within the same structure,
*     with unique names generated from a counter which increments on
*     every call.  This means that all objects created by this routine
*     lie within the same HDS structure inside a temporary container
*     file.  These objects may be deleted when they have been finished
*     with (e.g. with the TRN1_ANTMP routine) without confusing HDS
*     (this happens if DAT_TEMP is used directly to create objects
*     which are subsequently deleted).

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     24-MAR-1988:  Original version (DUVAD::RFWS)
*     13-MAY-1988:  Discovered the problems with HDS temporary objects
*        - re-wrote this routine to cope (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE    ! As for DAT_TEMP...
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS            ! Error status

*  Local Variables:
      INTEGER COUNT             ! Number of times this routine is called
      SAVE COUNT
      INTEGER NCH               ! Number of characters in formatted
                                ! COUNT
      CHARACTER * ( DAT__SZNAM ) NAME
                                ! Object name
      CHARACTER * ( DAT__SZLOC ) TMPLOC
      SAVE TMPLOC               ! Locator to HDS temporary structure to
                                ! contain objects created by this
                                ! routine

*  Local Data:
      DATA COUNT / 0 /          ! Initialise COUNT
      DATA NAME / 'TRN_' /      ! Prefix for object names

*.

*  Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Increment the number of calls count.
      COUNT = COUNT + 1

*  If this is the first call, create a temporary structure to hold
*  subsequently created objects.  Report any errors.
      IF( COUNT .EQ. 1 ) THEN
        CALL DAT_TEMP( 'TRN_TEMPORARY', 0, 0, TMPLOC, STATUS )
        IF( STATUS .NE. SAI__OK )
     :    CALL TRN1_ERROR( 'DAT_TEMP', ' ', STATUS )
      ENDIF

*  If there is no error, create a unique object name.
      IF( STATUS .EQ. SAI__OK ) THEN
        CALL CHR_ITOC( COUNT, NAME( 5 : ), NCH )

*  Create the object and obtain a locator to it.
        CALL DAT_NEW( TMPLOC, NAME, TYPE, NDIM, DIM, STATUS )
        CALL DAT_FIND( TMPLOC, NAME, LOC, STATUS )
      ENDIF

*  Exit routine.
      END
