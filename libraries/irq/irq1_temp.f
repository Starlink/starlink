      SUBROUTINE IRQ1_TEMP( TYPE, DIM, LOC, STATUS )
*+
*  Name:
*     IRQ1_TEMP

*  Purpose:
*     Create a temporary HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_TEMP( TYPE, DIM, LOC, STATUS )

*  Description:
*     The routine creates a temporary HDS object with the specified
*     type and shape. On the first invocation a temporary structure is
*     created to contain such objects. Subsequently, temporary objects
*     are created within this enclosing structure.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS type of object to be created.
*     DIM = INTEGER (Given)
*        Object dimension (zero for a scalar)
*     LOC = CHARACTER * ( * ) (Returned)
*        Locator to temporary object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A blank string will be returned for the LOC argument if this
*     routine is called with STATUS set, although no further processing
*     will occur. The same value will also be returned if the routine
*     should fail for any reason.
*     -  This routine is a work-around to avoid the problems associated
*     with calling DAT_TEMP if the objects created must subsequently be
*     erased.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (DSB):
*        Original, derived from the equivalent AIF_ routine.
*     4-JUL-2018 (DSB):
*        Ensure the enclosing HDS object ois locked for access by the
*        current thread. Accessing the SAVEd variables will be safe
*        because multi-threaded invocations of all F77 functions are
*        serialised by mutex locking in the F77 macros.
*     24-OCT-2019 (DSB):
*        Now a wrapper around IRQ1_TEMP8.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER DIM

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER*8 DIM8
*.

      DIM8 = DIM
      CALL IRQ1_TEMP8( TYPE, DIM8, LOC, STATUS )

      END
