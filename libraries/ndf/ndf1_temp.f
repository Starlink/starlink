      SUBROUTINE NDF1_TEMP( TYPE, NDIM, DIM, LOC, STATUS )
*+
*  Name:
*     NDF1_TEMP

*  Purpose:
*     Create a temporary HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_TEMP( TYPE, NDIM, DIM, LOC, STATUS )

*  Description:
*     The routine creates a temporary HDS object with the specified
*     type and shape. On the first invocation a temporary structure is
*     created to contain such objects. Subsequently, temporary objects
*     are created within this enclosing structure.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS type of object to be created.
*     NDIM = INTEGER (Given)
*        Number of object dimensions.
*     DIM( * ) = INTEGER (Given)
*        Object dimensions.
*     LOC = CHARACTER * ( * ) (Returned)
*        Locator to temporary object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of DAT__NOLOC will be returned for the LOC argument if
*     this routine is called with STATUS set, although no further
*     processing will occur. The same value will also be returned if
*     the routine should fail for any reason.
*     -  This routine is a work-around to avoid the problems associated
*     with calling DAT_TEMP if the objects created must subsequently be
*     erased.

*  Algorithm:
*     -  Initialise the LOC argument, before checking the inherited
*     status.
*     -  On the first invocation, create a temporary enclosing
*     structure and tune HDS to expect a large number of components in
*     it.
*     -  Subsequently, create a unique name for the temporary object
*     required.
*     -  Create the object within the enclosing structure and obtain a
*     locator to it.
*     -  If an error occurred, then reset the LOC argument.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
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
*     AJC: A. J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     28-NOV-1989 (RFWS):
*        Added code to ensure that the value DAT__NOLOC is returned for
*        the LOC argument under error conditions.
*     22-MAR-1990 (RFWS):
*        Call HDS_TUNE to optimise the enclosing structure for temporary
*        objects to expect a large number of components.
*     20-FEB-2003 (AJC):
*        Changed COUNT and TMPLOC from saved local to global (COMMON) for
*        web services.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Data
      INCLUDE 'NDF_TMP'          ! COMMON associated with Temporary files

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZNAM ) NAME ! Temporary object name
      INTEGER DUMMY( 1 )         ! Dummy dimensions array
      INTEGER NCHAR              ! Number of characters formatted
*.

*  Initialise the LOC argument.
      LOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Increment the count of temporary objects created.
      COUNT = COUNT + 1

*  Before creating the first object, create a temporary enclosing
*  structure and tune HDS to expect a large number of components in it.
      IF ( COUNT .EQ. 1 ) THEN
         CALL DAT_TEMP( 'NDF_TEMP', 0, DUMMY, TMPLOC, STATUS )
         CALL HDS_TUNE( 'NCOMP', 20, STATUS )
      END IF

*  Form a unique name for the temporary object.
      IF ( STATUS .EQ. SAI__OK ) THEN
         NAME = 'NDF_'
         CALL CHR_ITOC( COUNT, NAME( 5 : ), NCHAR )

*  Create an object inside the enclosing structure and obtain a locator
*  to it.
         CALL DAT_NEW( TMPLOC, NAME, TYPE, NDIM, DIM, STATUS )
         CALL DAT_FIND( TMPLOC, NAME, LOC, STATUS )

*  If an error occurred, then reset the LOC argument.
         IF ( STATUS .NE. SAI__OK ) THEN
            LOC = DAT__NOLOC
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_TEMP', STATUS )

      END
