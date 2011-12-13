      SUBROUTINE SUBPAR_CREDYN ( NAMECODE, HDSTYPE, NDIMS, DIMS, BOTLOC,
     :  STATUS )
*+
*  Name:
*     SUBPAR_CREDYN

*  Purpose:
*     Create internal dynamic default storage.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CREDYN ( NAMECODE, HDSTYPE, NDIMS, DIMS, BOTLOC,

*  Description:
*     Return a locator to internal dynamic default storage associated
*     with a parameter. If space doesn't already exist, create it.

*  Arguments:
*     NAMECODE=INTEGER ( given)
*        pointer to the parameter
*     HDSTYPE=CHARACTER*(*) (given)
*        type of storage to be created
*     NDIMS=INTEGER (given)
*        number of dimensions required
*     DIMS(*)=INTEGER (given)
*        size of dimensions required
*     BOTLOC=CHARACTER*(DAT__SZLOC) (returned)
*        locator to the created storage
*     STATUS=INTEGER

*  Algorithm:
*     The top-level locator to the program's private storage for dynamic
*     defaults is obtained at program activation and stored in common.
*     The required space is created beneath this locator, with the same
*     name as the parameter.
*     If the space already exists, then it is deleted and re-created.

*  Copyright:
*     Copyright (C) 1984, 1985, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1984 (BDK):
*        Original
*     06-SEP-1985 (BDK):
*        take locator to default storage from COMMON
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     10-AUG-1993 (AJC):
*        Remove INCLUDE DAT_ERR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAI Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter number

      CHARACTER*(*) HDSTYPE             ! type of storage to be created

      INTEGER NDIMS                     ! number of dimensions required

      INTEGER DIMS(*)                   ! size of dimensions required


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) BOTLOC     ! locator to the created storage

*    Status return :
      INTEGER STATUS                    ! Status Return


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      LOGICAL THERE

*.


      IF (STATUS .NE. SAI__OK) RETURN

*
*   If a dynamic default exists for the parameter, delete it.
*
      CALL DAT_THERE ( DYNLOC, PARNAMES(NAMECODE), THERE, STATUS )
      IF ( THERE ) THEN
         CALL DAT_ERASE ( DYNLOC, PARNAMES(NAMECODE), STATUS )
      ENDIF
*
*   Create the HDS storage for the dynamic values.
*
      CALL DAT_NEW ( DYNLOC, PARNAMES(NAMECODE), HDSTYPE, NDIMS, DIMS,
     :  STATUS )
      CALL DAT_FIND ( DYNLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )

      END
