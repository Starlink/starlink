      SUBROUTINE SUBPAR_PUT(LOC, TYPE, NDIM, DIMS, VALUES, STATUS)
*+
*  Name:
*     NAME

*  Purpose:
*     SUBPAR_PUT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUT(LOC, TYPE, NDIM, DIMS, VALUES, STATUS)

*  Description:
*     Put data into the HDS primitive item described by locator
*     copying the values from the character string array
*     VALUES.

*  Arguments:
*     LOC  =  CHARACTER*(*) (given)
*        locator to component to be written
*     TYPE =  CHARACTER*(*) (given)
*        HDS type name of the primitive item
*     NDIM =  INTEGER (given)
*        Number of dimensions of data to be written
*     DIMS(NDIM) = INTEGER (given)
*        Array containing dimensions of data
*     VALUES(*) = CHARACTER*(*) (given)
*        Array containing the values to be written
*     STATUS = INTEGER
*        Status return

*  Algorithm:
*     Call DAT_PUTC.
*     This routine could be replaced by direct calls to
*     DAT_PUTC.

*  Implementation Deficiencies:
*     The dimensions must match that of the HDS item.

*  Copyright:
*     Copyright (C) 1987, 1991, 1993 Science & Engineering Research Council.
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
*     JAB: J A Bailey (AAO)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     05-JAN-1987 (JAB):
*        Original version
*     16-JUL-1991 (AJC):
*        Use HDS conversion remove separate LIB$CVT_DX_DX
*        conversion for each type
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) TYPE
      INTEGER NDIM
      INTEGER DIMS(*)
      CHARACTER*(*) VALUES(*)


*  Status:
      INTEGER STATUS

*.


*     If STATUS OK on entry,
*     store the values, converted to the correct type, in the HDS object
      IF (STATUS .EQ. SAI__OK) THEN

          CALL DAT_PUTC( LOC, NDIM, DIMS, VALUES, STATUS)

      ENDIF

      END
