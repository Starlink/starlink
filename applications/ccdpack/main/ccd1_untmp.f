      SUBROUTINE CCD1_UNTMP( ID, STATUS )
*+
*  Name:
*     CCD1_UNTMP

*  Purpose:
*     Unmaps temporary workspace created by CCD1_MKTMP and mapped in by
*     CCD1_MPTMP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_UNTMP( ID, STATUS )

*  Description:
*     Use this routine when unmapping in non-NDF temporary disk-file
*     space allocated by CCD1_MKTMP and mapped by CCD1_MPTMP. If the
*     identifier is replaced by -1 then all currently mapped in
*     workspace is unmaped.

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier to the workspace which is to be unmapped.
*        (Returned by CCD1_MPTMP). If this is -1 then all workspace
*        is unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine works regardless of the STATUS and reports no
*     errors. This routine should not be called before CCD1_MKTMP which
*     initialises the common block - unless status is set.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Global Variables:
      INCLUDE 'CCD1_TMPCM'       ! Temporary workspace common block
*        CCD1_TMPPO( CCD1__MXPNT ) = INTEGER (Read and Write)
*           Array of pointers to any data which is allocated. This is
*           updated with the value of the pointer on mapping and the
*           value is removed on unmapping.
*        CCD1_TMPLO( CCD1__MXPNT ) = CHARACTER (Read)
*           Array of locators to any data objects created by this
*           routine. This is updated when objects are annulled.

*  Arguments Given:
      INTEGER ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Begin a new error context.
      CALL ERR_BEGIN( STATUS )

*  Check for the unmap everything case.
      IF ( ID .LE. 0 ) THEN

*  Unmap all workspace.
         DO 1 I = 1, CCD1__MXPNT
            IF ( CCD1_TMPPO( I ) .NE. -1 ) THEN
               CALL DAT_UNMAP( CCD1_TMPLO( I ), STATUS )
               CCD1_TMPPO( I ) =  -1
            END IF
 1       CONTINUE
      ELSE

*  Try to unmap it
         CALL DAT_UNMAP( CCD1_TMPLO( ID ), STATUS )
         CCD1_TMPPO( ID ) =  -1
      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
* $Id$
