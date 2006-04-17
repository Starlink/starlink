      SUBROUTINE MSG1_PTINF( INF )
*+
*  Name:
*     MSG1_PTINF

*  Purpose:
*     Set the value of element MSGINF in the MSG_CMN common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_PTINF( INF )

*  Description:
*     This routine sets the value of element MSGINF in the MSG_CMN 
*     common block. This should be used instead of directly accessing the
*     common block since access from a different shared library may result in
*     the common block value being uninitialised by the corresponding BLOCK DATA
*     module.

*  Arguments:
*     INF = INTEGER (Given)
*        The new value for MSGINF.

*  Notes:
*     - This routine attempts to execute even if an error has previous
*     occurred.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-2004 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
      IMPLICIT NONE                     
      INCLUDE 'MSG_CMN'                 
      INTEGER INF

      MSGINF = INF

      END
