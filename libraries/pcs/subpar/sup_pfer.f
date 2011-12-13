      SUBROUTINE SUBPAR_PFER( STATUS )
*+
*  Name:
*     SUBPAR_PFER

*  Purpose:
*     To report a system dependent message on being unable to open
*     the task parameter file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PFER( STATUS )

*  Description:
*     This outputs a message relevant to UNIX ADAM

*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.

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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1992 (AJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*.

      CALL EMS_REP( 'SUP_PFER1',
     : 'Have you created directory $HOME/adam or $ADAM_USER ?',
     : STATUS )

      END
