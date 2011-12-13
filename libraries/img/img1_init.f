      BLOCK DATA IMG1_INIT
*+
*  Name:
*     IMG1_INIT

*  Purpose:
*     Initialise the IMG common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     This routine initialises the common blocks used internally by the
*     IMG system.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     18-FEB-1992 (RFWS):
*        Original version.
*     19-JUL-1994 (PDRAPER):
*        Added initialisation for ECB.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Write)
*           Parameter name.

      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM )
*        Extension name.
*
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Write)
*           The number of primtives located in a extension.

*  Global Data:
      DATA PCB_PARAM / IMG__MXPAR * ' ' /
      DATA ECB_XNAME / IMG__NEXTS * ' ' /
      DATA ECB_XNSTK / IMG__NEXTS * -1 /
*.

      END
* $Id$
