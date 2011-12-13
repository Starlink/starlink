      BLOCK DATA LPG1_INIT
*+
*  Name:
*     LPG1_INIT

*  Purpose:
*     Initialise the LPG common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'LPG_CONST'        ! LPG private constants.
      INCLUDE 'GRP_PAR'          ! GRP constant.

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG common blocks.

*  Global Data:
      DATA PNAME  / LPG__MXPAR*' ' /
      DATA PNAME2 / LPG__MXPAR*' ' /
      DATA IGRP   / LPG__MXPAR*GRP__NOID /
      DATA SIZE   / LPG__MXPAR*0 /
      DATA NPAR   /0/
      DATA NPAR2  /0/
      DATA NRUN   /0/
      DATA OLD    /LPG__MXPAR*.FALSE. /
      DATA REP    /LPG__MXPAR*.FALSE. /
      DATA VERB   /.FALSE./
      DATA DISAB  /.TRUE./
*.

      END
