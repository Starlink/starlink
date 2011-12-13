      BLOCK DATA FIOPA_BLK
*+
*  Name:
*     FIOPA_BLK

*  Purpose:
*     FIO Block Data Initialisation

*  Language:
*     Starlink Fortran 77

*  Description:
*     Initialise the FIOGOPA_CMN Common Block to force activation
*     of FIO parameter system.

*  Copyright:
*     Copyright (C) 2002 CCLRC

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
*     AJC: A.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     31-JUL-2002 (AJC):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'FIOGOPA_CMN'        ! FIO Initialisation Switches
*        FIOSLP = LOGICAL (Returned)
*           Is package asleep?

*  Global data:
      DATA FIOSLP /.TRUE./

*.

      END
