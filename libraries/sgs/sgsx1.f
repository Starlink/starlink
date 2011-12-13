      PROGRAM SGSX1

*+
*  Name:
*     sgsx1

*  Purpose:
*     USES SGS PACKAGE TO DRAW BOX WITH 'STARLINK' WRITTEN IN IT

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      CHARACTER*20 WKSTN



*  GET WORKSTATION TYPE & NUMBER
      PRINT *, 'WORKSTATION?'
      READ (*,'(A)') WKSTN

*  OPEN
      CALL SGS_OPEN(WKSTN,IZONID,J)

*  DECLARE A SQUARE ZONE
      CALL SGS_ZSHAP(1.0,'BL',IZ,J)

*  BOX
      CALL SGS_BOX(0.1,0.9,0.4,0.6)

*  MESSAGE
      CALL SGS_SHTX(0.1)
      CALL SGS_STXJ('CC')
      CALL SGS_SFONT(106)
      CALL SGS_BTEXT(0.5,0.5)
      CALL SGS_ATEXT('STARLINK')

*  WRAP UP
      CALL SGS_CLOSE

      END
