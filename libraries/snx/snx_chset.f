      SUBROUTINE snx_CHSET (I)
*+
*  Name:
*     CHSET

*  Purpose:
*     Select character set

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine sets the variable MODE in the NCAR labelled
*     COMMON block /PUSER/.

*  Arguments:
*     I = INTEGER (Given)
*         Character set: 1='duplex', 2='complex'

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council. All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      INTEGER I

*  NCAR labelled COMMON block
      INTEGER MODE
      COMMON /PUSER/ MODE


      MODE=I

      END
