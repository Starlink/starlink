      SUBROUTINE KPG1_COPY( TYPE, NEL, IPIN, IPOUT, STATUS )
*+
*  Name:
*     KPG1_COPY

*  Purpose:
*     Copies an array of a given type to another array of the same type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_COPY( TYPE, NEL, IPIN, IPOUT, STATUS )

*  Description:
*     This routine copies a one-dimensional array of numerical or
*     character values from an input array to an output array.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be copied. Must be one of the HDS numeric
*        types, _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _INT64, _REAL or
*         _DOUBLE, or "_CHAR*<N>)" where "<N>" is an integer giving the
*        length of the character strings.
*     NEL = INTEGER (Given)
*        The number of elements in the vectorised arrays pointed to by
*        IPIN and IPOUT.
*     IPIN = INTEGER (Given)
*        Pointer to the data to be copied.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to the array to contain the copied data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008, 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David S. Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PDRAPER):
*        Original version.
*     29-SEP-2005 (PDRAPER):
*        Converted into KAPLIBS routine.
*     21-JAN-2008 (DSB):
*        Added support for copying character arrays.
*     2012-05-09 (TIMJ):
*        Add _INT64
*     20-FEB-2020 (DSB):
*        Call 8-byte version to do the work.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER NEL
      INTEGER IPIN

*  Arguments Given and Returned:
      INTEGER IPOUT

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variable:
      INTEGER*8 NEL8
*.

*  Convert INTEGER to INTEGER*8 and call the 8-byte version.
      NEL8 = NEL
      CALL KPG1_COPY8( TYPE, NEL8, IPIN, IPOUT, STATUS )

      END
