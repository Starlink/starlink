      INTEGER FUNCTION GAI1_BSWAPUB( VALUE )
*+
*  Name:
*     GAI1_SWAPUB

*  Purpose:
*     Byte swap a byte data value, available for completeness - no effect.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPUB ( VALUE )

*  Description:
*     This routine returns the data value. It is part of the
*     GAI1_BSWAP<T> suite of routines and is included merely for
*     completeness and convenience when generating generic routines that
*     make use of all numeric data types.

*  Arguments:
*     VALUE = BYTE (Given)
*        The value to byte swap.

*  Notes:
*     -  Not portable, assumes size of Fortran variables and uses
*        non-standard data types.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JAN-2000 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      BYTE VALUE

*.
      GAI1_BSWAPUB = VALUE
      RETURN
      END
