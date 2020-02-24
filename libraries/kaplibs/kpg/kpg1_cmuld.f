      SUBROUTINE KPG1_CMULD( BAD, EL, A, CONST, B, NBAD, STATUS )
*+
*  Name:
*     KPG1_CMULD

*  Purpose:
*     Multiplies each element of a vectorised double precision array by
*     a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CMULD( BAD, EL, A, CONST, B, NBAD, STATUS )

*  Description:
*     The routine multiplies each element of a vectorised double
*     precision array by a constant to produce a new double precision
*     array. Bad value checking is performed if required.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad values in the input array.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     A( EL ) = DOUBLE PRECISION (Given)
*        Input array.
*     CONST = DOUBLE PRECISION (Given)
*        Constant by which each array element is to be multiplied.
*     B( EL ) = DOUBLE PRECISION (Returned)
*        Output array.
*     NBAD = INTEGER (Returned)
*        Number of bad values in the output array B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is intended for processing double-precision data
*     only. There is a related generic routine for processing other data
*     types.
*     -  This routine will handle numerical errors (i.e. overflow) by
*     assigning the appropriate "bad" value to affected output array
*     elements.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*                   Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-APR-1990 (RFWS):
*        Original version.
*     11-MAR-1991 (RFWS):
*        Changed to return the total number of bad pixels in the output
*        array.
*     11-MAR-1991 (RFWS):
*        Derived a separate version for handling double precision
*        arrays.
*     1996 May 20 (MJC):
*        Replaced LIB$ESTABLISH and LIB$REVERT calls.
*     2004 Oct 1 (TIMJ):
*        No longer use NUM_ERROR directly
*     20-FEB-2020 (DSB):
*        Call 8-byte versiuon to do the work.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      DOUBLE PRECISION A( EL )
      DOUBLE PRECISION CONST

*  Arguments Returned:
      DOUBLE PRECISION B( EL )
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 EL8
      INTEGER*8 NBAD8

*.

*  Call the 8-byte version to do the work.
      EL8 = EL
      CALL KPG1_CMUL8D( BAD, EL8, A, CONST, B, NBAD8, STATUS )
      NBAD = NBAD8

      END
