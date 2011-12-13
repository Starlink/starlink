      REAL FUNCTION CCD1_RNORM( A, B, STATUS )
*+
*  Name:
*     CCD1_RNORM

*  Purpose:
*     To return a pseudo-random real number taken from a normal
*     ( Gaussian ) distribution, with mean A and standard deviation B.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = RANNORM( A, B, STATUS )

*  Description:
*     The routine calls the PDA library function PDA_DRNOR to return the
*     pseudo random number.  Before the first call of this routine,
*     PDA_DSTART must have been called to initialise the random number
*     generator.

*  Arguments:
*     A = REAL (Given)
*        The mean of the normal distribution.
*     B = REAL (Given)
*        The standard deviation of the distribution.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     RANORM = REAL
*        An random number from the normal distribution, with mean A and
*        standard deviation B.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996, 2001, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     19-SEP-1996 (PDRAPER):
*        Now uses PDA routine instead of NAG routine.
*     15-MAR-2001 (MBT):
*        Removed call to PDA_DSTART.
*     14-JUL-2004 (TIMJ):
*        PDA_DRNOR now PDA_DRANN
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL A
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PDA_DRANN
      DOUBLE PRECISION PDA_DRANN ! Pseudo random normal number generator

*  Local Variables:
      DOUBLE PRECISION AA        ! buffer for A
      DOUBLE PRECISION BB        ! buffer for B
      INTEGER ISEED              ! seed value for generator
*.

      AA = DBLE( A )
      BB = DBLE( B )
      CCD1_RNORM = REAL( PDA_DRANN() ) * BB + AA

      END
* $Id$
