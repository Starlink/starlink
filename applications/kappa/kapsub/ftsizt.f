      SUBROUTINE FTSIZT( INSIZ, OUTSIZ, STATUS )
*+
*  Name:
*     FTSIZT

*  Purpose:
*     Calculates a dimension of the Fourier transform by trimming.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTSIZT( INSIZ, OUTSIZ, STATUS )

*  Description:
*     Calculates the size of an array dimension which can be used by
*     the Fourier-transform routines. If the input value is acceptable
*     then it is returned as the output value. Otherwise the next
*     smaller acceptable size is returned.

*  Arguments:
*     INSIZ = INTEGER (Given)
*        The input axis size, i.e. the dimension of the input array
*        to be transformed elsewhere.
*     OUTSIZ = INTEGER (Returned)
*        The output axis size.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     - All dimensions are OK with the current FFTPACK routines, so
*       just return the supplied dimension.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Mar 29 (MJC):
*        Original version based on David Berry's IMSIZE.
*     17-FEB-1995 (DSB):
*        Modified to use FFTPACK instead of NAG.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  INSIZ

*  Arguments Returned:
      INTEGER
     :  OUTSIZ

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just return the supplied dimension.
      OUTSIZ = INSIZ

      END
