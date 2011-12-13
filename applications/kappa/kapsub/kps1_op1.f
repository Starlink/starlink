      SUBROUTINE KPS1_OP1( PSF, IN, OUT )
*+
*  Name:
*     KPS1_OP1

*  Purpose:
*     Creates a simulated data set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_OP1( PSF, IN, OUT )

*  Description:
*     See the description of routine OPUS.

*  Arguments:
*     PSF( C1_NPX, C1_NLN ) = REAL (Given)
*        The FFT of the PSF.
*     IN( C1_NPX, C1_NLN ) = REAL (Given)
*        The input image.
*     OUT( C1_NPX, C1_NLN ) = REAL (Returned)
*        The output simulated data.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1990 (DSB):
*        Original version.
*     28-FEB-1991 (DSB):
*        Name changed from OP1 to KPS1_OP1.
*     22-FEB-1995 (DSB):
*        Re-format comments.  Remove NAG.
*     20-MAR-1995 (DSB):
*        Modify to allow use of external arrays.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'C1_COM'           ! Common block needed to communicate

*  Arguments Given:
      REAL     PSF( C1_NPX, C1_NLN )
      REAL     IN( C1_NPX, C1_NLN )

*  Arguments Returned:
      REAL     OUT( C1_NPX, C1_NLN )

*  Local Variables:
      INTEGER STATUS             ! Status value

*.

*  Set status to OK.
      STATUS = SAI__OK

*  Take the FFT of the input image, storing the result back in the array
*  IN.  The array OUT is used here as work space.
      CALL KPG1_FFTFR( C1_NPX, C1_NLN, IN, OUT, IN, STATUS )

*  Multiply the FFT of the input image by the FFT of the PSF.  Store
*  the result back in the array IN.
      CALL KPG1_HMLTR( C1_NPX, C1_NLN, IN, PSF, IN, STATUS )

*  Take the inverse FFT of the product to get the simulated data.  IN
*  is used as work space.
      CALL KPG1_FFTBR( C1_NPX, C1_NLN, IN, IN, OUT, STATUS )

      END
