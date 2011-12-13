      SUBROUTINE KPS1_WIEPW( EL, M, N, FILL, BAD, FILE_6, FILE_2,
     :                       FILE_5, STATUS )
*+
*  Name:
*     KPS1_WIEPW

*  Purpose:
*     Replaces an image with its 2-d power spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WIEPW( EL, M, N, FILL, BAD, FILE_6, FILE_2, FILE_5,
*                      STATUS )

*  Description:
*     Any bad pixels in file 6 are first replaced with the supplied
*     fill value.  The Fourier transform of file 6 is then obtained,
*     and multiplied by its own complex conjugate to get the modulus
*     squared of the FT.  This is an approximation to the 2-d power
*     spectrum of the image.

*  Arguments:
*     EL = INTEGER (Given)
*        Total number of elements in each internal file.
*     M = INTEGER (Given)
*        Number of columns in the 2-d form of each internal file.
*     N = INTEGER (Given)
*        Number of rows in the 2-d form of each internal file.
*     FILL = REAL (Given)
*        The value to replace bad values within file 6.
*     BAD = LOGICAL (Given)
*        It is .TRUE. if there are any bad pixels in file 6.
*     FILE_6( EL ) = REAL (Given and Returned)
*        On input it is the mode image; on return it is the 2-d power
*        spectrum for the model image.
*     FILE_2( EL ) = REAL (Returned)
*        Work space.
*     FILE_5( EL ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Shortened long lines and minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      INTEGER M
      INTEGER N
      REAL FILL
      LOGICAL BAD

*  Arguments Given and Returned:
      REAL FILE_6( EL )

*  Arguments Returned:
      REAL FILE_2( EL )
      REAL FILE_5( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element count

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are any bad values in the supplied file, replace them with
*  the supplied fill value.
      IF ( BAD ) THEN

         DO I = 1, EL
            IF ( FILE_6( I ) .EQ. VAL__BADR ) FILE_6( I ) = FILL
         END DO

      END IF

*  Take the FFT of file 6 storing the results in file 5.  File 2 is
*  used as work space.
      CALL KPG1_FFTFR( M, N, FILE_6, FILE_2, FILE_5, STATUS )

*  Copy file 5 to file 6.
      DO I = 1, EL
         FILE_6( I ) = FILE_5( I )
      END DO

*  Replace file 5 with its complex conjugate.
      CALL KPG1_HCONR( M, N, FILE_5, STATUS )

*  Multiply the FT of the input image (file 6) by the complex conjugate
*  of the FT of the input image (file 5), to get the 2-d power spectrum
*  for the input image (in file 6).
      CALL KPG1_HMLTR( M, N, FILE_6, FILE_5, FILE_6, STATUS )

      END
