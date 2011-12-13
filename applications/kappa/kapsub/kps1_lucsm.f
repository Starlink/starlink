      SUBROUTINE KPS1_LUCSM( N, NPIX, NLIN, WLIM, TRANS, PSF, IN,
     :                       MASK, WORK, STATUS )
*+
*  Name:
*     KPS1_LUCSM

*  Purpose:
*     Smooths an array potentially containing bad pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCSM( N, NPIX, NLIN, WLIM, TRANS, PSF, IN,
*                      MASK, WORK, STATUS )

*  Description:
*     A mask is created holding 1 at every good data pixel, and zero at
*     every bad data pixel.  This mask is smoothed (by multiplying the
*     Fourier transforms of the mask and the PSF) to obtain the weight
*     of the good input pixels which contribute to each output pixel (a
*     value of 1.0 in the smoothed mask array means all contributing
*     input pixels were good, and a value of 0.0 means none of the
*     contributing pixels were good).  The bad values in the supplied
*     data array are replaced by zero and the data array is then
*     smoothed.  The effect of the differing number of contributing
*     pixels is then normalised out by dividing the smoothed data array
*     by the smoothed mask array.  Output pixels for which less than a
*     fraction WLIM of the input contibuting input pixels were good are
*     set bad in the returned array.
*
*     If the input data array does not, in fact, contain any bad
*     pixels, then it is just smoothed normally without creating the
*     associated mask (which would contain 1.0 at every pixel if it
*     were to be created).  This reduces the execution time by about
*     50%.

*  Arguments:
*     N = INTEGER (Given)
*        The number of elements in each internal file.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in each internal file.
*     NLIN = INTEGER (Given)
*        The number of lines in each internal file.
*     WLIM = REAL (Given)
*        The weight limit for good pixels.
*     TRANS = LOGICAL (Given)
*        If .TRUE. the array is to be smoothed with the transposed PSF.
*     PSF( N ) = REAL (Given)
*        The Fourier transform of the PSF.
*     IN( N ) = REAL (Given and Returned)
*        The array to be smoothed.
*     MASK( N ) = REAL (Returned)
*        Work space.
*     WORK( N ) = REAL (Returned)
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
*     27-FEB-1995 (DSB):
*        Original version.
*     1995 April 6 (MJC):
*        Corrected typo's and shortened long lines.  Minor stylistic
*        changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER N
      INTEGER NPIX
      INTEGER NLIN
      REAL    WLIM
      LOGICAL TRANS
      REAL    PSF( N )

*  Arguments Given and Returned:
      REAL    IN( N )

*  Arguments Returned:
      REAL    MASK( N )
      REAL    WORK( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BAD                ! Are there any bad values in the data?
      INTEGER I                  ! Element count

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a mask image containing 1 at every good input pixel and 0
*  at every bad pixel.  Also, replace bad input values with zero.
*  Copy the modified input array to the WORK array.
      BAD = .FALSE.

      DO I = 1, N
         IF ( IN( I ) .EQ. VAL__BADR ) THEN
            MASK( I ) = 0.0
            WORK( I ) = 0.0
            BAD = .TRUE.
         ELSE
            MASK( I ) = 1.0
            WORK( I ) = IN( I )
         END IF
      END DO

*  Smooth the modified input array, with the transposed PSF or the
*  non-transposed PSF, as required.  Store the output back in IN.
      IF ( TRANS ) THEN
         CALL KPS1_TROP2( NPIX, NLIN, PSF, WORK, IN, STATUS )
      ELSE
         CALL KPS1_OP2( NPIX, NLIN, PSF, WORK, IN, STATUS )
      END IF

*  If there are any bad values in the supplied array, smooth the mask
*  array, with the transposed PSF or the non-transposed PSF, as
*  required.  Store the output in WORK.
      IF ( BAD ) THEN
         IF ( TRANS ) THEN
            CALL KPS1_TROP2( NPIX, NLIN, PSF, MASK, WORK, STATUS )
         ELSE
            CALL KPS1_OP2( NPIX, NLIN, PSF, MASK, WORK, STATUS )
         END IF

*  Normalise the smoothed input array by dividing it by the smoothed
*  mask array.  If the smoothed mask value is too low, set the output
*  value bad.  The output values are returned in IN.
         DO I = 1, N
            IF ( WORK( I ) .GT. WLIM ) THEN
               IN( I ) = IN( I ) / WORK( I )
            ELSE
               IN( I ) = VAL__BADR
            END IF
         END DO

      END IF

      END
