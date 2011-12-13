      SUBROUTINE KPS1_FOFOR( M, N, UNTANG, DATA, WIM1, WIM2, WIM3,
     :                        STATUS )
*+
*  Name:
*     KPS1_FOFOx

*  Purpose:
*     Performs a forward FFT of a 2-d real array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FOFOx( M, N, UNTANG, DATA, WIM1, WIM2, WIM3, STATUS )

*  Description:
*     This routine performs a forward FFT of a real image.  The output
*     FFT is stored with the zero-frequency pixel at the centre of the
*     array, in either Hermitian form, or real and imaginary forms.

*  Arguments:
*     M = INTEGER (Given)
*        The number of columns per line in the data array.
*     N = INTEGER (Given)
*        The number of lines in the data array.
*     UNTANG = LOGICAL (Given)
*        If .TRUE. then the output Hermitian image is untangled into
*        separate real and imaginary parts.
*     DATA( M, N ) = ? (Given and Returned)
*        On input it is the purely real array.  On output it is the
*        Hermitian FFT.
*     WIM1( M, N ) = ? (Returned)
*        Work space, and the imaginary part of FFT if %UNTANG is .TRUE..
*     WIM2( * ) = ? (Returned)
*        Work space.  Must be at least ( 3*MAX(M,N)+15 ) elements long.
*     WIM3( M, N ) = ? (Returned)
*        Work space, and the real part of FFT if %UNTANG is .TRUE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision floating-
*     point data: replace "x" in the routine name by D or R as
*     appropriate.  The arrays supplied to the routine must have the
*     data type specified.

*  Copyright:
*     Copyright (C) 1988, 1990 Science & Engineering Research Council.
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
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 Jun 6 (DSB):
*        Original version.
*     1990 Mar 19 (MJC):
*        Converted to KAPPA and reordered the arguments.
*     6-JAN-1995 (DSB):
*        Comments re-formatted to edstar style.  Converted to use
*        FFTPACK instead of NAG.  Size of WIM2 array changed.
*     1995 March 29 (MJC):
*        Used a modern style of variable declaration.  Aligned some
*        comments.  Used conditional message reporting.
*     1995 March 30 (MJC):
*        Made generic from FFTFOR.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      INTEGER M
      INTEGER N
      LOGICAL UNTANG

*  Arguments Given and Returned:
      REAL DATA( M, N )

*  Arguments Returned:
      REAL WIM1( M, N )
      REAL WIM2( * )
      REAL WIM3( M, N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL AA                 ! Real part of FFT of (real
                                 ! part of FFT of data)
      REAL AB                 ! Real part of FFT of (imag.
                                 ! part of FFT of data)
      REAL BA                 ! Imag. part of FFT of (real
                                 ! part of FFT of data)
      REAL BB                 ! Imag. part of FFT of (imag.
                                 ! part of FFT of data)
      INTEGER FM                 ! 2 * MP1H ---  reflection axis
      INTEGER FN                 ! 2 * NP1H ---  reflection axis
      INTEGER J                  ! Column counter
      INTEGER JLIM               ! Upper limit of J in untangling
                                 ! section
      INTEGER K                  ! Line counter
      INTEGER KLIM               ! Upper limit of K in untangling
                                 ! section
      LOGICAL MEVEN              ! Whether number of columns is even
      INTEGER MHP1               ! M / 2 + 1
      INTEGER MHP2               ! M / 2 + 2
      INTEGER MM1                ! M - 1
      INTEGER MM1H               ! ( M-1 ) / 2
      INTEGER MP1H               ! ( M+1 ) / 2
      INTEGER MP3H               ! ( M+3 ) / 2
      LOGICAL NEVEN              ! Whether number of lines is even
      INTEGER NHP1               ! N / 2 + 1
      INTEGER NHP2               ! N / 2 + 2
      INTEGER NM1                ! N - 1
      INTEGER NM1H               ! ( N-1 ) / 2
      INTEGER NP1H               ! ( N+1 ) / 2
      INTEGER NP3H               ! ( N+3 ) / 2

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give an informational message.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'INFOFOR1',
     :                'Doing forward transformation', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Transform the supplied image, storing the FT in the WIM3 array.
      CALL KPG1_FFTFR( M, N, DATA, WIM2, WIM3, STATUS )

*  Set up frequently used constants.
      MM1 = M - 1
      NM1 = N - 1
      MM1H = ( M - 1 ) / 2
      MP1H = ( M + 1 ) / 2
      MP3H = ( M + 3 ) / 2
      NM1H = ( N - 1 ) / 2
      NP1H = ( N + 1 ) / 2
      NP3H = ( N + 3 ) / 2
      MHP1 = M / 2 + 1
      MHP2 = MHP1 + 1
      NHP1 = N / 2 + 1
      NHP2 = NHP1 + 1
      FN = 2 * NP1H
      FM = 2 * MP1H

*  Determine whether or not the number of columns and lines are even
*  numbers.
      IF ( MOD( N, 2 ) .EQ. 0 ) THEN
         NEVEN = .TRUE.
         KLIM = NM1
      ELSE
         NEVEN = .FALSE.
         KLIM = N
      END IF

      IF ( MOD( M, 2 ) .EQ. 0 ) THEN
         MEVEN = .TRUE.
         JLIM = MM1
      ELSE
         MEVEN = .FALSE.
         JLIM = M
      END IF

*  Swap quadrants round so that zero frequencies are at centre.
*  Array "DATA" will then hold the final Hermitian output.
      DO J = 1, MHP1

         DO K = 1, NHP1
            DATA( MM1H + J, NM1H + K ) = WIM3( J, K )
         END DO

         DO K = NHP2, N
            DATA( MM1H + J, K - NHP1 ) = -WIM3( J, K )
         END DO

      END DO

      DO J = MHP2, M

         DO K = 1, NHP1
            DATA( J - MHP1, NM1H + K ) = -WIM3( J, K )
         END DO

         DO K = NHP2, N
            DATA( J - MHP1, K - NHP1 ) = WIM3( J, K )
         END DO

      END DO

*  The next section will untangle the Hermitian form into straight-
*  forward real and imaginary images.  If the user doesn't require
*  these, then skip the next section.
      IF ( UNTANG ) THEN

*  The real and imaginary arrays are formed by linear combinations of
*  four arrays (though actually only single pixels from each are stored
*  at one time). These are the real and imaginary parts of the Fourier
*  transform of each column of the real and imaginary parts of the
*  Fourier transform of each line.  These arrays have symmetry so that
*  only one quadrant of each need be retained, and the 2-d Hermitian
*  transform obtained above consists of one quadrant from each array
*  packed together.  The unpacking is done by working through the
*  top-right quadrant co-ordinates and reflecting about both axes to
*  get the other quadrants.  Array WIM3 will hold the real array and
*  array WIM1 will hold the imaginary array.
*
*  First do the lowest line in top-right quadrant (this is an axis
*  of symmetry).
         WIM1( MP1H, NP1H ) = 0.0E0
         WIM3( MP1H, NP1H ) = DATA( MP1H, NP1H )

         DO J = MP3H, JLIM
            WIM3( J, NP1H ) = DATA( J, NP1H )
            WIM3( FM - J, NP1H ) = DATA( J, NP1H )
            WIM1( J, NP1H ) = -DATA( FM - J, NP1H )
            WIM1( FM - J, NP1H ) = DATA( FM - J, NP1H )
         END DO

         IF ( MEVEN ) THEN
            WIM1( M, NP1H ) = 0.0E0
            WIM3( M, NP1H ) = DATA( M, NP1H )
         END IF

*  Now do all the higher lines in turn.  If the number of columns
*  is even then there will be one unmatched line left at the end
*  which will be dealt with separately.
         DO K = NP3H, KLIM

            WIM3( MP1H, K ) = DATA( MP1H, K )
            WIM3( MP1H, FN - K ) = DATA( MP1H, K )
            WIM1( MP1H, K ) = -DATA( MP1H, FN - K )
            WIM1( MP1H, FN - K ) = DATA( MP1H, FN - K )

            DO J = MP3H, JLIM

*  Get the four values.
               AA = DATA( J, K )
               BB = DATA( FM - J, FN - K )
               AB = -DATA( FM - J, K )
               BA = -DATA( J, FN - K )

               WIM3( J, K ) = AA - BB
               WIM3( FM - J, K ) = AA + BB
               WIM3( J, FN - K ) = AA + BB
               WIM3( FM - J, FN - K ) = AA - BB
               WIM1( J, K ) = AB + BA
               WIM1( FM - J, K ) = -AB + BA
               WIM1( J, FN - K ) = AB - BA
               WIM1( FM - J, FN - K ) = -AB - BA
            END DO

            IF ( MEVEN ) THEN
               WIM3( M, K ) = DATA( M, K )
               WIM3( M, FN - K ) = DATA( M, K )
               WIM1( M, K ) = -DATA( M, FN - K )
               WIM1( M, FN - K ) = DATA( M, FN - K )
            END IF

         END DO

*  Now deal with any unmatched line.
         IF ( NEVEN ) THEN

            WIM3( MP1H, N ) = DATA( MP1H, N )
            WIM1( MP1H, N ) = 0.0E0

            DO J = MP3H, JLIM
               WIM3( J, N ) = DATA( J, N )
               WIM3( FM - J, N ) = DATA( J, N )
               WIM1( J, N ) = -DATA( FM - J, N )
               WIM1( FM - J, N ) = DATA( FM - J, N )
            END DO

            IF ( MEVEN ) THEN
               WIM3( M, N ) = DATA( M, N )
               WIM1( M, N ) = 0.0E0
            END IF

         END IF

*  End of untangling section.
      END IF

      END
