      SUBROUTINE OPUS( J, K )
*+
*  Name:
*     OPUS

*  Purpose:
*     Create a simulated data set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OPUS( J, K )

*  Description:
*     This routine calls a lower level routine to do the work.  The data
*     is produced by smoothing the input image with the PSF.  This is
*     done in Fourier space using the FFT of the PSF stored in file <3>.
*     The input image will have a blank margin around it to reduce
*     wrap-around effects at the image edges. 

*  Arguments:
*     J = INTEGER (Given)
*        The internal file number holding the image from which data is
*        to be simulated.
*     K = INTEGER (Given)
*        The internal file number to hold the simulated data set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1990 (DSB):
*        Original version.
*     20-MAR-1995 (DSB):
*        Modified to allow use of external arrays.
*     1995 April 7 (MJC):
*        Minor stylistic changes, and shortened long lines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ME_COM'           ! Common blocks required by MEMSYS3.
      INCLUDE 'C1_COM'           ! Common blocks use to communicate with
                                 ! MEM2D

*  Arguments Given:
      INTEGER  J
      INTEGER  K

*.

*  Call a lower-level routine to do the work.  Find the MEMSYS3 areas
*  either in a dynamically allocated external work array, or in
*  internal memory within common block /MECOMS/.  Area <3> holds the
*  FFT of the PSF.
      IF ( C1_WEXT ) THEN
         CALL KPS1_OP1( %VAL( C1_IP( 3 ) ), %VAL( C1_IP( J ) ), 
     :                  %VAL( C1_IP( K ) ) ) 

*  Pad the external area out with zeros.  This puts zeros between the
*  last image pixel and the end of the area (areas can be bigger than
*  images because of the need to have an integer number of buffers in
*  each area).  This simulates a response function of zero at these
*  pixels.
         CALL KPS1_MEMFX( ME_MK * ME_NK, C1_NPX * C1_NLN, 
     :                    %VAL( C1_IP( K ) ) )

      ELSE 
         CALL KPS1_OP1( ME_ST( ME_KB( 3 ) ), ME_ST( ME_KB( J ) ), 
     :                  ME_ST( ME_KB( K ) ) ) 

      END IF

      END

      subroutine FIX( N, D, E )
      implicit none
      integer N, E, I
      real D(N)

      DO I = E + 1, N
         D( I ) = 0.0
      end do

      end
