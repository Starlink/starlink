      SUBROUTINE TROPUS( K, J )
*+
*  Name:
*     TROPUS

*  Purpose:
*     Creates an image using the transposed response matrix.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TROPUS( K, J )

*  Description:
*     This routine calls a lower level routine to do the work.  The
*     data are smoothed with the PSF to produce the output image.  This
*     is done in Fourier space using the FFT of the PSF stored in file
*     <3>.  The output image has a blank margin around it to reduce
*     wrap-around effects at the image edges.

*  Arguments:
*     K = INTEGER (Given)
*        The internal file number holding the data set from which the
*        image is to be created.
*     J = INTEGER (Given)
*        The internal file number to hold the created image.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1990 (DSB):
*        Original version.
*      4-MAR-1991 (DSB):
*        Subroutine name changes incorporated.
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

*  Call a lower level routine to do the work.  Find the MEMSYS3 areas
*  either in a dynamically allocated external work array, or in
*  internal memory within common block /MECOMS/.  Area <3> holds the FFT
*  of the PSF.
      IF ( C1_WEXT ) THEN
         CALL KPS1_TROP1( %VAL( C1_IP( 3 ) ), %VAL( C1_IP( K ) ), 
     :                    %VAL( C1_IP( J ) ) ) 

*  Pad the external area out with zeros.  This puts zeros between the
*  last image pixel and the end of the area (areas can be bigger than
*  images because of the need to have an integer number of buffers in
*  each area).  This simulates a response function of zero at these
*  pixels.
         CALL KPS1_MEMFX( ME_MK * ME_NK, C1_NPX * C1_NLN, 
     :                    %VAL( C1_IP( J ) ) )

      ELSE 
         CALL KPS1_TROP1( ME_ST( ME_KB( 3 ) ), ME_ST( ME_KB( K ) ), 
     :                    ME_ST( ME_KB( J ) ) ) 

      END IF

      END
