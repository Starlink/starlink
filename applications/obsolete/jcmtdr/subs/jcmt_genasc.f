      SUBROUTINE JCMT_GENASCONFN (BSEP, PIXSEP, NPIX, UNBAL, NCFN,
     :   CONF)
*+
*  Name:
*     JCMT_GENASCONFN

*  Purpose:
*     Generate a convolution function to remove the chop function from
*     Dual Beam maps.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GENASCONFN (BSEP, PIXSEP, NPIX, UNBAL, NCFN, CONF)

*  Description:
*     This routine will return the asymmetric convolution function
*     required for deconvolving a dual beam map into a single beam map.
*     It is the convolution function that is derived in Emerson Klein
*     and Haslam (1979) Astron. Astrophys. 76 p92 paper.
*
*    For the case where UNBAL = 1, the two beams are of equal strength:-
*
*     The ideal convolution function would be the one whose FT was the
*     inverse of the FT of the chop function. Unfortunately, the chop
*     function FT has zeroes in it, at which the inverse FT will tend to
*     infinity. The ideal convolution function, therefore, does not exist.
*     This problem is avoided by generating a function whose FT is the
*     same as the ideal function except at the problem points, where it is
*     set to zero. This function consists of a series of delta functions
*     separated by the chop spacing, the central 2 points being half the
*     chop spacing on either side of the centre of the function.
*
*     The function is normalised such that convolving this function with
*     an original chop function of 2 delta functions of unit height will
*     give a delta function of height 2.
*
*    Don't understand what happens when UNBAL <> 1, but the code has left
*    doing the same as NOD2.
*
*     The convolution must cover the map even when the centre of the function
*     is at the left or right hand extremity of the map. Hence the convolution
*     function must be twice the length of the raw map.
*
*     Since the raw data is not sampled such that the chop spacing is an
*     integer number of samples, the actual convolution function must be
*     rebinned onto the sample mesh by sinc interpolation.
*
*     This routine is essentially a rewritten version of CONF22 from the
*     RESTOR program in the NOD2 package by Haslam (1974) Astron.
*     Astrophys. Suppl. 15 p333

*  Arguments:
*     BSEP = DOUBLE PRECISION (Given)
*        The beam separation in arcseconds
*     PIXSEP = DOUBLE PRECISION (Given)
*        The pixel separation in arcseconds
*     NPIX = INTEGER (Given)
*        The number of pixels in the x direction
*     UNBAL = REAL (Given)
*        The relative amplitudes of the right and left hand beams
*        UNBAL=amp(lhb)/abs(amp(rhb))
*     NCFN = INTEGER (Returned)
*        The length of the convolution array
*     CONF( * ) = REAL (Returned)
*        The convolution function

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1990 (JBVAD::PAH):
*        Original version.
*      3-MAY-1991 (REVAD::JFL): Comments and description enlarged.
*      8-MAY-1991 (REVAD::JFL): Convolution function reversed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'ASTRO_PAR'        ! astronomical constants

*  Arguments Given:
      DOUBLE PRECISION BSEP, PIXSEP
      INTEGER NPIX
      REAL UNBAL

*  Arguments Returned:
      REAL CONF( * )
      INTEGER NCFN


*  Local Variables:
      REAL AUNBAL                ! absolute value of unbal
      REAL PIXBSEP               ! the beam separation measured in
                                 !  pixels
      INTEGER NDELTA             ! the number of delta functions in the
                                 ! convolution array
      REAL NORM                  ! normalization factor for the
                                 ! convolution array
      INTEGER I                  ! counter
      REAL SUM                   ! temporary sum of sinc interpolation
      REAL DX                    ! distance from start of convolution
                                 ! array in pixels
      REAL B1                    ! position in pixels of the current
                                 ! delta function
      REAL ALPHA                 ! scaling factor for the delta functions
      INTEGER J                  ! counter
      REAL X                     ! temporary variable for element swapping

*  Internal References:
      REAL SINC                  ! SIN(PI*X)/(PI*X)

*.

      AUNBAL = ABS(UNBAL)

*  convolution function must be twice size of map, odd number of points
*  because function is symmetric about central pixel.

      NCFN = NPIX*2 - 1

*  the beam separation in map pixels

      PIXBSEP = BSEP / PIXSEP

*  calculate the number of delta functions in the convolution function.
*  In principle the convolution function has infinite length but the
*  delta functions far away from the centre have such a small effect that
*  they can be ignored. Therefore, take into account only those delta
*  functions covered by the length of the convolution function plus 3
*  on either side.

      NDELTA = (INT (ABS(DBLE(NPIX)/PIXBSEP)) + 3) * 2

      IF (AUNBAL .GT. 1.0) AUNBAL = 1.0 / AUNBAL
      NORM = 2d0 / (1d0 + AUNBAL**(NDELTA/2+1))

*  cycle through pixels in convolution function

      DO I = 1, NCFN

         SUM = 0
         DX = I - NPIX                       ! position in pixels rel to centre
         B1 = PIXBSEP/2 - (NDELTA/2)*PIXBSEP ! position of first delta rel to centre
         ALPHA=1.0

*  for each delta function...

         DO J = 1, NDELTA

*  ...add its contribution to this element of the convolution function by sinc
*  interpolation

            ALPHA = ALPHA * AUNBAL
            IF (J .EQ. NPIX/2+1) ALPHA = 1.0
            SUM = SUM + SIGN(ALPHA,UNBAL) * SINC(DX-B1) * SIGN(1.0,B1)
            B1 = B1 + PIXBSEP

         END DO

         CONF(I) = SUM * NORM

      END DO

*  for the case where UNBAL was greater than 1 the convolution array
*  must be swapped around the middle

      IF (ABS(UNBAL) .GT. 1) THEN
         DO I = 1, NPIX
            J = NPIX + 1 - I
            X = CONF(J)
            CONF(J) = - CONF(I)
            CONF(I) = - X
         END DO
      END IF

      END

