      SUBROUTINE SCULIB_2POS_CONFN (BSEP, PIXSEP, NPIX, UNBAL, NCFN,
     :   CONF, STATUS)
*+
*  Name:
*     SCULIB_2POS_CONFN

*  Purpose:
*     Generate a convolution function to remove the 2-position
*     chop function from raster scans.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_2POS_CONFN (BSEP, PIXSEP, NPIX, UNBAL, NCFN, CONF,
*    :  STATUS)

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
*     give a delta function of height 1.
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
*     BSEP = REAL (Given)
*        The beam separation in arcseconds
*     PIXSEP = REAL (Given)
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
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     John Lightfoot (RoE)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     22-JAN-1990 (JBVAD::PAH):
*        Original version.
*      3-MAY-1991 (REVAD::JFL): Comments and description enlarged.
*      8-MAY-1991 (REVAD::JFL): Convolution function reversed.
*     12-OCT-1995 (jfl@roe.ac.uk): Name changed to SCULIB_GENASCONFN
*     31-OCT-1995 (jfl@roe.ac.uk): Name changed to SCULIB_2POS_CONFN
*      6-JAN-1997 (jfl@roe.ac.uk): Change normalisation to give delta
*                                  function of unit height when convolved
*                                  with the chop function, add in more
*                                  delta functions beyond the ends of
*                                  the convolution function range.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      REAL BSEP, PIXSEP
      INTEGER NPIX
      REAL UNBAL

*  Arguments Returned:
      REAL CONF( * )
      INTEGER NCFN
      INTEGER STATUS


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

*  External References:
      REAL SCULIB_SINC           ! SIN(PI*X)/(PI*X)

*.

      IF (STATUS .NE. SAI__OK) RETURN

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
*  functions covered by the length of the convolution function plus 5
*  on either side.

      NDELTA = (INT (ABS(REAL(NPIX)/PIXBSEP)) + 5) * 2

      IF (AUNBAL .GT. 1.0) AUNBAL = 1.0 / AUNBAL
      NORM = 1.0 / (1.0 + AUNBAL**(NDELTA/2+1))

*  cycle through pixels in convolution function

      DO I = 1, NCFN
         SUM = 0.0
         DX = REAL (I - NPIX)                ! position in pixels rel to centre
         B1 = PIXBSEP/2 - REAL(NDELTA/2) * PIXBSEP
                                             ! position of first delta rel to
                                             ! centre
         ALPHA = 1.0

*  for each delta function...

         DO J = 1, NDELTA

*  ...add its contribution to this element of the convolution function by sinc
*  interpolation

            ALPHA = ALPHA * AUNBAL
            IF (J .EQ. NPIX/2+1) ALPHA = 1.0
            SUM = SUM + SIGN(ALPHA,UNBAL) * SCULIB_SINC(DX-B1) *
     :           SIGN(1.0,B1)
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

