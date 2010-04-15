      SUBROUTINE JCMT_3POS_DECONV (ZIN, NX, NY, FBAD, XINC, BSEP,
     :   CFUNC, ZOUT, STATUS)
*+
*  Name:
*     JCMT_DECONV

*  Purpose:
*     deconvolve a single beam map from 3-position chop scanned data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL JCMT_3POS_DECONV (ZIN, NX, NY, FBAD, XINC, BSEP, CFUNC,
*     :  ZOUT, STATUS)

*  Description:
*     Given all of the relevant map parameters and data this routine
*     produces the equivalent single beam map from a map made by scanning
*     a 3-position chop over the source.

*  Arguments:
*     ZIN( NX, NY ) = REAL (Given)
*        The input map
*     NX = INTEGER (Given)
*        the number of map xolumns
*     NY = INTEGER (Given)
*        the number of map rows
*     FBAD = REAL (Given)
*        the flag value to signal bad pixel
*     XINC = DOUBLE PRECISION (Given)
*        The pixel spacing in the X direction in arcseconds
*     BSEP = DOUBLE PRECISION (Given)
*        The chop size measured between 2 -ve spikes of chop in arcseconds
*     CFUNC( * ) = REAL (Given)
*        workspace for the convolution array it should be at least
*        NX*2+1
*     ZOUT( NX, NY ) = REAL (Returned)
*        The restored map
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot
*     {enter_new_authors_here}

*  History:
*      6-JUL-1993 (REVAD::JFL): Original version
*     10 Jun 1994 (hme). RNORM must be REAL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION XINC
      DOUBLE PRECISION BSEP
      REAL ZIN (NX, NY)
      REAL FBAD
      REAL CFUNC (*)             ! work array to contain correlation
                                 ! function

*  Arguments Returned:
      REAL ZOUT (NX, NY)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XMAX               ! maximum number of x pixels in map
      PARAMETER (XMAX = 512)

*  Local Variables:
      REAL BUFF (XMAX)           ! temp buffer for holding the
                                 ! result of the convolution
      REAL RNORM                 ! normalization factor for the
                                 ! convolution
      INTEGER IY                 ! counter
      INTEGER IX                 ! counter
      INTEGER NCFN               ! length of the convolution function
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  The data should contain NO information at the spatial frequency of the
*  half beam-throw (distance between -ve spike and central spike) or its
*  harmonics. Any signal that is there must be noise. This can be removed
*  in the Fourier domain by multiplying the signal by a function with zeroes
*  at the frequencies of zero sensitivity and 1 everywhere else. The same
*  effect is achieved by convolving the inverse FT of this with the map
*  data. This is NOD2's `symmetric function'. JCMT_GENSYCONFN generates the
*  required function.

      RNORM = 1.0
      CALL JCMT_GENSYCONFN (BSEP/2.0d0, XINC, NX, NCFN, CFUNC)

*  do the convolution row by row

      DO IY = 1, NY

         CALL JCMT_CONVOLVE (ZIN(1,IY), CFUNC, NX, NCFN, NX,
     :      FBAD, RNORM, BUFF, STATUS)
         DO IX = 1, NX
            ZOUT(IX,IY) = BUFF(IX)
         END DO

      END DO

*  Now perform the deconvolution. This could be done by dividing the FT of the
*  map by the FT of the beam chop, and calculating the inverse FT to give the
*  desired result. The equivalent is achieved by convolving the map data
*  with the function whose FT is as close as possible to 1 over the FT of
*  the chop function itself. This is the `asymmetric function' used in NOD2
*  and JCMT_3POS_CONFN calculates it for the 3-position chop

      RNORM = 1.0
      CALL JCMT_3POS_CONFN (BSEP, XINC, NX, NCFN, CFUNC)

*  do the convolution row by row

      DO IY = 1, NY

         CALL JCMT_CONVOLVE (ZOUT(1,IY), CFUNC, NX, NCFN, NX,
     :      FBAD, RNORM, BUFF, STATUS)
         DO IX = 1, NX
            ZOUT(IX,IY) = BUFF(IX)
         END DO

      END DO

      END


