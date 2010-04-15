      SUBROUTINE JCMT_DECONV_ERRORS (ZIN, ERRIN, NX, NY, FBAD,
     :   XINC, BSEP, UNBAL, CFUNC, ZOUT, ERROUT, STATUS)
*+
*  Name:
*     JCMT_DECONV_ERRORS

*  Purpose:
*     deconvolve single beam map with errors from dual beam map with errors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL JCMT_DECONV_ERRORS (ZIN, ERRIN, NX, NY, FBAD,
*     :   XINC, BSEP, CFUNC, ZOUT, ERROUT, STATUS)

*  Description:
*     Given all of the relevant map parameters and data this routine
*     produces the equivalent single beam map and errors from dual beam
*     data

*  Arguments:
*     ZIN( NX, NY ) = REAL (Given)
*        The input map
*     ERRIN( NX, NY ) = REAL (Given)
*        The input errors
*     NX = INTEGER (Given)
*        the number of map xolumns
*     NY = INTEGER (Given)
*        the number of map rows
*     FBAD = REAL (Given)
*        the bad value
*     XINC = DOUBLE PRECISION (Given)
*        The pixel spacing in the X direction in arcseconds
*     BSEP = DOUBLE PRECISION (Given)
*        The beam separation in arcseconds
*     UNBAL = REAL (Given)
*        The relative amplitudes of the right and left hand beams
*        UNBAL = amp(lhb) / abs(amp(rhb))
*     CFUNC( * ) = REAL (Given)
*        workspace for the convolution array it should be at least
*        NX*2+1
*     ZOUT( NX, NY ) = REAL (Returned)
*        The restored map
*     ERROUT( NX, NY) = REAL (Returned)
*        The `restored' errors
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL
*     {enter_new_authors_here}

*  History:
*     26-AUG-1991 (REVAD::JFL): Original version
*     10-JUN-1994 (REVAD::HME): RNORM must be REAL.
*     13-JUN-1994 (REVAD::HME): Add FBAD to argument list.
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
      REAL UNBAL
      REAL ZIN (NX, NY)
      REAL ERRIN (NX, NY)
      REAL FBAD
      REAL CFUNC (*)             ! work array to contain correlation
                                 ! function

*  Arguments Returned:
      REAL ZOUT (NX, NY)
      REAL ERROUT (NX, NY)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XMAX               ! maximum number of x pixels in map
      PARAMETER (XMAX = 1024)

*  Local Variables:
      REAL BUFF (XMAX)           ! temp buffer for holding the
                                 ! result of the convolution
      REAL BUFERR (XMAX)         ! temp buffer for holding the
                                 ! result of the convolution of the error array
      REAL RNORM                 ! normalization factor for the
                                 ! convolution
      DOUBLE PRECISION NPIXINC   ! beam separation measured in pixels
      INTEGER IY                 ! counter
      INTEGER IX                 ! counter
      INTEGER NCFN               ! length of the convolution function
      INTEGER IGNORE             !
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  beam separation measured in pixels

      NPIXINC = BSEP / XINC

*  If the beams are equal then the data should contain NO information
*  at the spatial frequency of the beam-throw or its harmonics. Any
*  signal that is there must be noise. This can be removed in the Fourier
*  domain by multiplying the signal by a function with zeroes at the
*  frequencies of zero sensitivity and 1 everywhere else. The same
*  effect is achieved by convolving the inverse FT of this with the map
*  data. This is NOD2's `symmetric function'. JCMT_GENSYCONFN generates the
*  required function.

      IF (ABS(ABS(UNBAL) - 1.0) .LT. 0.005) THEN

         RNORM = 1.0
         CALL JCMT_GENSYCONFN (BSEP, XINC, NX, NCFN, CFUNC)

*  do the convolution row by row

         DO IY = 1, NY

            CALL JCMT_CONVOLVE_ERRORS (ZIN(1,IY), ERRIN(1,IY), CFUNC,
     :         NX, NCFN, NX, FBAD, RNORM, BUFF, BUFERR, STATUS)
            DO IX = 1, NX
               ZOUT(IX,IY) = BUFF(IX)
               ERROUT(IX,IY) = BUFERR(IX)
            END DO

         END DO

      ELSE

*  If the beams are unbalanced then the data does contain information at
*  the chop throw harmonics and it is invalid to set these frequencies to
*  zero.

         IGNORE = 0
         CALL PAR_WRUSER ('The beams are unequal in amplitude. '//
     :     'Removal of chop-throw frequencies will not be performed.',
     :     IGNORE)

      ENDIF

*  Now perform the deconvolution. This could be done by dividing the FT of the
*  map by the FT of the beam chop, and calculating the inverse FT to give the
*  desired result. The equivalent is achieved by convolving the map data
*  with the function whose FT is as close as possible to 1 over the FT of
*  the chop function itself. This is the `asymmetric function' used in NOD2
*  and JCMT_GENASCONFN calculates it.

      RNORM = 2.0
      CALL JCMT_GENASCONFN (BSEP, XINC, NX, UNBAL, NCFN, CFUNC)

*  do the convolution row by row

      DO IY = 1, NY

         CALL JCMT_CONVOLVE_ERRORS (ZOUT(1,IY), ERROUT(1,IY), CFUNC,
     :      NX, NCFN, NX, FBAD, RNORM, BUFF, BUFERR, STATUS)
         DO IX = 1, NX
            ZOUT(IX,IY) = BUFF(IX)
            ERROUT(IX,IY) = BUFERR(IX)
         END DO

      END DO

      END


