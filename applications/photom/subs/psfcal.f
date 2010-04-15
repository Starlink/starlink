************************************************************************
      SUBROUTINE PSFCAL(XCEN, YCEN, DCEN, NPSF, IMAGE, NX, NY,
     :                  SEE, CLIP, PADU, SATURE, XACC, YACC, SHAPE, SKY,
     :			SIGMA, VSKY, CODE, SEARCH, POSTVE, MXSHFT,
     :                  MXITER, TOLER, STATUS)

*+
*  Name :
*     PSFCAL
*
*  Purpose :
*     This subroutine calculates the point spread function
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL PSFCAL( XCEN, YCEN, DCEN, NPSF, IMAGE, NX, NY, SEE, CLIP,
*                  PADU, SATURE, SHAPE, XACC, YACC, SKY, SIGMA, VSKY,
*                  CODE, SEARCH, POSTVE, MXSHFT, MXITER, TOLER, STATUS )
*
*
*  Description :
*
*
*  Arguments :
*     XCEN = REAL (Given)
*        X-position of PSF star
*     YCEN = REAL (Given)
*        Y-position of PSF star
*     DCEN = REAL (Given)
*        Radius searched for PSF star, if zero position is fixed
*     NPSF = INTEGER (Given)
*        Number of PSF stars (currently isn't used)
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     NX= INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     SEE = REAL (Given)
*        Approx seeing in pixels
*     PADU = REAL (Given)
*        Photons per data unit
*     SATURE = REAL (Given)
*        User supplied saturation level
*     XACC = REAL (Returned)
*        Accurate position of PSF intensity peak.
*     YACC = REAL (Returned)
*        Accurate position of PSF intensity peak.
*     SHAPE( 3 ) = REAL (Returned)
*        The three dimensions of the PSF
*     SKY = REAL (Given)
*        Value in sky aperture per pixel
*     SIGMA = REAL (Given)
*        Standard deviation in sky aperture
*     VSKY = REAL (Given)
*        Variance in sky aperture per pixel
*     CODE = CHARACTER*2 (Returned)
*        BAD or SATURATED pixel?
*     SEARCH = INTEGER (Given)
*        Size of search box
*     POSTVE = LOGICAL (Given)
*        Image features positive?
*     MXSHFT = REAL (Given)
*        Maximum shift between guess and output positions
*     MXITER = INTEGER (Given)
*        Maximum number of iterations for centroiding
*     TOLER = REAL (Given)
*        Positional accuracy required
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     TN: Tim Naylor (Keele University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997 (TN):
*        Original version written in FORTRAN 90 by Tim
*     14-DEC-1998 (AA):
*        Cut and hack to FORTRAN 77 for Starlink
*        Changes to calling parameters for PHOTOM
*     20-JAN-1999 (AA):
*        Added CODE to passed arguments
*     30-MAY-1999 (PWD):
*        Added XACC and YACC arguments.
*     19-AUG-2002 (AA):
*        Added additional centroiding code for PSF star finding
*        Added SEARCH, POSTVE, MXSHFT, MXITER and TOLER arguements
*     17-JUN-2008 (PWD):
*        Correct gaussian sigma to fhwm ratio to 1.665 (was 1.655).
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :

      INCLUDE 'SAE_PAR'

*  Arguments Given :

      INTEGER NX, NY
      INTEGER NPSF
      INTEGER SEARCH
      INTEGER MXITER

      REAL XCEN, YCEN, DCEN
      REAL CLIP, SEE, PADU, SATURE
      REAL SKY, SIGMA, VSKY
      REAL IMAGE(NX, NY)
      REAL MXSHFT
      REAL TOLER

      LOGICAL POSTVE

*  Arguments Given and Returned :

*  Arguments Returned :
      REAL XACC, YACC
      REAL SHAPE(3)
      CHARACTER * ( 2 ) CODE

*  Local Variables :

      CHARACTER * ( 72 ) TEXT   ! Buffer for output messages
      INTEGER STATUS

      INTEGER J
      REAL EPOS(2)

      LOGICAL CENTRO

      REAL XINIT, YINIT, XFINAL, YFINAL

*  APAR(1) = SHAPE(1) = First Gaussian FWHM
*  APAR(2) = SHAPE(2) = Second Gaussian FWHM
*  APAR(3) = SHAPE(3) = Rotation between the Gaussian profiles
*  APAR(4) = Normalisation
*  APAR(5) = X-center
*  APAR(6) = Y-center

      REAL APAR(6)

*.
*          WRITE(*,*) ' DEBUG --- --- Entering PSFCAL()'
*          WRITE(*,*) ' DEBUG --- --- STATUS = ', STATUS

*   Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Do initial check to see if the candidate PSF is on the frame

      IF( (XCEN .LT. 0.0) .OR. (REAL(XCEN) .GT. NX) .OR.
     :    (YCEN .LT. 0.0) .OR. (REAL(YCEN) .GT. NY) ) THEN
            STATUS  =  SAI__ERROR
            CALL ERR_REP( 'ERR_PSFCAL_NOSTAR',
     :        'PSFCAL: PSF candidate star not on frame', STATUS )
            GOTO 99
      ENDIF

*   Setup fitting for the PSF star
      APAR(5)=XCEN
      APAR(6)=YCEN

*   Set gaussian width
      APAR(1)=SEE/1.665
      APAR(2)=SEE/1.665

*   Set rotation
      APAR(3)=0.3

*   Normalisation to 1
      APAR(4)=1.0

*   Hunt for best count
*          WRITE(*,*) ' DEBUG --- --- calling FPEAK()'
*          WRITE(*,*) ' DEBUG --- --- APAR(1) XCEN = ', APAR(1)
*          WRITE(*,*) ' DEBUG --- --- APAR(2) YCEN = ', APAR(2)
*          WRITE(*,*) ' DEBUG --- --- APAR(5) XCEN = ', APAR(5)
*          WRITE(*,*) ' DEBUG --- --- APAR(6) YCEN = ', APAR(6)


      CALL FPEAK(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR, PADU,
     :           NX, NY, CODE, STATUS)


*  If required update the object position by centroiding.


*      WRITE(*,*) ' DEBUG --- --- returned from FPEAK()'
*      WRITE(*,*) ' DEBUG --- --- APAR(1) XCEN = ', APAR(1)
*      WRITE(*,*) ' DEBUG --- --- APAR(2) YCEN = ', APAR(2)
*      WRITE(*,*) ' DEBUG --- --- APAR(5) XCEN = ', APAR(5)
*      WRITE(*,*) ' DEBUG --- --- APAR(6) YCEN = ', APAR(6)

      CENTRO = .TRUE.
      IF ( CENTRO ) THEN

*          WRITE(*,*) 'DEBUG -- doing extra centroiding step'

          CALL ERR_MARK
          XINIT = APAR(5)
          YINIT = APAR(6)
*          WRITE(*,*) ' DEBUG --- --- calling LOCATE()'

*          WRITE(*,*) ' DEBUG --- --- NX     = ', NX
*          WRITE(*,*) ' DEBUG --- --- NY     = ', NY
*          WRITE(*,*) ' DEBUG --- --- XINIT  = ', XINIT
*          WRITE(*,*) ' DEBUG --- --- YINIT  = ', YINIT
*          WRITE(*,*) ' DEBUG --- --- DCEN   = ', DCEN
*          WRITE(*,*) ' DEBUG --- --- POSTVE = ', POSTVE
*          WRITE(*,*) ' DEBUG --- --- MXSHFT = ', MXSHFT
*          WRITE(*,*) ' DEBUG --- --- MXITER = ', MXITER


         CALL LOCATE( IMAGE, NX, NY, XINIT, YINIT, INT(DCEN), POSTVE,
     :                      MXSHFT, MXITER, TOLER, XFINAL, YFINAL,
     :                      STATUS )

*          WRITE(*,*) ' DEBUG --- --- returned from LOCATE()'
*
*          WRITE(*,*) ' DEBUG --- --- XFINAL = ', XFINAL
*          WRITE(*,*) ' DEBUG --- --- YFINAL = ', YFINAL
*          WRITE(*,*) ' DEBUG --- --- STATUS = ', STATUS

*  If the routine finished succesfully then replace values of
*  XPOS, YPOS.
          IF ( STATUS .EQ. SAI__OK ) THEN
              APAR(5) = XFINAL
              APAR(6) = YFINAL

*  If an error has occured in LOCATE then reset the status and use the
*  initial values for the position
          ELSE
              CALL ERR_ANNUL( STATUS )
              WRITE(TEXT, '(''WARNING > Problems during'
     :		         //'centroiding. '')')
          ENDIF
          CALL ERR_RLSE
      ENDIF

*   Set the normalisation.

      IF( IMAGE(NINT(APAR(5)),NINT(APAR(6)))
     :    -SKY.GT.SQRT(VSKY) ) THEN
            APAR(4) = IMAGE(NINT(APAR(5)),NINT(APAR(6))) - SKY
      ELSE
            APAR(4) = SQRT(VSKY)
      ENDIF

*   Call the gaussian fit routine

*      WRITE(*,*) ' DEBUG --- --- calling GFIT()'
*      WRITE(*,*) ' DEBUG --- --- APAR(1) XCEN = ', APAR(1)
*      WRITE(*,*) ' DEBUG --- --- APAR(2) YCEN = ', APAR(2)
*      WRITE(*,*) ' DEBUG --- --- APAR(5) XCEN = ', APAR(5)
*      WRITE(*,*) ' DEBUG --- --- APAR(6) YCEN = ', APAR(6)

      CALL GFIT(IMAGE, .FALSE., DCEN, .FALSE., SKY, VSKY,
     :          PADU, SATURE, APAR, EPOS, NX, NY, CODE, STATUS)

*      WRITE(*,*) ' DEBUG --- --- returned from GFIT()'
*      WRITE(*,*) ' DEBUG --- --- APAR(1) XCEN = ', APAR(1)
*      WRITE(*,*) ' DEBUG --- --- APAR(2) YCEN = ', APAR(2)
*      WRITE(*,*) ' DEBUG --- --- APAR(5) XCEN = ', APAR(5)
*      WRITE(*,*) ' DEBUG --- --- APAR(6) YCEN = ', APAR(6)


*   Return SHAPE

      DO J=1, 3
            SHAPE(J) = APAR(J)
      END DO

*   Return accurate centres.

      XACC = APAR(5)
      YACC = APAR(6)


*   End of routine

  99  CONTINUE
*      WRITE(*,*) ' DEBUG --- --- Leaving PSFCAL()'

      END
