************************************************************************
      SUBROUTINE GFIT(IMAGE, FIXSH, DCEN, FITSUB, SKY, VSKY,
     :          PADU, SATURE, APAR, EPOS, NX, NY, CODE, STATUS)

*+
*  Name :
*     GFIT
*
*  Purpose :
*     Fits a gaussian profile to a star in the array IMAGE(NX,NY)
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL GFIT(IMAGE, .FALSE., DCEN, .FALSE., SKY, VSKY,
*     :          PADU, SATURE, APAR, EPOS, NX, NY, CODE, STATUS)
*
*  Description :
*     First guess at the shape is held in APAR(6) passed from PSFCAL
*     via the FPEAK subroutine. The subroutine returns APAR(6) with
*     the fit parameters in it. You can demand that the shape is
*     fixed by setting FIXSH to .TRUE. or that the fit is subtracted
*     from the data by setting FITSUB to .TRUE. both are false when
*     this routine is called from PSFCAL
*
*
*
*  Arguments :
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     FIXSH = LOGICAL (Given)
*        Is the shape fixed (position and normalisation free)?
*     FITSUB = LOGICAL (Given)
*        If the fit subtracted from the data?
*     DCEN = REAL (Given)
*        Radius searched for star, if zero position is fixed
*     SKY = REAL (Given)
*        Value in sky aperture per pixel
*     SIGMA = REAL (Given)
*        Standard deviation in sky aperture
*     VSKY = REAL (Given)
*        Variance in sky aperture per pixel
*     APAR(6) = REAL (Given and Returned)
*        Parameters defining the shape of the profile
*     PADU = REAL (Given)
*        Photons per data unit
*     SATURE = REAL (Given)
*        User supplied saturation level
*     APAR( 6 ) = REAL (Given and Returned)
*        Parameters defining the shape of the profile
*     EPOS( 2 ) = REAL (Returned)
*        Error in X and Y positions of the first profile
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     CODE = CHARACTER*2 (Returned)
*        BAD or SATURATED pixel?
*     STATUS = INTEGER (Given and Returned)
*        The global status.
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997 (TN):
*        Original version written in FORTRAN 90 by Tim
*     14-DEC-1998 (AA):
*        Cut and hack to FORTRAN 77 for Starlink
*        Added NX and NY to calling arguments
*     20-JAN-1999 (AA):
*        Added CODE to passed arguements
*     28-SEP-2000 (AA):
*        Added code to handle bad pixels inside the PSF mask
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given :

      INTEGER NX, NY

      REAL IMAGE(NX, NY)
      REAL PADU, SATURE, DCEN
      REAL SKY, SIGMA, VSKY

      LOGICAL FIXSH, FITSUB

*  Arguments Given and Returned :

      REAL APAR(6)

*  Arguments Returned :

      REAL EPOS(2)
      CHARACTER * ( 2 ) CODE

*  Local Variables :

      INTEGER STATUS

      INTEGER I, J

      INTEGER IXCEN, IYCEN
      INTEGER IXBEG, IXEND, IYBEG, IYEND
      INTEGER NSAXIS(2)
      INTEGER ICOUNT

      INTEGER NPAR, NTERM
      INTEGER JFLAG
      INTEGER MODEW, ICURF

      INTEGER MAX_SIZE
      PARAMETER( MAX_SIZE = 1681 )

      REAL X1(MAX_SIZE), Y1(MAX_SIZE), W1(MAX_SIZE)

      REAL BCHI

      CHARACTER TEXT * 80

      INTEGER NAXIS2
      COMMON / OPT_LIST / NAXIS2

*   Hardwired replacement for the F90 allocated arrays DA(NPAR)
*   and COVAR(NPAR). In PHOTOM NPAR will always be SIZE(APAR)=6

      REAL DA(6), COVAR(6,6)

*   Declare FUNCTIONs

      INTEGER FIX_PAR, LIMIT_PAR, FREE_PAR

      REAL GAUSS
      EXTERNAL GAUSS, DGAUSS

*.

*      WRITE(*,*) ' DEBUG --- --- --- Entering GFIT()'
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise DA(6)

      DA(1) = 0.0
      DA(2) = 0.0
      DA(3) = 0.0
      DA(4) = 0.0
      DA(5) = 0.0
      DA(6) = 0.0

*   Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN


*   Set the center positions of the sub-array

      IXCEN = NINT(APAR(5))
      IYCEN = NINT(APAR(6))

*   Put the data into a 1D array

      IF( IXCEN-20 .GT. 1 ) THEN
            IXBEG = IXCEN-20
      ELSE
            IXBEG = 1
      ENDIF

      IF( IXCEN+20 .GT. NX ) THEN
            IXEND = NX
      ELSE
            IXEND = IXCEN+20
      ENDIF

      IF( IYCEN-20 .GT. 1 ) THEN
            IYBEG = IYCEN-20
      ELSE
            IYBEG = 1
      ENDIF

      IF( IYCEN+20 .GT. NY ) THEN
            IYEND = NY
      ELSE
            IYEND = IYCEN+20
      ENDIF

      NSAXIS(1) = IXEND-IXBEG+1
      NSAXIS(2) = IYEND-IYBEG+1
      NAXIS2 = NSAXIS(2)

      ICOUNT = 0
      DO I=IXBEG, IXEND
            DO J=IYBEG, IYEND
	          ICOUNT = ICOUNT + 1
		  X1(ICOUNT) = REAL(ICOUNT)
		  IF(IMAGE(I,J) .GE. SATURE) THEN

*   Unless the saturated pixel is within the mask this will not cause
*   a major problem so long as the sky estimate takes account of it.

                   TEXT = 'WARNING   > Saturated pixels'//
     :		          ' inside quick look box.'
		   CALL MSG_OUT( ' ', TEXT, STATUS )

		  ENDIF

*   Check for bad pixels inside the mask, this is a major problem for
*   PSF candiadate stars.

	          IF(IMAGE(I,J) .NE. VAL__BADR) THEN
		     Y1(ICOUNT) = IMAGE(I,J)-SKY
		     W1(ICOUNT) = VSKY + (IMAGE(I,J)-SKY)/PADU
		     IF(W1(ICOUNT) .LE. 0.0 ) THEN

*   This shouldn't happen, but if it does fudge the error
*   so that the world doesn't end

                        W1(ICOUNT) = VSKY
		     ENDIF
		     W1(ICOUNT) = 1.0/W1(ICOUNT)
		     W1(ICOUNT) = 1.0/W1(ICOUNT)
                  ELSE

*   If we have a bad pixel inside the PSF mask we have a serious problem
*   as the PSF FWHM will fit incorrectly and the photometry will be wrong!

                   TEXT = 'WARNING   > Bad pixels'//
     :		          ' inside quick look box.'
		   CALL MSG_OUT( ' ', TEXT, STATUS )
		     Y1(ICOUNT) = 0.0
		     W1(ICOUNT) = 0.0
		  ENDIF
            END DO
      END DO

      CALL RESET()

*    Hardwired replacement for the F90 statement NPAR=SIZE(APAR).
*    Under Tim's code APAR can have more than 2 gaussians functions
*    I don't use this (allocatable array problems again). Not a
*    major loss since OPPHOT never used this functionality anyway.

*    NPAR is the number of parameters
      NPAR=6

*    NTERM is the number of _free_ parameters
      NTERM=NPAR

*    The DO loop is here so I don't forget about it if I ever do
*    add multiple gaussians to APAR, the optimizer will get rid of
*    in its current state

      DO I=4, NPAR-2, 3
          APAR(I+1) = APAR(I+1) - REAL(IXBEG-1)
	  APAR(I+2) = APAR(I+2) - REAL(IYBEG-1)
      END DO

      IF(FIXSH) THEN
          JFLAG = FIX_PAR(1, NPAR, NTERM)
	  JFLAG = FIX_PAR(2, NPAR, NTERM)
      ELSE
*   Limit the Gaussian width to be positive and more the 0.5 of a pixel
*   NB: Note the 1.0E+10 which replaces the F90 huge(apar(1))

          JFLAG = LIMIT_PAR(1, 0.5, 1.0E+10)
          JFLAG = LIMIT_PAR(2, 0.5, 1.0E+10)
      ENDIF

*   Begin the fit with the rotation fixed

      JFLAG = FIX_PAR(3, NPAR, NTERM)

*   Limit the flux to be positive

      DO I=4, NPAR-2, 3
          JFLAG = LIMIT_PAR(I,0.0,1.0E+10)

*   Finally, the limits on position
          IF( DCEN .GT. 0.0 ) THEN
            JFLAG = LIMIT_PAR(I+1,APAR(I+1)-DCEN,APAR(I+1)+DCEN)
            JFLAG = LIMIT_PAR(I+2,APAR(I+2)-DCEN,APAR(I+2)+DCEN)
          ELSE
	    JFLAG = FIX_PAR(I+1, NPAR, NTERM)
	    JFLAG = FIX_PAR(I+2, NPAR, NTERM)
          ENDIF
      END DO

      MODEW=-1
      ICURF=1001

*      WRITE(*,*) ' DEBUG --- --- --- 1st call to CURFIT()'
      CALL CURFIT(X1,Y1,W1,NSAXIS(1)*NSAXIS(2),MODEW,APAR,DA,NPAR,
     :      NTERM,COVAR,NPAR,0.01,BCHI,-1.0,20,ICURF,GAUSS,DGAUSS,
     :      STATUS)

      IF( .NOT. FIXSH ) THEN

*   Let the rotation run free

          JFLAG = FREE_PAR(3, NPAR, NTERM)
	  JFLAG = LIMIT_PAR(3,-0.78539816,0.78539816)
	  ICURF = 1001
*          WRITE(*,*) ' DEBUG --- --- --- 2nd call to  CURFIT()'
          CALL CURFIT(X1,Y1,W1,NSAXIS(1)*NSAXIS(2),MODEW,APAR,DA,NPAR,
     :          NTERM,COVAR,NPAR,0.01,BCHI,-1.0,20,ICURF,GAUSS,DGAUSS,
     :          STATUS)

      ENDIF

      IF( FITSUB ) THEN

*   This is useful for debugging process only, do not set FITSUB to
*   .TRUE. for general use or you'll get some angry users and a
*   confused looking progammer since it mucks with the IMAGE(NX,NY)
*   array directly. If you get a flat image array after doing this
*   then the profile fit has worked well (we subtract the fitted
*   profile from the actual data).
*
*   A long comment since the relevance of this routine confused
*   me to start out with...

*   Make the output file the residuals

	  DO I=1, NSAXIS(1)*NSAXIS(2)
	       Y1(I) = Y1(I) - GAUSS(X1(I), APAR, NPAR) + SKY
	  END DO

*   Recover the data from the 1D array

          ICOUNT=0
          DO I=IXBEG,IXEND
	      DO J=IYBEG,IYEND
	         ICOUNT=ICOUNT+1
		 IMAGE(I,J) = Y1(ICOUNT)
	      END DO
	  END DO

      END IF

*   Return the correct values of APAR(5) and APAR(6)

      DO I=4, NPAR-2, 3
          APAR(I+1)=APAR(I+1)+REAL(IXBEG-1)
	  APAR(I+2)=APAR(I+2)+REAL(IYBEG-1)
      END DO

      IF(DCEN .GT. 0.0) THEN
          EPOS(1) = SQRT(ABS(COVAR(5,5)))
          EPOS(2) = SQRT(ABS(COVAR(6,6)))
      ELSE
          EPOS(1) = 0.0
	  EPOS(2) = 0.0
      ENDIF


*   End of routine

  99  CONTINUE
*      WRITE(*,*) ' DEBUG --- --- --- Leaving GFIT()'

      END










