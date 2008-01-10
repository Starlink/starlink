************************************************************************
      SUBROUTINE DOSUM(IMAGE, NX, NY, VVAR, RVAR, MASK, DATA, X,
     :                 Y, SKY, SIGMA, VSKY, APAR, PADU, SATURE,
     :                 CLIP, ERROR, BESTN, STAR, MIN1, MAX1,
     :                 MIN2, MAX2, CODE, STATUS)

*+
*  Name :
*     DOSUM
*
*  Purpose :
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL DOSUM(IMAGE, NX, NY, %VAL(CNF_PVAL(VVAR)), %VAL(CNF_PVAL(RVAR)),
*     :           %VAL(CNF_PVAL(MASK)), %VAL(CNF_PVAL(DATA)), X, Y, SKY,
*     :           SIGMA, VSKY, APAR, PADU, SATURE, CLIP, ERROR, BESTN,
*     :           STAR, MIN1, MAX1, MIN2, MAX2, STATUS)
*
*  Description :
*        This subroutine is not original to OPPHOT. To get round
*        the lack of allocatable arrays in F77, I'm using HDS
*        temporary objects and passing them to this routine to
*        do what SUM_FLUX used to do in F90. It should work, but
*        its not the neatest solution in the world.
*
*        If this is in the release version of the code, then I'm
*        not a happy bunny, there is a better solution and hopefully
*        I'll have time to implement it.
*
*  Arguments :
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     NX= INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     VVAR = REAL(MAX1-MIN1, MAX2-MIN2) (Given)
*        Array of variances
*     RVAR = REAL(MAX1-MIN1, MAX2-MIN2) (Given)
*        Array of _real_ variances
*     MASK = REAL(MAX1-MIN1, MAX2-MIN2) (Given)
*        Mask array (NOT bad pixel mask, the other one!)
*     DATA = REAL(MAX1-MIN1, MAX2-MIN2) (Given)
*        Data array, subset of IMAGE array
*     X = INTEGER (Given)
*        X dimension
*     Y = INTEGER (Given)
*        Y Dimension
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
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     ERROR = REAL (Returned)
*        Estimate of the error in STAR
*     BESTN = REAL (Returned)
*        Estimate of the best achievable noise
*     STAR = REAL (Returned)
*        Summed flux in star
*     MIN1 = INTEGER (Given)
*        X-minimum
*     MIN1 = INTEGER (Given)
*        X-maximum
*     MIN2 = INTEGER (Given)
*        Y-minimum
*     MIN2 = INTEGER (Given)
*        Y-maximum
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
*        Originally part of SUMFLX.F written in F90 by Tim
*     21-DEC-1998 (AA):
*        Hacked into a seperate subroutine and converted to F77
*     20-JAN-1999 (AA):
*        Added CODE to passed arguements
*     28-SEP-2000 (AA):
*        Added code to handle bad pixels inside the PSF mask.
*     10-JAN-2008 (PWD):
*        Switch do loop indexing to Fortran first fatest order.
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

      INTEGER X
      INTEGER Y

      INTEGER NX, NY

      REAL IMAGE(NX, NY)

      INTEGER MIN1, MAX1, MIN2, MAX2

      REAL VVAR(X,Y)
      REAL RVAR(X,Y)
      REAL MASK(X,Y)

      REAL CLIP, PADU, SATURE
      REAL SKY, SIGMA, VSKY

*  Note DATA moved here since Solaris won't let you pass dimensions to
*  a local array. This worked under the Linux and DEC Unix compilers.

      REAL DATA(X,Y)

*  Arguments Given and Returned :

      REAL APAR(6)

*  Arguments Returned :

      REAL STAR, ERROR, BESTN
      CHARACTER * ( 2 ) CODE

*  Local Variables :

      INTEGER I, J
      INTEGER ICOUNT
      INTEGER XCOUNT, YCOUNT

      REAL PEAK, DIST
      REAL NORM, HGAUSS

      INTEGER STATUS

      LOGICAL SOFT, BAD, SAT

*  Debug variables
*
*      REAL MSUM

*  Running counters

      DOUBLE PRECISION SNSUM, MASUM
      DOUBLE PRECISION FLUX

*  Functions

      REAL TGAUSS

*.

*  Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set SOFT to .TRUE. maybe make it user accesible later, but
*  it is very unlikely that you'd want a hard edge to your mask

      SOFT = .TRUE.
      BAD = .FALSE.
      SAT = .FALSE.

*  Fill DATA array (a sub-set of the IMAGE array)

      YCOUNT = 0
      DO J = MIN2, MAX2
         YCOUNT = YCOUNT + 1
         XCOUNT = 0
         DO I = MIN1, MAX1
            XCOUNT = XCOUNT + 1
            DATA(XCOUNT,YCOUNT) = IMAGE(I,J)
	 END DO
      END DO


*  Fill the variance array and the real variances array
      DO J = 1, Y
         DO I = 1, X
	     HGAUSS = TGAUSS( (MIN1-1+I),(MIN2-1+J) , APAR, 2)
      	     VVAR(I,J) = VSKY + HGAUSS/PADU
	     RVAR(I,J) = VSKY + (DATA(I,J)-SKY)/PADU
	     IF(RVAR(I,J) .LE. 0.0) RVAR(I,J) = VSKY
	 END DO
      END DO

      SNSUM = 0.0
      MASUM = 0.0

      ICOUNT = 0
      PEAK = APAR(4)
      APAR(4) = 1.0

      DO J = 1, Y
         DO I = 1, X
	     DIST = SQRT( (REAL(MIN1-1+I)-APAR(5))*
     :                    (REAL(MIN1-1+I)-APAR(5))+
     :                    (REAL(MIN2-1+J)-APAR(6))*
     :                    (REAL(MIN2-1+J)-APAR(6))  )

             IF( DIST .LT. (CLIP+0.5) ) THEN

*  This is a useful pixel
*
*  Calculate the value of the estimated profile at this point. Note
*  that the value of the Gaussian at this point is used. Perfectionists
*  may prefer to call the function IGAUSS, that integrates over the
*  pixel, but Tim has never found a case where this helped.

                 MASK(I,J) = TGAUSS((MIN1-1+I),(MIN2-1+J), APAR, 2)

		 IF( DIST .GT. (CLIP-0.5) ) THEN

*  Make a soft edge

		     IF(SOFT) THEN
		         MASK(I,J) = MASK(I,J)*(0.5-(DIST-CLIP))
		     ELSE IF( DIST .GT. CLIP) THEN
		         MASK(I,J) = 0.0
		     END IF
		 END IF

*  Keep tabs on the signal-to-noise

	         IF(DATA(I,J) .NE. VAL__BADR) THEN
                    SNSUM = SNSUM+DBLE((DATA(I,J)-SKY)**2.0)/RVAR(I,J)
		 ELSE
		    BAD = .TRUE.
		 ENDIF
                 MASUM = MASUM+DBLE(MASK(I,J))

		 ICOUNT = ICOUNT + 1
             ELSE

*  Throw it back

	         MASK(I,J) = 0.0
	     END IF

*  Is the star saturated, flag it as such.
	     IF(DATA(I,J).GE.SATURE .AND. MASK(I,J).GE.0.0) THEN
                   SAT = .TRUE.
	     ENDIF

         END DO
      END DO

      APAR(4) = PEAK

*   Normalise the profile to one
      DO J = 1, Y
         DO I = 1, X
             MASK(I,J) = MASK(I,J)/REAL(MASUM)
         END DO
      END DO


*   Calculate the term to divide by

      NORM = 0.0
      DO J = 1, Y
         DO I = 1, X
	     NORM = NORM + ( MASK(I,J)*MASK(I,J)/VVAR(I,J) )
         END DO
      END DO

*   Calculate the weight mask and finalise output

      FLUX = 0.0
      ERROR = 0.0
      DO J = 1, Y
         DO I = 1, X
             MASK(I,J) = MASK(I,J)/VVAR(I,J)
	     MASK(I,J) = MASK(I,J)/NORM

*  Check to see if we have a bad pixel, if so, flag it
	     IF(DATA(I,J) .NE. VAL__BADR) THEN
                 FLUX = FLUX + DBLE( MASK(I,J)*(DATA(I,J)-SKY) )
                 ERROR = ERROR + RVAR(I,J)*MASK(I,J)*MASK(I,J)
     :                   + SIGMA*SIGMA*REAL(MASUM)
             ELSE
	      	BAD = .TRUE.
             ENDIF
         END DO
      END DO
      ERROR = SQRT(ERROR)

*   Output the flux

      STAR = REAL(FLUX)

*   Find the available signal-to-noise

      IF( SNSUM .GT. DBLE(ICOUNT) ) THEN
          BESTN = FLUX/SQRT(REAL(SNSUM)-REAL(ICOUNT))
      ELSE

*   Sometimes happens for weak sources, flag it by returning
*   a negative number to tell the user to get better data!
          BESTN = FLUX/SQRT(REAL(ICOUNT)-SNSUM)
      ENDIF

*   Indicate the error codes
      IF ( BAD ) THEN
         CODE = 'B'
      ENDIF

      IF ( SAT ) THEN
         CODE = 'S'
      ENDIF

*   End of routine

  99  CONTINUE

      END

