************************************************************************
      SUBROUTINE FPEAK(IMAGE, CLIP, SKY, SIGMA, VSKY,
     :                 APAR, PADU, NX, NY, CODE, STATUS)

*+
*  Name :
*     FPEAK
*
*  Purpose :
*     Finds the location of a star
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      CALL FPEAK(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR, PADU,
*     :           NX, NY, CODE, STATUS)
*
*  Description :
*     This subroutine searches the nine pixels around the supposed
*     position of the star and returns the best position.
*
*  Arguments :
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     CLIP = REAL (Given)
*        Clipping radius for weight map
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
*     NX= INTEGER (Given)
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
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997
*        Original version written in FORTRAN 90 by Tim
*     14-DEC-1998
*        Cut and hack to FORTRAN 77 for Starlink
*     20-JAN-1999
*        Added CODE to passed arguements
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

      REAL IMAGE(NX, NY)
      REAL CLIP, SEE
      REAL PADU
      REAL SKY, SIGMA, VSKY

*  Arguments Given and Returned :

      REAL APAR(6)

*  Arguments Returned :

      CHARACTER * ( 2 ) CODE

*  Local Variables :

      INTEGER STATUS

      INTEGER IPOS1, IPOS2, I, J
      REAL XPOS, YPOS
      REAL STAR, SAFE

*  Unused arguments from SUMFLX
      REAL ERROR, BESTN

*.

*   Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Define the loop counters

      IPOS1=NINT(APAR(5))
      IPOS2=NINT(APAR(6))

*   Start looking

      DO I=IPOS1-1, IPOS1+1
            DO J=IPOS2-1, IPOS2+1
	          APAR(5)=REAL(I)
		  APAR(6)=REAL(J)

*   Call SUMFLX with dummy SATURE argument, 1E+06, we don't
*   want it falling over in a heap on saturated stars at this
*   point, save that for later

		  CALL SUMFLX(IMAGE, CLIP, SKY, SIGMA, VSKY,
     :                        APAR, PADU, 1.0E+24, NX, NY, ERROR,
     :                        BESTN, STAR, CODE, STATUS)
                  IF((I.EQ.IPOS1-1) .AND. (J.EQ.IPOS2-1)) THEN
		         SAFE = STAR
			 XPOS = REAL(I)
			 YPOS = REAL(J)
	          ELSE
		         IF( STAR .GT. SAFE ) THEN
			       SAFE = STAR
			       XPOS = REAL(I)
			       YPOS = REAL(J)
			 END IF
	          ENDIF
            END DO
      END DO

      APAR(5) = XPOS
      APAR(6) = YPOS

*   End of routine

  99  CONTINUE

      END










