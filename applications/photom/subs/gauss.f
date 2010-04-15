************************************************************************
      REAL FUNCTION GAUSS(PIX, APAR, NPAR)

*+
*  Name :
*     GAUSS
*
*  Purpose :
*     This is the Gaussian used for fitting the profiles
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      RESULT = GAUSS(PIX, APAR, NPAR)
*
*  Description :
*
*  Arguments :
*     PIX = REAL (Given)
*        1D array pixel number
*     APAR(6) = REAL (Given)
*        Parameters defining the shape of the profile
*     NPAR = INTEGER (Given)
*        Number of parameters in APAR, currently 6
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
*     04-JAN-1998
*        Cut and hack to FORTRAN 77 for Starlink
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

      INTEGER NPAR
      REAL PIX, APAR(NPAR)

*  Arguments Given and Returned :

*  Arguments Returned :

*  Local Variables :

      REAL X, Y, WORK, R, THETA
      INTEGER I, J, K

      INTEGER NAXIS2
      COMMON/ OPT_LIST / NAXIS2

*.

*   Convert back from 1D pixels to 2D ones.

      I = NINT(PIX-1.0)/NAXIS2 + 1
      J = NINT(PIX) - (I-1)*NAXIS2

      GAUSS = 0.0

*   With NPAR = 6 this will loop from 4 to 4, in other words it
*   will run once only. This is here so I don't have to rewrite
*   lots of code if I ever add additional gaussian functions to
*   the APAR array. Appears in several other places as well.

      DO K=4, NPAR-2, 3

          X = REAL(I) - APAR(K+1)
	  Y = REAL(J) - APAR(K+2)

*   Now we convert into r, theta co-ordinates

          R = SQRT(X*X + Y*Y)

*   If R is zero then the function will come out OK whatever theta is,
*   otherwise find theta, getting the quadrant right

          IF(ABS(R) .GT. 0.0) THEN

*   Relpace the functionality of the F90 SIGN() call
                IF ( Y .GT. 0.0 ) THEN
	              THETA = -1.0*ACOS(X/R)
                ELSE
	              THETA = ACOS(X/R)
                ENDIF
          ENDIF

          WORK = R*R*( (COS(THETA+APAR(3))/APAR(1))**2.0
     :           + (SIN(THETA+APAR(3))/APAR(2))**2.0 )
          GAUSS = GAUSS+APAR(K)*EXP(-1.0*WORK)

      END DO

*   End of routine

  99  CONTINUE

      END










