************************************************************************
      REAL FUNCTION TGAUSS(I, J, APAR, INTER)

*+
*  Name :
*     TGAUSS
*
*  Purpose :
*     Returns the value for the pixel I,J
*
*  Language :
*     FORTRAN
*
*  Invocation :
*      GAUSS =  TGAUSS(I, J, APAR, INTER)
*
*  Description :
*
*  Arguments :
*     I = INTEGER (Given)
*        X co-ordinate
*     J = INTEGER (Given)
*        Y co-ordinate
*     APAR(6) = REAL (Given)
*        Parameters defining the shape of the profile
*     INTER = INTEGER (Given)
*        The square root of the number of points used for the integeration
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
*     ??-???-1997
*        Original version written in FORTRAN 90 by Tim
*     18-DEC-1998
*        Cut and hack to FORTRAN 77 for Starlink
*        Now subroutine instead of function
*        Added GAUSS return arguement
*     04-JAN-1998
*        Converted back to function from subroutine
*     10-JAN-2008 (PWD):
*        Make sure THETA is initialised.
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

      INTEGER I, J
      INTEGER INTER

      REAL APAR(6)

*  Arguments Given and Returned :

*  Arguments Returned :

*  Local Variables :


      REAL X, Y, WORK, R, THETA

*.

      X = REAL(I) - APAR(5)
      Y = REAL(J) - APAR(6)

*   Convert to R,THETA co-odinate system

      R = SQRT(X*X + Y*Y)

*   If R is zero then the function will come out OK whatever theta is,
*   otherwise find theta, getting the quadrant right

      IF(ABS(R) .GT. 0.0) THEN

*   Replace the functionality of the F90 SIGN() call
            IF ( Y .GT. 0.0 ) THEN
	          THETA = -1.0*ACOS(X/R)
            ELSE
	          THETA = ACOS(X/R)
            ENDIF
      ELSE
            THETA = 0.0
      ENDIF

      WORK = R*R*( (COS(THETA+APAR(3))/APAR(1))**2.0
     :       + (SIN(THETA+APAR(3))/APAR(2))**2.0 )
      TGAUSS = APAR(4)*EXP(-1.0*WORK)


*   End of routine

  99  CONTINUE

      END










