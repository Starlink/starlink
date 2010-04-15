************************************************************************

      SUBROUTINE DGAUSS( PIX, APAR, DA, NPAR, DFDA )

*+
*  Name :
*     DGAUSS
*
*  Purpose :
*     Calculates the differential of the gaussian for fitting purposes
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL DGAUSS( PIX, APAR, DA, NPAR, DFDA )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*     PIX = REAL (Given)
*        1D array pixel number
*     APAR( 6 ) = REAL (Given)
*        Parameters defining the shape of the profile
*     DA( 6 ) = REAL (Given)
*        Step for numerical differenciation
*     NPAR = INTEGER (Given)
*        Number of parameters in APAR, currently 6
*     DFDA( 6 ) = REAL (Returned)
*        Differential
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     Since we pass this through several subroutines the calling
*     arguements are set in stone unless I modifiy them all to
*     take this into account. If I replace MRQ_COF with the correct
*     PDA function I might be able to work round this. For now
*     this function does _not_ return STATUS as normal.
*     {routine_deficiencies}...
*
*  Authors :
*     TN: Tim Naylor (Keele University)
*     AA: Alasdair Allan (Starlink, Keele University
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997 (TN):
*        Original version written in FORTRAN 90 by Tim
*     04-JAN-1998 (AA):
*        Cut and hack to FORTRAN 77 for Starlink
*
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :

*  Arguments Given :

      INTEGER NPAR

      REAL PIX
      REAL APAR(NPAR)
      REAL DA(NPAR)

*  Arguments Returned :

      REAL DFDA(NPAR)

*  Local Variables :

      INTEGER NAXIS2
      COMMON/ OPT_LIST / NAXIS2

      REAL X, Y, WORK, R, THETA
      REAL WORK1, WORK2

      INTEGER I, J, K

*.

*   Convert back from 1D pixels to 2D ones.

      I = NINT(PIX-1.0)/NAXIS2 + 1
      J = NINT(PIX) - (I-1)*NAXIS2

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

          WORK = EXP( -1.0*WORK )

*   If we are going round the loop for the first time, with NPAR=6
*   this will always be true

          IF( K .EQ. 4 ) THEN

*   Calculate the differentials w.r.t. the image shape parameters
            DFDA(1) = 2.0*APAR(4)*WORK*((R*COS(THETA+APAR(3)))**2.0)/
     :	              (APAR(1)*APAR(1)*APAR(1))
            DFDA(2) = 2.0*APAR(4)*WORK*((R*SIN(THETA+APAR(3)))**2.0)/
     :	              (APAR(2)*APAR(2)*APAR(2))
            DFDA(3) = 2.0*APAR(4)*WORK*R*R*(
     :      (COS(THETA+APAR(3))*SIN(THETA+APAR(3))/(APAR(1)*APAR(1)))-
     :      (SIN(THETA+APAR(3))*COS(THETA+APAR(3))/(APAR(2)*APAR(2))))

          END IF

*   Differential w.r.t the normalisation

          DFDA(K) = WORK

*   And w.r.t. the position

          WORK1 = (X*COS(APAR(3)) - Y*SIN(APAR(3)) ) /
     :            (APAR(1)*APAR(1) )

	  WORK2 = (Y*COS(APAR(3)) + X*SIN(APAR(3)) ) /
     :            (APAR(2)*APAR(2) )

	  DFDA(K+1) = 2.0*APAR(K)*WORK*
     :      (WORK2*SIN(APAR(3)) + WORK1*COS(APAR(3)))

	  DFDA(K+2) = 2.0*APAR(K)*WORK*
     :      (WORK2*COS(APAR(3)) - WORK1*SIN(APAR(3)))


      END DO

*   End of routine

  99  CONTINUE

      END

