*+RV_NORM        Gets unit vector in direction of input vector
*	7/29/93	P. Brisco	Explicitly declared RV_MOD
      SUBROUTINE RV_NORM(VA,VB)
 
*  Calling Arguments
      REAL VA(3)		! In	Vector, A
      REAL VB(3)		! Out	Unit vector in direction of A
*-
 
*  Local Variables
      REAL R, RV_MOD
      INTEGER I
 
      R = 1.0 / RV_MOD(VA)
 
      DO I=1,3
         VB(I) = VA(I) * R
      END DO
 
      END
