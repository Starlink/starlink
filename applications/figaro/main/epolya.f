      SUBROUTINE EPOLYA(EL,KP1,COEFFS,X,Y)
*+
*  Name:
*     EPOLYA

*  Purpose:
*     Evaluate polynomial to fill array with values from an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL EPOLYA(EL,KP1,COEFFS,X,Y)

*  Description:
*     The polynomial is evaluated and placed into the array Y.
*        Y(J) = SUM(COEFFS(I)*X(J)**(I-1))

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in DATA
*     KP1 = INTEGER (Given)
*        Number of coeficients (=order+1)
*     COEFFS = DOUBLE PRECISION ARRAY (Given)
*        Coeficients, constant term last
*     X = REAL ARRAY (Given)
*        X Data array-points to evaluate polynomial at
*     Y = REAL ARRAY (Returned)
*        Polynomial values

*  Authors:
*     TNW: T.N.Wilkins (Durham)
*     AJH: A.J.Holloway (Manchester)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1993 (TNW):
*        Original version.
*     20-AUG-1993 (TNW):
*        Handle case of EL = 1, version with array input
*      8-OCT-1993 (TNW):
*        Order of coefficients reversed for use in twodspec
*     20-OCT-1997 (AJH):
*        Order of coefficients reversed for use in PDA.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:

      IMPLICIT NONE              ! No implicit typing
      INTEGER EL,KP1
      REAL X(EL),Y(EL)
      DOUBLE PRECISION COEFFS(KP1)
      double precision revcof(11)
      INTEGER I,J
      DOUBLE PRECISION VALUE,WAVE


      do i=1,kp1
         revcof(kp1-i+1)=coeffs(i)
      end do

      DO I = 1, EL
         WAVE = DBLE(X(I))
*         VALUE = COEFFS(1)
         VALUE = revcof(1)
         DO J =  2, KP1
*            VALUE = VALUE * WAVE + COEFFS(J)
            VALUE = VALUE * WAVE + revcof(J)
         ENDDO
         Y(I) = REAL(VALUE)
      ENDDO
      END
