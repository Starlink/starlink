      SUBROUTINE SCULIB_GAUSSJ (A, N, NP, B, M, MP, STATUS)
*+
*  Name:
*     SCULIB_GAUSSJ

*  Purpose:
*     Numerical Recipes in Fortran routine for solution of
*     linear equations by Gauss-Jordan elimination

*  Description:
*     Linear equation solution by Gauss-Jordan elimination. A(1:N,1:N) is an
*     input matrix stored in an array of physical dimensions NP by NP.
*     B(1:N,1:M) is an input matrix containing the M right-hand side vectors,
*     stored in an array of physical dimensions NP by MP. On output,
*     A(1:N,1:N) is replaced by its matrix inverse and B(1:N,1:M) is replaced
*     by the corresponding set of solution vectors. Parameter NMAX is the
*     largest anticipated value of N.
*     Copied from GAUSSJ on p.30 of Numerical Recipes in Fortran, with
*     STATUS added.

*  Invocation:
*     CALL SCULIB_GAUSSJ (A, N, NP, B, M, MP, STATUS)

*  Arguments:
*     A ( NP, NP ) = REAL (Given & Returned)
*       Input matrix. On exit, contains its inverse.
*     N = INTEGER (Given)
*       Required size of A (< NP)
*     NP = INTEGER (Given)
*       Dimensions of input matrix.
*     B ( NP, MP) = REAL (Given & Returned)
*       Input matrix containing M right-hand side vectors. On exit, contains
*       the solution vectors.
*     M = INTEGER (Given)
*       Required size of B. (< MP)
*     MP = INTEGER (Given)
*       Size of second dimension of B.
*     STATUS = INTEGER (Given & Returned)
*       Global status.

*  Authors:
*     J.LIGHTFOOT copied from Numerical Recipes (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*     13-SEP-1993: Original copy.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER M
      INTEGER MP
      INTEGER N
      INTEGER NP

*  Arguments Given & Returned:
      REAL A (NP,NP)
      REAL B (NP,MP)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER NMAX
      PARAMETER (NMAX = 50)

*  Local variables:
      INTEGER I
      INTEGER ICOL
      INTEGER IROW
      INTEGER J
      INTEGER K
      INTEGER L
      INTEGER LL
      INTEGER INDXC(NMAX)
      INTEGER INDXR(NMAX)
      INTEGER IPIV(NMAX)
      REAL    BIG
      REAL    DUM
      REAL    PIVINV

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  the integer arrays IPIV, INDXR and INDXC are used for book-keeping on the
*  pivoting

      DO J = 1, N
         IPIV (J) = 0
      END DO

*  this is the main loop over the columns to be reduced

      DO I = 1, N

         BIG = 0.0

*  this is the outer loop of the search for a pivot element

         DO J = 1, N

            IF (IPIV(J) .NE. 1) THEN
               DO K = 1, N
                  IF (IPIV(K) .EQ. 0) THEN
                     IF (ABS(A(J,K)) .GE. BIG) THEN
                        BIG = ABS(A(J,K))
                        IROW = J
                        ICOL = K
                     END IF
                  ELSE IF (IPIV(K) .GT. 1) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'SCULIB_GAUSSJ: singular '//
     :                 'matrix', STATUS)
                     RETURN
                  END IF
               END DO
            END IF
         END DO
         IPIV (ICOL) = IPIV (ICOL) + 1

*  we now have the pivot element, so we interchange rows if necessary to put
*  the pivot element on the diagonal. The columns are not physically changed,
*  only relabled: INDXC(I), the column of the Ith pivot element, is the Ith
*  column that is reduced, while INDXR(I) is the row in which that pivot
*  element was originally located. If INDXR(I)#INDXC(I) there is an implied
*  column interchange. With this form of book-keeping the solution B's will
*  end up in the correct order, and the inverse matrix will be scrambled by
*  columns.

         IF (IROW .NE. ICOL) THEN
            DO L = 1, N
               DUM = A (IROW,L)
               A (IROW,L) = A (ICOL,L)
               A (ICOL,L) = DUM
            END DO
            DO L = 1, M
               DUM = B (IROW, L)
               B (IROW,L) = B (ICOL,L)
               B (ICOL,L) = DUM
            END DO
         END IF

*  we are now ready to divide the pivot row by the pivot element, located at
*  IROW and ICOL.

         INDXR (I) = IROW
         INDXC (I) = ICOL

         IF (A(ICOL,ICOL) .EQ. 0.0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_GAUSSJ: singular matrix', STATUS)
            RETURN
         END IF

         PIVINV = 1.0 / A (ICOL,ICOL)
         A (ICOL,ICOL) = 1.0
         DO L = 1, N
            A (ICOL,L) = A (ICOL,L) * PIVINV
         END DO
         DO L = 1, M
            B (ICOL,L) = B (ICOL,L) * PIVINV
         END DO

*  next we reduce the rows, except for the pivot one of course

         DO LL = 1, N
            IF (LL .NE. ICOL) THEN
               DUM = A (LL,ICOL)
               A (LL,ICOL) = 0.0
               DO L = 1, N
                  A (LL,L) = A (LL,L) - A (ICOL,L) * DUM
               END DO
               DO L = 1, M
                  B (LL,L) = B (LL,L) - B (ICOL,L) * DUM
               END DO
            END IF
         END DO
      END DO

*  it only remains to to unscramble the solution in view of the column
*  interchanges. We do this by interchanging pairs of columns in the reverse
*  order that the permutation was built up.

      DO L = N, 1, -1
         IF (INDXR(L) .NE. INDXC(L)) THEN
            DO K = 1, N
               DUM = A (K,INDXR(L))
               A (K,INDXR(L)) = A (K,INDXC(L))
               A (K,INDXC(L)) = DUM
            END DO
         END IF
      END DO

      END
