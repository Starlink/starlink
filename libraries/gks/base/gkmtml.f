C# IL>=a, OL>=1
      SUBROUTINE GKMTML(IN1,IN2,OUT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Each of the arguments is a 2x3 matrix (in FORTRAN a (3,2) array) which
*     can perform a transformation on a point as follows:
*
*         -   -     -                           -     -   -
*        |X new|   | A (1,1)   B (2,1)   C (3,1) |   |X old|
*        |Y new| = | D (1,2)   E (2,2)   F (3,2) | x |Y old|
*        |  1  | =  -                           -    |  1  |
*         -   -                                       -   -
*
*     or    P new   =   M  x  P old
*
*     Given 2 matrices IN1 and IN2 which are applied in sequence:
*
*           P intermediate   =   IN1  x  P old
*      then P new   =            IN2  x  P intermediate
*
*     the routine delivers a matrix OUT such that:
*
*           P new   =   OUT  x  P old
*
*
*  MAINTENANCE LOG
*  ---------------
*      9/11/83  JRG   Created
*     30/11/83  AS    Change name from MTML to KMTML
*     04/03/84  JGW   use TMPOUT to permit Pnew = Pold
*     10/04/84  JGW   dimension TMPOUT
*
*  ARGUMENTS
*  ---------
*     INP  IN1     First matrix
*     INP  IN2     Second matrix
*     OUT  OUT     Resulting matrix
*
      REAL IN1(3,2), IN2(3,2), OUT(3,2)
*
*  LOCALS
*  ------
*     EXTRA  Array which holds the extension to each of the matrices
*            (see "ALGORITHM" below)
*     I,J    Loop counters
*
      INTEGER I,J
      REAL EXTRA(3),TMPOUT(3,2)
      DATA EXTRA/0.0,0.0,1.0/
*
*  ALGORITHM
*  ---------
*     The result is achieved as follows:
*
*       (a) Extend each of the input matrices by a 3rd row (in
*           array EXTRA) consisting of the values (0,0,1)
*
*       (b) Multiply the resulting matrices:
*                OUT = IN2 x IN1  (extended)
*           but without producing the 3rd row of the result.
*
*---------------------------------------------------------------------


*   Loop over each index in turn
      DO 200 I=1,3
        DO 100 J=1,2
*         Calculate element in Ith column and Jth row of result
            TMPOUT(I,J)=IN2(1,J)*IN1(I,1)
     :                 +IN2(2,J)*IN1(I,2)
     :                 +IN2(3,J)*EXTRA(I)
  100   CONTINUE
  200 CONTINUE
      DO 400 I=1,3
        DO 300 J=1,2
           OUT(I,J) = TMPOUT(I,J)
  300   CONTINUE
  400 CONTINUE
      END
