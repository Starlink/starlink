*********************************************************************
*
*   SUBROUTINE INTEP
*
*   Interpolates a function value P for a given
*   argument value XP using a table of N values (X,F).
*
*   This is a spline interpolation scheme based on Hermite
*   polynomials.   The code is from
*   G. Hill, Publ. DAO, vol 16, no. 6 (1982)
*   which uses US Airforce Surveys in Geophysics no. 272
*   as its source.
*
*   Usage:
*   For random values of XP,
*      CALL INTEP (XP,P,X,F,N,IER)
*   or, after the first call to INTEP with monotonically
*   increasing or decreasing values of XP consistent with the
*   X vector,
*      CALL EINTEP (XP,P,X,F,N,IER,EINTEP).
*
*   XP   the chosen argument value
*   P    the resultant interpolated value
*   X    the vector of independent values
*   F    the vector of function or dependent values
*   N    the number of points in the X,P vectors
*   IER  the resultant error parameter.
*   EINTEP Jump to entry point EINTEP?
*
*   If XP is beyond the extrema in the vector X, the value of F
*   at the appropriate extreme is adopted, and IER set to 2.
*
*********************************************************************

       SUBROUTINE INTEP(XP,P,X,F,N,IER,EINTEP)
       IMPLICIT NONE

       INTEGER N,IER
       REAL XP, P, F(N), X(N)

       REAL LP1, LP2, L1, L2, FP1, FP2, XPI, XPI1
       INTEGER IO, IUP, N1, I
       LOGICAL EINTEP

       LP1 = 0
       LP2 = 0
       L1 = 0
       L2 = 0
       FP1 = 0
       FP2 = 0
       XPI = 0
       XPI1 = 0
       IO = 0
       IUP = 0
       N1 = 0

       IER = 1
       IO = 1
       IUP = 0

       IF (X(2).LT.X(1)) IUP = 1
       N1 = N - 1

       IF ((XP.GE.X(N) .AND. IUP.EQ.0) .OR. (XP.LE.X(N) .AND. IUP.EQ.1))
     : THEN
          P = F(N)
          GOTO 50
       ELSEIF ((XP.LE.X(1) .AND. IUP.EQ.0) .OR.
     : (XP.GE.X(1) .AND. IUP.EQ.1)) THEN
          P = F(1)
   50     CONTINUE
          IER = 2
          GOTO 300
       ENDIF

C     ENTRY EINTEP(XP,P,X,F,N,IER)
    1 CONTINUE

       DO 100 I = IO, N
          IF (XP.LT.X(I) .AND. IUP.EQ.0) GOTO 200
          IF (XP.GT.X(I) .AND. IUP.EQ.1) GOTO 200
  100  CONTINUE
       P = F(N)
       GOTO 50
  200  CONTINUE
       I = I - 1
       IF (I.NE.IO-1) THEN
          IO = I + 1
          LP1 = 1.0/(X(I)-X(I+1))
          LP2 = 1.0/(X(I+1)-X(I))
          IF (I.EQ.1) FP1 = (F(2)-F(1))/(X(2)-X(1))
          IF (I.NE.1) THEN
             FP1 = (F(I+1)-F(I-1))/(X(I+1)-X(I-1))
          ENDIF
          IF (I.GE.N1) FP2 = (F(N)-F(N-1))/(X(N)-X(N-1))
          IF (I.LT.N1) THEN
             FP2 = (F(I+2)-F(I))/(X(I+2)-X(I))
          ENDIF
       ENDIF
       XPI1 = XP - X(I+1)
       XPI = XP - X(I)
       L1 = XPI1*LP1
       L2 = XPI*LP2
       P = F(I)*(1.0-2.0*LP1*XPI)*L1**2 + F(I+1)*(1.0-2.0*LP2*XPI1)
     :     *L2**2 + FP2*XPI1*L2**2 + FP1*XPI*L1**2

  300  CONTINUE

       END
