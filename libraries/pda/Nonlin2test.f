      PROGRAM NONLIN2TEST

C     Illustrate the use of the Hanson-Krogh nonlinear least
C     squares solver for fitting two exponentials to data.
C
C     The problem is to find the four variables x(1),...,x(4)
C     that are in the model function
C
C          h(t) = x(1)*exp(x(2)*t) + x(3)*exp(x(4)*t)
C     There are values of h(t) given at five values of t,
C     t=0.05, 0.1, 0.4, 0.5, and 1.0.
C     We also have problem constraints that x(2), x(4) .le. 0, x(1),
C     x(3) .ge. 0, and a minimal separation of 0.05 between x(2) and
C     x(4).  Nothing more about the values of the parameters is known
C     except that x(2),x(4) are approximately .ge. 1/min t.
C     Thus we have no further knowledge of their values.
C     For that reason all of the initial values are set to zero.
C
C     Dimension for the nonlinear solver.
      DOUBLE PRECISION FJ(6,5),BL(5),BU(5),X(4),ROPT(001),WA(640)
C  EDIT on 950228-1300:
      DOUBLE PRECISION RNORM
      INTEGER IND(5),IOPT(24),IWA(084)

      EXTERNAL DQEDEX

      DATA LDFJ,LWA,LIWA/6,640,084/

      MCON = 1
      MEQUA = 5
      NVARS = 4
C     Define the constraints for variables.
      BL(1) = 0.
      BL(2) = -25.
      BU(2) = 0.
      BL(3) = 0.
      BL(4) = -25.
      BU(4) = 0.
C     Define the constraining value (separation) for the arguments.
      BL(5) = 0.05
C     Define all of the constraint indicators.
      IND(1) = 1
      IND(2) = 3
      IND(3) = 1
      IND(4) = 3
      IND(5) = 1
C     Define the initial values of the variables.
C     We don't know anything more, so all variables are set zero.
      DO 10 J = 1,NVARS
         X(J) = 0.0D0
   10 CONTINUE
C     Tell how much storage we gave the solver.
      IWA(1) = LWA
      IWA(2) = LIWA
C     No additional options are in use.
      IOPT(01) = 99
      CALL PDA_DQED(DQEDEX,MEQUA,NVARS,MCON,IND,BL,BU,X,FJ,LDFJ,RNORM,
     :              IGO,IOPT,ROPT,IWA,WA)
      NOUT = 6
      WRITE (NOUT,9001) (X(J),J=1,NVARS)
      WRITE (NOUT,9011) RNORM
      WRITE (NOUT,9021) IGO

      STOP

 9001 FORMAT (' MODEL IS H(T) = X(1)*EXP(-T*X(2)) + X(3)*EXP(T*X(4))',/,
     .  ' X(1),X(2),X(3),X(4) = ',/,4F12.6)
 9011 FORMAT (' RESIDUAL AFTER THE FIT = ',1PD12.4)
 9021 FORMAT (' OUTPUT FLAG FROM SOLVER =',17X,I6)
      END
      SUBROUTINE DQEDEX(X,FJ,LDFJ,IGO,IOPT,ROPT)
C     This is the subprogram for evaluating the functions
C     and derivatives for the nonlinear solver, DQED.
C
C     The user problem has MCON constraint functions,
C     MEQUA least squares equations, and involves NVARS
C     unknown variables.
C
C     When this subprogram is entered, the general (near)
C     linear constraint partial derivatives, the derivatives
C     for the least squares equations, and the associated
C     function values are placed into the array FJ(*,*).
C     All partials and functions are evaluated at the point
C     in X(*).  Then the subprogram returns to the calling
C     program unit. Typically one could do the following
C     steps:
C
C     step 1. Place the partials of the i-th constraint
C             function with respect to variable j in the
C             array FJ(i,j), i=1,...,MCON, j=1,...,NVARS.
C     step 2. Place the values of the i-th constraint
C             equation into FJ(i,NVARS+1).
C     step 3. Place the partials of the i-th least squares
C             equation with respect to variable j in the
C             array FJ(MCON+i,j), i=1,...,MEQUA,
C             j=1,...,NVARS.
C     step 4. Place the value of the i-th least squares
C             equation into FJ(MCON+i,NVARS+1).
C     step 5. Return to the calling program unit.
      DOUBLE PRECISION FJ(LDFJ,*),X(*),ROPT(*)
      DOUBLE PRECISION T(5),F(5)
      INTEGER IOPT(*)

      DATA T/0.05,0.10,0.40,0.50,1.00/
      DATA F/2.206D+00,1.994D+00,1.350D+00,1.216D+00,.7358D0/

      DATA MCON,MEQUA,NVARS/1,5,4/

C     Define the derivatives of the constraint with respect to the x(j).
      FJ(1,1) = 0.D0
      FJ(1,2) = 1.D0
      FJ(1,3) = 0.D0
      FJ(1,4) = -1.D0
C     Define the value of this constraint.
      FJ(1,5) = X(2) - X(4)
C     Define the derivatives and residuals for the data model.
      DO 10 I = 1,MEQUA
         E1 = EXP(X(2)*T(I))
         E2 = EXP(X(4)*T(I))
         FJ(MCON+I,1) = E1
         FJ(MCON+I,2) = X(1)*T(I)*E1
         FJ(MCON+I,3) = E2
         FJ(MCON+I,4) = X(3)*T(I)*E2
         FJ(MCON+I,5) = X(1)*E1 + X(3)*E2 - F(I)
   10 CONTINUE
      RETURN
      END
