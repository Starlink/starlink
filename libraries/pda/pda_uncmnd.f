      SUBROUTINE PDA_UNCMND (N,X0,FCN,X,F,INFO,W,LW)
C***BEGIN PROLOGUE  PDA_UNCMND
C***DATE WRITTEN   870923    (YYMMDD)
C***REVISION DATE  871222    (YYMMDD)
C***CATEGORY NO.  G1B1A1
C***KEYWORDS  UNCONSTRAINED MINIMIZATION
C***AUTHOR  NASH, S.G., (GEORGE MASON UNIVERSITY)
C***PURPOSE  PDA_UNCMND minimizes a smooth nonlinear function of n variables.
C            A subroutine that computes the function value at any point
C            must be supplied, but derivative values are not required.
C            PDA_UNCMND provides a simple interface to more flexible lower
C            level routines.  User has no control over options.
C
C***DESCRIPTION
C     From the book, "Numerical Methods and Software" by
C                D. Kahaner, C. Moler, S. Nash
C                Prentice Hall, 1988
C
C     This routine uses a quasi-Newton algorithm with line search
C     to minimize the function represented by the subroutine FCN.
C     At each iteration, the nonlinear function is approximated
C     by a quadratic function derived from a Taylor series.
C     The quadratic function is minimized to obtain a search direction,
C     and an approximate minimum of the nonlinear function along
C     the search direction is found using a line search.  The
C     algorithm computes an approximation to the second derivative
C     matrix of the nonlinear function using quasi-Newton techniques.
C
C     The PDA_UNCMND package is quite general, and provides many options
C     for the user.  However, this subroutine is designed to be
C     easy to use, with few choices allowed.  For example:
C
C     1.  Only function values need be computed.  First derivative
C     values are obtained by finite-differencing.  This can be
C     very costly when the number of variables is large.
C
C     2.  It is assumed that the function values can be obtained
C     accurately (to an accuracy comparable to the precision of
C     the computer arithmetic).
C
C     3.  At most 150 iterations are allowed.
C
C     4.  It is assumed that the function values are well-scaled,
C     that is, that the optimal function value is not pathologically
C     large or small.
C
C     For more information, see the reference listed below.
C
C PARAMETERS
C ----------
C N            --> INTEGER
C                  Dimension of problem
C X0(N)        --> DOUBLE PRECISION
C                  Initial estimate of minimum
C FCN          --> Name of routine to evaluate minimization function.
C                  Must be declared EXTERNAL in calling routine, and
C                  have calling sequence
C                      SUBROUTINE FCN(N, X, F)
C                  with N and X as here, F the computed function value.
C X(N)        <--  DOUBLE PRECISION
C                  Local minimum
C F           <--  DOUBLE PRECISION
C                  Function value at local minimum X
C INFO        <--  INTEGER
C                  Termination code
C                      INFO =  0:  Optimal solution found
C                      INFO =  1:  Terminated with gradient small,
C                                  X is probably optimal
C                      INFO =  2:  Terminated with stepsize small,
C                                  X is probably optimal
C                      INFO =  3:  Lower point cannot be found,
C                                  X is probably optimal
C                      INFO =  4:  Iteration limit (150) exceeded
C                      INFO =  5:  Too many large steps,
C                                  function may be unbounded
C                      INFO = -1:  Insufficient workspace
C W(LW)        --> DOUBLE PRECISION
C                  Workspace
C LW           --> INTEGER
C                  Size of workspace, at least N*(N+10)
C
C***REFERENCES  R.B. SCHNABEL, J.E. KOONTZ, AND BE.E. WEISS, A MODULAR
C                 SYSTEM OF ALGORITHMS FOR UNCONSTRAINED MINIMIZATION,
C                 REPORT CU-CS-240-82, COMP. SCI. DEPT., UNIV. OF
C                 COLORADO AT BOULDER, 1982.
C***MODIFICATION
C   950404 Remove calls to XERROR, which are only level-0 messages that
C          duplicate information in the INFO argument.  (HME).
C***ROUTINES CALLED  PDA_OPTDRD, XERROR
C***END PROLOGUE  PDA_UNCMND
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(N),X(N),W(LW)
      CHARACTER ERRMSG*80
      EXTERNAL  FCN, PDA_D1FCND, PDA_D2FCND
C----------------------------------------------------------------
C SUBDIVIDE WORKSPACE
C----------------------------------------------------------------
C***FIRST EXECUTABLE STATEMENT  PDA_UNCMND
      IG  = 1
      IT  = IG  + N
      IW1 = IT  + N
      IW2 = IW1 + N
      IW3 = IW2 + N
      IW4 = IW3 + N
      IW5 = IW4 + N
      IW6 = IW5 + N
      IW7 = IW6 + N
      IW8 = IW7 + N
      IA  = IW8 + N
      LWMIN = IA + N*N-1
      IF (LWMIN .GT. LW) THEN
          INFO = -1
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND ERROR (INFO=-1) -- INSUFFICIENT WORKSPACE'',
     *      '', LW = '', I5 )' ) LW
C         CALL XERROR(ERRMSG(1:60), 60, -1, 0)
          RETURN
      END IF
C----------------------------------------------------------------
C SET UP PARAMETERS FOR PDA_OPTDRD
C----------------------------------------------------------------
C PARAMETERS THAT SHOULD NOT BE CHANGED WHEN USING CONDENSED CODE
C
C NR     = PARAMETER USED TO DIVIDE WORKSPACE
C METHOD = 1 (LINE SEARCH) -- DO NOT CHANGE
C MSG    = 9 => NO PRINTING, N=1 ALLOWED
C IAGFLG = 1 => ANALYTIC GRADIENT SUPPLIED (0 OTHERWISE)
C IAHFLG = 1 => ANALYTIC HESSIAN  SUPPLIED (0 OTHERWISE)
C IPR    = DEVICE FOR OUTPUT (IRRELEVANT IN CURRENT VERSION)
C DLT    = (IRRELEVANT PARAMETER FOR METHOD = 1)
C EPSM   = MACHINE EPSILON
C IEXP   = 1 => FUNCTION EXPENSIVE TO EVALUATE (IEXP = 0 => CHEAP)
C
      NR = N
      METHOD = 1
      MSG = 9
      IAGFLG = 0
      IAHFLG = 0
      IPR = 0
      DLT = -1.0D0
      EPSM = PDA_D1MACH(4)
      IEXP = 1
C
C PARAMETERS THAT MAY BE CHANGED:
C
C NDIGIT = -1 => PDA_OPTDRD ASSUMES F IS FULLY ACCURATE
C ITNLIM = 150 = MAXIMUM NUMBER OF ITERATIONS ALLOWED
C GRADTL = ZERO TOLERANCE FOR GRADIENT, FOR CONVERGENCE TESTS
C STEPMX = MAXIMUM ALLOWABLE STEP SIZE
C STEPTL = ZERO TOLERANCE FOR STEP, FOR CONVERGENCE TESTS
C FSCALE = TYPICAL ORDER-OF-MAGNITUDE SIZE OF FUNCTION
C TYPSIZ = TYPICAL ORDER-OF-MAGNITUDE SIZE OF X (STORED IN W(LT))
C
      NDIGIT = -1
      ITNLIM = 150
      GRADTL = EPSM**(1.0D0/3.0D0)
      STEPMX = 0.0D0
      STEPTL = SQRT(EPSM)
      FSCALE = 1.0D0
      DO 10 LT = IT,IT+N-1
          W(LT) = 1.0D0
   10 CONTINUE
C
C MINIMIZE FUNCTION
C
      CALL PDA_OPTDRD (NR, N, X0, FCN, PDA_D1FCND, PDA_D2FCND,
     +             W(IT), FSCALE,
     +             METHOD, IEXP, MSG, NDIGIT, ITNLIM, IAGFLG, IAHFLG,
     +             IPR, DLT, GRADTL, STEPMX, STEPTL,
     +             X, F, W(IG), INFO, W(IA),
     +             W(IW1), W(IW2), W(IW3), W(IW4),
     +             W(IW5), W(IW6), W(IW7), W(IW8))
C
      IF (INFO .EQ. 1) THEN
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND WARNING -- INFO = 1'',
     *      '': PROBABLY CONVERGED, GRADIENT SMALL'')' )
C         CALL XERROR(ERRMSG(1:62), 62, INFO, 0)
      END IF
      IF (INFO .EQ. 2) THEN
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND WARNING -- INFO = 2'',
     *      '': PROBABLY CONVERGED, STEPSIZE SMALL'')' )
C         CALL XERROR(ERRMSG(1:62), 62, INFO, 0)
      END IF
      IF (INFO .EQ. 3) THEN
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND WARNING -- INFO = 3'',
     *      '': CANNOT FIND LOWER POINT'')' )
C         CALL XERROR(ERRMSG(1:51), 51, INFO, 0)
      END IF
      IF (INFO .EQ. 4) THEN
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND WARNING -- INFO = 4'',
     *      '': TOO MANY ITERATIONS'')' )
C         CALL XERROR(ERRMSG(1:47), 47, INFO, 0)
      END IF
      IF (INFO .EQ. 5) THEN
          WRITE(ERRMSG, '(
     *      ''PDA_UNCMND WARNING -- INFO = 5'',
     *      '': TOO MANY LARGE STEPS, POSSIBLY UNBOUNDED'')' )
C         CALL XERROR(ERRMSG(1:72), 68, INFO, 0)
      END IF
C
      RETURN
      END
