      SUBROUTINE PDA_DBOCLS(W,MDW,MCON,MROWS,NCOLS,BL,BU,IND,IOPT,X,
     *                      RNORMC,RNORM,MODE,RW,IW)
C***BEGIN PROLOGUE  DBOCLS
C***DATE WRITTEN   821220   (YYMMDD)
C***REVISION DATE  880722   (YYMMDD)
C***CATEGORY NO.  K1A2A,G2E,G2H1,G2H2
C***KEYWORDS  BOUNDS,CONSTRAINTS,INEQUALITY,LEAST SQUARES,LINEAR
C***AUTHOR  HANSON, R. J., SNLA
C***PURPOSE  Solve the bounded and constrained least squares
C            problem consisting of solving the equation
C                      E*X = F  (in the least squares sense)
C             subject to the linear constraints
C                            C*X = Y.
C***DESCRIPTION
C
C     This subprogram solves the bounded and constrained least squares
C     problem. The problem statement is:
C
C     Solve E*X = F (least squares sense), subject to constraints
C     C*X=Y.
C
C     In this formulation both X and Y are unknowns, and both may
C     have bounds on any of their components.  This formulation
C     of the problem allows the user to have equality and inequality
C     constraints as well as simple bounds on the solution components.
C
C     This constrained linear least squares subprogram solves E*X=F
C     subject to C*X=Y, where E is MROWS by NCOLS, C is MCON by NCOLS.
C
C      The user must have dimension statements of the form
C
C      DIMENSION W(MDW,NCOLS+MCON+1), BL(NCOLS+MCON), BU(NCOLS+MCON),
C     * X(2*(NCOLS+MCON)+2+NX), RW(6*NCOLS+5*MCON)
C       INTEGER IND(NCOLS+MCON), IOPT(17+NI), IW(2*(NCOLS+MCON))
C
C     (here NX=number of extra locations required for the options; NX=0
C     if no options are in use. Also NI=number of extra locations
C     for options 1-9.
C
C    INPUT
C    -----
C
C    -------------------------
C    W(MDW,*),MCON,MROWS,NCOLS
C    -------------------------
C     The array W contains the (possibly null) matrix [C:*] followed by
C     [E:F].  This must be placed in W as follows:
C          [C  :  *]
C     W  = [       ]
C          [E  :  F]
C     The (*) after C indicates that this data can be undefined. The
C     matrix [E:F] has MROWS rows and NCOLS+1 columns. The matrix C is
C     placed in the first MCON rows of W(*,*) while [E:F]
C     follows in rows MCON+1 through MCON+MROWS of W(*,*). The vector F
C     is placed in rows MCON+1 through MCON+MROWS, column NCOLS+1. The
C     values of MDW and NCOLS must be positive; the value of MCON must
C     be nonnegative. An exception to this occurs when using option 1
C     for accumulation of blocks of equations. In that case MROWS is an
C     OUTPUT variable only, and the matrix data for [E:F] is placed in
C     W(*,*), one block of rows at a time. See IOPT(*) contents, option
C     number 1, for further details. The row dimension, MDW, of the
C     array W(*,*) must satisfy the inequality:
C
C     If using option 1,
C                     MDW .ge. MCON + max(max. number of
C                     rows accumulated, NCOLS)
C
C     If using option 8, MDW .ge. MCON + MROWS.
C     Else, MDW .ge. MCON + max(MROWS, NCOLS).
C
C     Other values are errors, but this is checked only when using
C     option=2.  The value of MROWS is an output parameter when
C     using option number 1 for accumulating large blocks of least
C     squares equations before solving the problem.
C     See IOPT(*) contents for details about option 1.
C
C    ------------------
C    BL(*),BU(*),IND(*)
C    ------------------
C     These arrays contain the information about the bounds that the
C     solution values are to satisfy. The value of IND(J) tells the
C     type of bound and BL(J) and BU(J) give the explicit values for
C     the respective upper and lower bounds on the unknowns X and Y.
C     The first NVARS entries of IND(*), BL(*) and BU(*) specify
C     bounds on X; the next MCON entries specify bounds on Y.
C
C    1.    For IND(J)=1, require X(J) .ge. BL(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .ge. BL(J).
C          (the value of BU(J) is not used.)
C    2.    For IND(J)=2, require X(J) .le. BU(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .le. BU(J).
C          (the value of BL(J) is not used.)
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J);
C          IF J.gt.NCOLS,        Y(J-NCOLS) .ge. BL(J) and
C                                Y(J-NCOLS) .le. BU(J).
C          (to impose equality constraints have BL(J)=BU(J)=
C          constraining value.)
C    4.    For IND(J)=4, no bounds on X(J) or Y(J-NCOLS) are required.
C          (the values of BL(J) and BU(J) are not used.)
C
C     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
C     IND(J)=3 (upper and lower bounds) the condition BL(J) .gt. BU(J)
C     is  an  error.   The values BL(J), BU(J), J .gt. NCOLS, will be
C     changed.  Significant changes mean that the constraints are
C     infeasible.  (Users must make this decision themselves.)
C     The new values for BL(J), BU(J), J .gt. NCOLS, define a
C     region such that the perturbed problem is feasible.  If users
C     know that their problem is feasible, this step can be skipped
C     by using option number 8 described below.
C
C    -------
C    IOPT(*)
C    -------
C     This is the array where the user can specify nonstandard options
C     for DBOCLS( ). Most of the time this feature can be ignored by
C     setting the input value IOPT(1)=99. Occasionally users may have
C     needs that require use of the following subprogram options. For
C     details about how to use the options see below: IOPT(*) CONTENTS.
C
C     Option Number   Brief Statement of Purpose
C     ------ ------   ----- --------- -- -------
C           1         Return to user for accumulation of blocks
C                     of least squares equations.  The values
C                     of IOPT(*) are changed with this option.
C                     The changes are updates to pointers for
C                     placing the rows of equations into position
C                     for processing.
C           2         Check lengths of all arrays used in the
C                     subprogram.
C           3         Column scaling of the data matrix, [C].
C                                                        [E]
C           4         User provides column scaling for matrix [C].
C                                                             [E]
C           5         Provide option array to the low-level
C                     subprogram DBOLS( ).
C                     {Provide option array to the low-level
C                     subprogram DBOLSM( ) by imbedding an
C                     option array within the option array to
C                     DBOLS(). Option 6 is now disabled.}
C           7         Move the IOPT(*) processing pointer.
C           8         Do not preprocess the constraints to
C                     resolve infeasibilities.
C           9         Do not pretriangularize the least squares matrix.
C          99         No more options to change.
C
C    ----
C    X(*)
C    ----
C     This array is used to pass data associated with options 4,5 and
C     6. Ignore this parameter (on input) if no options are used.
C     Otherwise see below: IOPT(*) CONTENTS.
C
C
C    OUTPUT
C    ------
C
C    -----------------
C    X(*),RNORMC,RNORM
C    -----------------
C     The array X(*) contains a solution (if MODE .ge.0 or .eq.-22) for
C     the constrained least squares problem. The value RNORMC is the
C     minimum residual vector length for the constraints C*X - Y = 0.
C     The value RNORM is the minimum residual vector length for the
C     least squares equations. Normally RNORMC=0, but in the case of
C     inconsistent constraints this value will be nonzero.
C     The values of X are returned in the first NVARS entries of X(*).
C     The values of Y are returned in the last MCON entries of X(*).
C
C    ----
C    MODE
C    ----
C     The sign of MODE determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status. A
C     value of MODE .ge. 0 signifies that the subprogram has completed
C     normally. The value of mode (.ge. 0) is the number of variables
C     in an active status: not at a bound nor at the value zero, for
C     the case of free variables. A negative value of MODE will be one
C     of the cases (-57)-(-41), (-37)-(-22), (-19)-(-2). Values .lt. -1
C     correspond to an abnormal completion of the subprogram. These
C     error messages are in groups for the subprograms DBOCLS(),
C     DBOLSM(), and DBOLS().  An approximate solution will be returned
C     to the user only when max. iterations is reached, MODE=-22.
C
C    -----------
C    RW(*),IW(*)
C    -----------
C     These are working arrays.  (normally the user can ignore the
C     contents of these arrays.)
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify some internal variables
C     in the subprogram without recompiling the source code. A central
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. At the pointer position the option number is extracted and
C     used for locating other information that allows for options to be
C     changed. The portion of the array IOPT(*) that is used for each
C     option is fixed; the user and the subprogram both know how many
C     locations are needed for each option. The value of LP is updated
C     for each option based on the amount of storage in IOPT(*) that is
C     required. A great deal of error checking is done by the
C     subprogram on the contents of the option array. Nevertheless it
C     is still possible to give the subprogram optional input that is
C     meaningless. For example option 4 uses the locations
C     X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing scaling data.
C     The user must manage the allocation of these locations.
C
C   1
C   -
C     This option allows the user to solve problems with a large number
C     of rows compared to the number of variables. The idea is that the
C     subprogram returns to the user (perhaps many times) and receives
C     new least squares equations from the calling program unit.
C     Eventually the user signals "that's all" and a solution is then
C     computed. The value of MROWS is an output variable when this
C     option is used. Its value is always in the range 0 .le. MROWS
C     .le. NCOLS+1. It is the number of rows after the
C     triangularization of the entire set of equations. If LP is the
C     processing pointer for IOPT(*), the usage for the sequential
C     processing of blocks of equations is
C
C
C        IOPT(LP)=1
C         Move block of equations to W(*,*) starting at
C         the first row of W(*,*).
C        IOPT(LP+3)=# of rows in the block; user defined
C
C     The user now calls DBOCLS( ) in a loop. The value of IOPT(LP+1)
C     directs the user's action. The value of IOPT(LP+2) points to
C     where the subsequent rows are to be placed in W(*,*). Both of
C     these values are first defined in the subprogram. The user
C     changes the value of IOPT(LP+1) (to 2) as a signal that all of
C     the rows have been processed.
C
C
C      .<LOOP
C      . CALL PDA_DBOCLS( )
C      . IF(IOPT(LP+1) .EQ. 1) THEN
C      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
C      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN
C      .    W(*,*) STARTING AT ROW MCON + IOPT(LP+2).
C      .
C      .    IF( THIS IS THE LAST BLOCK OF EQUATIONS ) THEN
C      .       IOPT(LP+1)=2
C      .<------CYCLE LOOP
C      .    ELSE IF (IOPT(LP+1) .EQ. 2) THEN
C      <-------EXIT LOOP SOLUTION COMPUTED IF MODE .GE. 0
C      . ELSE
C      . ERROR CONDITION; SHOULD NOT HAPPEN.
C      .<END LOOP
C
C     Use of this option adds 4 to the required length of IOPT(*).
C
C   2
C   -
C     This option is useful for checking the lengths of all arrays used
C     by DBOCLS( ) against their actual requirements for this problem.
C     The idea is simple: the user's program unit passes the declared
C     dimension information of the arrays. These values are compared
C     against the problem-dependent needs within the subprogram. If any
C     of the dimensions are too small an error message is printed and a
C     negative value of MODE is returned, -41 to -47. The printed error
C     message tells how long the dimension should be. If LP is the
C     processing pointer for IOPT(*),
C
C        IOPT(LP)=2
C        IOPT(LP+1)=Row dimension of W(*,*)
C        IOPT(LP+2)=Col. dimension of W(*,*)
C        IOPT(LP+3)=Dimensions of BL(*),BU(*),IND(*)
C        IOPT(LP+4)=Dimension of X(*)
C        IOPT(LP+5)=Dimension of RW(*)
C        IOPT(LP+6)=Dimension of IW(*)
C        IOPT(LP+7)=Dimension of IOPT(*)
C         .
C        CALL PDA_DBOCLS( )
C
C     Use of this option adds 8 to the required length of IOPT(*).
C
C   3
C   -
C     This option can change the type of scaling for the data matrix.
C     Nominally each nonzero column of the matrix is scaled so that the
C     magnitude of its largest entry is equal to the value ONE. If LP
C     is the processing pointer for IOPT(*),
C
C        IOPT(LP)=3
C        IOPT(LP+1)=1,2 or 3
C            1= Nominal scaling as noted;
C            2= Each nonzero column scaled to have length ONE;
C            3= Identity scaling; scaling effectively suppressed.
C         .
C        CALL PDA_DBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   4
C   -
C     This options allows the user to provide arbitrary (positive)
C     column scaling for the matrix. If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=4
C        IOPT(LP+1)=IOFF
C        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
C        = Positive scale factors for cols. of E.
C         .
C        CALL PDA_DBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*)
C     and NCOLS to the required length of X(*).
C
C   5
C   -
C     This option allows the user to provide an option array to the
C     low-level subprogram DBOLS( ). If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=5
C        IOPT(LP+1)= Position in IOPT(*) where option array
C                    data for DBOLS( ) begins.
C         .
C        CALL PDA_DBOCLS( )
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   6
C   -
C     This option is no longer operative.  To pass an option array
C     to the low-level subprogram DBOLSM( ), imbed it within an option
C     array passed to DBOLS() using option 5.
C
C   7
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing pointer moves to locations
C     LP+2 if option number 7 is used with the value -7.  For
C     example to skip over locations 3,...,NCOLS+2,
C
C       IOPT(1)=7
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL PDA_DBOCLS( )
C
C     CAUTION: Misuse of this option can yield some very hard-to-find
C     bugs. Use it with care. It is intended to be used for passing
C     option arrays to other subprograms.
C
C   8
C   -
C     This option allows the user to suppress the algorithmic feature
C     of DBOCLS( ) that processes the constraint equations C*X = Y and
C     resolves infeasibilities. The steps normally done are to solve
C     C*X - Y = 0 in a least squares sense using the stated bounds on
C     both X and Y. Then the "reachable" vector Y = C*X is computed
C     using the solution X obtained. Finally the stated bounds for Y are
C     enlarged to include C*X. To suppress the feature:
C
C
C       IOPT(LP)=8
C         .
C       CALL PDA_DBOCLS( )
C
C     Use of this option adds 1 to the required length of IOPT(*).
C
C   9
C   -
C     This option allows the user to suppress the pretriangularizing
C     step of the least squares matrix that is done within DBOCLS( ).
C     This is primarily a means of enhancing the subprogram efficiency
C     and has little effect on accuracy. To suppress the step, set:
C
C       IOPT(LP)=9
C         .
C       CALL PDA_DBOCLS( )
C
C     Use of this option adds 1 to the required length of IOPT(*).
C
C   99
C   --
C     There are no more options to change.
C
C     Only option numbers -99, -9,-8,...,-1, 1,2,...,9, and 99 are
C     permitted. Other values are errors. Options -99,-1,...,-9 mean
C     that the respective options 99,1,...,9 are left at their default
C     values. An example is the option to suppress the preprocessing of
C     contraints:
C
C       IOPT(1)=-8 Option is recognized but not changed
C       IOPT(2)=99
C       CALL PDA_DBOCLS( )
C
C    Error Messages for DBOCLS()
C    ----- -------- --- --------
C
C WARNING in...
C DBOCLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
C OF EFFECTIVE ROWS=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        41
C
C WARNING IN...
C DBOCLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+
C MCON+1=(I2).
C           IN ABOVE MESSAGE, I1=         2
C           IN ABOVE MESSAGE, I2=         3
C ERROR NUMBER =        42
C
C WARNING IN...
C DBOCLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1)
C MUST BE .GE. NCOLS+MCON=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        43
C
C WARNING IN...
C DBOCLS(). THE DIMENSION OF X()=(I1) MUST BE
C .GE. THE REQD.LENGTH=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        44
C
C WARNING IN...
C DBOCLS(). THE .
C DBOCLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS+2*MCON=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         4
C ERROR NUMBER =        46
C
C WARNING IN...
C DBOCLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD.
C LEN.=(I2).
C           IN ABOVE MESSAGE, I1=        16
C           IN ABOVE MESSAGE, I2=        18
C ERROR NUMBER =        47
C
C WARNING IN...
C DBOCLS(). ISCALE OPTION=(I1) MUST BE 1-3.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        48
C
C WARNING IN...
C DBOCLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCALING
C MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        49
C
C WARNING IN...
C DBOCLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE.
C  COMPONENT (I1) NOW = (R1).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=    0.
C ERROR NUMBER =        50
C
C WARNING IN...
C DBOCLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.
C           IN ABOVE MESSAGE, I1=      1001
C ERROR NUMBER =        51
C
C WARNING IN...
C DBOCLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE. MDW-MCON=(I2).
C           IN ABOVE MESSAGE, I1=         2
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        52
C
C WARNING IN...
C DBOCLS(). MDW=(I1) MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        53
C
C WARNING IN...
C DBOCLS(). MCON=(I1) MUST BE NONNEGATIVE.
C           IN ABOVE MESSAGE, I1=        -1
C ERROR NUMBER =        54
C
C WARNING IN...
C DBOCLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =        55
C
C WARNING IN...
C DBOCLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =        56
C
C WARNING IN...
C DBOCLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=     .1000000000E+01
C           IN ABOVE MESSAGE, R2=    0.
C ERROR NUMBER =        57
C           LINEAR CONSTRAINTS, SNLA REPT. SAND82-1517, AUG., (1982).
C***REFERENCES  HANSON, R. J. LINEAR LEAST SQUARES WITH BOUNDS AND
C                 LINEAR CONSTRAINTS, SIAM J. SCI. STAT. COMPUT., VOL. 7,
C                 NO. 3, JULY, 1986.
C***ROUTINES CALLED  PDA_D1MACH,PDA_DASUM,DBOLS,DCOPY,PDA_DDOT,PDA_DNRM2,DSCAL,XERRWV
C***COMMON BLOCKS    DBOCOM
C***END PROLOGUE  DBOCLS
C     REVISED 880722-1100
C     REVISED YYMMDD-HHMM
C
C    PURPOSE
C    -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE LEAST SQUARES
C     PROBLEM CONSISTING OF LINEAR CONSTRAINTS
C
C              C*X = Y
C
C     AND LEAST SQUARES EQUATIONS
C
C              E*X = F
C
C     IN THIS FORMULATION THE VECTORS X AND Y ARE BOTH UNKNOWNS.
C     FURTHER, X AND Y MAY BOTH HAVE USER-SPECIFIED BOUNDS ON EACH
C     COMPONENT.  THE USER MUST HAVE DIMENSION STATEMENTS OF THE
C     FORM
C
C     DIMENSION W(MDW,NCOLS+MCON+1), BL(NCOLS+MCON),BU(NCOLS+MCON),
C               X(2*(NCOLS+MCON)+2+NX), RW(6*NCOLS+5*MCON)
C
C     INTEGER IND(NCOLS+MCON), IOPT(16+NI), IW(2*(NCOLS+MCON))
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM TO DBOCLS AND THE STRINGS
C     /SDOT/ TO /PDA_DDOT/, /SNRM2/ TO /PDA_DNRM2/, /SRELPR/ TO /DRELPR/,
C     /R1MACH/ TO /PDA_D1MACH/, /E0/ TO /D0/, /SCOPY/ TO /DCOPY/,
C     /SSCAL/ TO /DSCAL/, /SASUM/ TO /DASUM/, /DBOLS/ TO /DBOLS/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*),X(*),RW(*)
      DOUBLE PRECISION ANORM, CNORM, ONE, RNORM, RNORMC, DRELPR
      DOUBLE PRECISION T, T1, T2, PDA_DDOT, PDA_DNRM2, WT, ZERO
      DOUBLE PRECISION PDA_DASUM, PDA_D1MACH
      EXTERNAL PDA_DDOT, PDA_DNRM2, PDA_D1MACH, PDA_DASUM
C     THIS VARIABLE REMAINS TYPED REAL.
      REAL RDUM
      INTEGER IND(*),IOPT(*),IW(*),JOPT(05),TROWS,TCOLS
      LOGICAL CHECKL,FILTER,ACCUM,PRETRI
      COMMON /DBOCOM/ IDOPE(5)
      SAVE IGO,ACCUM,CHECKL
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  DBOCLS
      NERR = 0
      MODE = 0
      LEVEL = 1
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              NERR = 53
              NCHAR = 36
              CALL PDA_XERRWV('DBOCLS(). MDW=(I1) MUST BE POSITIVE.',
     *             NCHAR, NERR,LEVEL,1,MDW,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT NUMBER OF CONSTRAINTS IS NONNEGATIVE.
          IF (MCON.LT.0) THEN
              NERR = 54
              NCHAR = 40
              CALL PDA_XERRWV(
     *             'DBOCLS(). MCON=(I1) MUST BE NONNEGATIVE.',
     *             NCHAR,NERR,LEVEL,1,MCON,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              NERR = 55
              NCHAR = 59
              CALL PDA_XERRWV(
     *     'DBOCLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.'
     *                    ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS + MCON
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  NERR = 56
                  NCHAR = 46
                  CALL PDA_XERRWV(
     *                  'DBOCLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.'
     *                        ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS + MCON
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                      NERR = 57
                      NCHAR = 58
                      CALL PDA_XERRWV(
     *      'DBOCLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).'
     *                            ,NCHAR,NERR,LEVEL,1,J,IDUM,2,
     *                             REAL(BL(J)), REAL(BU(J)) )
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
              END IF
   20     CONTINUE
C     END PROCEDURE
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.D0
          ONE = 1.D0
          DRELPR = PDA_D1MACH(4)
          CHECKL = .FALSE.
          FILTER = .TRUE.
          LENX = 2* (NCOLS+MCON) + 2
          ISCALE = 1
          IGO = 1
          ACCUM = .FALSE.
          PRETRI = .TRUE.
          LOPT = 0
          LP = 0
          LDS = 0
C     DO FOREVER
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS TO CHANGE.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = LP+1
C
C     SEND COL. SCALING TO DBOLS().
              IDOPE(4)=1
C
C     NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
              IDOPE(5)=1
C
C     CHANGE PRETRIANGULARIZATION FACTOR IN DBOLSM().
              IDOPE(1) = NCOLS + MCON + 1
C
C     PASS WEIGHT TO DBOLSM() FOR RANK TEST.
              IDOPE(2) = NCOLS + MCON + 2
              IDOPE(3) = MCON
C     EXIT FOREVER
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
C     CYCLE FOREVER
              GO TO 50
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG LOCATION, ROW STACKING POINTER
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*) BELOW THE ROWS FOR THE CONSTRAINT MATRIX C.
C       SET IOPT(LOCACC+2)=NO. OF LEAST SQUARES EQUATIONS IN BLOCK.
C              LOOP
C              CALL PDA_DBOCLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS. INTO W(*,*), STARTING AT
C                      ROW IOPT(LOCACC+1).
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
                  IOPT(LOCACC+1) = MCON + 1
                  ACCUM = .TRUE.
                  IOPT(LOCACC) = IGO
              END IF
              LDS = 4
C     CYCLE FOREVER
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MCON+MAX(MOUT,NCOLS), IF MCON.GT.0 .AND FILTER
C     LMDW.GE.MCON+MOUT, OTHERWISE
C
C     LNDW.GE.NCOLS+MCON+1
C     LLB .GE.NCOLS+MCON
C     LLX .GE.2*(NCOLS+MCON)+2+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.6*NCOLS+5*MCON
C     LLIW.GE.2*(NCOLS+MCON)
C     LIOP.GE. AMOUNT REQD. FOR OPTION ARRAY.
                  LMDW = IOPT(LOCDIM)
                  LNDW = IOPT(LOCDIM+1)
                  LLB = IOPT(LOCDIM+2)
                  LLX = IOPT(LOCDIM+3)
                  LLRW = IOPT(LOCDIM+4)
                  LLIW = IOPT(LOCDIM+5)
                  LIOPT = IOPT(LOCDIM+6)
                  CHECKL = .TRUE.
              END IF
              LDS = 8
C     CYCLE FOREVER
              GO TO 30
C
C     OPTION TO MODIFY THE COLUMN SCALING.
          ELSE IF (JP.EQ.3) THEN
              IF (IP.GT.0) THEN
                  ISCALE = IOPT(LP+2)
C
C     SEE THAT ISCALE IS 1 THRU 3.
                  IF (ISCALE.LT.1 .OR. ISCALE.GT.3) THEN
                      NERR = 48
                      NCHAR = 41
                      CALL PDA_XERRWV(
     *                       'DBOCLS(). ISCALE OPTION=(I1) MUST BE 1-3.'
     *                            ,NCHAR,NERR,LEVEL,1,ISCALE,IDUM,0,
     *                            RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
              END IF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
C     SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
          ELSE IF (JP.EQ.4) THEN
              IF (IP.GT.0) THEN
                  ISCALE = 4
                  IF (IOPT(LP+2).LE.0) THEN
                      NERR = 49
                      NCHAR = 86
                      CALL PDA_XERRWV(
     *'DBOCLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCAL
     *ING MUST BE POSITIVE.',NCHAR,NERR,LEVEL,1,IOPT(LP+2),IDUM,0,RDUM,
     *                            RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  END IF
                  CALL PDA_DCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          NERR = 50
                          NCHAR = 84
                          CALL PDA_XERRWV(
     *'DBOCLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE. COMP.
     * (I1)   NOW = (R1).',NCHAR,NERR,LEVEL,1,J,IDUM,1,REAL(RW(J)),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 260
                      END IF
   40             CONTINUE
              END IF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLS().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              END IF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLSM().
C     (NO LONGER USED.) OPTION NOW MUST BE PASSED IMBEDDED IN
C     OPTION ARRAY FOR DBOLS().
          ELSE IF (JP.EQ.6) THEN
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS A
C     POINTER VALUE TO SKIP TO NEXT.
          ELSE IF (JP.EQ.7) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2)-1
                  LDS = 0
              ELSE
                  LDS = 2
              END IF
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION AVOIDS THE CONSTRAINT RESOLVING PHASE FOR
C     THE LINEAR CONSTRAINTS C*X=Y.
          ELSE IF (JP.EQ.8) THEN
              FILTER = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION SUPPRESSES PRETRIANGULARIZATION OF THE LEAST
C     SQUARES EQATIONS.
          ELSE IF (JP.EQ.9) THEN
              PRETRI = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              NERR = 51
              NCHAR = 48
              CALL PDA_XERRWV(
     *                'DBOCLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.'
     *                    ,NCHAR,NERR,LEVEL,1,JP,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
C     END FOREVER
C     END PROCEDURE
   50     CONTINUE
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
           IF(FILTER .AND. .NOT.ACCUM) THEN
                MDWL=MCON+MAX(MROWS,NCOLS)
           ELSE IF (ACCUM) THEN
                MDWL=MCON+NCOLS+1
           ELSE
                MDWL=MCON+NCOLS
           END IF
              IF (LMDW.LT.MDWL) THEN
                  NERR = 41
                  NCHAR = 88
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
     *OF EFFECTIVE ROWS=(I2).',NCHAR,NERR,LEVEL,2,LMDW,
     *MDWL,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LNDW.LT.NCOLS+MCON+1) THEN
                  NERR = 42
                  NCHAR = 75
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+MC
     *ON+1=(I2).',NCHAR,NERR,LEVEL,2,LNDW,NCOLS+MCON+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLB.LT.NCOLS+MCON) THEN
                  NERR = 43
                  NCHAR = 94
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1)
     *MUST BE .GE. NCOLS+MCON=(I2).',NCHAR,NERR,LEVEL,2,LLB,NCOLS+MCON,
     *                        0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLX.LT.LENX) THEN
                  NERR = 44
                  NCHAR = 71
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH
     *=(I2).',NCHAR,NERR,LEVEL,2,LLX,LENX,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLRW.LT.6*NCOLS+5*MCON) THEN
                  NERR = 45
                  NCHAR = 70
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 6*NCOLS+5*MCON=
     *(I2).',NCHAR,NERR,LEVEL,2,LLRW,6*NCOLS+5*MCON,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LLIW.LT.2*NCOLS+2*MCON) THEN
                  NERR = 46
                  NCHAR = 69
                  CALL PDA_XERRWV(
     *'DBOCLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS+2*MCON=(
     *I2).',NCHAR,NERR,LEVEL,2,LLIW,2*NCOLS+2*MCON,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
              IF (LIOPT.LT.LP+17) THEN
                  NERR = 47
                  NCHAR = 72
                  CALL PDA_XERRWV(
     *'DBOCLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN
     *.=(I2).',NCHAR,NERR,LEVEL,2,LIOPT,LP+17,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              END IF
C     END PROCEDURE
          END IF
      END IF
C
C     OPTIONALLY GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
C     EQUATIONS AND DIRECTIONS FOR PROCESSING THESE EQUATIONS.
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      IF (ACCUM) THEN
          MROWS = IOPT(LOCACC+1) - 1 - MCON
          INROWS = IOPT(LOCACC+2)
          MNEW = MROWS + INROWS
          IF (MNEW.LT.0 .OR. MNEW+MCON.GT.MDW) THEN
              NERR = 52
              NCHAR = 66
              CALL PDA_XERRWV(
     *'DBOCLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE.MDW-MCON=(I2)
     *',NCHAR,NERR,LEVEL,2,MNEW,MDW-MCON,0,RDUM,RDUM)
C    (RETURN TO USER PROGRAM UNIT)
              GO TO 260
          END IF
      END IF
C
C     USE THE SOFTWARE OF DBOLS( ) FOR THE TRIANGULARIZATION OF THE
C     LEAST SQUARES MATRIX.  THIS MAY INVOLVE A SYSTALTIC INTERCHANGE
C     OF PROCESSING POINTERS BETWEEN THE CALLING AND CALLED (DBOLS())
C     PROGRAM UNITS.
      JOPT(01) = 1
      JOPT(02) = 2
      JOPT(04) = MROWS
      JOPT(05) = 99
      IRW = NCOLS + 1
      IIW = 1
      IF (ACCUM .OR. PRETRI) THEN
C
C     NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
         IDOPE(5)=0
         CALL PDA_DBOLSQ(W(MCON+1,1),MDW,MOUT,NCOLS,BL,BU,IND,JOPT,X,
     .                  RNORM,MODE,RW(IRW),IW(IIW))
      ELSE
          MOUT = MROWS
      END IF
      IF (ACCUM) THEN
          ACCUM = IOPT(LOCACC) .EQ. 1
          IOPT(LOCACC+1) = JOPT(03) + MCON
          MROWS = MIN(NCOLS+1,MNEW)
      END IF
C     END PROCEDURE
      IF (ACCUM) RETURN
C     DO(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C
C     MOVE RIGHT HAND SIDE OF LEAST SQUARES EQUATIONS.
      CALL PDA_DCOPY(MOUT,W(MCON+1,NCOLS+1),1,W(MCON+1,NCOLS+MCON+1),1)
      IF (MCON.GT.0 .AND. FILTER) THEN
C
C     PROJECT THE LINEAR CONSTRAINTS INTO A REACHABLE SET.
          DO 60 I = 1,MCON
              CALL PDA_DCOPY(NCOLS,W(I,1),MDW,W(MCON+1,NCOLS+I),1)
   60     CONTINUE
C
C      PLACE (-)IDENTITY MATRIX AFTER CONSTRAINT DATA.
          DO 70 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL PDA_DCOPY(MCON,W(1,J),0,W(1,J),1)
   70     CONTINUE
          W(1,NCOLS+1) = -ONE
          CALL PDA_DCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
C
C     OBTAIN A 'FEASIBLE POINT' FOR THE LINEAR CONSTRAINTS.
          JOPT(01) = 99
          IRW = NCOLS + 1
          IIW = 1
C
C     NOTE THAT DBOLS() WAS CALLED BY DBOCLS()
              IDOPE(5)=0
          CALL PDA_DBOLSQ(W,MDW,MCON,NCOLS+MCON,BL,BU,IND,JOPT,X,RNORMC,
     *               MODEC,RW(IRW),IW(IIW))
C
C     ENLARGE THE BOUNDS SET, IF REQUIRED, TO INCLUDE POINTS THAT
C     CAN BE REACHED.
          DO 130 J = NCOLS + 1,NCOLS + MCON
              ICASE = IND(J)
              IF (ICASE.LT.4) THEN
                  T = PDA_DDOT(NCOLS,W(MCON+1,J),1,X,1)
              END IF
              GO TO (80,90,100,110),ICASE
              GO TO 120
C     CASE 1
   80         BL(J) = MIN(T,BL(J))
              GO TO 120
C     CASE 2
   90         BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 3
  100         BL(J) = MIN(T,BL(J))
              BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 4
  110         CONTINUE
  120         CONTINUE
  130     CONTINUE
C
C     MOVE CONSTRAINT DATA BACK TO THE ORIGINAL AREA.
          DO 140 J = NCOLS + 1,NCOLS + MCON
              CALL PDA_DCOPY(NCOLS,W(MCON+1,J),1,W(J-NCOLS,1),MDW)
  140     CONTINUE
      END IF
      IF (MCON.GT.0) THEN
          DO 150 J = NCOLS + 1,NCOLS + MCON
              W(MCON+1,J) = ZERO
              CALL PDA_DCOPY(MOUT,W(MCON+1,J),0,W(MCON+1,J),1)
  150     CONTINUE
C
C     PUT IN (-)IDENTITY MATRIX (POSSIBLY) ONCE AGAIN.
          DO 160 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL PDA_DCOPY(MCON,W(1,J),0,W(1,J),1)
  160     CONTINUE
          W(1,NCOLS+1) = -ONE
          CALL PDA_DCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
      END IF
C
C     COMPUTE NOMINAL COLUMN SCALING FOR THE UNWEIGHTED MATRIX.
      CNORM = ZERO
      ANORM = ZERO
      DO 170 J = 1,NCOLS
          T1 = PDA_DASUM(MCON,W(1,J),1)
          T2 = PDA_DASUM(MOUT,W(MCON+1,1),1)
          T = T1 + T2
          IF (T.EQ.ZERO) T = ONE
          CNORM = MAX(CNORM,T1)
          ANORM = MAX(ANORM,T2)
          X(NCOLS+MCON+J) = ONE/T
  170 CONTINUE
      GO TO (180,190,210,220),ISCALE
      GO TO 230
C     CASE 1
  180 CONTINUE
      GO TO 230
C     CASE 2
C
C     SCALE COLS. (BEFORE WEIGHTING) TO HAVE LENGTH ONE.
  190 DO 200 J = 1,NCOLS
          T = PDA_DNRM2(MCON+MOUT,W(1,J),1)
          IF (T.EQ.ZERO) T = ONE
          X(NCOLS+MCON+J) = ONE/T
  200 CONTINUE
      GO TO 230
C     CASE 3
C
C     SUPPRESS SCALING (USE UNIT MATRIX).
  210 X(NCOLS+MCON+1) = ONE
      CALL PDA_DCOPY(NCOLS,X(NCOLS+MCON+1),0,X(NCOLS+MCON+1),1)
      GO TO 230
C     CASE 4
C
C     THE USER HAS PROVIDED SCALING.
  220 CALL PDA_DCOPY(NCOLS,RW,1,X(NCOLS+MCON+1),1)
  230 CONTINUE
      DO 240 J = NCOLS + 1,NCOLS + MCON
          X(NCOLS+MCON+J) = ONE
  240 CONTINUE
C
C     WEIGHT THE LEAST SQUARES EQUATIONS.
      WT = SQRT(DRELPR)
      IF (ANORM.GT.ZERO) WT = WT/ANORM
      IF (CNORM.GT.ZERO) WT = WT*CNORM
      DO 250 I = 1,MOUT
          CALL PDA_DSCAL(NCOLS,WT,W(I+MCON,1),MDW)
  250 CONTINUE
      CALL PDA_DSCAL(MOUT,WT,W(MCON+1,MCON+NCOLS+1),1)
      LRW = 1
      LIW = 1
C
C     SET THE NEW TRIANGULARIZATION FACTOR.
      X(NCOLS+MCON+IDOPE(1))= ZERO
C
C     SET THE WEIGHT TO USE IN COMPONENTS .GT. MCON,
C     WHEN MAKING LINEAR INDEPENDENCE TEST.
      X(NCOLS+MCON+IDOPE(2))= ONE/WT
      IDOPE(5)=1
      TROWS = NCOLS + MCON
      TCOLS = NCOLS + MCON
      CALL PDA_DBOLSQ(W,MDW,TROWS,TCOLS,BL,BU,IND,IOPT(LOPT),X,
     *           RNORM,MODE,RW(LRW),IW(LIW))
C     END PROCEDURE
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  260 IF(MODE.GE.0)MODE=-NERR
      IGO = 0
      RETURN
C     END PROGRAM
      END
