      SUBROUTINE PDA_DBOLSN(W,MDW,MINPUT,NCOLS,BL,BU,IND,IOPT,X,RNORM,
     .                      MODE,RW,WW,SCL,IBASIS,IBB)
C***BEGIN PROLOGUE  DBOLSM
C***REFER TO  DBOCLS,DBOLS
C***ROUTINES CALLED  IVOUT,PDA_D1MACH,DAXPY,DCOPY,PDA_DDOT,DMOUT,PDA_DNRM2,DROT,
C                    DROTG,DSWAP,DVOUT,XERRWV
C***COMMON BLOCKS    DBOCOM
C***DESCRIPTION
C
C          Solve E*X = F (least squares sense) with bounds on
C            selected X values.
C     The user must have dimension statements of the form:
C
C       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
C      * X(NCOLS+NX), RW(NCOLS), WW(NCOLS), SCL(NCOLS)
C       INTEGER IND(NCOLS), IOPT(1+NI), IBASIS(NCOLS), IBB(NCOLS)
C
C     (here NX=number of extra locations required for options 1,...,7;
C     NX=0 for no options; here NI=number of extra locations possibly
C     required for options 1-7; NI=0 for no options; NI=14 if all the
C     options are simultaneously in use.)
C
C    INPUT
C    -----
C
C    --------------------
C    W(MDW,*),MROWS,NCOLS
C    --------------------
C     The array w(*,*) contains the matrix [E:F] on entry. The matrix
C     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in
C     the array W(*,*) with E occupying the first NCOLS columns and the
C     right side vector F in column NCOLS+1. The row dimension, MDW, of
C     the array W(*,*) must satisfy the inequality MDW .ge. MROWS.
C     Other values of MDW are errors. The values of MROWS and NCOLS
C     must be positive. Other values are errors.
C
C    ------------------
C    BL(*),BU(*),IND(*)
C    ------------------
C     These arrays contain the information about the bounds that the
C     solution values are to satisfy. The value of IND(J) tells the
C     type of bound and BL(J) and BU(J) give the explicit values for
C     the respective upper and lower bounds.
C
C    1.    For IND(J)=1, require X(J) .ge. BL(J).
C    2.    For IND(J)=2, require X(J) .le. BU(J).
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J).
C    4.    For IND(J)=4, no bounds on X(J) are required.
C     The values of BL(*),BL(*) are modified by the subprogram. Values
C     other than 1,2,3 or 4 for IND(J) are errors. In the case IND(J)=3
C     (upper and lower bounds) the condition BL(J) .gt. BU(J) is an
C     error.
C
C    -------
C    IOPT(*)
C    -------
C     This is the array where the user can specify nonstandard options
C     for DBOLSM( ). Most of the time this feature can be ignored by
C     setting the input value IOPT(1)=99. Occasionally users may have
C     needs that require use of the following subprogram options. For
C     details about how to use the options see below: IOPT(*) CONTENTS.
C
C     Option Number   Brief Statement of Purpose
C     ----- ------   ----- --------- -- -------
C           1         Move the IOPT(*) processing pointer.
C           2         Change rank determination tolerance.
C           3         Change blow-up factor that determines the
C                     size of variables being dropped from active
C                     status.
C           4         Reset the maximum number of iterations to use
C                     in solving the problem.
C           5         The data matrix is triangularized before the
C                     problem is solved whenever (NCOLS/MROWS) .lt.
C                     FAC. Change the value of FAC.
C           6         Redefine the weighting matrix used for
C                     linear independence checking.
C           7         Debug output is desired.
C          99         No more options to change.
C
C    ----
C    X(*)
C    ----
C     This array is used to pass data associated with options 1,2,3 and
C     5. Ignore this input parameter if none of these options are used.
C     Otherwise see below: IOPT(*) CONTENTS.
C
C    ----------------
C    IBASIS(*),IBB(*)
C    ----------------
C     These arrays must be initialized by the user. The values
C         IBASIS(J)=J, J=1,...,NCOLS
C         IBB(J)   =1, J=1,...,NCOLS
C     are appropriate except when using nonstandard features.
C
C    ------
C    SCL(*)
C    ------
C     This is the array of scaling factors to use on the columns of the
C     matrix E. These values must be defined by the user. To suppress
C     any column scaling set SCL(J)=1.0, J=1,...,NCOLS.
C
C    OUTPUT
C    ------
C
C    ----------
C    X(*),RNORM
C    ----------
C     The array X(*) contains a solution (if MODE .ge.0 or .eq.-22) for
C     the constrained least squares problem. The value RNORM is the
C     minimum residual vector length.
C
C    ----
C    MODE
C    ----
C     The sign of mode determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status.
C     A value of MODE .ge. 0 signifies that the subprogram has completed
C     normally. The value of MODE (.ge. 0) is the number of variables
C     in an active status: not at a bound nor at the value ZERO, for
C     the case of free variables. A negative value of MODE will be one
C     of the 20 cases -40,-39,...,-22, or -1. Values .lt. -1 correspond
C     to an abnormal completion of the subprogram. To understand the
C     abnormal completion codes see below: ERROR MESSAGES for DBOLSM( )
C     An approximate solution will be returned to the user only when
C     max. iterations is reached, MODE=-22.
C
C    -----------
C    RW(*),WW(*)
C    -----------
C     These are working arrays each with NCOLS entries. The array RW(*)
C     contains the working (scaled, nonactive) solution values. The
C     array WW(*) contains the working (scaled, active) gradient vector
C     values.
C
C    ----------------
C    IBASIS(*),IBB(*)
C    ----------------
C     These arrays contain information about the status of the solution
C     when MODE .ge. 0. The indices IBASIS(K), K=1,...,MODE, show the
C     nonactive variables; indices IBASIS(K), K=MODE+1,..., NCOLS are
C     the active variables. The value (IBB(J)-1) is the number of times
C     variable J was reflected from its upper bound. (normally the user
C     can ignore these parameters.)
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify internal variables in
C     the subprogram without recompiling the source code. A central
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. The value is updated as the options are processed.  At the
C     pointer position the option number is extracted and used for
C     locating other information that allows for options to be changed.
C     The portion of the array IOPT(*) that is used for each option is
C     fixed; the user and the subprogram both know how many locations
C     are needed for each option. A great deal of error checking is
C     done by the subprogram on the contents of the option array.
C     Nevertheless it is still possible to give the subprogram optional
C     input that is meaningless. For example some of the options use
C     the location X(NCOLS+IOFF) for passing data. The user must manage
C     the allocation of these locations when more than one piece of
C     option data is being passed to the subprogram.
C
C   1
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing pointer is moved to location
C     LP+2 of IOPT(*) in case IOPT(LP)=-1.  For example to skip over
C     locations 3,...,NCOLS+2 of IOPT(*),
C
C       IOPT(1)=1
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL PDA_DBOLSM( )
C
C     CAUTION: Misuse of this option can yield some very hard
C     -to-find bugs.  Use it with care.
C
C   2
C   -
C     The algorithm that solves the bounded least squares problem
C     iteratively drops columns from the active set. This has the
C     effect of joining a new column vector to the QR factorization of
C     the rectangular matrix consisting of the partially triangularized
C     nonactive columns. After triangularizing this matrix a test is
C     made on the size of the pivot element. The column vector is
C     rejected as dependent if the magnitude of the pivot element is
C     .le. TOL* magnitude of the column in components strictly above
C     the pivot element. Nominally the value of this (rank) tolerance
C     is TOL = DRELPR, where DRELPR is relative machine
C     precision. To change only the value of TOL, for example,
C
C       X(NCOLS+1)=TOL
C       IOPT(1)=2
C       IOPT(2)=1
C       IOPT(3)=99
C       CALL PDA_DBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=TOL
C       IOPT(LP)=2
C       IOPT(LP+1)=IOFF
C        .
C       CALL PDA_DBOLSM()
C
C     The required length of IOPT(*) is increased by 2 if option 2 is
C     used; The required length of X(*) is increased by 1. A value of
C     IOFF .le. 0 is an error. A value of TOL .le. DRELPR gives a
C     warning message; it is not considered an error.
C     Here DRELPR is the relative machine precision.
C
C   3
C   -
C     A solution component is left active (not used) if, roughly
C     speaking, it seems too large. Mathematically the new component is
C     left active if the magnitude is .ge.((vector norm of F)/(matrix
C     norm of E))/BLOWUP. Nominally the factor BLOWUP = SQRT(DRELPR)
C     where DRELPR is the relative machine precision. To change only
C     the value of BLOWUP, for example,
C
C       X(NCOLS+2)=BLOWUP
C       IOPT(1)=3
C       IOPT(2)=2
C       IOPT(3)=99
C       CALL PDA_DBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=BLOWUP
C       IOPT(LP)=3
C       IOPT(LP+1)=IOFF
C        .
C       CALL PDA_DBOLSM()
C
C     The required length of IOPT(*) is increased by 2 if option 3 is
C     used; the required length of X(*) is increased by 1. A value of
C     IOFF .le. 0 is an error. A value of BLOWUP .le. 0.0 is an error.
C
C   4
C   -
C     Normally the algorithm for solving the bounded least squares
C     problem requires between NCOLS/3 and NCOLS drop-add steps to
C     converge. (this remark is based on examining a small number of
C     test cases.) The amount of arithmetic for such problems is
C     typically about twice that required for linear least squares if
C     there are no bounds and if plane rotations are used in the
C     solution method. Convergence of the algorithm, while
C     mathematically certain, can be much slower than indicated. To
C     avoid this potential but unlikely event ITMAX drop-add steps are
C     permitted. Nominally ITMAX=5*(MAX(MROWS,NCOLS)). To change the
C     value of ITMAX, for example,
C
C       IOPT(1)=4
C       IOPT(2)=ITMAX
C       IOPT(3)=99
C       CALL PDA_DBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       IOPT(LP)=4
C       IOPT(LP+1)=ITMAX
C        .
C       CALL PDA_DBOLSM()
C
C     The value of ITMAX must be .gt. 0. Other values are errors. Use
C     of this option increases the required length of IOPT(*) by 2.
C
C   5
C   -
C     For purposes of increased efficiency the MROWS by NCOLS+1 data
C     matrix [E:F] is triangularized as a first step whenever MROWS
C     satisfies FAC*MROWS .gt. NCOLS. Nominally FAC=0.  To change the
C     value of FAC,
C
C       X(NCOLS+3)=FAC
C       IOPT(1)=5
C       IOPT(2)=3
C       IOPT(3)=99
C       CALL PDA_DBOLSM()
C
C     Generally, if LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=FAC
C       IOPT(LP)=5
C       IOPT(LP+1)=IOFF
C        .
C       CALL PDA_DBOLSM()
C
C     The value of FAC must be nonnegative. Other values are errors.
C     Using the value FAC=0.0 suppresses the initial triangularization step.
C     Use of this option increases the required length of IOPT(*) by 2;
C     The required length of of X(*) is increased by 1.
C
C   6
C   -
C     The norm used in testing the magnitudes of the pivot element
C     compared to the mass of the column above the pivot line can be
C     changed. The type of change that this option allows is to weight
C     the components with an index larger than MVAL by the parameter
C     WT. Normally MVAL=0 and WT=1. To change both the values MVAL and
C     WT, where LP is the processing pointer for IOPT(*),
C
C       X(NCOLS+IOFF)=WT
C       IOPT(LP)=6
C       IOPT(LP+1)=IOFF
C       IOPT(LP+2)=MVAL
C
C     Use of this option increases the required length of IOPT(*) by 3.
C     The length of X(*) is increased by 1. Values of MVAL must be
C     nonnegative and not greater than MROWS. Other values are errors.
C     The value of WT must be positive. Any other value is an error. If
C     either error condition is present a message will be printed.
C
C   7
C   -
C     Debug output, showing the detailed add-drop steps for the
C     constrained least squares problem, is desired. This option is
C     intended to be used to locate suspected bugs.  To print,
C
C       IOPT(LP)=7
C
C   99
C   --
C     There are no more options to change.
C
C     The values for options are 1,...,7,99, and are the only ones
C     permitted. Other values are errors. Options -99,-1,...,-7 mean
C     that the repective options 99,1,...,7 are left at their default
C     values. An example is the option to modify the (rank) tolerance:
C
C       X(NCOLS+1)=TOL
C       IOPT(1)=-2
C       IOPT(2)=1
C       IOPT(3)=99
C
C    Error Messages for DBOLSM( )
C    ----- -------- --- ---------
C    WARNING IN...
C    DBOLSM(). MORE THAN (I1)=ITMAX ITERATIONS SOLVING BOUNDED LEAST
C    SQUARES PROBLEM.
C              IN ABOVE MESSAGE, I1=         3
C    ERROR NUMBER =        22
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM. THE OPTION NUMBER=(I1) IS NOT DEFINED.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        23
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE OFFSET=(I1) BEYOND POSTION NCOLS=(I2)
C    MUST BE POSITIVE FOR OPTION NUMBER 2.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        24
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE TOLERANCE FOR RANK DETERMINATION=(R1)
C    IS LESS THAN MACHINE PRECISION=(R2).
C              IN ABOVE MESSAGE, R1=    0.
C              IN ABOVE MESSAGE, R2=     .7105427358E-14
C    ERROR NUMBER =        25
C
C    WARNING IN...
C    DBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST
C     BE POSTIVE FOR OPTION NUMBER 3.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        26
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE RECIPROCAL OF THE BLOW-UP FACTOR FOR REJECTING
C    VARIABLES MUST BE POSITIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        27
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE MAXIMUM NUMBER OF ITERATIONS=(I1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        28
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE
C    POSTIVE FOR OPTION NUMBER 5.
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        29
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE FACTOR (NCOLS/MROWS) WHERE PRETRIANGULARIZING IS
C    PERFORMED MUST BE NONNEGATIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    -.2500000000E+00
C    ERROR NUMBER =        30
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE NUMBER OF ROWS=(I1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        31
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE NUMBER OF COLS.=(I1) MUST BE POSTIVE.
C              IN ABOVE MESSAGE, I1=         0
C    ERROR NUMBER =        32
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE
C    NUMBER OF ROWS =(I2).
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        33
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). FOR J=(I1) THE CONSTRAINT INDICATOR MUST BE 1-4.
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, I2=         0
C    ERROR NUMBER =        34
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). FOR J=(I1) THE LOWER BOUND=(R1) IS .GT. THE UPPER
C     BOUND=(R2).
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, R1=    0.
C              IN ABOVE MESSAGE, R2=    -.1000000000E+01
C    ERROR NUMBER =        35
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE INPUT ORDER OF COLUMNS=(I1) IS NOT BETWEEN 1
C    AND NCOLS=(I2).
C              IN ABOVE MESSAGE, I1=         0
C              IN ABOVE MESSAGE, I2=         1
C    ERROR NUMBER =        36
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE BOUND POLARITY FLAG IN COMPONENT J=(I1) MUST
C    BE POSITIVE.  NOW=(I2).
C              IN ABOVE MESSAGE, I1=         1
C              IN ABOVE MESSAGE, I2=         0
C    ERROR NUMBER =        37
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE ROW SEPARATOR TO APPLY WEIGHTING (I1) MUST LIE
C    BETWEEN 0 AND MROWS (I2). WEIGHT (R1) MUST BE POSITIVE.
C              IN ABOVE MESSAGE, I1=        -1
C              IN ABOVE MESSAGE, I2=         2
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        38
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE OFFSET (I1) BEYOND POSITION NCOLS=(I2) MUST BE
C    POSITIVE FOR OPTION NUMBER 7.
C              IN ABOVE MESSAGE, I1=        -1
C              IN ABOVE MESSAGE, I2=         2
C    ERROR NUMBER =        39
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C    WARNING IN...
C    DBOLSM(). THE COLUMN PIVOTING THRESHOLD FACTOR MUST BE
C    POSITIVE. NOW=(R1).
C              IN ABOVE MESSAGE, R1=    0.
C    ERROR NUMBER =        40
C    (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C***END PROLOGUE  DBOLSM
C
C     PURPOSE
C     -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE BOUNDED
C     LEAST SQUARES PROBLEM.  THE PROBLEM SOLVED HERE IS:
C
C     SOLVE E*X =  F  (LEAST SQUARES SENSE)
C     WITH BOUNDS ON SELECTED X VALUES.
C
C     REVISED 880722-1100
C     REVISED YYMMDD-HHMM
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THE SUBPROGRAM NAME TO DBOLSM AND THE STRINGS
C     /SAXPY/ TO /DAXPY/, /SCOPY/ TO /DCOPY/,
C     /SDOT/ TO /PDA_DDOT/, /SNRM2/ TO /PDA_DNRM2/,
C     /SROT/ TO /DROT/, /SROTG/ TO /DROTG/, /R1MACH/ TO /PDA_D1MACH/,
C     /SVOUT/ TO /DVOUT/, /SMOUT/ TO /DMOUT/,
C     /SSWAP/ TO /DSWAP/, /E0/ TO /D0/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
C
C     THIS VARIABLE REMAINS TYPE REAL.
      REAL RDUM
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*)
      DOUBLE PRECISION X(*),RW(*),WW(*),SCL(*)
      DOUBLE PRECISION ALPHA,BETA,BOU,COLABV,COLBLO
      DOUBLE PRECISION CL1,CL2,CL3,ONE,BIG
      DOUBLE PRECISION FAC,RNORM,SC,SS,T,TOLIND,WT
      DOUBLE PRECISION TWO,T1,T2,WLARGE,WLA,WLB,XNEW
      DOUBLE PRECISION ZERO,PDA_DDOT,PDA_DNRM2
      DOUBLE PRECISION PDA_D1MACH,TOLSZE
      EXTERNAL PDA_DDOT, PDA_DNRM2, PDA_D1MACH
      INTEGER IBASIS(*),IBB(*),IND(*),IOPT(*)
      LOGICAL FOUND,CONSTR,CNZ
      COMMON /DBOCOM/IDOPE(5)
      INEXT(IDUM) = MIN(IDUM+1,MROWS)
C***FIRST EXECUTABLE STATEMENT  DBOLSM
      LEVEL = 1
C
C    VERIFY THAT THE PROBLEM DIMENSIONS ARE DEFINED PROPERLY.
      IF (MINPUT.LE.0) THEN
          NERR = 31
          NCHAR = 51
          CALL PDA_XERRWV(
     .             'DBOLSM(). THE NUMBER OF ROWS=(I1) MUST BE POSITIVE.'
     .                ,NCHAR,NERR,LEVEL,1,MINPUT,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600
*
      END IF
*
      IF (NCOLS.LE.0) THEN
          NERR = 32
          NCHAR = 51
          CALL PDA_XERRWV(
     .             'DBOLSM(). THE NUMBER OF COLS.=(I1) MUST BE POSTIVE.'
     .                ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600
*
      END IF
*
      IF (MDW.LT.MINPUT) THEN
          NERR = 33
          NCHAR = 78
          CALL PDA_XERRWV(
     .'DBOLSM(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE. THE NUMBER
     .OF ROWS=(I2).',NCHAR,NERR,LEVEL,2,MDW,MROWS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600
*
      END IF
C
C     VERIFY THAT BOUND INFORMATION IS CORRECT.
      DO 10 J = 1,NCOLS
         IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
             NERR = 34
             NCHAR = 58
             CALL PDA_XERRWV(
     .      'DBOLSM(). FOR J=(I1) THE CONSTRAINT INDICATOR MUST BE 1-4.'
     .                   ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
   10 CONTINUE
      DO 20 J = 1,NCOLS
         IF (IND(J).EQ.3) THEN
             IF (BU(J).LT.BL(J)) THEN
                 NERR = 35
                 NCHAR = 71
                 CALL PDA_XERRWV(
     .'DBOLSM(). FOR J=(I1) THE LOWER BOUND=(R1) IS .GT. THE UPPER BOUND
     .=(R2).',NCHAR,NERR,LEVEL,1,J,IDUM,2,REAL(BL(J)),REAL(BU(J)) )
C     DO(RETURN TO USER PROGRAM UNIT)
                 GO TO 600
*
             END IF
*
         END IF
*
   20 CONTINUE
C
C     CHECK THAT PERMUTATION AND POLARITY ARRAYS HAVE BEEN SET.
      DO 30 J = 1,NCOLS
         IF (IBASIS(J).LT.1 .OR. IBASIS(J).GT.NCOLS) THEN
             NERR = 36
             NCHAR = 74
             CALL PDA_XERRWV(
     .'DBOLSM(). THE INPUT ORDER OF COLUMNS=(I1) IS NOT BETWEEN 1 AND NC
     .OLS=(I2).',NCHAR,NERR,LEVEL,2,IBASIS(J),NCOLS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
         IF (IBB(J).LE.0) THEN
             NERR = 37
             NCHAR = 81
             CALL PDA_XERRWV(
     .'DBOLSM(). THE BOUND POLARITY FLAG IN COMPONENT J=(I1) MUST BE POS
     .ITIVE. NOW=(I2).',NCHAR,NERR,LEVEL,2,J,IBB(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
             GO TO 600
*
         END IF
*
   30 CONTINUE
C     DO(PROCESS OPTION ARRAY)
      GO TO 570
*
   40 CONTINUE
C     DO(INITIALIZE VARIABLES AND DATA VALUES)
      GO TO 460
*
   50 CONTINUE
      IF (IPRINT.GT.0) THEN
          CALL PDA_DMOUT(MROWS,NCOLS+1,MDW,W,
     .        '('' PRETRI. INPUT MATRIX'')',-4)
          CALL PDA_DVOUT(NCOLS,BL,'('' LOWER BOUNDS'')',-4)
          CALL PDA_DVOUT(NCOLS,BU,'('' UPPER BOUNDS'')',-4)
      END IF
*
   60 CONTINUE
      ITER = ITER + 1
      IF (ITER.LE.ITMAX) GO TO 80
      NERR = 22
      NCHAR = 80
      CALL PDA_XERRWV(
     .'DBOLSM(). MORE THAN (I1)=ITMAX ITERATIONS SOLVING BOUNDED LEAST S
     .QUARES PROBLEM.',NCHAR,NERR,LEVEL,1,ITMAX,IDUM,0,RDUM,RDUM)
C     DO(RESCALE AND TRANSLATE VARIABLES)
      IGOPR = 1
      GO TO 130
*
   70 CONTINUE
C     DO(RETURN TO USER PROGRAM UNIT)
      GO TO 600
*
   80 CONTINUE
C     DO(FIND A VARIABLE TO BECOME NON-ACTIVE)
      GO TO 180
*
   90 CONTINUE
      IF (FOUND) GO TO 110
C     DO(RESCALE AND TRANSLATE VARIABLES)
      IGOPR = 2
      GO TO 130
*
  100 CONTINUE
      MODE = NSETB
      RETURN
*
  110 CONTINUE
C     DO(MAKE MOVE AND UPDATE FACTORIZATION)
      GO TO 280
*
  120 CONTINUE
      GO TO 60
C     PROCEDURE(RESCALE AND TRANSLATE VARIABLES)
  130 CONTINUE
      CALL PDA_DCOPY(NSETB,X,1,RW,1)
      X(1) = ZERO
      CALL PDA_DCOPY(NCOLS,X,0,X,1)
      DO 140 J = 1,NSETB
         JCOL = ABS(IBASIS(J))
         X(JCOL) = RW(J)*ABS(SCL(JCOL))
  140 CONTINUE
      DO 150 J = 1,NCOLS
         IF (MOD(IBB(J),2).EQ.0) X(J) = BU(J) - X(J)
  150 CONTINUE
      DO 160 J = 1,NCOLS
         JCOL = IBASIS(J)
         IF (JCOL.LT.0) X(-JCOL) = BL(-JCOL) + X(-JCOL)
  160 CONTINUE
      DO 170 J = 1,NCOLS
         IF (SCL(J).LT.ZERO) X(J) = -X(J)
  170 CONTINUE
      CALL PDA_DSCAL(MROWS-MVAL,WT,W(INEXT(MVAL),NCOLS+1),1)
      RNORM = PDA_DNRM2(MROWS-MAX(NSETB,MVAL),
     .        W(INEXT(MAX(NSETB,MVAL)),NCOLS+1),1)
C     END PROCEDURE
      GO TO (70,100),IGOPR
C     PROCEDURE(FIND A VARIABLE TO BECOME NON-ACTIVE)
  180 CONTINUE
C
C     COMPUTE (NEGATIVE) OF GRADIENT VECTOR, W=
C     (TRANSPOSE OF E)*(F-E*X).
      WW(1) = ZERO
      CALL PDA_DCOPY(NCOLS,WW,0,WW,1)
      DO 190 J = NSETB + 1,NCOLS
         JCOL = ABS(IBASIS(J))
         WW(J) = PDA_DDOT(MROWS-NSETB,W(INEXT(NSETB),J),1,
     .           W(INEXT(NSETB),NCOLS+1),1)*ABS(SCL(JCOL))
  190 CONTINUE
      IF (IPRINT.GT.0) THEN
          CALL PDA_DVOUT(NCOLS,WW,'('' GRADIENT VALUES'')',-4)
          CALL PDA_IVOUT(NCOLS,IBASIS,'('' INTERNAL VARIABLE ORDER'')',
     .                   -4)
          CALL PDA_IVOUT(NCOLS,IBB,'('' BOUND POLARITY'')',-4)
      END IF
*
  200 CONTINUE
C
C     IF ACTIVE SET = NUMBER OF TOTAL ROWS, QUIT.
      IF (NSETB.EQ.MROWS) THEN
          FOUND = .FALSE.
C     EXIT PROCEDURE
          GO TO 90
*
      END IF
C
C     CHOOSE AN EXTREMAL COMPONENT OF GRADIENT VECTOR
C     FOR A CANDIDATE TO BECOME NON-ACTIVE.
      WLARGE = -BIG
      JBIG = 0
      CNZ = .FALSE.
      DO 210 J = NSETB + 1,NCOLS
         T = WW(J)
C     SKIP LOOKING AT COMPONENTS FLAGGED AS NON-CANDIDATES.
         IF (T.EQ.BIG) GO TO 210
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         IF (NSETB.LT.MVAL) THEN
             CL1 = PDA_DNRM2(NSETB,W(1,J),1)
             CL2 = PDA_DNRM2(MVAL-NSETB,W(INEXT(NSETB),J),1)
             COLABV = CL1
             COLBLO = CL2
*
         ELSE
             CL1 = PDA_DNRM2(MVAL,W(1,J),1)
             CL2 = ABS(WT)*PDA_DNRM2(NSETB-MVAL,W(INEXT(MVAL),J),1)
             CL3 = ABS(WT)*PDA_DNRM2(MROWS-NSETB,W(INEXT(NSETB),J),1)
             CALL PDA_DROTG(CL1,CL2,SC,SS)
             COLABV = ABS(CL1)
             COLBLO = CL3
         END IF
*
         IF (ITEMP.LT.0) THEN
             IF (MOD(IBB(JCOL),2).EQ.0) T = -T
C     SKIP LOOKING AT COMPONENTS THAT WOULD NOT DECREASE OBJECTIVE.
             IF (T.LT.ZERO) GO TO 210
         END IF
C     THIS IS A COLUMN PIVOTING STEP THAT MAXIMIZES THE RATIO OF
C     COLUMN MASS ON AND BELOW THE PIVOT LINE RELATIVE TO THAT
C     STRICTLY ABOVE THE PIVOT LINE.
         IF (COLABV.EQ.ZERO .AND. .NOT. CNZ) THEN
             T = COLBLO*ABS(SCL(JCOL))
             IF (WLARGE.LT.T) THEN
                 WLARGE = T
                 JBIG = J
             END IF
*
         ELSE
             IF ( .NOT. CNZ) THEN
                 WLA = ZERO
                 WLB = ZERO
                 CNZ = .TRUE.
             END IF
*
           IF(SQRT(COLBLO)*SQRT(WLA) .GE. SQRT(COLABV)*SQRT(WLB)) THEN
                WLB=COLBLO
                WLA=COLABV
                JBIG=J
           END IF
         END IF
*
  210 CONTINUE
      IF (JBIG.EQ.0) THEN
          FOUND = .FALSE.
          IF (IPRINT.GT.0) THEN
              CALL PDA_IVOUT(0,I,'('' FOUND NO VARIABLE TO ENTER'')',-4)
          END IF
C     EXIT PROCEDURE
          GO TO 90
*
      END IF
C
C     SEE IF THE INCOMING COL. IS SUFFICIENTLY INDEPENDENT.
C     THIS TEST IS MADE BEFORE AN ELIMINATION IS PERFORMED.
      IF (IPRINT.GT.0) THEN
          CALL PDA_IVOUT(1,JBIG,'('' TRY TO BRING IN THIS COL.'')',-4)
      END IF
*
      IF (CNZ) THEN
      IF(WLB.LE.WLA*TOLIND) THEN
          FOUND = .FALSE.
          IF (IPRINT.GT.0) THEN
              CALL PDA_IVOUT(0,I,
     .           '('' VARIABLE IS DEPENDENT, NOT USED.'')', -4)
          END IF
*
          WW(JBIG) = BIG
          GO TO 200
*
      END IF
      END IF
C
C     SWAP MATRIX COLS. NSETB+1 AND JBIG, PLUS POINTER INFO., AND
C     GRADIENT VALUES.
      NSETB = NSETB + 1
      IF (NSETB.NE.JBIG) THEN
          CALL PDA_DSWAP(MROWS,W(1,NSETB),1,W(1,JBIG),1)
          CALL PDA_DSWAP(1,WW(NSETB),1,WW(JBIG),1)
          ITEMP = IBASIS(NSETB)
          IBASIS(NSETB) = IBASIS(JBIG)
          IBASIS(JBIG) = ITEMP
      END IF
C
C     ELIMINATE ENTRIES BELOW THE PIVOT LINE IN COL. NSETB.
      IF (MROWS.GT.NSETB) THEN
          DO 220 I = MROWS,NSETB + 1,-1
             IF (I.EQ.MVAL+1) GO TO 220
             CALL PDA_DROTG(W(I-1,NSETB),W(I,NSETB),SC,SS)
             W(I,NSETB) = ZERO
             CALL PDA_DROT(NCOLS-NSETB+1,W(I-1,NSETB+1),MDW,
     .                     W(I,NSETB+1),MDW,SC,SS)
  220     CONTINUE
          IF ((MVAL.GE.NSETB) .AND. (MVAL.LT.MROWS)) THEN
              T = W(NSETB,NSETB)
              IF (T.NE.ZERO) THEN
                  T = WT*W(MVAL+1,NSETB)/T
*
              ELSE
                  T = BIG
              END IF
*
              IF (TOLIND*ABS(T).GT.ONE) THEN
                  CALL PDA_DSWAP(NCOLS-NSETB+2,W(NSETB,NSETB),MDW,
     .                       W(MVAL+1,NSETB),MDW)
                  CALL PDA_DSCAL(NCOLS-NSETB+2,WT,W(NSETB,NSETB),MDW)
                  CALL PDA_DSCAL(NCOLS-NSETB+2,ONE/WT,W(MVAL+1,NSETB),
     .                           MDW)
              END IF
*
              CALL PDA_DROTG(W(NSETB,NSETB),W(MVAL+1,NSETB),SC,SS)
              W(MVAL+1,NSETB) = ZERO
              CALL PDA_DROT(NCOLS-NSETB+1,W(NSETB,NSETB+1),MDW,
     .                  W(MVAL+1,NSETB+1),MDW,SC,SS)
          END IF
*
      END IF
*
      IF (W(NSETB,NSETB).EQ.ZERO) THEN
          WW(NSETB) = BIG
          NSETB = NSETB - 1
          IF (IPRINT.GT.0) THEN
              CALL PDA_IVOUT(0,I,'('' PIVOT IS ZERO, NOT USED.'')',-4)
          END IF
*
          GO TO 200
*
      END IF
C
C     CHECK THAT NEW VARIABLE IS MOVING IN THE RIGHT DIRECTION.
      ITEMP = IBASIS(NSETB)
      JCOL = ABS(ITEMP)
      XNEW = (W(NSETB,NCOLS+1)/W(NSETB,NSETB))/ABS(SCL(JCOL))
C CONT: DO BLOCK
C QUIT: DO BLOCK
      IF (ITEMP.LT.0) THEN
C     IF(WW(NSETB).GE.ZERO.AND.XNEW.LE.ZERO) EXIT(QUIT)
C     IF(WW(NSETB).LE.ZERO.AND.XNEW.GE.ZERO) EXIT(QUIT)
          IF (WW(NSETB).GE.ZERO .AND. XNEW.LE.ZERO) GO TO 230
          IF (WW(NSETB).LE.ZERO .AND. XNEW.GE.ZERO) GO TO 230
      END IF
C     EXIT(CONT)
      GO TO 240
C     END BLOCK
  230 CONTINUE
      WW(NSETB) = BIG
      NSETB = NSETB - 1
      IF (IPRINT.GT.0) THEN
         CALL PDA_IVOUT(0,I,
     .        '('' VARIABLE HAS BAD DIRECTION, NOT USED.'')', -4)
      END IF
*
      GO TO 200
C     END BLOCK
  240 CONTINUE
      FOUND = .TRUE.
C     EXIT PROCEDURE
      GO TO 250
*
  250 CONTINUE
C     END PROCEDURE
      GO TO 90
C     PROCEDURE(SOLVE THE TRIANGULAR SYSTEM)
  260 CONTINUE
      CALL PDA_DCOPY(NSETB,W(1,NCOLS+1),1,RW,1)
      DO 270 J = NSETB,1,-1
         RW(J) = RW(J)/W(J,J)
         JCOL = ABS(IBASIS(J))
         T = RW(J)
         IF (MOD(IBB(JCOL),2).EQ.0) RW(J) = -RW(J)
         CALL PDA_DAXPY(J-1,-T,W(1,J),1,RW,1)
         RW(J) = RW(J)/ABS(SCL(JCOL))
  270 CONTINUE
      IF (IPRINT.GT.0) THEN
          CALL PDA_DVOUT(NSETB,RW,'('' SOLN. VALUES'')',-4)
          CALL PDA_IVOUT(NSETB,IBASIS,'('' COLS. USED'')',-4)
      END IF
C     END PROCEDURE
      GO TO (290,430),LGOPR
C     PROCEDURE(MAKE MOVE AND UPDATE FACTORIZATION)
  280 CONTINUE
C     DO(SOLVE THE TRIANGULAR SYSTEM)
      LGOPR = 1
      GO TO 260
*
  290 CONTINUE
C
C     SEE IF THE UNCONSTRAINED SOL. (OBTAINED BY SOLVING THE
C     TRIANGULAR SYSTEM) SATISFIES THE PROBLEM BOUNDS.
      ALPHA = TWO
      BETA = TWO
      X(NSETB) = ZERO
      DO 300 J = 1,NSETB
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         T1 = TWO
         T2 = TWO
         IF (ITEMP.LT.0) THEN
             BOU = ZERO
*
         ELSE
             BOU = BL(JCOL)
         END IF
*
         IF ((-BOU).NE.BIG) BOU = BOU/ABS(SCL(JCOL))
         IF (RW(J).LE.BOU) T1 = (X(J)-BOU)/ (X(J)-RW(J))
         BOU = BU(JCOL)
         IF (BOU.NE.BIG) BOU = BOU/ABS(SCL(JCOL))
         IF (RW(J).GE.BOU) T2 = (BOU-X(J))/ (RW(J)-X(J))
C
C     IF NOT, THEN COMPUTE A STEP LENGTH SO THAT THE
C     VARIABLES REMAIN FEASIBLE.
         IF (T1.LT.ALPHA) THEN
             ALPHA = T1
             JDROP1 = J
         END IF
*
         IF (T2.LT.BETA) THEN
             BETA = T2
             JDROP2 = J
         END IF
*
  300 CONTINUE
      CONSTR = ALPHA .LT. TWO .OR. BETA .LT. TWO
      IF (CONSTR) GO TO 310
C
C     ACCEPT THE CANDIDATE BECAUSE IT SATISFIES THE STATED BOUNDS
C     ON THE VARIABLES.
      CALL PDA_DCOPY(NSETB,RW,1,X,1)
      GO TO 120
*
  310 CONTINUE
C
C     TAKE A STEP THAT IS AS LARGE AS POSSIBLE WITH ALL
C     VARIABLES REMAINING FEASIBLE.
      DO 320 J = 1,NSETB
         X(J) = X(J) + MIN(ALPHA,BETA)* (RW(J)-X(J))
  320 CONTINUE
      IF (ALPHA.LE.BETA) THEN
          JDROP2 = 0
*
      ELSE
          JDROP1 = 0
      END IF
*
  330 IF (JDROP1+JDROP2.GT.0 .AND. NSETB.GT.0) GO TO 340
      GO TO 450
*
  340 JDROP = JDROP1 + JDROP2
      ITEMP = IBASIS(JDROP)
      JCOL = ABS(ITEMP)
      IF (JDROP2.GT.0) THEN
C
C     VARIABLE IS AT AN UPPER BOUND.  SUBTRACT MULTIPLE OF THIS COL.
C     FROM RIGHT HAND SIDE.
          T = BU(JCOL)
          IF (ITEMP.GT.0) THEN
              BU(JCOL) = T - BL(JCOL)
              BL(JCOL) = -T
              ITEMP = -ITEMP
              SCL(JCOL) = -SCL(JCOL)
              DO 350 I = 1,JDROP
                 W(I,JDROP) = -W(I,JDROP)
  350         CONTINUE
*
          ELSE
              IBB(JCOL) = IBB(JCOL) + 1
              IF (MOD(IBB(JCOL),2).EQ.0) T = -T
          END IF
C     VARIABLE IS AT A LOWER BOUND.
      ELSE
          IF (ITEMP.LT.ZERO) THEN
              T = ZERO
*
          ELSE
              T = -BL(JCOL)
              BU(JCOL) = BU(JCOL) + T
              ITEMP = -ITEMP
          END IF
*
      END IF
*
      CALL PDA_DAXPY(JDROP,T,W(1,JDROP),1,W(1,NCOLS+1),1)
C
C     MOVE CERTAIN COLS. LEFT TO ACHIEVE UPPER HESSENBERG FORM.
      CALL PDA_DCOPY(JDROP,W(1,JDROP),1,RW,1)
      DO 360 J = JDROP + 1,NSETB
         IBASIS(J-1) = IBASIS(J)
         X(J-1) = X(J)
         CALL PDA_DCOPY(J,W(1,J),1,W(1,J-1),1)
  360 CONTINUE
      IBASIS(NSETB) = ITEMP
      W(1,NSETB) = ZERO
      CALL PDA_DCOPY(MROWS-JDROP,W(1,NSETB),0,W(JDROP+1,NSETB),1)
      CALL PDA_DCOPY(JDROP,RW,1,W(1,NSETB),1)
C
C     TRANSFORM THE MATRIX FROM UPPER HESSENBERG FORM TO
C     UPPER TRIANGULAR FORM.
      NSETB = NSETB - 1
C SMLL:
C     *DO BLOCK
C NRML:
C     *DO BLOCK
      DO 380 I = JDROP,NSETB
C
C     LOOK FOR SMALL PIVOTS AND AVOID MIXING WEIGHTED AND
C     NONWEIGHTED ROWS.
         IF (I.EQ.MVAL) THEN
             T = ZERO
             DO 370 J = I,NSETB
                JCOL = ABS(IBASIS(J))
                T1 = ABS(W(I,J)*SCL(JCOL))
                IF (T1.GT.T) THEN
                    JBIG = J
                    T = T1
                END IF
*
  370        CONTINUE
C     EXIT(NRML)
             GO TO 390
*
         END IF
*
         CALL PDA_DROTG(W(I,I),W(I+1,I),SC,SS)
         W(I+1,I) = ZERO
         CALL PDA_DROT(NCOLS-I+1,W(I,I+1),MDW,W(I+1,I+1),MDW,SC,SS)
  380 CONTINUE
C     EXIT (SMLL)
      GO TO 420
C     END BLOCK
  390 CONTINUE
C
C     THE TRIANGULARIZATION IS COMPLETED BY GIVING UP
C     THE HESSENBERG FORM AND TRIANGULARIZING A RECTANGULAR MATRIX.
      CALL PDA_DSWAP(MROWS,W(1,I),1,W(1,JBIG),1)
      CALL PDA_DSWAP(1,WW(I),1,WW(JBIG),1)
      CALL PDA_DSWAP(1,X(I),1,X(JBIG),1)
      ITEMP = IBASIS(I)
      IBASIS(I) = IBASIS(JBIG)
      IBASIS(JBIG) = ITEMP
      JBIG = I
      DO 410 J = JBIG,NSETB
         DO 400 I = J + 1,MROWS
            CALL PDA_DROTG(W(J,J),W(I,J),SC,SS)
            W(I,J) = ZERO
            CALL PDA_DROT(NCOLS-J+1,W(J,J+1),MDW,W(I,J+1),MDW,SC,SS)
  400    CONTINUE
  410 CONTINUE
C     END BLOCK
  420 CONTINUE
C
C     SEE IF THE REMAINING COEFFICIENTS ARE FEASIBLE.  THEY SHOULD
C     BE BECAUSE OF THE WAY MIN(ALPHA,BETA) WAS CHOSEN.  ANY THAT ARE
C     NOT FEASIBLE WILL BE SET TO THEIR BOUNDS AND
C     APPROPRIATELY TRANSLATED.
      JDROP1 = 0
      JDROP2 = 0
C     DO(SOLVE THE TRIANGULAR SYSTEM)
      LGOPR = 2
      GO TO 260
*
  430 CONTINUE
      CALL PDA_DCOPY(NSETB,RW,1,X,1)
      DO 440 J = 1,NSETB
         ITEMP = IBASIS(J)
         JCOL = ABS(ITEMP)
         IF (ITEMP.LT.0) THEN
             BOU = ZERO
*
         ELSE
             BOU = BL(JCOL)
         END IF
*
         IF ((-BOU).NE.BIG) BOU = BOU/ABS(SCL(JCOL))
         IF (X(J).LE.BOU) THEN
             JDROP1 = J
             GO TO 330
*
         END IF
*
         BOU = BU(JCOL)
         IF (BOU.NE.BIG) BOU = BOU/ABS(SCL(JCOL))
         IF (X(J).GE.BOU) THEN
             JDROP2 = J
             GO TO 330
*
         END IF
*
  440 CONTINUE
      GO TO 330
*
  450 CONTINUE
C     END PROCEDURE
      GO TO 120
C     PROCEDURE(INITIALIZE VARIABLES AND DATA VALUES)
  460 CONTINUE
C
C     PRETRIANGULARIZE RECTANGULAR ARRAYS OF CERTAIN SIZES
C     FOR INCREASED EFFICIENCY.
      IF (FAC*MINPUT.GT.NCOLS) THEN
          DO 480 J = 1,NCOLS + 1
             DO 470 I = MINPUT,J + MVAL + 1,-1
                CALL PDA_DROTG(W(I-1,J),W(I,J),SC,SS)
                W(I,J) = ZERO
                CALL PDA_DROT(NCOLS-J+1,W(I-1,J+1),MDW,W(I,J+1),MDW,SC,
     .                        SS)
  470        CONTINUE
  480     CONTINUE
          MROWS = NCOLS + MVAL + 1
*
      ELSE
          MROWS = MINPUT
      END IF
C
C      SET THE X(*) ARRAY TO ZERO SO ALL COMPONENTS ARE DEFINED.
      X(1) = ZERO
      CALL PDA_DCOPY(NCOLS,X,0,X,1)
C
C     THE ARRAYS IBASIS(*), IBB(*) ARE INITIALIZED BY THE CALLING
C     PROGRAM UNIT.
C     THE COL. SCALING IS DEFINED IN THE CALLING PROGRAM UNIT.
C    'BIG' IS PLUS INFINITY ON THIS MACHINE.
      BIG = PDA_D1MACH(2)
      DO 540 J = 1,NCOLS
         ICASE = IND(J)
C     DO CASE(ICASE,4)
         GO TO (490,500,510,520),ICASE
*
         GO TO 530
C     CASE 1
  490    BU(J) = BIG
         GO TO 530
C     CASE 2
  500    BL(J) = -BIG
         GO TO 530
C     CASE 3
  510    GO TO 530
C     CASE 4
  520    BL(J) = -BIG
         BU(J) = BIG
C     END CASE
  530    CONTINUE
  540 CONTINUE
      DO 560 J = 1,NCOLS
         IF ((BL(J).LE.ZERO.AND.ZERO.LE.BU(J).AND.ABS(BU(J)).LT.
     .       ABS(BL(J))) .OR. BU(J).LT.ZERO) THEN
             T = BU(J)
             BU(J) = -BL(J)
             BL(J) = -T
             SCL(J) = -SCL(J)
             DO 550 I = 1,MROWS
                W(I,J) = -W(I,J)
  550        CONTINUE
         END IF
C
C     INDICES IN SET T(=TIGHT) ARE DENOTED BY NEGATIVE VALUES
C     OF IBASIS(*).
         IF (BL(J).GE.ZERO) THEN
             IBASIS(J) = -IBASIS(J)
             T = -BL(J)
             BU(J) = BU(J) + T
             CALL PDA_DAXPY(MROWS,T,W(1,J),1,W(1,NCOLS+1),1)
         END IF
*
  560 CONTINUE
      NSETB = 0
      ITER = 0
C     END PROCEDURE
      GO TO 50
C     PROCEDURE(PROCESS OPTION ARRAY)
  570 CONTINUE
      IF (IDOPE(5).EQ.1) THEN
          FAC = X(NCOLS+IDOPE(1))
          WT = X(NCOLS+IDOPE(2))
          MVAL = IDOPE(3)
*
      ELSE
          FAC = 0.
          WT = 1.
          MVAL = 0
      END IF
*
      ZERO = 0.D0
      ONE = 1.D0
      TWO = 2.D0
      TOLIND =MIN(1.D-5, SQRT(PDA_D1MACH(4)))
      TOLSZE = SQRT(PDA_D1MACH(4))
C Edit on 950228-1345:
C      ITMAX = 5*MAX(MROWS,NCOLS)
      ITMAX = 5*MAX(MINPUT,NCOLS)
      IPRINT = 0
C
C     CHANGES TO SOME PARAMETERS CAN OCCUR THROUGH THE OPTION
C     ARRAY, IOPT(*).  PROCESS THIS ARRAY LOOKING CAREFULLY
C     FOR INPUT DATA ERRORS.
      LP = 0
      LDS = 0
  580 CONTINUE
      LP = LP + LDS
C
C     TEST FOR NO MORE OPTIONS.
      IP = IOPT(LP+1)
      JP = ABS(IP)
      IF (IP.EQ.99) THEN
          GO TO 590
*
      ELSE IF (JP.EQ.99) THEN
          LDS = 1
          GO TO 580
*
      ELSE IF (JP.EQ.1) THEN
C
C     MOVE THE IOPT(*) PROCESSING POINTER.
          IF (IP.GT.0) THEN
              LP = IOPT(LP+2) - 1
              LDS = 0
*
          ELSE
              LDS = 2
          END IF
*
          GO TO 580
*
      ELSE IF (JP.EQ.2) THEN
C
C     CHANGE TOLERANCE FOR RANK DETERMINATION.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 24
                  NCHAR = 89
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE OFFSET=(I1) BEYOND POSTION NCOLS=(I2) MUST BE POSIT
     .IVE FOR OPTION NUMBER 2.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              TOLIND = X(NCOLS+IOFF)
              IF (TOLIND.LT.PDA_D1MACH(4)) THEN
                  NERR = 25
                  NLEVEL = 0
                  NCHAR = 88
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE TOLERANCE FOR RANK DETERMINATION=(R1) IS LESS THAN
     .MACHINE PRECISION=(R2).',NCHAR,NERR,NLEVEL,0,IDUM,IDUM,2,
     .REAL(TOLIND),REAL(PDA_D1MACH(4)) )
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580
*
      ELSE IF (JP.EQ.3) THEN
C
C     CHANGE BLOWUP FACTOR FOR ALLOWING VARIABLES TO BECOME
C     INACTIVE.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 26
                  NCHAR = 89
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE POST
     .IVE FOR OPTION NUMBER 3.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              TOLSZE = X(NCOLS+IOFF)
              IF (TOLSZE.LE.ZERO) THEN
                  NERR = 27
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE RECIPROCAL OF THE BLOW-UP FACTOR FOR REJECTING VARI
     .ABLES MUST BE POSITIVE. NOW=(R1).',NCHAR,NERR,LEVEL,0,IDUM,IDUM,1,
     .                        REAL(TOLSZE),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580
*
      ELSE IF (JP.EQ.4) THEN
C
C     CHANGE THE MAX. NO. OF ITERATIONS ALLOWED.
          IF (IP.GT.0) THEN
              ITMAX = IOPT(LP+2)
              IF (ITMAX.LE.0) THEN
                  NERR = 28
                  NCHAR = 65
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE MAXIMUM NUMBER OF ITERATIONS=(I1) MUST BE POSITIVE.
     .',NCHAR,NERR,LEVEL,1,ITMAX,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580
*
      ELSE IF (JP.EQ.5) THEN
C
C     CHANGE THE FACTOR FOR PRETRIANGULARIZING THE DATA MATRIX.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  NERR = 29
                  NCHAR = 89
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE OFFSET=(I1) BEYOND POSITION NCOLS=(I2) MUST BE POST
     .IVE FOR OPTION NUMBER 5.',NCHAR,NERR,LEVEL,2,IOFF,NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
              FAC = X(NCOLS+IOFF)
              IF (FAC.LT.ZERO) THEN
                  NERR = 30
                  NLEVEL = 0
                  NCHAR = 104
                  CALL PDA_XERRWV(
     .'DBOLSM(). THE FACTOR (NCOLS/MROWS) WHERE PRE-TRIANGULARIZING IS P
     .ERFORMED MUST BE NONNEGATIVE. NOW=(R1).',NCHAR,NERR,NLEVEL,0,IDUM,
     .                        IDUM,1,REAL(FAC),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 600
*
              END IF
*
          END IF
*
          LDS = 2
          GO TO 580
*
      ELSE IF (JP.EQ.6) THEN
C
C     CHANGE THE WEIGHTING FACTOR (FROM ONE) TO APPLY TO COMPONENTS
C     NUMBERED .GT. MVAL (INITIALLY SET TO 1.)  THIS TRICK IS NEEDED
C     FOR APPLICATIONS OF THIS SUBPROGRAM TO THE HEAVILY WEIGHTED
C     LEAST SQUARES PROBLEM THAT COME FROM EQUALITY CONSTRAINTS.
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              MVAL = IOPT(LP+3)
              WT = X(NCOLS+IOFF)
          END IF
*
          IF (MVAL.LT.0 .OR. MVAL.GT.MINPUT .OR. WT.LE.ZERO) THEN
              NERR = 38
              NLEVEL = 0
              NCHAR = 116
              CALL PDA_XERRWV(
     .'DBOLSM(). THE ROW SEPARATOR TO APPLY WEIGHTING (I1) MUST LIE BETW
     .EEN 0 AND MROWS (I2). WEIGHT (R1) MUST BE POSITIVE.',NCHAR,NERR,
     .                    NLEVEL,2,MVAL,MINPUT,1,REAL(WT),RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 600
*
          END IF
*
          LDS = 3
          GO TO 580
C
C     TURN ON DEBUG OUTPUT.
      ELSE IF (JP.EQ.7) THEN
          IF (IP.GT.0) IPRINT = 1
          LDS = 1
          GO TO 580
*
      ELSE
          NERR = 23
          NCHAR = 46
          CALL PDA_XERRWV(
     .         'DBOLSM. THE OPTION NUMBER=(I1) IS NOT DEFINED.',
     .         NCHAR,NERR,LEVEL,1,IP,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 600
*
      END IF
*
  590 CONTINUE
C     END PROCEDURE
      GO TO 40
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  600 CONTINUE
      MODE = -NERR
      RETURN
C     END PROCEDURE
C     END PROGRAM
      END
