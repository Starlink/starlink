      SUBROUTINE PDA_DBOLSQ(W,MDW,MROWS,NCOLS,BL,BU,IND,IOPT,X,RNORM,
     .                      MODE,RW,IW)
C***BEGIN PROLOGUE  DBOLS
C***DATE WRITTEN   821220   (YYMMDD)
C***REVISION DATE  880722   (yymmdd)
C***CATEGORY NO.  K1A2A,G2E,G2H1,G2H2
C***KEYWORDS  BOUNDS,CONSTRAINTS,INEQUALITY,LEAST SQUARES,LINEAR
C***AUTHOR  HANSON, R. J., SNLA
C***PURPOSE  Solve the problem
C                 E*X = F (in the least  squares  sense)
C            with bounds on selected X values.
C***DESCRIPTION
C
C     The user must have dimension statements of the form:
C
C       DIMENSION W(MDW,NCOLS+1), BL(NCOLS), BU(NCOLS),
C      * X(NCOLS+NX), RW(5*NCOLS)
C       INTEGER IND(NCOLS), IOPT(1+NI), IW(2*NCOLS)
C
C     (here NX=number of extra locations required for option 4; NX=0
C     for no options; NX=NCOLS if this option is in use. Here NI=number
C     of extra locations required for options 1-6; NI=0 for no
C     options.)
C
C   INPUT
C   -----
C
C    --------------------
C    W(MDW,*),MROWS,NCOLS
C    --------------------
C     The array W(*,*) contains the matrix [E:F] on entry. The matrix
C     [E:F] has MROWS rows and NCOLS+1 columns. This data is placed in
C     the array W(*,*) with E occupying the first NCOLS columns and the
C     right side vector F in column NCOLS+1. The row dimension, MDW, of
C     the array W(*,*) must satisfy the inequality MDW .ge. MROWS.
C     Other values of MDW are errrors. The values of MROWS and NCOLS
C     must be positive. Other values are errors. There is an exception
C     to this when using option 1 for accumulation of blocks of
C     equations. In that case MROWS is an OUTPUT variable ONLY, and the
C     matrix data for [E:F] is placed in W(*,*), one block of rows at a
C     time.  MROWS contains the number of rows in the matrix after
C     triangularizing several blocks of equations. This is an OUTPUT
C     parameter ONLY when option 1 is used. See IOPT(*) CONTENTS
C     for details about option 1.
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
C          (the value of BU(J) is not used.)
C    2.    For IND(J)=2, require X(J) .le. BU(J).
C          (the value of BL(J) is not used.)
C    3.    For IND(J)=3, require X(J) .ge. BL(J) and
C                                X(J) .le. BU(J).
C    4.    For IND(J)=4, no bounds on X(J) are required.
C          (the values of BL(J) and BU(J) are not used.)
C
C     Values other than 1,2,3 or 4 for IND(J) are errors. In the case
C     IND(J)=3 (upper and lower bounds) the condition BL(J) .gt. BU(J)
C     is an error.
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
C     ------ ------   ----- --------- -- -------
C           1         Return to user for accumulation of blocks
C                     of least squares equations.
C           2         Check lengths of all arrays used in the
C                     subprogram.
C           3         Standard scaling of the data matrix, E.
C           4         User provides column scaling for matrix E.
C           5         Provide option array to the low-level
C                     subprogram DBOLSM( ).
C           6         Move the IOPT(*) processing pointer.
C           7         User has called DBOLS() directly.
C          99         No more options to change.
C
C    ----
C    X(*)
C    ----
C     This array is used to pass data associated with option 4. Ignore
C     this parameter if this option is not used. Otherwise see below:
C     IOPT(*) CONTENTS.
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
C     The sign of MODE determines whether the subprogram has completed
C     normally, or encountered an error condition or abnormal status. A
C     value of MODE .ge. 0 signifies that the subprogram has completed
C     normally. The value of MODE (.GE. 0) is the number of variables
C     in an active status: not at a bound nor at the value ZERO, for
C     the case of free variables. A negative value of MODE will be one
C     of the cases -37,-36,...,-22, or -17,...,-2. Values .lt. -1
C     correspond to an abnormal completion of the subprogram. To
C     understand the abnormal completion codes see below: ERROR
C     MESSAGES for DBOLS( ). AN approximate solution will be returned
C     to the user only when max. iterations is reached, MODE=-22.
C     Values for MODE=-37,...,-22 come from the low-level subprogram
C     DBOLSM(). See the section ERROR MESSAGES for DBOLSM() in the
C     documentation for DBOLSM().
C
C    -----------
C    RW(*),IW(*)
C    -----------
C     These are working arrays with 5*NCOLS and 2*NCOLS entries.
C     (normally the user can ignore the contents of these arrays,
C     but they must be dimensioned properly.)
C
C    IOPT(*) CONTENTS
C    ------- --------
C     The option array allows a user to modify internal variables in
C     the subprogram without recompiling the source code. A central
C     goal of the initial software design was to do a good job for most
C     people. Thus the use of options will be restricted to a select
C     group of users. The processing of the option array proceeds as
C     follows: a pointer, here called LP, is initially set to the value
C     1. This value is updated as each option is processed. At the
C     pointer position the option number is extracted and used for
C     locating other information that allows for options to be changed.
C     The portion of the array IOPT(*) that is used for each option is
C     fixed; the user and the subprogram both know how many locations
C     are needed for each option. A great deal of error checking is
C     done by the subprogram on the contents of the option array.
C     Nevertheless it is still possible to give the subprogram optional
C     input that is meaningless. For example option 4 uses the
C     locations X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1) for passing
C     scaling data. The user must manage the allocation of these
C     locations.
C
C   1
C   -
C     This option allows the user to solve problems with a large number
C     of rows compared to the number of variables. The idea is that the
C     subprogram returns to the user (perhaps many times) and receives
C     new least squares equations from the calling program unit.
C     Eventually the user signals "that's all" and then computes the
C     solution with one final call to subprogram DBOLS( ). The value of
C     MROWS is an OUTPUT variable when this option is used. Its value
C     is always in the range 0 .le. MROWS .le. NCOLS+1. It is equal to
C     the number of rows after the triangularization of the entire set
C     of equations. If LP is the processing pointer for IOPT(*), the
C     usage for the sequential processing of blocks of equations is
C
C        IOPT(LP)=1
C        Move block of equations to W(*,*) starting at
C        the first row of W(*,*).
C        IOPT(LP+3)=# of rows in the block; user defined
C
C     The user now calls DBOLS( ) in a loop. The value of IOPT(LP+1)
C     directs the user's action. The value of IOPT(LP+2) points to
C     where the subsequent rows are to be placed in W(*,*).
C
C      .<LOOP
C      . CALL PDA_DBOLS()
C      . IF(IOPT(LP+1) .EQ. 1) THEN
C      .    IOPT(LP+3)=# OF ROWS IN THE NEW BLOCK; USER DEFINED
C      .    PLACE NEW BLOCK OF IOPT(LP+3) ROWS IN
C      .    W(*,*) STARTING AT ROW IOPT(LP+2).
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
C
C   2
C   -
C     This option is useful for checking the lengths of all arrays used
C     by DBOLS() against their actual requirements for this problem.
C     The idea is simple: the user's program unit passes the declared
C     dimension information of the arrays. These values are compared
C     against the problem-dependent needs within the subprogram. If any
C     of the dimensions are too small an error message is printed and a
C     negative value of MODE is returned, -11 to -17. The printed error
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
C        CALL PDA_DBOLS()
C
C     Use of this option adds 8 to the required length of IOPT(*).
C
C   3
C   -
C     This option changes the type of scaling for the data matrix E.
C     Nominally each nonzero column of E is scaled so that the
C     magnitude of its largest entry is equal to the value ONE. If LP
C     is the processing pointer for IOPT(*),
C
C        IOPT(LP)=3
C        IOPT(LP+1)=1,2 or 3
C            1= Nominal scaling as noted;
C            2= Each nonzero column scaled to have length ONE;
C            3= Identity scaling; scaling effectively suppressed.
C         .
C        CALL PDA_DBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   4
C   -
C     This option allows the user to provide arbitrary (positive)
C     column scaling for the matrix E. If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=4
C        IOPT(LP+1)=IOFF
C        X(NCOLS+IOFF),...,X(NCOLS+IOFF+NCOLS-1)
C        = Positive scale factors for cols. of E.
C         .
C        CALL PDA_DBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*) and
C     NCOLS to the required length of X(*).
C
C   5
C   -
C     This option allows the user to provide an option array to the
C     low-level subprogram DBOLSM(). If LP is the processing pointer
C     for IOPT(*),
C
C        IOPT(LP)=5
C        IOPT(LP+1)= Position in IOPT(*) where option array
C                    data for DBOLSM() begins.
C         .
C        CALL PDA_DBOLS()
C
C     Use of this option adds 2 to the required length of IOPT(*).
C
C   6
C   -
C     Move the processing pointer (either forward or backward) to the
C     location IOPT(LP+1). The processing point is moved to entry
C     LP+2 of IOPT(*) if the option is left with -6 in IOPT(LP).  For
C     example to skip over locations 3,...,NCOLS+2 of IOPT(*),
C
C       IOPT(1)=6
C       IOPT(2)=NCOLS+3
C       (IOPT(I), I=3,...,NCOLS+2 are not defined here.)
C       IOPT(NCOLS+3)=99
C       CALL PDA_DBOLS()
C
C     CAUTION: Misuse of this option can yield some very hard
C     -to-find bugs.  Use it with care.
C
C   7
C   -
C     If the user is calling DBOLS() directly, use this option.
C     (This is necessary because DBOCLS() uses DBOLS() as a
C     low-level subprogram.  Due to weighting required within
C     DBOCLS(), the two cases must be known.) For example,
C
C       IOPT(1)=7
C       IOPT(1)=99
C
C   99
C   --
C     There are no more options to change.
C
C     Only option numbers -99, -7,-6,-5,...,-1, 1,2,...,7, and 99 are
C     permitted. Other values are errors. Options -99,-1,...,-7 mean
C     that the repective options 99,1,...,7 are left at their default
C     values. An example is the option to modify the (rank) tolerance:
C
C       IOPT(1)=-3 Option is recognized but not changed
C       IOPT(2)=2  Scale nonzero cols. to have length ONE
C       IOPT(3)=99
C
C    ERROR MESSAGES for DBOLS()
C    ----- -------- --- -------
C
C WARNING IN...
C DBOLS(). MDW=(I1) MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         2
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         3
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =         4
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, R1=    0.
C           IN ABOVE MESSAGE, R2=    ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         6
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). ISCALE OPTION=(I1) MUST BE 1-3.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         7
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED  COLUMN SCALING
C MUST BE POSITIVE.
C           IN ABOVE MESSAGE, I1=         0
C ERROR NUMBER =         8
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE.
C COMPONENT (I1) NOW = (R1).
C           IN ABOVE MESSAGE, I1=        ND. .LE. MDW=(I2).
C           IN ABOVE MESSAGE, I1=         1
C           IN ABOVE MESSAGE, I2=         0
C ERROR NUMBER =        10
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS().THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE.THE NUMBER OF ROWS=
C (I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        11
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+1=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        12
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS().THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1) MUST BE
C .GE. NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        13
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        14
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 5*NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         3
C ERROR NUMBER =        15
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         2
C ERROR NUMBER =        16
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C
C WARNING IN...
C DBOLS() THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN.=(I2).
C           IN ABOVE MESSAGE, I1=         0
C           IN ABOVE MESSAGE, I2=         1
C ERROR NUMBER =        17
C (NORMALLY A RETURN TO THE USER TAKES PLACE FOLLOWING THIS MESSAGE.)
C***REFERENCES  HANSON, R. J. LINEAR LEAST SQUARES WITH BOUNDS AND
C                 LINEAR CONSTRAINTS, SIAM J. SCI. STAT. COMPUT., VOL. 7,
C                 NO. 3, JULY, 1986.
C***ROUTINES CALLED  PDA_IDAMAX,DBOLSM,DCOPY,PDA_DNRM2,DROT,DROTG,XERRWV
C***COMMON BLOCKS    DBOCOM
C***END PROLOGUE  DBOLS
C
C     SOLVE LINEAR LEAST SQUARES SYSTEM WITH BOUNDS ON
C     SELECTED VARIABLES.
C     REVISED 880722-1100
C     REVISED YYMMDD-HHMM
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM NAME TO DBOLS AND THE STRINGS
C     /SCOPY/ TO /DCOPY/, /DBOL/ TO /DBOL/,
C     /SNRM2/ TO /PDA_DNRM2/, /ISAMAX/ TO /PDA_IDAMAX/,
C     /SROTG/ TO /DROTG/, /SROT/ TO /DROT/, /E0/ TO /D0/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*),X(*),RW(*)
      DOUBLE PRECISION SC, SS, ONE, PDA_DNRM2, RNORM, ZERO
      EXTERNAL PDA_DNRM2
C
C     THIS VARIABLE SHOULD REMAIN TYPE REAL.
      REAL RDUM
      INTEGER IND(*),IOPT(*),IW(*)
      INTEGER PDA_IDAMAX
      EXTERNAL PDA_IDAMAX
      LOGICAL CHECKL
      COMMON /DBOCOM/ IDOPE(5)
      SAVE IGO,LOCACC,LOPT,ISCALE
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  DBOLS
      NERR = 0
      MODE = 0
      LEVEL = 1
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              NERR = 2
              NCHAR = 35
              CALL PDA_XERRWV('DBOLS(). MDW=(I1) MUST BE POSITIVE.',
     .             NCHAR, NERR,LEVEL,1,MDW,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              NERR = 3
              NCHAR = 58
              CALL PDA_XERRWV(
     .      'DBOLS(). NCOLS=(I1) THE NO. OF VARIABLES MUST BE POSITIVE.'
     .                    ,NCHAR,NERR,LEVEL,1,NCOLS,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  NERR = 4
                  NCHAR = 45
                  CALL PDA_XERRWV(
     .                   'DBOLS(). FOR J=(I1), IND(J)=(I2) MUST BE 1-4.'
     .                        ,NCHAR,NERR,LEVEL,2,J,IND(J),0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                      NERR = 5
                      NCHAR = 57
                      CALL PDA_XERRWV(
     .       'DBOLS(). FOR J=(I1), BOUND BL(J)=(R1) IS .GT. BU(J)=(R2).'
     .                            ,NCHAR,NERR,LEVEL,1,J,IDUM,2,
     .                            REAL(BL(J)), REAL(BU(J)) )
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  END IF
              END IF
   20     CONTINUE
C     END PROCEDURE
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.D0
          ONE = 1.D0
          CHECKL = .FALSE.
          LENX = NCOLS
          ISCALE = IDOPE(4)
          IGO = 2
          LOPT = 0
          LP = 0
          LDS = 0
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = LP + 1
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
              GO TO 30
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG, ROW STACKING POINTER
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).
C      MUST ALSO START PROCESS WITH IOPT(LOCACC)=1.)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*).  SET IOPT(LOCACC+2)=NO. OF ROWS IN BLOCK.)
C              LOOP
C              CALL PDA_DBOLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS., STARTING AT ROW IOPT(LOCACC+1),
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
C              SET IOPT(LOCACC-1)=-OPTION NUMBER FOR SEQ. ACCUMULATION.
C              CALL PDA_DBOLS( )
                  IOPT(LOCACC+1) = 1
                  IGO = 1
              END IF
              LDS = 4
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MROWS
C     LNDW.GE.NCOLS+1
C     LLB .GE.NCOLS
C     LLX .GE.NCOLS+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.5*NCOLS
C     LLIW.GE.2*NCOLS
C     LIOP.GE. AMOUNT REQD. FOR IOPTION ARRAY.
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
              GO TO 30
C
C     OPTION TO MODIFY THE COLUMN SCALING.
          ELSE IF (JP.EQ.3) THEN
              IF (IP.GT.0) THEN
                  ISCALE = IOPT(LP+2)
C
C     SEE THAT ISCALE IS 1 THRU 3.
                  IF (ISCALE.LT.1 .OR. ISCALE.GT.3) THEN
                      NERR = 7
                      NCHAR = 40
                      CALL PDA_XERRWV(
     .                        'DBOLS(). ISCALE OPTION=(I1) MUST BE 1-3.'
     .                            ,NCHAR,NERR,LEVEL,1,ISCALE,IDUM,0,
     .                            RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
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
                      NERR = 8
                      NCHAR = 85
                      CALL PDA_XERRWV(
     .'DBOLS(). OFFSET PAST X(NCOLS) (I1) FOR USER-PROVIDED COLUMN SCALI
     .NG MUST BE POSITIVE.',NCHAR,NERR,LEVEL,1,IOPT(LP+2),IDUM,0,RDUM,
     .                            RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  END IF
                  CALL PDA_DCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          NERR = 9
                          NCHAR = 85
                          CALL PDA_XERRWV(
     .'DBOLS(). EACH PROVIDED COL. SCALE FACTOR MUST BE POSITIVE. COMPON
     .ENT (I1) NOW = (R1).',NCHAR,NERR,LEVEL,1,J,IDUM,1,REAL(RW(J)),
     .RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 190
                      END IF
   40             CONTINUE
              END IF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO DBOLSM().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              END IF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS THE PLACE TO
C     MOVE AT THE NEXT STEP OF PROCESSESING.
          ELSE IF (JP.EQ.6) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2)-1
                  LDS = 0
              ELSE
                  LDS = 2
              END IF
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION PROVIDES INFORMATION ABOUT WHO CALLED DBOLS.
C     IT WAS EITHER DBOCLS() OR THE USER.
          ELSE IF (JP.EQ.7) THEN
              LDS=1
              IF (IP.GT.0) THEN
                IDOPE(5)=0
                ISCALE=1
              END IF
C     CYCLE FOREVER
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              NERR = 6
              NCHAR = 47
              CALL PDA_XERRWV(
     .                 'DBOLS(). THE OPTION NUMBER=(I1) IS NOT DEFINED.'
     .                    ,NCHAR,NERR,LEVEL,1,JP,IDUM,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          END IF
   50     CONTINUE
C     END PROCEDURE
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
              IF (LMDW.LT.MROWS) THEN
                  NERR = 11
                  NCHAR = 76
                  CALL PDA_XERRWV(
     .'DBOLS(). THE ROW DIMENSION OF W(,)=(I1) MUST BE .GE.THE NUMBER OF
     . ROWS=(I2).',NCHAR,NERR,LEVEL,2,LMDW,MROWS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LNDW.LT.NCOLS+1) THEN
                  NERR = 12
                  NCHAR = 69
                  CALL PDA_XERRWV(
     .'DBOLS(). THE COLUMN DIMENSION OF W(,)=(I1) MUST BE .GE. NCOLS+1=(
     .I2).',NCHAR,NERR,LEVEL,2,LNDW,NCOLS+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLB.LT.NCOLS) THEN
                  NERR = 13
                  NCHAR = 88
                  CALL PDA_XERRWV(
     .'DBOLS(). THE DIMENSIONS OF THE ARRAYS BL(),BU(), AND IND()=(I1) M
     .UST BE .GE. NCOLS=(I2).',NCHAR,NERR,LEVEL,2,LLB,NCOLS,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLX.LT.LENX) THEN
                  NERR = 14
                  NCHAR = 70
                  CALL PDA_XERRWV(
     .'DBOLS(). THE DIMENSION OF X()=(I1) MUST BE .GE. THE REQD. LENGTH=
     .(I2).',NCHAR,NERR,LEVEL,2,LLX,LENX,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLRW.LT.5*NCOLS) THEN
                  NERR = 15
                  NCHAR = 62
                  CALL PDA_XERRWV(
     .  'DBOLS(). THE DIMENSION OF RW()=(I1) MUST BE .GE. 5*NCOLS=(I2).'
     .                        ,NCHAR,NERR,LEVEL,2,LLRW,5*NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LLIW.LT.2*NCOLS) THEN
                  NERR = 16
                  NCHAR = 61
                  CALL PDA_XERRWV(
     .   'DBOLS() THE DIMENSION OF IW()=(I1) MUST BE .GE. 2*NCOLS=(I2).'
     .                        ,NCHAR,NERR,LEVEL,2,LLIW,2*NCOLS,0,RDUM,
     .                        RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
              IF (LIOPT.LT.LP+1) THEN
                  NERR = 17
                  NCHAR = 71
                  CALL PDA_XERRWV(
     .'DBOLS(). THE DIMENSION OF IOPT()=(I1) MUST BE .GE. THE REQD. LEN.
     .=(I2).',NCHAR,NERR,LEVEL,2,LIOPT,LP+1,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              END IF
C     END PROCEDURE
          END IF
      END IF
      GO TO (60,90),IGO
      GO TO 180
C
C     GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
C     EQUATIONS AND DIRECTIONS TO QUIT PROCESSING.
C     CASE 1
   60 CONTINUE
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      MROWS = IOPT(LOCACC+1) - 1
      INROWS = IOPT(LOCACC+2)
      MNEW = MROWS + INROWS
      IF (MNEW.LT.0 .OR. MNEW.GT.MDW) THEN
          NERR = 10
          NCHAR = 61
          CALL PDA_XERRWV(
     .   'DBOLS(). NO. OF ROWS=(I1) MUST BE .GE. 0 .AND. .LE. MDW=(I2).'
     .                ,NCHAR,NERR,LEVEL,2,MNEW,MDW,0,RDUM,RDUM)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 190
      END IF
      DO 80 J = 1,MIN(NCOLS+1,MNEW)
          DO 70 I = MNEW,MAX(MROWS,J) + 1,-1
              IBIG = PDA_IDAMAX(I-J,W(J,J),1) + J - 1
C
C     PIVOT FOR INCREASED STABILITY.
              CALL PDA_DROTG(W(IBIG,J),W(I,J),SC,SS)
              CALL PDA_DROT(NCOLS+1-J,W(IBIG,J+1),MDW,W(I,J+1),MDW,SC,
     :                      SS)
              W(I,J) = ZERO
   70     CONTINUE
   80 CONTINUE
      MROWS = MIN(NCOLS+1,MNEW)
      IOPT(LOCACC+1) = MROWS + 1
      IGO = IOPT(LOCACC)
C     END PROCEDURE
      IF (IGO.EQ.2) THEN
          IGO = 0
      END IF
      GO TO 180
C     CASE 2
   90 CONTINUE
C     DO(INITIALIZE VARIABLES AND DATA VALUES)
C     PROCEDURE(INITIALIZE VARIABLES AND DATA VALUES)
      DO 150 J = 1,NCOLS
          GO TO (100,110,120,130),ISCALE
          GO TO 140
  100     CONTINUE
C     CASE 1
C
C     THIS IS THE NOMINAL SCALING. EACH NONZERO
C     COL. HAS MAX. NORM EQUAL TO ONE.
          IBIG = PDA_IDAMAX(MROWS,W(1,J),1)
          RW(J) = ABS(W(IBIG,J))
          IF (RW(J).EQ.ZERO) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          END IF
          GO TO 140
  110     CONTINUE
C     CASE 2
C
C     THIS CHOICE OF SCALING MAKES EACH NONZERO COLUMN
C     HAVE EUCLIDEAN LENGTH EQUAL TO ONE.
          RW(J) = PDA_DNRM2(MROWS,W(1,J),1)
          IF (RW(J).EQ.ZERO) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          END IF
          GO TO 140
  120     CONTINUE
C     CASE 3
C
C     THIS CASE EFFECTIVELY SUPPRESSES SCALING BY SETTING
C     THE SCALING MATRIX TO THE IDENTITY MATRIX.
          RW(1) = ONE
          CALL PDA_DCOPY(NCOLS,RW,0,RW,1)
          GO TO 160
  130     CONTINUE
C     CASE 4
          GO TO 160
  140     CONTINUE
  150 CONTINUE
  160 CONTINUE
C     END PROCEDURE
C     DO(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C
C     INITIALIZE IBASIS(*), J=1,NCOLS, AND IBB(*), J=1,NCOLS,
C     TO =J,AND =1, FOR USE IN DBOLSM( ).
      DO 170 J = 1,NCOLS
          IW(J) = J
          IW(J+NCOLS) = 1
          RW(3*NCOLS+J) = BL(J)
          RW(4*NCOLS+J) = BU(J)
  170 CONTINUE
      CALL PDA_DBOLSN(W,MDW,MROWS,NCOLS,RW(3*NCOLS+1),RW(4*NCOLS+1),IND,
     .            IOPT(LOPT),X,RNORM,MODE,RW(NCOLS+1),RW(2*NCOLS+1),RW,
     .            IW,IW(NCOLS+1))
C     END PROCEDURE
      IGO = 0
  180 CONTINUE
      RETURN
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  190 IF(MODE.GE.0)MODE = -NERR
      IGO = 0
      RETURN
C     END PROCEDURE
      END
