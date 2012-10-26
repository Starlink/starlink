*     30 Nov 1993 (hme):
*        Change .SPX extension to lower case (but leave temporary file at
*        TEMP.spx.
*     08 Dec 1993 (hme):
*        Must not use the integer returned by IGETLUN as if it were a
*        logical.
*        Must use consistent case for temporary file name TEMP.spx
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     01 Jan 1994 (rp):
*        Change TEMP to temp for consistency with normal useage
*     19 Feb 1997 (timj):
*        Change output format to allow >10000 for do loop indices
*      6 Jun 2000 (ajc):
*        Replace 'type *' with 'PRINT *'
*        Remove unsupported CARRIAGECONTROL from OPEN
*        Change DISP= to STATUS= in CLOSE
*        Unused in SCL_DO: GEN_IENDCH, J, IDIFF
*        Unused in SCL_ENDDO: IERR
*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION SCL_DO (IERR)

*  Routine to set up a top-level DO loop in SCL. May be short-lived:
*  does not represent the final solution but will work until all this
*  is worked out properly.

      IMPLICIT  NONE

*  Formal parameters:

      INTEGER*4 IERR

*  Include file(s):

      INCLUDE  'SCL_BUFFER.INC'
      INCLUDE  'SCL_DO.INC'
      INCLUDE  'CNF_PAR'

*  Functions:

      INTEGER   IGETLUN
      INTEGER   IFREELUN
      INTEGER   GEN_ILEN
      INTEGER   GEN_LINENO
      INTEGER   STACK_POINTER

*  Local variables:

      LOGICAL   READONLY
      INTEGER   I
      INTEGER   DO_PARS(3)
      INTEGER   ISP
      INTEGER   ISTAT
      INTEGER   LCOM
      INTEGER   NVALS
      INTEGER   SYM_ADDRESS
      INTEGER   SYM_INDEX
      INTEGER   STATUS
      CHARACTER TYPE*4
      INTEGER   LENGTH
      CHARACTER SYMBOL*16
      CHARACTER COMMAND*256
      CHARACTER TBUFFER*(MAXLB)

      SCL_DO = .TRUE.
      IERR   =  0

*  Get symbol to do on

      CALL GEN_GETSTR2 (1, 'Do variable name?',
     &                  ' ', ' ', SYMBOL, ISTAT)

*  Find the symbol

      CALL GEN_INQSYMB (SYMBOL, SYM_INDEX, TYPE, LENGTH,
     &                  SYM_ADDRESS, READONLY, ISTAT)

      IF (SYM_INDEX .EQ. 0) THEN
        IERR     = 100                   ! Variable not defined
        GO TO 99
      ELSE IF (READONLY) THEN
        IERR     = 102                   ! Readonly variable
        GO TO 99
      ELSE IF (TYPE(1:1).NE.'I') THEN
        IERR     = 80                    ! Do variable must be integer
        GO TO 99
      END IF

*  Get ranges from terminal

      ISTAT  = 0
      CALL GEN_GETI4A2 ('First, last, increment ?',   ! Prompt
     &                   DO_PARS(1), 3,               ! Default array
     &                   ' ',                         ! Format for def prompt
     &                   DO_PARS(1), NVALS,           ! Returned array
     &                   ISTAT)                       ! Status
      IF (ISTAT.EQ.0) THEN
        IF (NVALS.LT.2) DO_PARS(2) = DO_PARS(1)
        IF (NVALS.LT.3) DO_PARS(3) = SIGN (1, DO_PARS(2) - DO_PARS(1))
      END IF

CD    PRINT *,'-- DO --'
CD    PRINT *,'Index range'
CD    Type '((1X,I4,'' to '',I4 '' by '',I4))', (DO_PARS(I),I=1,3)

*  Find out what to DO, if DOing from command level

      ISP = STACK_POINTER()
      IF (ISP.EQ.0) THEN

        ISTAT = IGETLUN (DO_FILE, 'scl_do', .TRUE.)
        IF (ISTAT.NE.0) THEN
          ISTAT = IFREELUN (DO_FILE)
          IERR = 6
          RETURN
        END IF

        OPEN (DO_FILE, FILE='temp.spx', STATUS='UNKNOWN',
     &        ACCESS='SEQUENTIAL', IOSTAT=ISTAT)
        IF (ISTAT.NE.0) THEN
          CLOSE (DO_FILE, IOSTAT=ISTAT)
          ISTAT = IFREELUN (DO_FILE)
          IERR = 11
          RETURN
        END IF

CD      PRINT *,' Writing temp.spx: n1,n2,n3:',(do_pars(i),i=1,3)

        WRITE (DO_FILE, '('' do '', A16, 3(1X,I6))')
     &         SYMBOL, (DO_PARS(I),I=1,3)

        PRINT *,'Enter commands to do, line at a time, EOF to finish'

        ISTAT = 0
        DO WHILE (ISTAT.NE.2)
          CALL GEN_GETSTR2 (3, 'insert >> ',' ',' ', COMMAND, ISTAT)
          IF (ISTAT.NE.2) THEN
            LCOM = GEN_ILEN (COMMAND)
            WRITE (DO_FILE, *) COMMAND(:LCOM)
          ELSE
            WRITE (DO_FILE, *) 'enddo'
            WRITE (DO_FILE, *) 'return'

            TBUFFER = '@temp;' // BUFFER (IBPTR:MAXLB)   ! (IBPTR:MAXLB) // ' '
            BUFFER  =  TBUFFER
CD          PRINT *, 'Contents of buffer...'
CD          PRINT *,  BUFFER

            ILB    = 6 + (ILB-IBPTR+1)
            IBPTR  = 1

            CLOSE (DO_FILE)
            STATUS = IFREELUN (DO_FILE)
            DO_FILE = 0
          END IF
        END DO

*     Otherwise called from within a command file
      ELSE

*       Pointer to appropriate level

        DO_DEPTH = DO_DEPTH + 1

*       Address of variable to increment

        ADDRESS(DO_DEPTH) = SYM_ADDRESS

*       Remember the line number and calling level

        CALL_LEVEL(DO_DEPTH) = ISP
        CALL_LINE (DO_DEPTH) = GEN_LINENO()

*       Do limits, increment

        DO I = 1, 3
          DOVALS (I, DO_DEPTH) = DO_PARS(I)
        END DO

*       Initialize the DO variable

        CALL XCOPY (4, DOVALS(1,DO_DEPTH), 
     :              %VAL(CNF_PVAL(ADDRESS(DO_DEPTH))))

      END IF

CD    PRINT *, '-- scl_do --     do variable = ', dovals(1,do_depth)
CD    PRINT *, '                 do depth = ', do_depth

      RETURN

*  Error return

   99 CONTINUE

      SCL_DO = .FALSE.
      RETURN

      END

*-----------------------------------------------------------------------

      SUBROUTINE SCL_BREAK ()

*  Routine to break from currently executing DO loop

      IMPLICIT  NONE

*     Formal parameters:

*  Ok, go...

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SCL_ENDDO (ENDDO)

*  Routine to terminate the currently executing DO loop if necessary,
*  otherwise to increment the (currently single) parameter.

      IMPLICIT  NONE

*  Formal parameter(s):

      LOGICAL*4 ENDDO

*  Include file(s):

      INCLUDE  'SCL_DO.INC'
      INCLUDE  'CNF_PAR'

*  Functions

      INTEGER*4 GEN_LINENO

*  Local variables

      INTEGER*4 ISP
      INTEGER*4 INCRT
      INTEGER*4 IVAL
      INTEGER*4 LIMIT

*  OK? Go..

      IF (ENDDO) RETURN

*     Increment the DO variable and check to see if we have done it all.

      CALL XCOPY (4, %VAL(CNF_PVAL(ADDRESS(DO_DEPTH))), IVAL)
      LIMIT = DOVALS(2,DO_DEPTH)
      INCRT = DOVALS(3,DO_DEPTH)
CD    PRINT *, '-- scl_enddo --  current do variable = ', ival
CD    PRINT *, '                 limit/increment     = ', limit,incrt
      IVAL  = IVAL + INCRT
CD    PRINT *, '                 updated do variable = ', ival

      IF (      INCRT.GT.0 .AND. IVAL.LE.LIMIT
     &     .OR. INCRT.LT.0 .AND. IVAL.GE.LIMIT ) THEN

*       Update the DO variable
        CALL XCOPY (4, IVAL, %VAL(CNF_PVAL(ADDRESS(DO_DEPTH))))
        CALL XCOPY (4, %VAL(CNF_PVAL(ADDRESS(DO_DEPTH))), IVAL)
CD    PRINT *, '                 test put do variable = ', ival

*       Rewind the file the appropriate number of records

        ISP = CALL_LEVEL(DO_DEPTH)
        CALL GEN_REWIND (ISP, GEN_LINENO() - CALL_LINE(DO_DEPTH))

      ELSE
        IF (DO_DEPTH.EQ.1) ENDDO = .TRUE.
        DO_DEPTH = DO_DEPTH - 1
      END IF

CD    PRINT *, '                 do depth = ', do_depth

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SCL_UNWIND (N)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER*4 N

*     Include files

      INCLUDE 'SCL_DO.INC'
      INCLUDE 'SCL_IFDO.INC'

*     Functions

      INTEGER*4 IFREELUN
      INTEGER*4 STACK_POINTER
      LOGICAL*4 PULL_IFSTACK

*     Local variables

      INTEGER*4 ISP
      INTEGER*4 ISTAT
      INTEGER*4 IERR
      LOGICAL*4 STATUS

*     OK, Go..
*     Currently editing temp.spx file for DO? If so, close it:

      IF (DO_FILE.NE.0) THEN
        CLOSE (DO_FILE, STATUS='DELETE', IOSTAT=IERR)
        IF (IERR.EQ.0) THEN
          ISTAT = IFREELUN (DO_FILE)
          DO_FILE = 0
        END IF
      END IF

*     Release the requested number of command files

      IF (N.GT.0) CALL GEN_UNWIND (N)
      ISP = STACK_POINTER()

*     Now cancel any unfulfilled DO/IF requests

      IF (.NOT.ENDDO) THEN
        DO WHILE (DO_DEPTH.GT.0 .AND. CALL_LEVEL(DO_DEPTH).GT.ISP)
          DO_DEPTH = DO_DEPTH - 1
        END DO
        IF (DO_DEPTH.EQ.0) ENDDO = .TRUE.
      END IF

*     IF stack: pull frames from the stack one by one until we find
*     one (if any) at the current level

      STATUS = .TRUE.
      DO WHILE (IF_LEVEL.GT.ISP .AND. STATUS)
        STATUS  = PULL_IFSTACK (IF_LEVEL)
        IF_SKIP = 0
CD      PRINT *, '-- scl_unwind -- if_level: ', if_level
      END DO

      DO_TO_ELSEIF   = .TRUE.
      WAIT_FOR_ENDIF = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------
