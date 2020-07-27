
*+
*  Name:
*     ant_x

*  Purpose:
*     Routines for expression handling.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     CGP: Clive Page
*     ACD: A C Davenhall (Edinburgh)

*  History:
*     1993-JUL-29 (CGP):
*        Original Version
*     1996-MAY-09 (ACD):
*        Changed the check for whether there is space for another identifier
*        from '.LE.' to '.LT.' to correct and 'out by one' bug,
*     1999-AUG-13 (ACD):
*        Modified the error reporting to use the ADAM libraries rather than
*        Fortran WRITE statements.
*-

*+ANT_XCOMP  Compiles expression, returns exprn identifier.
      SUBROUTINE ANT_XCOMP(TID, EXPRN, XID, DTCODE, STATUS)
      CHARACTER EXPRN*(*)
      INTEGER TID, XID, DTCODE, STATUS
*TID    input  Table identifier.
*EXPRN  input  Text expression.
*XID    output Expression identifier, used later in ANT_XEVAL
*DTCODE output Data-type code of result, -2 if none, otherwise
*              -1=char, 0=logical, 1=byte, 2=half, 3=int, 4=real, 5=double.
*STATUS in/out Inherited status.
      INCLUDE 'antx1.inc'
      INCLUDE 'antx2.inc'
*-
*local storage
      INTEGER NEXT, ITYPE, LENGTH, CONID, IVAL, CID, DTYPE, ETYPE,
     $        CODE, TOPOP, J
      CHARACTER CVAL*80, ITEM*80, NCHAR*1
      DOUBLE PRECISION DVAL
      LOGICAL LVAL
      CHARACTER ERRBUF*30, ERRPNT*80
      INTEGER LSTAT
*ensure BLOCK DATA module is loaded.
      EXTERNAL ANT_XBLOCK
*
      IF(STATUS .NE. 0) RETURN
      NEXT = 1
      NTS  = 0
*Push start-of-expression code on operator stack
      OSTACK(1) = 1
      NOS       = 1
*Allocate expression identifier for this expression
      IF(NXID .LT. MAXXID) THEN
         NXID = NXID + 1
         XID  = NXID
         NOPC(XID) = 0
      ELSE
         CALL ANT_ERR ('ANT_XCOMP: too many expressions compiled.')
C        WRITE(*,*)'!ANT_XCOMP: too many expressions compiled'
         STATUS = -9
         GO TO 999
      END IF
*Get next item, expect constant/variable/unary-op next
100   CONTINUE
      CALL ANT_XSCANC(EXPRN, NEXT, ITEM, ITYPE)
***      PRINT *,' ctype', ITYPE, ' ', ITEM(1:20)
      IF(NEXT .LE. LEN(EXPRN)) THEN
         NCHAR = EXPRN(NEXT:NEXT)
      ELSE
         NCHAR = ' '
      END IF
      IF(ITYPE .EQ. 4 .OR. ITYPE .EQ. 8) THEN
*string constant enclosed in quotes: remove quotes and find length
*output codes to load character constant, push data-type on type-stack
         CALL ANT_SQTOS(ITEM, LENGTH, CVAL, STATUS)
         CALL ANT_XSTORC(CVAL(1:LENGTH), CONID, STATUS)
         CALL ANT_XPUTOP(XID, 2, STATUS)
         CALL ANT_XPUTOP(XID, CONID, STATUS)
         CALL ANT_XPUSHT(-1, STATUS)
      ELSE IF(ITYPE .EQ. 5) THEN
*unary #
         GO TO 300
      ELSE IF(ITYPE .EQ. 6 .OR. ITYPE .EQ. 11 .OR. ITYPE .EQ. 24) THEN
*integer constant (includes hex/octal/binary ones)
*output codes to load integer constant, push type on type-stack
         CALL ANT_STOI(ITEM, IVAL, STATUS)
         CALL ANT_XSTORD(DBLE(IVAL), CONID, STATUS)
         CALL ANT_XPUTOP(XID, 4, STATUS)
         CALL ANT_XPUTOP(XID, CONID, STATUS)
         CALL ANT_XPUSHT(3, STATUS)
      ELSE IF(ITYPE .EQ. 9) THEN
*left parenthesis: push on opstack immediately
         CODE = 3
         GO TO 400
      ELSE IF(ITYPE .EQ. 10) THEN
*unary + or -: ignore unary plus, change name of unary minus
         IF(ITEM(1:1) .EQ. '+') THEN
            GO TO 100
         ELSE
            ITEM = 'UNARY-'
            GO TO 300
         END IF
      ELSE IF(ITYPE .EQ. 12 .OR. ITYPE .EQ. 15 .OR. ITYPE .EQ. 21) THEN
*decimal floating-point constant
*output codes to load numerical constant, push type on type-stack
         CALL ANT_STOD(ITEM, DVAL, STATUS)
         CALL ANT_XSTORD(DVAL, CONID, STATUS)
         CALL ANT_XPUTOP(XID, 4, STATUS)
         CALL ANT_XPUTOP(XID, CONID, STATUS)
         CALL ANT_XPUSHT( 5, STATUS)
      ELSE IF(ITYPE .GE. 16 .AND. ITYPE .LE. 19) THEN
*sexagesimal angle in h:m:s or +d:m:s - convert to double prec constant
*output codes to load numerical constant, push type on type-stack
         CALL ANT_SEXRAD(ITEM, DVAL, STATUS)
         CALL ANT_XSTORD(DVAL, CONID, STATUS)
         CALL ANT_XPUTOP(XID, 4, STATUS)
         CALL ANT_XPUTOP(XID, CONID, STATUS)
         CALL ANT_XPUSHT( 5, STATUS)
      ELSE IF(ITYPE .EQ. 23) THEN
* could be unary .NOT., .FALSE. or .TRUE.  test for first of these
         CALL ANT_SUPPER(ITEM)
         IF(ITEM .EQ. '.NOT.') THEN
            GO TO 300
         ELSE
*assume logical constant, push type on type-stack
            CALL ANT_STOL(ITEM, LVAL, STATUS)
            CALL ANT_XSTORL(LVAL, CONID, STATUS)
            CALL ANT_XPUTOP(XID, 3, STATUS)
            CALL ANT_XPUTOP(XID, CONID, STATUS)
            CALL ANT_XPUSHT( 0, STATUS)
         END IF
      ELSE IF(ITYPE .EQ. 25) THEN
*variable name - assume function if next char is "("
         IF(NCHAR .EQ. '(') THEN
            GO TO 300
         ELSE
*assume column name - look up column identifier
*restrict ETYPE range to -1 (char), 0 (logical), +1 (numeric)
            CALL ANT_CFIND(TID, ITEM, CID, DTYPE, STATUS)
            DTYPE = MAX(-1,DTYPE)
            ETYPE = MIN(1,DTYPE)
            CALL ANT_XPUTOP(XID, 10+ETYPE, STATUS)
            CALL ANT_XPUTOP(XID, CID, STATUS)
            CALL ANT_XPUSHT( DTYPE, STATUS)
         END IF
      ELSE IF(ITYPE .EQ. 28) THEN
*array element of column, restrict data type range as above
         CALL ANT_CFIND(TID, ITEM, CID, DTYPE, STATUS)
         DTYPE = MAX(-1,DTYPE)
         ETYPE = MIN(1,DTYPE)
         CALL ANT_XPUTOP(XID, 10+ETYPE, STATUS)
         CALL ANT_XPUTOP(XID, CID, STATUS)
         CALL ANT_XPUSHT( DTYPE, STATUS)
      ELSE IF(ITYPE .EQ. 31) THEN
*date and time - transform to MJD and treat as DP constant
*output codes to load numerical constant, push type on type-stack
         CALL ANT_STOMJD(ITEM, DVAL, STATUS)
         CALL ANT_XSTORD(DVAL, CONID, STATUS)
         CALL ANT_XPUTOP(XID, 4, STATUS)
         CALL ANT_XPUTOP(XID, CONID, STATUS)
         CALL ANT_XPUSHT( 5, STATUS)
      ELSE IF(ITYPE .EQ. 1) THEN
         CALL ANT_ERR ('ANT_XCOMP: unexpected end of expression.')
C        WRITE(*,*)'!ANT_XCOMP: unexpected end of expression'
         STATUS = -9
         GO TO 999
      ELSE
*error of some other sort
         ERRBUF = ' '
         WRITE(ERRBUF, '(A20, I5)', IOSTAT=LSTAT) ITEM(1:20), ITYPE
         CALL ANT_ERRC ('ANT_XCOMP: unknown item,', ERRBUF)
C        WRITE(*,*)'!ANT_XCOMP: unknown item ', ITEM(1:20), ITYPE
         STATUS = -9
         GO TO 999
      END IF
      IF(STATUS .NE. 0) GO TO 999
*
*Get next item: expect binary-operator etc
*
200   CONTINUE
      CALL ANT_XSCANB(EXPRN, NEXT, ITEM, ITYPE)
***      PRINT *,' btype', ITYPE, ' ', ITEM(1:20)
      IF(ITYPE .EQ. 1) THEN
*end-of-expression:
         ITEM = '*END'
      ELSE IF(ITYPE .LE. 1 .OR. ITYPE .GT. 10) THEN
*error of some sort
         ERRBUF = ' '
         WRITE(ERRBUF, '(A20, I5)', IOSTAT=LSTAT) ITEM(1:20), ITYPE
         CALL ANT_ERRC ('ANT_XCOMP: unknown bin-op,', ERRBUF)
C        WRITE(*,*)'!ANT_XCOMP: unknown bin-op ', ITEM(1:20), ITYPE
         STATUS = -9
         GO TO 999
      END IF
*
*Handle operator or function
*
300   CONTINUE
      CALL ANT_XFIND(ITEM, CODE, STATUS)
      IF(STATUS .NE. 0) GO TO 999
*examine the top operator on the op-stack
350   CONTINUE
      TOPOP = OSTACK(NOS)
      IF( FPREC(TOPOP) .GT. FPREC(CODE) .OR.
     $   (FPREC(TOPOP) .EQ. FPREC(CODE) .AND.
     $    FPREC(TOPOP) .LT. 28)) THEN
*pop an operator off the stack
         NOS   = NOS - 1
         IF(TOPOP .EQ. 1) THEN
*start-of-expression - must have reached the end of exprn
            IF(CODE .NE. 2 .OR. NOS .NE. 0) THEN
               ERRBUF = ' '
               WRITE(ERRBUF, '(I5, I5)', IOSTAT=LSTAT) CODE, TOPOP
               CALL ANT_ERRC ('ANT_XCOMP: unexpected end,', ERRBUF)
C              WRITE(*,*)'!ANT_XCOMP: unexpected end', CODE, TOPOP
               STATUS = -9
            END IF
            GO TO 900
         ELSE IF(TOPOP .EQ. 3) THEN
*left-parenthesis - should only be popped by a right-parenthesis
            IF(CODE .EQ. 4) THEN
               GO TO 200
            ELSE
               ERRBUF = ' '
               WRITE(ERRBUF, '(I5, I5)', IOSTAT=LSTAT) CODE, TOPOP
               CALL ANT_ERRC ('Unmatched parentheses,', ERRBUF)
C              WRITE(*,*)'!unmatched parentheses', CODE, TOPOP
               STATUS = -9
               GO TO 999
            END IF
         ELSE IF(TOPOP .EQ. 5) THEN
*comma - ignore it and go around again
            GO TO 350
         ELSE
*apply the operator TOPOP to the value stack, generating opcodes
            CALL ANT_XGENOP(XID, TOPOP, STATUS)
         END IF
         IF(STATUS .NE. 0) GO TO 999
         GO TO 350
      END IF
400   CONTINUE
*then push the newly arrived operator on the stack
      IF(NOS .LT. MAXST) THEN
         NOS = NOS + 1
         OSTACK(NOS) = CODE
      ELSE
         CALL ANT_ERRC ('ANT_XCOMP: op-stack overflow,',
     $     EXPRN(1:NEXT-1))
C        WRITE(*,*)'!ANT_XCOMP: op-stack overflow ', EXPRN(1:NEXT-1)
         STATUS = -9
         GO TO 900
      END IF
      IF(NEXT .LE. LEN(EXPRN)) GO TO 100
      CALL ANT_ERR ('ANT_XCOMP: dangling operator at end.')
C     WRITE(*,*)'!ANT_XCOMP: dangling operator at end '
      STATUS = -9
*normal exit here
900   CONTINUE
      IF(NTS .EQ. 1) THEN
         DTCODE = TSTACK(1)
      ELSE IF(NTS .GT. 1) THEN
         CALL ANT_ERR ('ANT_XCOMP: unused values on stack.')
C        WRITE(*,*)'!ANT_XCOMP: unused values on stack'
         STATUS = -9
         DTCODE = -2
      ELSE
         DTCODE = -2
         CALL ANT_ERR ('ANT_XCOMP: warning - value stack empty.')
C        WRITE(*,*)'!ANT_XCOMP: warning - value stack empty'
      END IF
999   CONTINUE
      IF(STATUS .NE. 0) THEN
         CALL ANT_ERR (EXPRN)
         IF (NEXT .LE. LEN(ERRPNT)) THEN
            ERRPNT = ' '
            DO J=1,NEXT-1
               ERRPNT(J:J) = '-'
            END DO
            ERRPNT(NEXT:NEXT) = '^'
         ELSE
            ERRPNT = 'bad expression.'
         END IF
         CALL ANT_ERR (ERRPNT)
C        WRITE(*,998) EXPRN, ('-',J=1,NEXT-1), '^'
C998     FORMAT('!', 2X, A / '!', 1X, 132A1)
***         print *,'code=', code
***         print *,'nos=',nos, ' ostack=', (ostack(j),j=1,nos)
***         print *,'nts=',nts, ' tstack=', (tstack(j),j=1,nts)
      END IF
      END

*+ANT_XGENOP  Checks operator arguments, appends opcodes to output.
      SUBROUTINE ANT_XGENOP(XID, NFUNC, STATUS)
      INTEGER XID, NFUNC, STATUS
*XID    input  Expression identifier.
*NFUNC  input  Function/operator number in list (usually=opcode)
*STATUS in/out Inherited status.
      INCLUDE 'antx1.inc'
*-Author  Clive Page 1993-JUL-29
      INTEGER NARGS, RTYPE, ITYPE, OPCODE, IARG, N
      CHARACTER INCODE(-1:5)*1, SARGS*8
      DATA INCODE / 'C', 'L', 5*'N' /
*
*TSTACK contains argument stack data type codes:
* -1=char, 0=logical, 1=byte, 2=half, 3=int, 4=real, 5=double.
      IF(STATUS .NE. 0) RETURN
      NARGS = INDEX(FARGS(NFUNC), ' ') - 2
      IF(NARGS .GT. NTS) THEN
         CALL ANT_ERRC ('ANT_XGENOP: too few arguments for ',
     $     FNAME(NFUNC) )
C        WRITE(*,*)'!ANT_XGENOP: too few args for ', FNAME(NFUNC)
         STATUS = -9
         GO TO 999
      END IF
*Pop (N-1) args off the stack, so first arg is at TSTACK(NTS)
      NTS = NTS - NARGS + 1
*RTYPE will accumulate the max value of any input arg code
*SARGS will accumulate an actual arg type string from TSTACK
      RTYPE = -2
      SARGS = ' '
      DO IARG = 1,NARGS
         ITYPE = TSTACK(NTS+IARG-1)
         RTYPE = MAX(ITYPE,RTYPE)
         SARGS(IARG:IARG) = INCODE(ITYPE)
      END DO
      OPCODE = NFUNC
*Check that the actual args are same as permitted args
      IF(FARGS(NFUNC) .EQ. 'LXX') THEN
*special case of relational operators:
* two char args ==> even opcode,  two numeric args ==> next odd opcode
         IF(SARGS .EQ. 'CC') THEN
            OPCODE = (NFUNC/2) * 2
         ELSE IF(SARGS .EQ. 'NN') THEN
            OPCODE = (NFUNC/2) * 2 + 1
         ELSE
            CALL ANT_ERRC ('ANT_XGENOP: invalid comparison',
     $        FNAME(NFUNC) )
C           WRITE(*,*)'!ANT_XGENOP: invalid comparison', FNAME(NFUNC)
            STATUS = -9
            GO TO 999
         END IF
      ELSE IF(FARGS(NFUNC) .EQ. 'LXXX') THEN
*special case of FROM/TO comparison: needs args 'CCC' or 'NNN'
*also have to pop the FROM operator off the op-stack
         NOS = NOS - 1
         IF(SARGS .EQ. 'CCC') THEN
            OPCODE = (NFUNC/2) * 2
         ELSE IF(SARGS .EQ. 'NNN') THEN
            OPCODE = (NFUNC/2) * 2 + 1
         ELSE
            CALL ANT_ERR ('ANT_XGENOP: invalid args for FROM/TO.')
C           WRITE(*,*)'!ANT_XGENOP: invalid args for FROM/TO'
            STATUS = -9
            GO TO 999
         END IF
      ELSE IF(FARGS(NFUNC)(2:2) .EQ. '*') THEN
*special case for NULL function - can take any type
         OPCODE = NFUNC
      ELSE IF(SARGS .NE. FARGS(NFUNC)(2:)) THEN
*test for all other cases
         CALL ANT_ERRC ('ANT_XGENOP: invalid arguments for',
     $     FNAME(NFUNC) )
C        WRITE(*,*)'!ANT_XGENOP: invalid args for ', FNAME(NFUNC)
         STATUS = -9
         GO TO 999
      END IF
*Generate the output data type
      N = INDEX('CLBHIRD', FARGS(NFUNC)(1:1))
      IF(N .GE. 1) THEN
         RTYPE = N - 2
      ELSE IF(FARGS(NFUNC)(1:1) .EQ. 'F') THEN
*'F' means must be floating-point output
         RTYPE = MAX(RTYPE,4)
      END IF
*Output the OPCODE, push result data type back on TSTACK
      TSTACK(NTS) = RTYPE
      CALL ANT_XPUTOP(XID, OPCODE, STATUS)
999   CONTINUE
      END

*+ANT_XSCANB  Scans expression expecting binary operator
      SUBROUTINE ANT_XSCANB(EXPRN, NEXT, ITEM, ITYPE)
      CHARACTER*(*) EXPRN, ITEM
      INTEGER NEXT, ITYPE
*EXPRN  input  Expression to be parsed.
*NEXT   in/out On entry points to next char to be examined,
*              returns pointing to char after the end of the item.
*ITEM   output Next lexical item in the expression.
*ITYPE  output Item type code:
*  1     end-of-expression (valid at this point)
*  2     comma
*  3     )
*  6     delimited operator e.g. .AND.
*  7     word operator e.g. AND
*  8     binary + or -
*  9,10  binary operator, 1 or 2 chars, / * | = & = > <
*  11    (
* others error
*-
*local storage
      INTEGER MAXCLS, MAXTYP
      PARAMETER (MAXCLS = 8, MAXTYP = 12)
*BCLASS has the character class for parsing binary operators etc
      INTEGER BCLASS(0:255), BTABLE(0:MAXCLS,MAXTYP)
      SAVE BCLASS, BTABLE
*BCLASS: character classes for binary operator
*class:       0     1     2   3   4    5    6      7      8
*          others space   ,   )   .   A-Z   +-   /*=<>&|  (
      DATA BCLASS / 32*0,
*char   sp !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
     $  1, 0, 0, 0, 0, 0, 7, 0, 8, 3, 7, 6, 2, 6, 4, 7,
*char   0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
     $  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 0,
     $  0, 26*5, 6*0, 26*5, 0, 7, 0, 0, 0, 128*0 /
*BTABLE: state table for binary operator,
*result: 1     2    3    4      5     6     7    8      9        10
*      empty comma  )  ERROR  ERROR  .op.  oper  +=  1-charop  2-charop
*CHAR  oth sp  ,  )  . AZ +- /*=><|&  (
*        0  1  2  3  4  5  6    7     8
      DATA BTABLE /
     1  12, 1, 2, 3, 4, 7, 8, 9,11,
     2   0, 0, 0, 0, 0, 0, 0, 0, 0,
     3   0, 0, 0, 0, 0, 0, 0, 0, 0,
     4   0, 0, 0, 0, 0, 5, 0, 0, 0,
     5   0, 0, 0, 0, 6, 5, 0, 0, 0,
     6   0, 0, 0, 0, 0, 0, 0, 0, 0,
     7   0, 0, 0, 0, 0, 7, 0, 0, 0,
     8   0, 0, 0, 0, 0, 0, 0, 0, 0,
     9   0, 0, 0, 0, 0, 0, 0,10, 0,
     A   0, 0, 0, 0, 0, 0, 0, 0, 0,
     1   0, 0, 0, 0, 0, 0, 0, 0, 0,
     2   0, 0, 0, 0, 0, 0, 0, 0, 0 /
*
      CALL ANT_XSCANE(MAXCLS, MAXTYP, BCLASS, BTABLE, EXPRN, NEXT,
     $               ITEM, ITYPE)
      END

*+ANT_XSCANC  Scans expression expecting constant/variable/unary-operator
      SUBROUTINE ANT_XSCANC(EXPRN, NEXT, ITEM, ITYPE)
      CHARACTER*(*) EXPRN, ITEM
      INTEGER NEXT, ITYPE
*EXPRN  input  Expression to be parsed.
*NEXT   in/out On entry points to next char to be examined,
*              returns pointing to char after the end of the item.
*ITEM   output Next lexical item in the expression.
*KPOS   output Points to first char of item found.
*ITYPE  output Item type code:
*  4,8       quoted string e.g. "a string"  or 'don''t'
*  5         unary # (unary .NOT.)
*  6         hex/octal/binary number starting with %
*  9         (
* 10         unary + or -
* 11,24      integer decimal number
* 12,15,21   decimal number in floating point or exponential format
* 16-19      angle in sexagesimal notation e.g. +12:34:56.7
* 23         logical constant .TRUE. or .FALSE. or operator .NOT.
* 25         variable name or function
* 28         array element e.g. name(99)
* 31         {date time} with enclosing braces
* others     error
*-
*local storage
      INTEGER MAXCLS, MAXTYP
      PARAMETER (MAXCLS = 16, MAXTYP = 31)
*CCLASS has the character class for parsing constants etc
      INTEGER CCLASS(0:255), CTABLE(0:MAXCLS,MAXTYP), I, J
      SAVE CCLASS, CTABLE
      DATA CCLASS / 32*0,
*char   sp !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
     $  1, 0, 2, 3, 0, 4, 0, 5, 6, 0, 0, 7, 0, 7, 8, 0,
*char   0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
     $  9, 9, 9, 9, 9, 9, 9, 9, 9, 9,10, 0, 0, 0, 0, 0,
*char  @   ABC   DE    F-Z   [   \   ]  ^  _
     $ 0, 3*12, 2*11, 21*12, 13, 0, 14, 0,12,
     $ 0, 3*12, 2*11, 21*12, 15, 0, 16, 2*0,  128*0 /
      DATA ((CTABLE(I,J),I=0,MAXCLS),J=1,15) /
*char   ?     "  #  %  '  ( +-  . 09  : ed az  [  ]  {  }
     1  2, 1, 3, 5, 6, 7, 9,10,20,24,16,25,25, 0, 0,29, 0,
     2  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     3  3, 3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     4  0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     5  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     6  0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 6, 6, 0, 0, 0, 0,
     7  7, 7, 7, 7, 7, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     8  0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     9  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     A  0, 0, 0, 0, 0, 0, 0, 0, 0,11, 0, 0, 0, 0, 0, 0, 0,
     B  0, 0, 0, 0, 0, 0, 0, 0,12,11,16,13, 0, 0, 0, 0, 0,
     C  0, 0, 0, 0, 0, 0, 0, 0, 0,12, 0,13, 0, 0, 0, 0, 0,
     D  0, 0, 0, 0, 0, 0, 0,14, 0,15, 0, 0, 0, 0, 0, 0, 0,
     E  0, 0, 0, 0, 0, 0, 0, 0, 0,15, 0, 0, 0, 0, 0, 0, 0,
     F  0, 0, 0, 0, 0, 0, 0, 0, 0,15, 0, 0, 0, 0, 0, 0, 0/
      DATA ((CTABLE(I,J),I=0,MAXCLS),J=16,31) /
*char   ?     "  #  %  '  ( +-  . 09  : ed az  [  ]  {  }
     6  0, 0, 0, 0, 0, 0, 0, 0,17,16,18, 0, 0, 0, 0, 0, 0,
     7  0, 0, 0, 0, 0, 0, 0, 0, 0,17, 0, 0, 0, 0, 0, 0, 0,
     8  0, 0, 0, 0, 0, 0, 0, 0,19,18, 0, 0, 0, 0, 0, 0, 0,
     9  0, 0, 0, 0, 0, 0, 0, 0, 0,19, 0, 0, 0, 0, 0, 0, 0,
     A  0, 0, 0, 0, 0, 0, 0, 0, 0,21, 0,22,22, 0, 0, 0, 0,
     1  0, 0, 0, 0, 0, 0, 0, 0, 0,21, 0,13, 0, 0, 0, 0, 0,
     2  0, 0, 0, 0, 0, 0, 0, 0,23, 0, 0,22,22, 0, 0, 0, 0,
     3  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     4  0, 0, 0, 0, 0, 0, 0, 0,12,24,16,13, 0, 0, 0, 0, 0,
     5  0, 0, 0, 0, 0, 0, 0, 0, 0,25, 0,25,25,26, 0, 0, 0,
     6  0, 0, 0, 0, 0, 0, 0, 0, 0,27, 0, 0, 0, 0, 0, 0, 0,
     7  0, 0, 0, 0, 0, 0, 0, 0, 0,27, 0, 0, 0, 0,28, 0, 0,
     8  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     9 30,30, 0, 0, 0, 0, 0,30,30,30,30,30,30, 0, 0, 0, 0,
     A 30,30, 0, 0, 0, 0, 0,30,30,30,30,30,30, 0, 0, 0,31,
     1  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
*
      CALL ANT_XSCANE(MAXCLS, MAXTYP, CCLASS, CTABLE, EXPRN, NEXT,
     $               ITEM, ITYPE)
      END

*+ANT_XSCANE  Scans expression for next item of specified type.
      SUBROUTINE ANT_XSCANE(MAXCLS, MAXSTA, CLASS, TABLE, EXPRN, NEXT,
     $  ITEM, ITYPE)
      INTEGER MAXCLS, MAXSTA, CLASS(0:255), TABLE(0:MAXCLS,MAXSTA),
     $        NEXT, ITYPE
      CHARACTER*(*) EXPRN, ITEM
*MAXCLS  input  Maximum character class, range 0 to MAXCLS.
*MAXSTA  input  Number of states.
*CLASS   input  Character class for each value of ICHAR in char set.
*TABLE   input  State table, newstate=table(class,oldstate)
*EXPRN   input  Text string containing expression.
*NEXT    in/out Enters pointing to first character to check,
*               returns pointing to first char after valid item.
*ITEM    Output Next lexical item in the expression.
*ITYPE   output Final state, i.e. type of lexical item found.
*-
*local storage
      INTEGER KPOS, ICLASS, NTYPE
*
      ITYPE = 1
*set KPOS to point to first non-space
*look up the class of the next character (as long as NEXT not off end)
100   CONTINUE
      IF(ITYPE .EQ. 1) KPOS = NEXT
      IF(NEXT .LE. LEN(EXPRN)) THEN
         ICLASS = CLASS(ICHAR(EXPRN(NEXT:NEXT)))
*Get newtype by looking up the state table as fn of (class,current-type)
*If the new type is zero then exit at once, else go around again
         NTYPE = TABLE(ICLASS,ITYPE)
         IF(NTYPE .GT. 0) THEN
            NEXT  = NEXT + 1
            ITYPE = NTYPE
            GO TO 100
         END IF
      END IF
      IF(NEXT .GT. KPOS) THEN
         ITEM = EXPRN(KPOS:NEXT-1)
      ELSE
         ITEM = ' '
      END IF
      END

*+ANT_XINIT  Initialises expression system.
      SUBROUTINE ANT_XINIT(STATUS)
      INTEGER STATUS
*STATUS  in/out  Inherited status.
      INCLUDE 'antx2.inc'
*-
      IF(STATUS .NE. 0) RETURN
*Set number of expression identifiers to zero, and number of constants
* of all three kinds
      NXID = 0
      NCCHAR  = 0
      NCDBLE  = 0
      NCLOG  = 0
***      print *,'xinit called'
      END

*+ANT_XSTORC  Stores character constant
      SUBROUTINE ANT_XSTORC(STRING, ID, STATUS)
      CHARACTER*(*) STRING
      INTEGER ID, STATUS
*STRING  input  Value to store
*ID      output Index to constant store.
*STATUS  in/out Inherited status.
      INCLUDE 'antx2.inc'
*-
      IF(STATUS .NE. 0) RETURN
      IF(NCCHAR .LT. MAXCON) THEN
         NCCHAR = NCCHAR + 1
         CCONST(NCCHAR) = STRING
         LENCC(NCCHAR) = MIN(LEN(STRING), LEN(CCONST(1)))
         ID     = NCCHAR
***         print *,'Stored ', STRING, ' as Cconst', ID, ' length',
***     $        LENCC(NCCHAR)
      ELSE
         CALL ANT_ERRC ('ANT_XSTORC: too many constants to store',
     $     STRING)
C          WRITE(*,*)'!ANT_XSTORC: too many constants to store ', STRING
         STATUS = -9
      END IF
      END

*+ANT_XSTORD  Stores double precision constant
      SUBROUTINE ANT_XSTORD(VALUE, ID, STATUS)
      DOUBLE PRECISION VALUE
      INTEGER ID, STATUS
*VALUE   input  Value to store
*ID      output Index to constant store.
*STATUS  in/out Inherited status.
      INCLUDE 'antx2.inc'
*-
      IF(STATUS .NE. 0) RETURN
      IF(NCDBLE .LT. MAXCON) THEN
         NCDBLE = NCDBLE + 1
         DCONST(NCDBLE) = VALUE
         ID     = NCDBLE
***         print *,'Stored ', VALUE, ' as Dconst', ID
      ELSE
         CALL ANT_ERRD ('ANT_XSTORD: too many constants to store,',
     $     VALUE)
C        WRITE(*,*)'!ANT_XSTORD: too many constants to store', VALUE
         STATUS = -9
      END IF
      END

*+ANT_XSTORL  Stores logical constant
      SUBROUTINE ANT_XSTORL(VALUE, ID, STATUS)
      LOGICAL VALUE
      INTEGER ID, STATUS
*VALUE   input  Value to store
*ID      output Index to constant store.
*STATUS  in/out Inherited status.
      INCLUDE 'antx2.inc'
*-
      IF(STATUS .NE. 0) RETURN
      IF(NCLOG .LT. MAXCON) THEN
         NCLOG = NCLOG + 1
         IF(VALUE) THEN
            LCONST(NCLOG) = +1
         ELSE
            LCONST(NCLOG) = -1
         END IF
         ID     = NCLOG
***         print *,'Stored ', VALUE, ' as Lconst', ID
      ELSE
         CALL ANT_ERR ('ANT_XSTORL: too many constants to store,')
C        WRITE(*,*)'!ANT_XSTORL: too many constants to store', VALUE
         STATUS = -9
      END IF
      END

*+ANT_XPUTOP  Appends op-code to specified expression opcode list.
      SUBROUTINE ANT_XPUTOP(XID, CODE, STATUS)
      INTEGER XID, CODE, STATUS
*XID    input  Expression identifier.
*CODE   input  Operation code to be appended.
*STATUS in/out Inherited status.
      INCLUDE 'antx2.inc'
      CHARACTER ERRBUF*10
      INTEGER LSTAT
*-
      IF(STATUS .NE. 0) RETURN
      IF(XID .GE. 1 .AND. XID .LE. MAXXID) THEN
         IF(NOPC(XID) .GE. 0 .AND. NOPC(XID) .LT. MAXOPC) THEN
            NOPC(XID) = NOPC(XID) + 1
            OPLIST(NOPC(XID),XID) = CODE
         ELSE
            ERRBUF = ' '
            WRITE(ERRBUF, '(I5, I5)', IOSTAT=LSTAT) XID, CODE
            CALL ANT_ERRC ('ANT_XPUTOP: too many opcodes,', ERRBUF)
C           WRITE(*,*)'!ANT_XPUTOP: too many opcodes', XID, CODE
            STATUS = -9
         END IF
      ELSE
         CALL ANT_ERRI ('ANT_XPUTOP: invalid expression id,', XID)
C        WRITE(*,*)'!ANT_XPUTOP: invalid expression id', XID
         STATUS = -9
      END IF
      END

*+ANT_XPUSHT  Pushes data-type code on type stack
      SUBROUTINE ANT_XPUSHT(DTYPE, STATUS)
      INTEGER DTYPE, STATUS
*DTYPE  input  Data type code.
*STATUS in/out Inherited status.
      INCLUDE 'antx1.inc'
*-
      IF(STATUS .NE. 0) RETURN
      IF(NTS .LT. MAXST) THEN
         NTS = NTS + 1
         TSTACK(NTS) = DTYPE
      ELSE
         CALL ANT_ERR ('ANT_XPUSHT: type stack overflow.')
C        WRITE(*,*)'!ANT_XPUSHT: type stack overflow'
         STATUS = -9
      END IF
      END

*+ANT_XFIND  Looks up operator/function, returns opcode.
      SUBROUTINE ANT_XFIND(NAME, CODE, STATUS)
      CHARACTER NAME*(*)
      INTEGER CODE, STATUS
      INCLUDE 'antx1.inc'
*-
      INTEGER J, K
      CHARACTER UNAME*6
*
      IF(STATUS .NE. 0) RETURN
*If name starts with a dot remove it
      IF(NAME(1:1) .EQ. '.') THEN
         UNAME = NAME(2:)
*in which case expect a trailing dot, remove it also
         K = INDEX(UNAME, '.')
         IF(K .GT. 0) UNAME(K:) = ' '
      ELSE
         UNAME = NAME
      END IF
*Convert to upper-case
      CALL ANT_SUPPER(UNAME)
*Search the list starting at 30 (lower values unused/not needed)
      DO J = 1,MAXFUN
         IF(FNAME(J) .EQ. UNAME) THEN
            CODE = J
            GO TO 999
         END IF
      END DO
      CALL ANT_ERRC ('ANT_XFIND: unknown operator/function,', NAME)
C     WRITE(*,*)'!ANT_XFIND: unknown operator/function ', NAME
      STATUS = -9
      CODE   = 0
999   CONTINUE
      END

*+ANT_XEVALC  Evaluates expression, returning character value from stack.
      SUBROUTINE ANT_XEVALC(XID, NROW, NULL, VALUE, STATUS)
      INTEGER XID, NROW, STATUS
      LOGICAL NULL
      CHARACTER*(*) VALUE
*XID     input  Expression identifier, from prior call to ANT_XCOMP
*NROW    input  Row number - passed down to ANT_FGETx.
*NULL    output .true. if undefined result.
*VALUE   output Value from stack.
*STATUS  in/out Inherited status
      INCLUDE 'antx3.inc'
*-Author   Clive Page   1993-JULY-27
      IF(STATUS .NE. 0) RETURN
      CALL ANT_XEVAL(XID, NROW, STATUS)
      IF(NS .NE. 1) THEN
         CALL ANT_ERRI ('ANT_XEVALC: warning - NS =', NS)
C        WRITE(*,*)'!ANT_XEVALC: warning - NS=', NS
      END IF
      IF(NSTACK(1)) THEN
         NULL = .TRUE.
      ELSE
         VALUE = CSTACK(1)(1:ISTACK(1))
         NULL  = .FALSE.
      END IF
      END

*+ANT_XEVALD  Evaluates expression, returning DP value from stack.
      SUBROUTINE ANT_XEVALD(XID, NROW, NULL, VALUE, STATUS)
      INTEGER XID, NROW, STATUS
      LOGICAL NULL
      DOUBLE PRECISION VALUE
*XID     input  Expression identifier, from prior call to ANT_XCOMP
*NROW    input  Row number - passed down to ANT_FGETx.
*NULL    output .true. if undefined result.
*VALUE   output Value from stack.
*STATUS  in/out Inherited status
      INCLUDE 'antx3.inc'
*-Author   Clive Page   1993-JULY-27
      IF(STATUS .NE. 0) RETURN
      CALL ANT_XEVAL(XID, NROW, STATUS)
      IF(NS .NE. 1) THEN
         CALL ANT_ERRI ('ANT_XEVALD: warning - NS =', NS)
C        WRITE(*,*)'!ANT_XEVALD: warning - NS=', NS
      END IF
      IF(NSTACK(1)) THEN
         NULL = .TRUE.
      ELSE
         VALUE = DSTACK(1)
         NULL  = .FALSE.
      END IF
      END

*+ANT_XEVALL  Evaluates expression, returning logical value from stack.
      SUBROUTINE ANT_XEVALL(XID, NROW, NULL, VALUE, STATUS)
      INTEGER XID, NROW, STATUS
      LOGICAL NULL
      LOGICAL VALUE
*XID     input  Expression identifier, from prior call to ANT_XCOMP
*NROW    input  Row number - passed down to ANT_FGETx.
*NULL    output .true. if undefined result.
*VALUE   output Value from stack.
*STATUS  in/out Inherited status
      INCLUDE 'antx3.inc'
*-Author   Clive Page   1993-JULY-27
*
* Modification history:
*    Fixed bug in which the null value flag was not returned if it
*    was false (that is, the expression is non-null).
*                                                        ACD 1997-NOV-16
      IF(STATUS .NE. 0) RETURN
      CALL ANT_XEVAL(XID, NROW, STATUS)
      IF(NS .NE. 1) THEN
         CALL ANT_ERRI ('ANT_XEVALL warning - NS =', NS)
C        WRITE(*,*)'!ANT_XEVALL warning - NS=', NS
      END IF
      IF(NSTACK(1)) THEN
         NULL = .TRUE.
         VALUE = .FALSE.
      ELSE
         NULL  = .FALSE.
         VALUE = LSTACK(1) .EQ. 1
      END IF
      END

*+ANT_XEVAL  Evaluates expression, result left on stack
      SUBROUTINE ANT_XEVAL(XID, NROW, STATUS)
      INTEGER XID, NROW, STATUS
*XID     input  Expression identifier, from prior call to ANT_XCOMP
*NROW    input  Row number - passed down to ANT_FGETx.
*STATUS  in/out Inherited status
      INCLUDE 'antx2.inc'
      INCLUDE 'antx3.inc'
*-Author   Clive Page   1993-JULY-27
*-Modified Clive Davenhall 1995-JAN-26, 1995-JAN-30 (Added function GREAT).
*-   "     Clive Davenhall 1996-AUG-15  (added function SCALE).
*-   "     Clive Davenhall 2000-JUN-13  (Fixed bug in function ATAN2 (the
*                                       stack was not being decremented)
*                                       and added function PANGLE).
*   Added Great Circle Distance function.
      DOUBLE PRECISION PI, DTOR, RTOD, HTOR
      PARAMETER (PI = 3.14159265358979D0, DTOR = PI/180.0D0,
     $  RTOD = 180.0D0/PI, HTOR = PI/12.0D0)
      DOUBLE PRECISION MINVAL
      PARAMETER (MINVAL = 1.0D-8)
      INTEGER ID, J, K
      CHARACTER CTEMP*(MAXLC)
      LOGICAL LVALUE, NEG
      DOUBLE PRECISION W1, W2, W3
      EXTERNAL ANT_SLEN
      INTEGER ANT_SLEN, IAND, IOR, IEOR
      DOUBLE PRECISION SLA_DSEP
*3-valued logic functions (-1=false, 0=unknown, +1=true).
      INTEGER LOGEQV(-1:1,-1:1), LOGXOR(-1:1,-1:1), LOGOR(-1:1,-1:1),
     $   LOGAND(-1:1,-1:1),  LOGNOT(-1:1)
      DATA LOGEQV / 1, 0,-1,   0, 0, 0,  -1, 0, 1 /
      DATA LOGXOR /-1, 0, 1,   0, 0, 0,   1, 0,-1 /
      DATA LOGOR  /-1, 0, 1,   0, 0, 1,   1, 1, 1 /
      DATA LOGAND /-1,-1,-1,  -1, 0, 0,  -1, 0, 1 /
      DATA LOGNOT / 1, 0,-1 /
*
      IF(STATUS .NE. 0) RETURN
*Initialise stack pointer at zero
      NS = 0
      ID = 0
1000  CONTINUE
      ID = ID + 1
      IF(ID .GT. NOPC(XID)) GO TO 999
      GO TO(
     $    1, 2, 3, 4, 1, 1, 1, 1, 9,10,11, 1, 1, 1, 1, 1, 1, 1,19,20,
     $   21, 1, 1, 1, 1, 1, 1, 1, 1,30,31,32,33,34,35,36,37,38,39,40,
     $   41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     $   61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     $   81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96),
     $     OPLIST(ID,XID)
*Invalid opcodes here
1     CONTINUE
      CALL ANT_ERRI ('ANT_XEVAL: invalid opcode', OPLIST(ID,XID) )
C     WRITE(*,*)'!ANT_XEVAL: invalid opcode', OPLIST(ID,XID)
      STATUS = -9
      GO TO 999
*LOAD character constant
2     CONTINUE
      NS = NS + 1
      ID = ID + 1
*  integer stack stores length
      ISTACK(NS) = MIN(MAXLC, LENCC(OPLIST(ID,XID)))
      CSTACK(NS)(1:ISTACK(NS)) = CCONST(OPLIST(ID,XID))(1:ISTACK(NS))
      NSTACK(NS) = .FALSE.
      GO TO 1000
*LOAD logical constant from LCONST (value range  -1/0/+1)
3     CONTINUE
      NS = NS + 1
      ID = ID + 1
      LSTACK(NS) = LCONST(OPLIST(ID,XID))
      NSTACK(NS) = LSTACK(NS) .EQ. 0
      GO TO 1000
*LOAD numerical constant
4     CONTINUE
      NS = NS + 1
      ID = ID + 1
      DSTACK(NS) = DCONST(OPLIST(ID,XID))
      NSTACK(NS) = .FALSE.
      GO TO 1000
*LOAD character variable
9     CONTINUE
      NS = NS + 1
      ID = ID + 1
      CALL ANT_FGETC(OPLIST(ID,XID), NROW, NSTACK(NS),
     $               CSTACK(NS), STATUS)
*Set length to that declared for the variable, need explicit TRIM
* function to trim off trailing spaces.
*** needs improvement here***
      ISTACK(NS) = LEN(CSTACK(NS))
      GO TO 1000
*LOAD logical variable: convert to 3-valued integer in LSTACK
10    CONTINUE
      NS = NS + 1
      ID = ID + 1
      CALL ANT_FGETL(OPLIST(ID,XID), NROW, NSTACK(NS), LVALUE, STATUS)
      IF(NSTACK(NS)) THEN
         LSTACK(NS) = 0
      ELSE IF(LVALUE) THEN
         LSTACK(NS) = +1
      ELSE
         LSTACK(NS) = -1
      END IF
      GO TO 1000
*LOAD byte/half/int/real/double variable
11    CONTINUE
      NS = NS + 1
      ID = ID + 1
      CALL ANT_FGETD(OPLIST(ID,XID), NROW, NSTACK(NS), DSTACK(NS),
     $                STATUS)
      GO TO 1000
*store functions not yet implemented
19    CONTINUE
20    CONTINUE
21    CONTINUE
      GO TO 1
*Logical operators all work on 3-valued integers (1=false,2=unknown,3=true).
* operators evaluated by look-up on 3x3 table.
* .EQV.
30    CONTINUE
      NS = NS - 1
      LSTACK(NS) = LOGEQV(LSTACK(NS),LSTACK(NS+1))
      GO TO 1000
* .NEQV. logical
31    CONTINUE
      NS = NS - 1
      LSTACK(NS) = LOGXOR(LSTACK(NS),LSTACK(NS+1))
      GO TO 1000
* .OR.  logical
32    CONTINUE
33    CONTINUE
      NS = NS - 1
      LSTACK(NS) = LOGOR(LSTACK(NS),LSTACK(NS+1))
      GO TO 1000
* .AND. logical
34    CONTINUE
35    CONTINUE
      NS = NS - 1
      LSTACK(NS) = LOGAND(LSTACK(NS),LSTACK(NS+1))
      GO TO 1000
* .NOT. or \  logical negation
36    CONTINUE
37    CONTINUE
      LSTACK(NS) = LOGNOT(LSTACK(NS))
      GO TO 1000
*Relational operators: need to test for either arg null.
* == char
38    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .EQ.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* == numeric
39    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .EQ. DSTACK(NS+1)
      GO TO 800
* >= char
40    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .GE.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* >= numeric
41    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .GE. DSTACK(NS+1)
      GO TO 800
* > char
42    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .GT.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* > numeric
43    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .GT. DSTACK(NS+1)
      GO TO 800
* <= char
44    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .LE.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* <= numeric
45    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .LE. DSTACK(NS+1)
      GO TO 800
* < char
46    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .LT.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* < numeric
47    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .LT. DSTACK(NS+1)
      GO TO 800
* /= (.NE.) char
48    CONTINUE
      NS = NS - 1
      LVALUE = CSTACK(NS)(1:ISTACK(NS)) .NE.
     $         CSTACK(NS+1)(1:ISTACK(NS+1))
      GO TO 800
* /= (.NE.) numeric
49    CONTINUE
      NS = NS - 1
      LVALUE = DSTACK(NS) .NE. DSTACK(NS+1)
      GO TO 800
*
* LIKE (not yet implementd)
50    CONTINUE
      GO TO 38
51    CONTINUE
      GO TO 39
* FROM str1 TO str2  (character string comparisons)
52    CONTINUE
      NS = NS - 2
*   Test all three arguments for null-ness
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. NSTACK(NS+2)) THEN
         LSTACK(NS) = 0
      ELSE IF(CSTACK(NS)(1:ISTACK(NS)) .GE.
     $        CSTACK(NS+1)(1:ISTACK(NS+1)) .AND.
     $        CSTACK(NS)(1:ISTACK(NS)) .LE.
     $        CSTACK(NS+2)(1:ISTACK(NS+2))) THEN
         LSTACK(NS) = +1
      ELSE
         LSTACK(NS) = -1
      END IF
      GO TO 1000
* FROM num1 to num2  (numerical comparisons)
53    CONTINUE
      NS = NS - 2
*   Test all three arguments for null-ness
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. NSTACK(NS+2)) THEN
         LSTACK(NS) = 0
      ELSE IF(DSTACK(NS) .GE. DSTACK(NS+1) .AND.
     $        DSTACK(NS) .LE. DSTACK(NS+2)) THEN
         LSTACK(NS) = +1
      ELSE
         LSTACK(NS) = -1
      END IF
      GO TO 1000
* // character concatenation
54    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         CTEMP = CSTACK(NS)(1:ISTACK(NS)) //
     $           CSTACK(NS+1)(1:ISTACK(NS+1))
         ISTACK(NS) = MIN(MAXLC, ISTACK(NS) + ISTACK(NS+1))
         CSTACK(NS) = CTEMP
      END IF
      GO TO 1000
* + numeric
55    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = DSTACK(NS) + DSTACK(NS+1)
      END IF
      GO TO 1000
* - numeric
56    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = DSTACK(NS) - DSTACK(NS+1)
      END IF
      GO TO 1000
* UNARY- i.e. negate numeric - if value is null then this just stays on stack
57    CONTINUE
      DSTACK(NS) = -DSTACK(NS)
      GO TO 1000
* multiply numeric
58    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = DSTACK(NS) * DSTACK(NS+1)
      END IF
      GO TO 1000
* / numeric
59    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. DSTACK(NS+1) .EQ. 0.0D0) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = DSTACK(NS) / DSTACK(NS+1)
      END IF
      GO TO 1000
*  power D**D numeric
*** use ABS to avoid log of -ve number.
60    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = ABS(DSTACK(NS)) ** DSTACK(NS+1)
      END IF
      GO TO 1000
* BYTE(numeric) - set null flag if value exceeds byte range (0-255)
61    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. DSTACK(NS) .LE. 0.0D0 .OR.
     $                             DSTACK(NS) .GT. 255.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = AINT(DSTACK(NS))
      GO TO 1000
* INT2(numeric) - set null flag if value exceeds halfward range
62    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GT. 32767.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = AINT(DSTACK(NS))
      GO TO 1000
* INT(numeric) - set null flag if value exceeds integer range
63    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GE. 2147483647.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = AINT(DSTACK(NS))
      GO TO 1000
* NINT(numeric) - round to nearest integer, set NULL if outside integer range
64    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GE. 2147483647.5D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = ANINT(DSTACK(NS))
      GO TO 1000
* REAL(numeric) - no operation needed.
65    CONTINUE
      GO TO 1000
* DBLE(numeric) - no operation needed as all internal works are in D.P.
66    CONTINUE
      GO TO 1000
* MIN(numeric,numeric)
67    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = MIN(DSTACK(NS), DSTACK(NS+1))
      END IF
      GO TO 1000
* MAX(numeric,numeric)
68    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = MAX(DSTACK(NS), DSTACK(NS+1))
      END IF
      GO TO 1000
* MOD(numeric,numeric)
69    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. DSTACK(NS+1) .EQ. 0.0D0)THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = MOD(DSTACK(NS), DSTACK(NS+1))
      END IF
      GO TO 1000
* ABS(numeric)
70    CONTINUE
      DSTACK(NS) = ABS(DSTACK(NS))
      GO TO 1000
* SQRT(numeric)
71    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. DSTACK(NS) .LT. 0.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = SQRT(DSTACK(NS))
      GO TO 1000
* LOG(numeric)
72    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. DSTACK(NS) .LE. 0.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = LOG(DSTACK(NS))
      GO TO 1000
* LOG10(numeric)
73    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. DSTACK(NS) .LE. 0.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = LOG10(DSTACK(NS))
      GO TO 1000
* EXP(numeric)
74    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GT. 87.5D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = EXP(DSTACK(NS))
      GO TO 1000
* SIN(numeric)
75    CONTINUE
      DSTACK(NS) = SIN(DSTACK(NS))
      GO TO 1000
* COS(numeric)
76    CONTINUE
      DSTACK(NS) = COS(DSTACK(NS))
      GO TO 1000
* TAN(numeric)
77    CONTINUE
      DSTACK(NS) = TAN(DSTACK(NS))
      GO TO 1000
* ASIN(numeric)
78    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GT. 1.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = ASIN(DSTACK(NS))
      GO TO 1000
* ACOS(numeric)
79    CONTINUE
      NSTACK(NS) = NSTACK(NS) .OR. ABS(DSTACK(NS)) .GT. 1.0D0
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = ACOS(DSTACK(NS))
      GO TO 1000
* ATAN(numeric)
80    CONTINUE
      DSTACK(NS) = ATAN(DSTACK(NS))
      GO TO 1000
* ATAN2(numeric,numeric)
81    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. (DSTACK(NS+1) .EQ. 0.0D0
     $   .AND. DSTACK(NS+1) .EQ. 0.0D0)) THEN
         NSTACK(NS) = .TRUE.
      END IF
      IF(.NOT. NSTACK(NS)) DSTACK(NS) = ATAN2(DSTACK(NS),DSTACK(NS+1))
      GO TO 1000
* IAND(int,int)
82    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = IAND(NINT(DSTACK(NS)), NINT(DSTACK(NS+1)))
      END IF
      GO TO 1000
* IOR(int,int)
83    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = IOR(NINT(DSTACK(NS)), NINT(DSTACK(NS+1)))
      END IF
      GO TO 1000
* IEOR(int,int)
84    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = IEOR(NINT(DSTACK(NS)), NINT(DSTACK(NS+1)))
      END IF
      GO TO 1000
* DTOR(numeric)
85    CONTINUE
      DSTACK(NS) = DSTACK(NS) * DTOR
      GO TO 1000
* RTOD(numeric)
86    CONTINUE
      DSTACK(NS) = DSTACK(NS) * RTOD
      GO TO 1000
* UPCASE(string)
87    CONTINUE
      CALL ANT_SUPPER(CSTACK(NS)(1:ISTACK(NS)))
      GO TO 1000
* STRIP(char)
88    CONTINUE
      CALL ANT_STRIPS(CSTACK(NS)(1:ISTACK(NS)), CTEMP, J)
      CSTACK(NS) = CTEMP
      ISTACK(NS) = J
      GO TO 1000
* SUBSTR(char,int,int)
89    CONTINUE
      NS = NS - 2
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. NSTACK(NS+2)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         J = NINT(DSTACK(NS+1))
         K = NINT(DSTACK(NS+2))
*Ensure that 1  <=  J  <=  K  <=  MAXLC, i.e. resulting length at least one
         K = MAX(1,MIN(K,MAXLC))
         J = MAX(1,MIN(J,K))
         CSTACK(NS) = CSTACK(NS)(J:K)
         ISTACK(NS) = K - J + 1
      END IF
      GO TO 1000
* SCAN(string,substr)
90    CONTINUE
      NS = NS - 1
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         LSTACK(NS) = 0
      ELSE IF(INDEX(CSTACK(NS)(1:ISTACK(NS)),
     $              CSTACK(NS+1)(1:ISTACK(NS+1))) .NE. 0) THEN
         LSTACK(NS) = +1
      ELSE
         LSTACK(NS) = -1
      END IF
      GO TO 1000
* NULL(anytype)  returns .true. if null value, else .false.
91    CONTINUE
      IF(NSTACK(NS)) THEN
         LSTACK(NS) = +1
      ELSE
         LSTACK(NS) = -1
      END IF
      NSTACK(NS) = .FALSE.
      GO TO 1000
* HMSRAD(num,num,num)  hours-mins-secs to angle in radians
92    CONTINUE
      NS = NS - 2
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. NSTACK(NS+2)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
* check for a sign on any component, if so negate result
         NEG = DSTACK(NS) .LT. 0.0D0 .OR. DSTACK(NS+1) .LT. 0.0D0
     $    .OR. DSTACK(NS+2) .LT. 0.0D0
         DSTACK(NS) = (ABS(DSTACK(NS)) + ABS(DSTACK(NS+1))/60.0D0 +
     $                 ABS(DSTACK(NS+2))/3600.0D0) * HTOR
         IF(NEG) DSTACK(NS) = -DSTACK(NS)
      END IF
      GO TO 1000
* DMSRAD(str,num,num,num)  sign-degs-mins-secs to angle in radians
93    CONTINUE
      NS = NS - 3
      IF(NSTACK(NS) .OR. NSTACK(NS+1) .OR. NSTACK(NS+2) .OR.
     $   NSTACK(NS+3)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
* check for a sign on any component or '-' at any point in the SIGN
         NEG = DSTACK(NS+1) .LT. 0.0D0 .OR. DSTACK(NS+2) .LT. 0.0D0
     $    .OR. DSTACK(NS+3) .LT. 0.0D0 .OR.
     $         (INDEX(CSTACK(NS)(1:ISTACK(NS)),'-') .NE. 0)
         DSTACK(NS) = (ABS(DSTACK(NS+1)) + ABS(DSTACK(NS+2))/60.0D0 +
     $                 ABS(DSTACK(NS+3))/3600.0D0) * DTOR
         IF(NEG) DSTACK(NS) = -DSTACK(NS)
      END IF
      GO TO 1000
* GREAT(num,num,num,num)  Great Circle Distance function.
94    CONTINUE
*
*    Arguments:
*     1  -  Right Ascension of first  position (radians).
*     2  -  Declination     "    "       "     (   "   ).
*     3  -  Right Ascension of second position (radians).
*     4  -  Declination     "    "       "     (   "   ).
*
*    If (a1,d1) and (a2,d2) are the coordinates of the two positions
*    then the Great Circle distance, D, between them is:
*
*    D = acos( abs( sin(d1)*sin(d2)  +  cos(a1-a2)*cos(d1)*cos(d2) ) )
*

*    Decrement the stack pointer.
      NS = NS - 3

*    Check whether any of the arguments are null.  If so then set the
*    result to null.  Otherwise compute the Great Circle distance.
*
*    Note that the great circle distance is computed using the
*    appropriate SLA routine, rather than a naive application of
*    the great circle distance formula because the former uses a
*    vector formulation which gives better accuracy for small angles
*    (and the function will often be used for small angles, of course).
*
*    If the simple formula were to be used it would be coded as
*    follows:
*
*        DSTACK(NS) = ACOS( ABS(
*    $     (SIN(DSTACK(NS+1)) * SIN(DSTACK(NS+3)) ) +
*    $     (COS(DSTACK(NS+2)-DSTACK(NS)) * COS(DSTACK(NS+1)) *
*    $     COS(DSTACK(NS+3)) )  ))

      IF(NSTACK(NS)  .OR.  NSTACK(NS+1)  .OR.  NSTACK(NS+2)  .OR.
     $  NSTACK(NS+3)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = SLA_DSEP(DSTACK(NS), DSTACK(NS+1), DSTACK(NS+2),
     $     DSTACK(NS+3) )
      END IF
      GO TO 1000
* SCALE(num,num,num,num,num)  Scale value to given range.
95    CONTINUE
*
*    Arguments:
*     1  -  value to be scaled,              v,
*     2  -  minimum of range to be scaled,   vmin,
*     3  -  maximum "    "   "  "    "   ,   vmax,
*     4  -  minimum scaled value,            smin,
*     5  -  maximum scaled value,            xmax.
*
*    The scaled value is computed as follows:
*
*    * If v is less than vmin it is set to smin.
*    * If v is greater than vmax it is set to smax.
*    * If v is within the range vmin to vmax it is scaled linearly
*      using usual the formula:
*                                            (smax - smin)
*      scaled value, s = smin  +  (v - vmin).-------------
*                                            (vmax - vmin)
*
*    * If any of the input values are null then the result is null.

*     Decrement the stack pointer.
       NS = NS - 4

*     Check for input null values.
      IF(NSTACK(NS)  .OR.  NSTACK(NS+1)  .OR.  NSTACK(NS+2)  .OR.
     $  NSTACK(NS+3)  .OR.  NSTACK(NS+4)) THEN
         NSTACK(NS) = .TRUE.
      ELSE

*       Compute the value.
         IF (DSTACK(NS) .LE. DSTACK(NS+1)) THEN
            DSTACK(NS) = DSTACK(NS+3)
         ELSE IF (DSTACK(NS) .GE. DSTACK(NS+2)) THEN
            DSTACK(NS) = DSTACK(NS+4)
         ELSE
            W1 = DSTACK(NS) - DSTACK(NS+1)
            W2 = DSTACK(NS+4) - DSTACK(NS+3)
            W3 = DSTACK(NS+2) - DSTACK(NS+1)

            IF (ABS(W1) .GT. MINVAL  .AND.  ABS(W2) .GT. MINVAL  .AND.
     $          ABS(W3) .GT. MINVAL) THEN
               DSTACK(NS) = DSTACK(NS+3) + (W1*W2/W3)
            ELSE
               NSTACK(NS) = .TRUE.
            END IF
         END IF
      END IF
      GO TO 1000
* PANGLE(num,num,num,num)  Position Angle function.
96    CONTINUE
*
*    Arguments:
*     1  -  Right Ascension of first  position (radians).
*     2  -  Declination     "    "       "     (   "   ).
*     3  -  Right Ascension of second position (radians).
*     4  -  Declination     "    "       "     (   "   ).
*
*    Computes the position angle of point (a2,d2) from point (a1,d1).
*    The result is returned in radians.
*
*    The position angle is computed using SLA routine SLA_DBEAR.  See
*    the documentation of this routine in SUN/67 for a description
*    of the sign convention of the result returned.

*    Decrement the stack pointer.
      NS = NS - 3

*    Check whether any of the arguments are null.  If so then set the
*    result to null.  Otherwise compute the Position Angle.
      IF(NSTACK(NS)  .OR.  NSTACK(NS+1)  .OR.  NSTACK(NS+2)  .OR.
     $  NSTACK(NS+3)) THEN
         NSTACK(NS) = .TRUE.
      ELSE
         DSTACK(NS) = SLA_DBEAR(DSTACK(NS), DSTACK(NS+1), DSTACK(NS+2),
     $     DSTACK(NS+3) )
      END IF
      GO TO 1000

*----------------- add new functions here -----------
*Handle null values from relational operators here
*If either operand is null return null (0), else true (+1) or false (-1).
800   CONTINUE
      IF(NSTACK(NS) .OR. NSTACK(NS+1)) THEN
         LSTACK(NS) = 0
      ELSE IF(LVALUE) THEN
         LSTACK(NS) = 1
      ELSE
         LSTACK(NS) = -1
      END IF
      GO TO 1000
*End of OPLIST list here
999   CONTINUE
      END
