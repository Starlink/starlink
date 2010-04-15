C+
      SUBROUTINE DTA_CRNAM (ENAME,ONAME,NDIM,DIMS,NAME,STATUS)
C
C     D T A _ C R N A M
C
C     Creates an item name from an environment name, an object
C     name, and a dimension specification.  The name generated
C     has the general form ename.oname[dim1,dim2..]
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ENAME      (Character) The name of the environment.
C                    Delimited either by the end of the string
C                    or by the first blank encountered.
C     (>) ONAME      (Character) The name of the object. Also
C                    delimited by the end of the string or the
C                    first blank.
C     (>) NDIM       (Integer) The number of dimensions of the
C                    item.  If scalar, should be passed as 0,
C                    and no [..] part of the name will be
C                    generated.
C     (>) DIMS       (Integer array) Gives the dimensions of the
C                    item.
C     (<) NAME       (Character) Returns with the generated name
C                    for the item.  If NAME is too short for the
C                    name it will have been truncated; if too long
C                    it will be blank filled. The case of any
C                    characters will be the same as in ENAME and
C                    ONAME - no conversion is performed.
C     (<) STATUS     (Integer) Returns with a status code.
C                    0 => OK  Possible error codes are
C                    DTA_TRUNC  => NAME too short, truncated
C                    DTA_INVDIM => Invalid dimensions specified
C                    DTA_INVNAM => Invalid name - either ENAME
C                                  or ONAME.
C     Example -
C
C     DIMS(1)=62
C     DIMS(2)=1524
C     CALL DTA_CRNAM('OUTPUT.X','DATA',2,DIMS,NAME,STATUS)
C
C     will return NAME = 'OUTPUT.X.DATA[62,1524]'
C-
C     Subroutines / functions used - Only standard Fortran functions
C
C                                           KS / CIT  26th April 1984
C     Modified:
C
C     10th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     20th Jul  1993.  HME / UoE, Starlink.  After setting STATUS=DTA_INVDIM
C                      this routine would proceed by setting it to a
C                      different value which would cause the caller to
C                      give the wrong error message.
C+
      IMPLICIT NONE
C
C     Parameters -
C
C     Note, DIMS would normally be dimensioned (NDIM), but
C     using (1) as a dummy allows for the NDIM=0 case
C
      CHARACTER*(*) ENAME,ONAME,NAME
      INTEGER STATUS,NDIM,DIMS(1)
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Local variables
C
      INTEGER NPTR,IWPT,LIMPTR,I,N
      CHARACTER CHR,WORK*(20)
C
C     Copy ENAME into NAME. Check that it is not just
C     a blank string
C
      LIMPTR=LEN(NAME)
      NPTR=1
      DO I=1,LEN(ENAME)
         CHR=ENAME(I:I)
         IF (CHR.EQ.' ') THEN
            IF (I.GT.1) THEN
               GO TO 320
            ELSE
               GO TO 530
            END IF
         END IF
         IF (NPTR.GT.LIMPTR) GO TO 510
         NAME(NPTR:NPTR)=CHR
         NPTR=NPTR+1
      END DO
  320 CONTINUE
C
C     Copy ONAME into NAME, with a '.' separating it
C     from ENAME.  Check that it is not just a blank string.
C
      IF (NPTR.GT.LIMPTR) GO TO 510
      NAME(NPTR:NPTR)='.'
      NPTR=NPTR+1
      DO I=1,LEN(ONAME)
         CHR=ONAME(I:I)
         IF (CHR.EQ.' ')  THEN
            IF (I.GT.1) THEN
               GO TO 340
            ELSE
               GO TO 530
            END IF
         END IF
         IF (NPTR.GT.LIMPTR)  GO TO 510
         NAME(NPTR:NPTR)=CHR
         NPTR=NPTR+1
      END DO
  340 CONTINUE
C
C     Check for dimensional information.  First check not illegal..
C
      IF (NDIM.LT.0) GO TO 520
      IF (NDIM.EQ.0) THEN
C
C        No dimensional info, set rest of NAME to blanks
C
         IF (NPTR.LT.LIMPTR) THEN
            NAME(NPTR:)=' '
         END IF
      ELSE
C
C        Have dimensional info, format it one dimension at at
C        time into WORK, then copy into NAME.
C        (Probably a moot point whether or not an internal write
C        would be better.)
C
         DO I=1,NDIM
            IF (NPTR.GT.LIMPTR)  GO TO 510
            IF (I.EQ.1) THEN
               NAME(NPTR:NPTR)='['
            ELSE
               NAME(NPTR:NPTR)=','
            END IF
            NPTR=NPTR+1
            N=DIMS(I)
            IF (N.LE.0) GO TO 520
            IWPT=20
            DO WHILE (N.GT.0)
               WORK(IWPT:IWPT)=CHAR(MOD(N,10)+ICHAR('0'))
               IWPT=IWPT-1
               N=N/10
            END DO
            N=NPTR+19-IWPT
            IF (N.GT.LIMPTR)  GO TO 510
            NAME(NPTR:N)=WORK(IWPT+1:)
            NPTR=N+1
         END DO
         IF (NPTR.GT.LIMPTR) GO TO 510
         NAME(NPTR:)=']'
      END IF
C
C     Normal end
C
      STATUS=0
      GO TO 600
C
C     Error conditions.
C
  510 CONTINUE
      STATUS=DTA_TRUNC
      GO TO 600
  520 CONTINUE
      STATUS=DTA_INVDIM
      GO TO 600
  530 CONTINUE
      STATUS=DTA_INVNAM
C
  600 CONTINUE
      END

