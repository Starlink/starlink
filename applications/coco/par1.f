      SUBROUTINE PAR1 (INREC,IPTR,J)
*+
*
*  PAR1: subroutine of COCO utility which finds a single
*        parameter in a string
*
*  Given:
*     INREC     char   string
*     IPTR      int    REC pointer
*
*  Returned:
*     J         int    result:
*                       -1  =  no parameter found
*                        0  =  parameter at REC(IPTR:IPTR)
*                       +1  =  trailing non-spaces
*
*  Called:
*     KTEST
*
*  P T Wallace   Starlink   18 May 1992
*-

      IMPLICIT NONE

      CHARACTER*(*) INREC
      INTEGER IPTR,J

      INTEGER KTEST



*  Reached end of string already?
      IF (IPTR.GE.LEN(INREC)) THEN

*     Yes:  no parameter found
         J=-1
      ELSE

*     No:  advance the pointer and find the next non-space
         IPTR=IPTR+1
         DO WHILE (IPTR.LT.LEN(INREC).AND.
     :             INREC(IPTR:IPTR).EQ.' ')
            IPTR=IPTR+1
         END DO

*     Any parameter?
         IF (INREC(IPTR:IPTR).EQ.' ') THEN

*        No
            J=-1
         ELSE

*        Yes:  any trailing non-spaces?
            IF (KTEST(INREC,IPTR+1).NE.0) THEN

*           Yes
               J=1
            ELSE

*           No
               J=0
            END IF
         END IF
      END IF

      END
