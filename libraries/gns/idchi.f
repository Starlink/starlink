      SUBROUTINE gns_1IDCHI (STRING, NPTR, LENSTR, NVEC, DIGIT)
*+
*
*     - - - - - - 
*      I D C H I   (Internal routine)
*     - - - - - -
*
*   Identify next character in string for INTIN
*
*   Given
*      STRING      c       string
*      NPTR        i       pointer to character to be identified
*      LENSTR      i       string length
*
*   Returned
*      NPTR        i       incremented unless end of field
*      NVEC        i       vector for identified character
*      DIGIT       d       double precision digit if 0-9
*
*     NVEC takes the following values:
*
*      1     0-9
*      2     space 
*      3     +
*      4     -
*      5     , or _
*      6     else
*      7     outside string
*
*   P.T.Wallace, D.L.Terrett   Starlink   Jan 1987
*+
      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NPTR,LENSTR,NVEC
      DOUBLE PRECISION DIGIT

      INTEGER NCHAR

*  Character/vector tables
      INTEGER NCREC
      PARAMETER (NCREC=15)
      CHARACTER KCTAB(NCREC)
      INTEGER KVTAB(NCREC)
      DATA KCTAB/'0','1','2','3','4','5','6','7','8','9',
     :           ' ','+','-',',','_'/
      DATA KVTAB/10*1,2,3,4,2*5/



*  Handle pointer outside field
      IF (NPTR.LT.1.OR.NPTR.GT.LENSTR) THEN
         NVEC=7
      ELSE

*     Not end of field

*     Identify character
         DO 10 NCHAR=1,NCREC
            IF (KCTAB(NCHAR).EQ.STRING(NPTR:NPTR)) GO TO 2200
   10    CONTINUE

*     Unrecognised
         NVEC=6
         GO TO 2300

*     Recognised
 2200    CONTINUE
         NVEC=KVTAB(NCHAR)

*     Allow for numerals
         DIGIT=DBLE(NCHAR-1)

*     Increment pointer
 2300    CONTINUE
         NPTR=NPTR+1
      END IF

*  Return

      END
