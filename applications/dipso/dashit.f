       SUBROUTINE DASHIT
     : (CMD,IXS,NV,PARAMS,VSIZE,MAXSTK,NONSTK,VARRAY,OK)

*
*  decodes strings of form " n1 - n2 " into array
*  of real numbers
*
*  CMD - Name of invoking command
*  VARRAY(VSIZE) - Array of real variables into
*     which decoded values are entered
*  NV - integer variable representing last entry
*     in VARRAY on exit
*  IXS - integer containing number of invalid entries
*  OK - TRUE on success
*

       IMPLICIT NONE

*

       INTEGER NV, VSIZE, MAXSTK, NONSTK
       REAL VARRAY(VSIZE)
       LOGICAL OK
       CHARACTER*(*) PARAMS
       CHARACTER*(*) CMD
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       INTEGER IDASH, IX, I1, I2, INCR, SL, SLEN
       INTEGER II, IXS, IXD, IMIN
       REAL TVARS(2)
       LOGICAL NODASH
       CHARACTER*20 SUBSTR

*
*  Initialise
*

       NV = 0
       IXS = 0
       IXD = 0
       OK = .TRUE.
       NODASH = .FALSE.

*
*  Look for "-"
*

       DO 200 WHILE (.NOT.NODASH)

          IDASH = INDEX(PARAMS,'-')

          IF (IDASH.EQ.0) THEN
             NODASH = .TRUE.
          ELSE

*  Find length of string associated with dash (I1-I2)

             DO 20 IX = IDASH - 1, 1, -1
                IF (PARAMS(IX:IX).NE.' ') THEN
                   DO 5 II = IX, 1, -1
                      IF (PARAMS(II:II).EQ.' ') THEN
                         I1 = II + 1
                         GOTO 40
                      ENDIF
    5              CONTINUE
                   I1 = 1
                   GOTO 40
                ENDIF
   20        CONTINUE
*  No string found preceding "-"
             OK = .FALSE.
             WRITE (*,
     :       '(''   '',A,'':  error decoding parameters'')')
     :       CMD
             GOTO 300
*  Start of string found

   40        CONTINUE
             SL = SLEN(PARAMS)
             DO 60 IX = IDASH + 1, SL, 1
                IF (PARAMS(IX:IX).NE.' ') THEN
                   DO 45 II = IX, SL, 1
                      IF (PARAMS(II:II).EQ.' ') THEN
                         I2 = II - 1
                         GOTO 80
                      ENDIF
   45              CONTINUE
                   I2 = SL
                   GOTO 80
                ENDIF
   60        CONTINUE
*  No string found following "-"
             OK = .FALSE.
             WRITE (*,
     :      '(''   '',A,       '':  error decoding parameters'')')
     :       CMD
             GOTO 300
*  End of string found

*
*  Decode substring to obtain limits
*

   80        CONTINUE
             PARAMS(IDASH:IDASH) = ' '
             SUBSTR = PARAMS(I1:I2)
             PARAMS(I1:I2) = ' '

             CALL DECODE(CMD,SUBSTR,2,2,TVARS,' ',OK)
             IF (.NOT.OK) GOTO 300

*  Load VARRAY

             I1 = NINT(MIN(TVARS(1),TVARS(2)))
             I2 = NINT(MAX(TVARS(1),TVARS(2)))
             INCR = 1
             IF (CMD.EQ.'PM') THEN
                I1 = NINT(TVARS(1))
                I2 = NINT(TVARS(2))
                IF (I1.GT.I2) INCR = -1
                IMIN = 0
             ELSE
                IMIN = 1
             ENDIF
             DO 100 IX = I1, I2, INCR
                IF (IX.LT.IMIN .OR. IX.GT.MAXSTK) THEN
                   IXS = IXS + 1
                ELSEIF (IX.GT.NONSTK) THEN
                   IF (CMD.NE.'DEL') THEN
                      WRITE (*,'(''   '',A,
     :                '':  entry'',I3,'' is empty'')')
     :                CMD, IX
                   ELSE
                      IXD = IXD + 1
                   ENDIF
                ELSE
                   NV = NV + 1
                   IF (NV.GT.VSIZE) THEN
                      OK = .FALSE.
                      WRITE (*,
     :                '(''   '',A,'':  too many parameters'')')
     :                CMD
                      GOTO 300
                   ENDIF
                   VARRAY(NV) = IX
                ENDIF
  100        CONTINUE
*             IF (IXD.NE.0) THEN
*                WRITE (*,
*     :          '(''   DEL: '',I<(NINT(LOG10(REAL(IXD)))+1.0)>,
*     :          '' specified entries already empty'',A)')
*     :          IXD, BLEEP
*             ENDIF

          ENDIF

  200  CONTINUE

  300  CONTINUE

       END
