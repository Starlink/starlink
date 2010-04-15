      SUBROUTINE CHEAPSORT(NDATA, CDATA, KEY)
*
* Numerical recipes sorting routine
*
* Input:
*       NDATA = Number of data values
*       CDATA = Character string data
* Output:
*       KEY   = Key to sorted data values (i.e. CDATA(KEY(I)) ascends)
*
      IMPLICIT NONE
      INTEGER NDATA, L, I, IR, IK, J
      CHARACTER*80 X
      CHARACTER*(*) CDATA(NDATA)
      INTEGER KEY(NDATA)
*
* Initialise KEY
*
      DO I = 1, NDATA
        KEY(I) = I
      END DO
      IF(NDATA.EQ.1) RETURN
*
      L = NDATA/2+1
      IR = NDATA
10    CONTINUE
      IF(L.GT.1) THEN
        L = L - 1
        IK = KEY(L)
        X = CDATA(IK)
      ELSE
        IK = KEY(IR)
        X = CDATA(IK)
        KEY(IR) = KEY(1)
        IR = IR - 1
        IF(IR.EQ.1) THEN
          KEY(1) = IK
          RETURN
        END IF
      END IF
      I = L
      J = L + L
20    IF(J.LE.IR) THEN
        IF(J.LT.IR) THEN
          IF(CDATA(KEY(J)).LT.CDATA(KEY(J+1))) J = J + 1
        END IF
        IF(X.LT.CDATA(KEY(J))) THEN
          KEY(I) = KEY(J)
          I = J
          J = J + J
        ELSE
          J = IR + 1
        END IF
        GOTO 20
      END IF
      KEY(I) = IK
      GOTO 10
      END

      INTEGER FUNCTION LENSTR(STRING)
*
* Returns position of last non-blank
* character of a string. Returns 0 for
* an all blank string.
*
      IMPLICIT NONE
      INTEGER INT, I
      CHARACTER*(*) STRING
      INT=LEN(STRING)
      DO I=INT,1,-1
        IF(STRING(I:I).NE.' ') THEN
          LENSTR=I
          RETURN
        END IF
      END DO
      LENSTR=0
      RETURN
      END

      INTEGER FUNCTION ISTSTR(STRING)
*
* Returns position of the first non-blank
* character of a string. Returns 0 if whole string
* is blank.
*
      CHARACTER*(*) STRING
      INT=LEN(STRING)
      DO I=1,INT
        IF(STRING(I:I).NE.' ') THEN
          ISTSTR=I
          RETURN
        END IF
      END DO
      ISTSTR=0
      RETURN
      END

      LOGICAL FUNCTION SAME( STRING, MASTER )
*
*  Checks strings are the same.
*  Compares all non-blank characters
*  between two strings STRING and MASTER.
*  SAME = .TRUE. if they match.
*  Strings will match if:
*
*      (1) Both STRING and MASTER are blank
*      (2) STRING is same or shorter in length than MASTER and
*          all letters match
*
* Same is case sensitive
*
      IMPLICIT NONE
      CHARACTER*(*) STRING, MASTER
      INTEGER L1, M1, L2, M2, N
*
      SAME = .TRUE.
      L1 = LEN(STRING)
      M1 = LEN(MASTER)
      L2 = 1
      M2 = 1
      N = 0
100   CONTINUE
*
* Find non-blank characters
*
      N = N + 1
200   CONTINUE
      IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
        L2 = L2 + 1
        GOTO 200
      END IF
      L2 = MIN(L1, L2)
*
300   CONTINUE
      IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
        M2 = M2 + 1
        GOTO 300
      END IF
      M2 = MIN(M1, M2)
*
      IF(STRING(L2:L2).NE.' ' .AND. MASTER(M2:M2).NE.' ' .AND.
     &   STRING(L2:L2).NE.MASTER(M2:M2)) THEN
        SAME = .FALSE.
        RETURN
      ELSE IF(STRING(L2:L2).EQ.' ') THEN
        IF(N.EQ.1 .AND. MASTER(M2:M2).NE.' ') SAME = .FALSE.
        RETURN
      ELSE IF(MASTER(M2:M2).EQ.' ' .AND. STRING(L2:L2).NE.' ') THEN
        SAME = .FALSE.
        RETURN
      END IF
*
* Move onto next character
*
      L2 = L2 + 1
      M2 = M2 + 1
      IF(L2.LE.L1 .AND. M2.LE.M1) GOTO 100
*
      IF(M2.GT.M1 .AND. L2.LE.L1) THEN
400     CONTINUE
        IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
          L2 = L2 + 1
          GOTO 400
        END IF
        L2 = MIN(L2, L1)
        IF(STRING(L2:L2).NE.' ') SAME = .FALSE.
      END IF
      RETURN
      END

      LOGICAL FUNCTION SAMECI( STRING, MASTER )
*
* Case-insensitive version of SAME
*
*  Checks strings are the same.
*  Compares all non-blank characters
*  between two strings STRING and MASTER.
*  SAME = .TRUE. if they match.
*  Strings will match if:
*
*      (1) Both STRING and MASTER are blank
*      (2) STRING is same or shorter in length than MASTER and
*          all letters match
*
*
      IMPLICIT NONE
      CHARACTER*(*) STRING, MASTER
      INTEGER L1, M1, L2, M2, N
      INTEGER IS, IM
*
      SAMECI = .TRUE.
      L1 = LEN(STRING)
      M1 = LEN(MASTER)
      L2 = 1
      M2 = 1
      N = 0
100   CONTINUE
*
* Find non-blank characters
*
      N = N + 1
200   CONTINUE
      IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
        L2 = L2 + 1
        GOTO 200
      END IF
      L2 = MIN(L1, L2)
*
300   CONTINUE
      IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
        M2 = M2 + 1
        GOTO 300
      END IF
      M2 = MIN(M1, M2)
*
      IS = ICHAR(STRING(L2:L2))
      IF(IS.GE.97 .AND. IS.LE.122) IS = IS - 32
      IM = ICHAR(MASTER(M2:M2))
      IF(IM.GE.97 .AND. IM.LE.122) IM = IM - 32
      IF(STRING(L2:L2).NE.' ' .AND. MASTER(M2:M2).NE.' ' .AND.
     &   IS.NE.IM) THEN
        SAMECI = .FALSE.
        RETURN
      ELSE IF(STRING(L2:L2).EQ.' ') THEN
        IF(N.EQ.1 .AND. MASTER(M2:M2).NE.' ') SAMECI = .FALSE.
        RETURN
      ELSE IF(MASTER(M2:M2).EQ.' ' .AND. STRING(L2:L2).NE.' ') THEN
        SAMECI = .FALSE.
        RETURN
      END IF
*
* Move onto next character
*
      L2 = L2 + 1
      M2 = M2 + 1
      IF(L2.LE.L1 .AND. M2.LE.M1) GOTO 100
*
      IF(M2.GT.M1 .AND. L2.LE.L1) THEN
400     CONTINUE
        IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
          L2 = L2 + 1
          GOTO 400
        END IF
        L2 = MIN(L2, L1)
        IF(STRING(L2:L2).NE.' ') SAMECI = .FALSE.
      END IF
      RETURN
      END

      LOGICAL FUNCTION ESAME( STRING, MASTER )
*
* Same as SAME but also needs exact match of all non-blank letters.
*
*  Checks strings are the same.
*  Compares all non-blank characters
*  between two strings STRING and MASTER.
*  ESAME = .TRUE. if they match.
*  Strings will match if:
*
*      (1) Both STRING and MASTER are blank
*      (2) STRING is same length as MASTER and all letters match
*
* Case sensitive
*
      IMPLICIT NONE
      CHARACTER*(*) STRING, MASTER
      INTEGER L1, M1, L2, M2, N
*
      ESAME = .TRUE.
      L1 = LEN(STRING)
      M1 = LEN(MASTER)
      L2 = 1
      M2 = 1
      N = 0
100   CONTINUE
*
* Find non-blank characters
*
      N = N + 1
200   CONTINUE
      IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
        L2 = L2 + 1
        GOTO 200
      END IF
      L2 = MIN(L1, L2)
*
300   CONTINUE
      IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
        M2 = M2 + 1
        GOTO 300
      END IF
      M2 = MIN(M1, M2)
*
      IF(STRING(L2:L2).NE.' ' .AND. MASTER(M2:M2).NE.' ') THEN
        IF(STRING(L2:L2).NE.MASTER(M2:M2)) THEN
          ESAME = .FALSE.
          RETURN
        END IF
      ELSE IF(STRING(L2:L2).EQ.' ' .AND. MASTER(M2:M2).EQ.' ') THEN
        RETURN
      ELSE
        ESAME = .FALSE.
        RETURN
      END IF
*
* Move onto next character
*
      L2 = L2 + 1
      M2 = M2 + 1
      IF(L2.LE.L1 .AND. M2.LE.M1) GOTO 100
*
      IF(M2.GT.M1 .AND. L2.LE.L1) THEN
400     CONTINUE
        IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
          L2 = L2 + 1
          GOTO 400
        END IF
        L2 = MIN(L2, L1)
        IF(STRING(L2:L2).NE.' ') ESAME = .FALSE.
      ELSE IF(M2.LE.M1 .AND. L2.GT.L1) THEN
500     CONTINUE
        IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
          M2 = M2 + 1
          GOTO 500
        END IF
        M2 = MIN(M2, M1)
        IF(MASTER(M2:M2).NE.' ') ESAME = .FALSE.
      END IF
      RETURN
      END

      LOGICAL FUNCTION ESAMECI( STRING, MASTER )
*
* Case-insensitive version of SAME but also needs exact
* match of letters.
*
*  Checks strings are the same.
*  Compares all non-blank characters
*  between two strings STRING and MASTER.
*  SAME = .TRUE. if they match.
*  Strings will match if:
*
*      (1) Both STRING and MASTER are blank
*      (2) STRING is same length as MASTER and all letters match
*
*
      IMPLICIT NONE
      CHARACTER*(*) STRING, MASTER
      INTEGER L1, M1, L2, M2, N
      INTEGER IS, IM
*
      ESAMECI = .TRUE.
      L1 = LEN(STRING)
      M1 = LEN(MASTER)
      L2 = 1
      M2 = 1
      N = 0
100   CONTINUE
*
* Find non-blank characters
*
      N = N + 1
200   CONTINUE
      IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
        L2 = L2 + 1
        GOTO 200
      END IF
      L2 = MIN(L1, L2)
*
300   CONTINUE
      IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
        M2 = M2 + 1
        GOTO 300
      END IF
      M2 = MIN(M1, M2)
*
      IF(STRING(L2:L2).NE.' ' .AND. MASTER(M2:M2).NE.' ') THEN
        IS = ICHAR(STRING(L2:L2))
        IF(IS.GE.97 .AND. IS.LE.122) IS = IS - 32
        IM = ICHAR(MASTER(M2:M2))
        IF(IM.GE.97 .AND. IM.LE.122) IM = IM - 32
        IF(IS.NE.IM) THEN
          ESAMECI = .FALSE.
          RETURN
        END IF
      ELSE IF(STRING(L2:L2).EQ.' ' .AND. MASTER(M2:M2).EQ.' ') THEN
        RETURN
      ELSE
        ESAMECI = .FALSE.
        RETURN
      END IF
*
* Move onto next character
*
      L2 = L2 + 1
      M2 = M2 + 1
      IF(L2.LE.L1 .AND. M2.LE.M1) GOTO 100
*
      IF(M2.GT.M1 .AND. L2.LE.L1) THEN
400     CONTINUE
        IF(L2.LE.L1 .AND. STRING(L2:L2).EQ.' ') THEN
          L2 = L2 + 1
          GOTO 400
        END IF
        L2 = MIN(L2, L1)
        IF(STRING(L2:L2).NE.' ') ESAMECI = .FALSE.
      ELSE IF(M2.LE.M1 .AND. L2.GT.L1) THEN
500     CONTINUE
        IF(M2.LE.M1 .AND. MASTER(M2:M2).EQ.' ') THEN
          M2 = M2 + 1
          GOTO 500
        END IF
        M2 = MIN(M2, M1)
        IF(MASTER(M2:M2).NE.' ') ESAMECI = .FALSE.
      END IF
      RETURN
      END

      INTEGER FUNCTION SEARC(LIST, NLIST, MXLIST, NAME)
*
* Searches for NAME amongst a list LIST(NLIST) of character
* variables. Uses SAMECI (case independent, only checks letters
* of NAME) to compare. If 1 match is found it returns the number
* of the item in the list, if more than 1 match is found it returns
* the negative of the first item, if none are found it returns 0.
* No ordering or case assumed.
*
      IMPLICIT NONE
      INTEGER NLIST, MXLIST, I
      CHARACTER*(*) LIST(MXLIST), NAME
      LOGICAL SAMECI
*
      DO I = 1, NLIST
        IF( SAMECI(NAME, LIST(I)) ) THEN
          SEARC = I
          GOTO 100
        END IF
      END DO
*
* No matches found, return with SEARC set to zero
*
      SEARC = 0
      RETURN
*
* Look for any other matches. If any other found, set SEARC to -SEARC
* and return
*
100   DO I = SEARC+1, NLIST
        IF( SAMECI(NAME, LIST(I)) ) THEN
          SEARC = - SEARC
          RETURN
        END IF
      END DO
      RETURN
      END

      INTEGER FUNCTION ESEARC(LIST, NLIST, MXLIST, NAME)
*
* Searches for NAME amongst a list LIST(NLIST) of character
* variables. Uses ESAMECI (case independent, exact match) to
* compare. If 1 match is found it returns the number
* of the item in the list, if more than 1 match is found it returns
* the negative of the first item, if none are found it returns 0.
* No ordering or case assumed.
*
      IMPLICIT NONE
      INTEGER NLIST, MXLIST, I
      CHARACTER*(*) LIST(MXLIST), NAME
      LOGICAL ESAMECI
*
      DO I = 1, NLIST
        IF( ESAMECI(NAME, LIST(I)) ) THEN
          ESEARC = I
          GOTO 100
        END IF
      END DO
*
* No matches found, return with ESEARC set to zero
*
      ESEARC = 0
      RETURN
*
* Look for any other matches. If any other found, set ESEARC to -ESEARC
* and return
*
100   DO I = ESEARC+1, NLIST
        IF( ESAMECI(NAME, LIST(I)) ) THEN
          ESEARC = - ESEARC
          RETURN
        END IF
      END DO
      RETURN
      END


      SUBROUTINE UPPER_CASE(STRING)
*
* Converts a string into upper case
*
      CHARACTER*(*) STRING
      INTEGER LENGTH, I, IC
*
      LENGTH = LEN(STRING)
      DO I = 1, LENGTH
        IC = ICHAR(STRING(I:I))
        IF(IC.GE.97 .AND. IC.LE.122) THEN
          STRING(I:I) = CHAR(IC-32)
        END IF
      END DO
      RETURN
      END

