       SUBROUTINE MODELS(A,N,TOTWEI,MODEL,OK,STATUS)

       INCLUDE 'SAE_PAR'
       INTEGER STATUS

!
! Declaration of parameters
!
       PARAMETER (MAXCL=18,MAXARR=30,MOUT=120)
!
! Declarations for subroutine arguments
!
       REAL A(1)
       INTEGER N
       INTEGER JSORT
       INTEGER SLEN
!
! Declarations for subroutine variables
!
       LOGICAL EXISTS(MAXARR), OK,MODEL
       LOGICAL BEEP
       COMMON /BEEP/ BEEP
       CHARACTER*1 BLEEP
       COMMON /BLEEP / BLEEP

       INTEGER I, J, K
       INTEGER DELIM, CLOUD, SHIFT
       INTEGER NTERMS

       CHARACTER OPTION*1, STRING*80, SUBSTR*80
       CHARACTER*80 OUTSTR

       REAL SRTARY(MOUT)
       REAL B, V, COLUMN
       REAL TEMP1, TEMP(2)
!
! Formats
!
 1000  FORMAT (A,T40,A)
 2000  FORMAT ('   INPUT CLOUD',I2,': b, Vr, W (Free Format) ',$)
!
       DATA ZERO/0.0/
!

       IF( STATUS .NE. SAI__OK ) RETURN

       IF (.NOT.MODEL) THEN
          N = 0
          MODEL = .FALSE.
          DO I = 1, 3*MAXCL
             A(I) = 0.0
          ENDDO
       ENDIF
!
! Initialisation of EXISTS
!
       DO 1 I=1,N
          EXISTS(I)=.TRUE.
    1  CONTINUE
       DO 2 I=N+1,MAXARR
          EXISTS(I)=.FALSE.
    2  CONTINUE
!
       IF (N.GT.0) THEN
          JSORT = 0
          IF (N.GT.1) THEN
             DO I = 2, N
                IF (A(I*3-1).LT.A(I*3-4)) JSORT = JSORT + 1
             ENDDO
          ENDIF
          CALL VELSORT
     :    (A,N,NTERMS,EXISTS,SRTARY,MAXCL,MAXARR)
          WRITE (*,
     :    '(''   ISINP:  current number of clouds is'',I3)') N
          WRITE (*,
     :    '(T4,A,T18,A,T33,A,T44,A,T60,A)')
     :    'Cloud no.', 'B (km/s)', 'V (km/s)', ' N (cm-2) ',
     :    'Log(N)'
          DO I = 1, MAXARR
             IF (EXISTS(I)) THEN
               IF (A(I*3).GT.0.0) THEN
                WRITE (*,'(I8,2F15.1,1PE15.4,0PF11.2)')
     :          I, A(I*3-2),A(I*3-1),A(I*3),LOG10(A(I*3))
               ELSE
                WRITE (*,'(I8,2F15.1,1PE15.4,0PF11.2)')
     :          I, A(I*3-2),A(I*3-1),A(I*3),ZERO
               ENDIF
             ENDIF
          ENDDO
       ENDIF
!
 300   CONTINUE
       STRING = ' '
       CALL RDSTR( ' ','   ISINP> ', ' ', STRING, STATUS )

       IF( STATUS .NE. SAI__OK ) THEN
          OK = .FALSE.
          GO TO 500
       END IF

       IF (STRING.EQ.' ') GO TO 300
       I = INDEX(STRING,'=')
       IF (I.NE.0) STRING(I:I) = ' '
       I = INDEX(STRING,':')
       IF (I.NE.0) STRING(I:I) = ' '
       CALL DTOUPP(STRING)
       CALL SSTRIP(STRING)

       IF (STRING(1:3).EQ.'QIS') THEN
          NLOST = 0
          DO I = 1, MAXCL
             IF (EXISTS(I)) THEN
                IF ((A(I*3-2).LE.0.) .OR. (A(I*3).LE.0.)) THEN
                   NLOST = NLOST + 1
                ENDIF
             ENDIF
          ENDDO
          IF (NLOST.GT.0) THEN
            IF (.NOT.BEEP) THEN
             WRITE (*,
     :       '(''   ISINP_QIS: '',I2,'' cloud(s) will be deleted'',
     :       '' (zero "b" or "N")'')') NLOST
            ELSE
             WRITE (*,
     :       '(''   ISINP_QIS: '',I2,'' cloud(s) will be deleted'',
     :       '' (zero "b" or "N")'',A)') NLOST, BLEEP
            ENDIF
             WRITE (*,
     :       '(''      Do you still want to quit (y/n): '',$)')
  200        CONTINUE
             READ (*,'(A)') STRING
             CALL SSTRIP (STRING)
             CALL DTOUPP (STRING)
             IF (STRING(1:1).EQ.'N') GO TO 300
             IF (STRING(1:1).NE.'Y') THEN
                WRITE (*,
     :          '(''      Please answer yes or no: '',$)')
                GO TO 200
             ENDIF
             DO I = 1, MAXCL
                IF (EXISTS(I) .AND.
     :          ((A(I*3-2).LE.0.) .OR. (A(I*3).LE.0.))) THEN
                   EXISTS(I) = .FALSE.
                   N = N - 1
                ENDIF
             ENDDO
             CALL VELSORT
     :       (A,N,NTERMS,EXISTS,SRTARY,MAXCL,MAXARR)
          ENDIF
          DO I = 1, MAXCL
             IF (.NOT.EXISTS(I)) THEN
                A(I*3) = 0.
                A(I*3-1) = 0.
                A(I*3-2) = 0.
             ENDIF
          ENDDO
          IF (N.GT.0) THEN
             JSORT = 0
             IF (N.GT.1) THEN
                DO I = 2, N
                   IF (A(I*3-1).LT.A(I*3-4)) JSORT = JSORT + 1
                ENDDO
             ENDIF
             CALL VELSORT
     :       (A,N,NTERMS,EXISTS,SRTARY,MAXCL,MAXARR)
             IF (JSORT.GT.0) WRITE (*,
     :       '(''   ISINP:  renumbering clouds (sort by velocity)'')')
          ENDIF
          TOTWEI = 0.
          DO I = 1, N
             TOTWEI = TOTWEI + A(3*I)
          ENDDO
          IF (N.EQ.0) THEN
             MODEL = .FALSE.
             WRITE (*,
     :       '(''   ISINP:  no clouds in model'')')
          ELSE
             MODEL = .TRUE.
          ENDIF
          GOTO 500
       ELSEIF (STRING(1:1).EQ.'Q') THEN
          IF (.NOT.BEEP) THEN
             WRITE (*,
     :       '(''   ISINP:  quit command is QIS'')')
          ELSE
             WRITE (*,
     :       '(''   ISINP:  quit command is QIS'',A)') BLEEP
          ENDIF
          GO TO 300
       ENDIF
       OPTION = STRING(1:1)
       STRING(1:) = STRING(2:)
       CALL SSTRIP(STRING)
       IF (OPTION.EQ.'H') THEN
          WRITE (*,
     :    '(''   ISINP_H:''/
     :      ''      Syntax is <Parameter> <operator> <value>''/
     :      ''      The operator can be, optionally and'',
     :      '' equivalently, blank, "=", or ":"''/
     :      ''      Parameters are:''/
     :      ''       Bn -  "b" value for cloud n''/
     :      ''       Dn -  Delete cloud n from model (no <value>)''/
     :      ''       Nn -  (Log) column density'',
     :      '' for cloud n''/
     :      ''       Vn -  Central velocity'',
     :      '' for cloud n''/
     :      ''      Other valid inputs are:''/
     :      ''       H  -  Help''/
     :      ''       L  -  List current cloud model (and re-sort'',
     :      '' by velocity)''/
     :     ''       QIS - Quit ISINP (and re-sort clouds by velocity)''/
     :      ''   On exiting ISINP, each cloud must have positive''/
     :      ''   values for both b and N if it is to be retained.'')')
          GO TO 300
       ELSEIF (OPTION.EQ.'L') THEN
          IF (N.EQ.0) THEN
             WRITE (*,
     :       '(''   ISINP_L:  '',
     :       ''there are no clouds present in the model'')')
          ELSE
             JSORT = 0
             IF (N.GT.1) THEN
                DO I = 2, N
                   IF (A(I*3-1).LT.A(I*3-4)) JSORT = JSORT + 1
                ENDDO
             ENDIF
             DO I = 2, MAXCL
                IF (EXISTS(I).AND.(.NOT.EXISTS(I-1))) JSORT = JSORT+1
             ENDDO
             CALL VELSORT
     :       (A,N,NTERMS,EXISTS,SRTARY,MAXCL,MAXARR)
             WRITE (*,
     :       '(''   ISINP_L:  number of clouds is'',I3)') N
             IF (JSORT.GT.0) WRITE (*,
     :       '(''             Changing numbering (sort by velocity)'')')
             OUTSTR = ' '
             WRITE (*,
     :       '(T4,A,T18,A,T33,A,T44,A,T60,A)')
     :       'Cloud no.', 'B (km/s)', 'V (km/s)', ' N (cm-2) ',
     :       'Log(N)'
             NSTAR = 0
             DO I = 1, MAXARR
                IF (EXISTS(I)) THEN
                  IF (A(I*3).GT.0.0) THEN
                   OUTSTR = ' '
                   WRITE (OUTSTR,'(I8,2F15.1,1PE15.4,0PF11.2)')
     :             I, A(I*3-2),A(I*3-1),A(I*3),LOG10(A(I*3))
                  ELSE
                   OUTSTR = ' '
                   WRITE (OUTSTR,'(I8,2F15.1,1PE15.4,0PF11.2)')
     :             I, A(I*3-2),A(I*3-1),A(I*3),ZERO
                  ENDIF
                   IF ((A(I*3-2).EQ.0.0).OR.(A(I*3).EQ.0.0)) THEN
                      OUTSTR(65:65) = '*'
                      NSTAR = NSTAR + 1
                   ENDIF
                   WRITE (*,'(A)') OUTSTR(1:SLEN(OUTSTR))
                ENDIF
             ENDDO
             IF (NSTAR.GT.0) THEN
                WRITE (*,
     :     '(''   * - cloud not fully specified (b or N = 0)'')')
             ENDIF
          ENDIF
          GO TO 300
       ENDIF
       IF (OPTION.EQ.'D') THEN
          CALL DECODE
     :    ('ISINP_D',STRING,1,1,TEMP1,'Cloud_number ',OK)
          IF (.NOT.OK) THEN
             WRITE (*,
     :       '(''   ISINP:  cloud not deleted'')')
             OK = .TRUE.
          ELSE
             J = NINT(TEMP1)
             IF ((J.LE.0).OR.(J.GT.MAXCL).OR.(.NOT.EXISTS(J))) THEN
                CALL ERMESS (OPTION,MAXCL,J)
             ELSE
                EXISTS(J) = .FALSE.
                N = N-1
                DO I = (3*J-2), (3*J)
                   A(I) = 0.0
                ENDDO
             ENDIF
          ENDIF
          GO TO 300
       ENDIF
       IF (OPTION.EQ.'B') THEN
          CALL DECODE
     :    ('ISINP_B',STRING,2,2,TEMP,'Cloud_no. B_value ',OK)
          IF (.NOT.OK) THEN
             OK = .TRUE.
             GOTO 300
          ENDIF
          J = NINT(TEMP(1))
          IF ((J.LE.0).OR.(J.GT.MAXCL)) THEN
             CALL ERMESS (OPTION,MAXCL,J)
          ELSEIF (TEMP(2).LE.0.) THEN
             IF (.NOT.BEEP) THEN
                WRITE (*,
     :          '(''   ISINP:  "b" value must be positive'')')
             ELSE
                WRITE (*,
     :          '(''   ISINP:  "b" value must be positive'',A)') BLEEP
             ENDIF
          ELSE
             A(J*3-2) = TEMP(2)
             IF (.NOT.EXISTS(J)) THEN
                EXISTS(J) = .TRUE.
                N = N + 1
             ENDIF
          ENDIF
          GO TO 300
       ELSEIF (OPTION.EQ.'N') THEN
          CALL DECODE
     :    ('ISINP_N',STRING,2,2,TEMP,'Cloud_no. Column ',OK)
          IF (.NOT.OK) THEN
             OK = .TRUE.
             GOTO 300
          ENDIF
          J = NINT(TEMP(1))
          IF ((J.LE.0).OR.(J.GT.MAXCL)) THEN
             CALL ERMESS (OPTION,MAXCL,J)
          ELSEIF (TEMP(2).LE.0.) THEN
             IF (.NOT.BEEP) THEN
                WRITE (*,
     :          '(''   ISINP:  (log) column must be greater than 0'')')
             ELSE
                WRITE (*,
     :          '(''   ISINP:'',
     :          ''  (log) column must be greater than 0'',A)') BLEEP
             ENDIF
          ELSE
             IF (TEMP(2).LT.35.) THEN
                TEMP(2) = 10.0**TEMP(2)
             ENDIF
             A(J*3) = TEMP(2)
             IF (.NOT.EXISTS(J)) THEN
                EXISTS(J) = .TRUE.
                N = N + 1
             ENDIF
          ENDIF
          GO TO 300
       ELSEIF (OPTION.EQ.'V') THEN
          CALL DECODE
     :    ('ISINP_V',STRING,2,2,TEMP,'Cloud_no. Velocity ',OK)
          IF (.NOT.OK) THEN
             OK = .TRUE.
             GOTO 300
          ENDIF
          J = NINT(TEMP(1))
          IF ((J.LE.0).OR.(J.GT.MAXCL)) THEN
             CALL ERMESS (OPTION,MAXCL,J)
          ELSE
             A(J*3-1) = TEMP(2)
             IF (.NOT.EXISTS(J)) THEN
                EXISTS(J) = .TRUE.
                N = N + 1
             ENDIF
          ENDIF
          GO TO 300
       ELSE
          IF (.NOT.BEEP) THEN
             WRITE (*,
     :       '(''   ISINP:  input not recognised'')')
          ELSE
             WRITE (*,
     :       '(''   ISINP:  input not recognised'',A)') BLEEP
          ENDIF
          GO TO 300
       ENDIF

  500  CONTINUE

       END
