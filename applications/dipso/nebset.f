*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE NEBSET
*
*   SETS UP DATA FOR CALL TO NEBULAR CONTINUUM CODE
*
*   IMPORTS:
*     IUNIT        INTEGER   STREAM FOR READING DATA
*
*   EXPORTS:
*     OK           LOGICAL   TRUE ON SUCCESS
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE NEBSET(IUNIT,X,Y,ASIZE1,NPOINT,BREAK,MAXBRK,NBREAK,
     : MODE1,MODE2,MODE3,OK,STATUS)
*
*
       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS
*
*
       CHARACTER*80 STRING, TEXT
*
       INTEGER I, J, K, L
       INTEGER SLEN
       INTEGER IUNIT
       INTEGER LLEN
       INTEGER IX
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER BREAK(NBREAK)
       INTEGER MODE1, MODE2, MODE3
*
       REAL X(ASIZE1), Y(ASIZE1)
       REAL FNEB
       REAL AMS, ZMS, XMS
       REAL TEMP
       REAL ELDENS
       REAL FZ(5,4)
       REAL TZ(4)
       REAL FNMS(4), DMS(4)
       REAL TNEB, FHOLOG, CHB
       REAL FHILOG, FHBETA, FACT, COLHP, HER, HEPR, XS
*
       REAL FHELOG, FHE, FACTHE
       REAL LOGF(2)
       REAL TSTVAL
       PARAMETER (TSTVAL=-321.654)
*
*
       LOGICAL OK
       IF( STATUS .NE. SAI__OK ) RETURN

*
*   READ Te(1-4),Ne
*
*
       READ (IUNIT,*,IOSTAT=IX) (TZ(I),I=1,4)
       IF (IX.NE.0) THEN
          WRITE (*,'(''   NEBCONT:  error reading temperatures'')')
          OK = .FALSE.
          GOTO 1000
       ENDIF
       DO 100 I = 2, 4
          IF (ABS(TZ(I)).LT.1.0) THEN
             TZ(I) = TZ(I-1)
          ENDIF
  100  CONTINUE

       STRING = '              Te( )'
       DO 200 I = 1, 4
          WRITE (STRING(18:18),'(I1)',IOSTAT=IX) I
          CALL PUTEXT(TEXT,LLEN,TZ(I))
          CALL UPDATE(STRING,TEXT,LLEN,MODE3,OK,TZ(I),STATUS)
          IF (.NOT.OK) GOTO 1000
          STRING(4:11) = '        '
  200  CONTINUE
       TNEB = TZ(1)
*
*
       READ (IUNIT,*,IOSTAT=IX) ELDENS
       IF (IX.NE.0) THEN
          WRITE (*,'(''   NEBCONT:  error reading density'')')
          OK = .FALSE.
          GOTO 1000
       ENDIF

       STRING = '                Ne X'
       CALL PUTEXT(TEXT,LLEN,ELDENS)
       CALL UPDATE(STRING,TEXT,LLEN,MODE3,OK,ELDENS,STATUS)
       IF (.NOT.OK) GOTO 1000
*
*
*   READ Hbeta (or 1640) flux, 'C'
*
*
*
       STRING = ' '
  300  CONTINUE
       READ (IUNIT,'(A40)',IOSTAT=IX) STRING(1:40)
       IF (STRING.EQ.' ') GOTO 300
       I = INDEX(STRING,'!')
       IF (I.NE.0) STRING = STRING(1:I-1)
*
       IF (IX.NE.0) THEN
  350     CONTINUE
          WRITE (*,'(''   NEBCONT:  error reading Hbeta flux'')')
          OK = .FALSE.
          GOTO 1000
       ENDIF

       LOGF(1) = TSTVAL
       LOGF(2) = TSTVAL
       CALL DECODE('Line Flux',STRING,0,2,LOGF,' ',OK)
       IF (.NOT.OK) GOTO 350
       IF (LOGF(1).EQ.TSTVAL) GOTO 350
       FHOLOG = LOGF(1)
       FHELOG = LOGF(2)
       IF (LOGF(1).NE.0.0) THEN
          WRITE (*,'(''  '')')
          STRING = '   Log H(beta) flux'
          CALL PUTEXT(TEXT,LLEN,FHOLOG)
          CALL UPDATE(STRING,TEXT,LLEN,MODE2,OK,FHOLOG,STATUS)
          IF (.NOT.OK) GOTO 1000
          IF (LOGF(2).NE.TSTVAL .AND. LOGF(2).NE.0.0) THEN
             WRITE (*,'(''   !  Log He(1640) flux ignored'')')
          ENDIF
       ELSE
          WRITE (*,'('' '')')
          STRING = '  Log He(1640) flux'
          CALL PUTEXT(TEXT,LLEN,FHELOG)
          CALL UPDATE(STRING,TEXT,LLEN,MODE2,OK,FHELOG,STATUS)
          IF (.NOT.OK) GOTO 1000
       ENDIF

       READ (IUNIT,*,IOSTAT=IX) CHB
       IF (IX.NE.0) THEN
          WRITE (*,
     :    '(''   NEBCONT:  error reading reddening parameter'')')
          OK = .FALSE.
          GOTO 1000
       ENDIF

       STRING = '               ''C'' X'
       CALL PUTEXT(TEXT,LLEN,CHB)
       CALL UPDATE(STRING,TEXT,LLEN,MODE2,OK,CHB,STATUS)
       IF (.NOT.OK) GOTO 1000
*
*
*   READ ION ABUNDANCES
*
*
       READ (IUNIT,*,IOSTAT=IX) (FZ(5,I),I=1,2)
       IF (IX.NE.0) GOTO 360
       DO 400 J = 1, 4
          READ (IUNIT,*,IOSTAT=IX) (FZ(J,I),I=1,4)
          IF (IX.NE.0) THEN
  360        CONTINUE
             WRITE (*,'(''   NEBCONT:  error reading from file'')')
             OK = .FALSE.
             GOTO 1000
          ENDIF
  400  CONTINUE
*
*
*
       WRITE (*,'(''0  Abundances [ = 1000*N(ion)/N(H+)]'')')
*
       CALL PUTEXT(TEXT,LLEN,FZ(5,1))
       STRING = '             He(1+)'
       CALL UPDATE(STRING,TEXT,LLEN,MODE1,OK,FZ(5,1),STATUS)
       IF (.NOT.OK) GOTO 1000
*
       CALL PUTEXT(TEXT,LLEN,FZ(5,2))
       STRING = '             He(2+)'
       CALL UPDATE(STRING,TEXT,LLEN,MODE1,OK,FZ(5,2),STATUS)
       IF (.NOT.OK) GOTO 1000
*
       STRING = '               ( +)'
       DO 500 I = 1, 4
          IF (I.EQ.1) THEN
             STRING(14:15) = ' N'
          ELSEIF (I.EQ.2) THEN
             STRING(14:15) = ' C'
          ELSEIF (I.EQ.3) THEN
             STRING(14:15) = ' O'
          ELSEIF (I.EQ.4) THEN
             STRING(14:15) = 'Ne'
          ENDIF
          DO 450 J = 1, 4
             WRITE (STRING(17:17),'(I1)') J
             CALL PUTEXT(TEXT,LLEN,FZ(I,J))
             CALL UPDATE(STRING,TEXT,LLEN,MODE1,OK,FZ(I,J),STATUS)
             IF (.NOT.OK) GOTO 1000
  450     CONTINUE
  500  CONTINUE

*   NEBF code starts here

       IF (FHOLOG.EQ.0.0) THEN
          FHE = 10.0**(CHB*2.136+FHELOG)
          FACT = 1.234E-25*TNEB**(-0.87)
          FACTHE = 9.732E-24*TZ(2)**(-0.858)
          FHBETA = FHE*FACT/FACTHE/FZ(5,2)*1000.0
          FHILOG = LOG10(FHBETA)
          COLHP = FHE/FACTHE/FZ(5,2)*1000.0
          TEMP = LOG10(FHBETA)
          TEMP = TEMP - CHB
          WRITE (*,
     :    '(''   NEBCONT:  Log of predicted (reddened)'',
     :    '' H(beta) flux ='',1pe12.4)') TEMP
       ELSE
          FHILOG = CHB + FHOLOG
          FHBETA = 10.0**FHILOG
          FACT = 1.234E-25*TNEB**(-0.87)
          COLHP = FHBETA/FACT
       ENDIF
*
*
       DO 600 I = 1, 5
          DO 550 J = 1, 4
             FZ(I,J) = 0.001*FZ(I,J)
  550     CONTINUE
  600  CONTINUE

       AMS = 2.051E-21*COLHP
       DO 700 I = 1, 4
          DMS(I) = AMS*(FZ(1,I)+FZ(2,I)+FZ(3,I)+FZ(4,I))
  700  CONTINUE
       FNMS(1) = 5.
       FNMS(2) = 7.
       FNMS(3) = 9.
       FNMS(4) = 12.
       DO 800 I = 1, 4
          ZMS = I
          XMS = 15.789*ZMS**2/(TZ(I)*FNMS(I)**2)
          DMS(I) = DMS(I)*I**2*(EXP(XMS)*(1.0+XMS/FNMS(I))+1.0)
     :    /SQRT(TZ(I))
  800  CONTINUE

       HER = FZ(5,1)
       HEPR = FZ(5,2)

       CALL NEBCONT(NPOINT,X,Y,TZ,HER,HEPR,ELDENS,COLHP,FHBETA)

       DO 900 I = 1, NPOINT
          XS = X(I)
          Y(I) = Y(I) + FNEB(XS,COLHP,TZ,FZ,DMS)
  900  CONTINUE

       WRITE (TEXT,'(1PE15.5)',IOSTAT=IX) COLHP
       CALL SSTRIP(TEXT)
       LLEN = SLEN(TEXT)
       WRITE (*,'('' '')')
       WRITE (*,'(''   NEBCONT:  N(H+) = '',A)') TEXT(1:LLEN)
       TEXT = ' Nebular continuum'
       CALL UPUSH(ASIZE1,X,Y,NPOINT,MAXBRK,BREAK,NBREAK,TEXT,1.0,OK)

 1000  CONTINUE

       END
