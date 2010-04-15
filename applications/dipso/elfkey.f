      SUBROUTINE ELFKEY( IF, COMTXT, COMFIL, STATUS )
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      LOGICAL COMTXT
      CHARACTER COMFIL*(*)

      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/DEBUG/NY
      CHARACTER*1 BLEEP
      COMMON /BLEEP/ BLEEP
      DIMENSION A(80),B(80),P(10),INDX(80),IREL(80),LP(20)
      DIMENSION FLX(20),KARD(80)


!
       CHARACTER*80 ELFSTR
!
      IF( STATUS .NE. SAI__OK ) RETURN

      IF=0
      NDI4=4*NDIM
      NDI5=5*NDIM
C     CALL TALPHA

!
!   Fix to change QUIT command
   24 ELFSTR = ' '
      IF( .NOT. COMTXT ) THEN
         CALL RDSTR( ' ', 'ELFINP>  ', ' ', ELFSTR, STATUS )
         IF( STATUS .NE. SAI__OK ) ELFSTR='QELF'
      ELSE
         READ( 67, '(A800)', IOSTAT=IHX ) ELFSTR
         IF( IHX .NE. 0 ) THEN
            ELFSTR = 'QELF'
            IF( IHX .NE. -1 ) THEN
               WRITE (*,'(A,A)') '   Error reading from command file: ',
     :                           COMFIL
            END IF
         END IF
      END IF


      CALL SSTRIP (ELFSTR)
      CALL DTOUPP (ELFSTR)
      IF (ELFSTR(1:4).EQ.'QELF') THEN
         ELFSTR(2:) = ' '
         KARD(1) = ICHAR(ELFSTR(1:1))
C        READ (ELFSTR(1:1),112) KARD
      ELSEIF (ELFSTR(1:1).EQ.'H') THEN
         ELFSTR(2:) = ' '
         ELFSTR(1:1) = '?'
         KARD(1) = ICHAR(ELFSTR(1:1))
C        READ (ELFSTR(1:1),112) KARD
      ELSEIF (ELFSTR(1:1).EQ.'Q') THEN
         WRITE (*,
     :   '(''   ELFINP:  to Quit type QELF'',A)') BLEEP
         GO TO 24
      ELSEIF (ELFSTR(1:1).EQ.'D' .OR. ELFSTR(1:1).EQ.'P') THEN
         I = INDEX(ELFSTR,'=')
         IF (I.EQ.0) THEN
            WRITE (*,
     :      '(''   ELFINP:  '',A,'' variable must be defined'',
     :      '' using `='''' operator'',A)') ELFSTR(1:1), BLEEP
            GOTO 24
         ENDIF
C        READ (ELFSTR,112) KARD
         DO I=1,80
            KARD(I) = ICHAR(ELFSTR(I:I))
         END DO
      ELSEIF (ELFSTR(1:5).EQ.'CLEAR') THEN
         IF (ELFSTR(6:).NE.' ') THEN
            WRITE (*,
     :      '(''   ELFINP:  use CLEAR without an argument list '',
     :      ''to clear the current model''/
     :        ''            (command not obeyed)'',A)') BLEEP
            GOTO 24
         ENDIF
         CALL ELFCLR
         WRITE (*,'(''   ELFINP:  current model cleared'',A)') BLEEP
         GO TO 24
      ELSE
C        READ (ELFSTR,112) KARD
         DO I=1,80
            KARD(I) = ICHAR(ELFSTR(I:I))
         END DO
      ENDIF

!
C     READ(5,112) KARD
C 112 FORMAT(80A1)
      CALL KDCODE(KARD,KV1,KV2,F1,KODE,NDIM)
      IF(NY.GT.0) WRITE(*,500) KV1,KV2,F1,KODE
  500 FORMAT(5X,2I5,F15.5,I5)
      IF(KV1.EQ.0) GO TO 24
      IF(KV1.EQ.-1) GO TO 23
      IF(KV1.EQ.-2) GO TO 23
      IF(KV1.EQ.-3) THEN
        CALL SOFAR(A,P,INDX,IREL,NPAR,NDIM)
        GO TO 24
      ENDIF
      IF(KV1.EQ.-4.OR.KV1.EQ.-5) THEN
*        WRITE (*,
*     :  '(''   ELFINP_H:''/
*     :''     The syntax is ''/
*     :''         <parameter><line number><operator><value>''/
*     :''     where the parameter can be one of''/
*     :''         W, C, I, D or P''/
*     :''         (=Width, Centre, Intensity, Degree of poly,''
*     :,'' Profile type);''/
*     :''     the line number identifies the line concerned;''/
*     :''     the operator can be "=" or ":", where "=" fixes the''/
*     :''     parameter given value, while ":" merely initialises it,''/
*     :''     leaving it to be optimised by ELFOPT;  and''/
*     :''     the "value" can be expressed numerically, or as an''/
*     :''     arithmetic expression relating the parameter for a''/
*     :''     given line to that of other lines.   For example:''/
*     :''         W1:5''/
*     :''     initialises the width of line 1 to 5 units;''/
*     :''         W2=W1''/
*     :''     constrains the width of line 2 to be equal that of''/
*     :''     line 1.''/
*     :''     To list the current profile specification, type L''/
*     :''     To clear the current specification, type CLEAR''/
*     :''     To leave the ELF  parameter editor, type QELF'')')
        GO TO 24
      ENDIF
      IF(KV1.GT.0.AND.KV1.LT.NDI4) THEN
        INDX(KV1)=KODE
        A(KV1)=F1
        IF(KODE.EQ.3) THEN
          KV=KV2-NDIM*(KV2/NDIM)
          IREL(KV1)=KV
        ENDIF
        GO TO 24
      ENDIF
      IF(KV1.EQ.(NDI5+5)) THEN
        IF(F1.GE.0.) THEN
          NPAR=F1+1
          GO TO 24
        ELSE
          NPAR=0
          GO TO 24
        ENDIF
      ENDIF
   23 NDI3=3*NDIM
      DO I=1,NDI3
      IF(INDX(I).NE.0) THEN
      J=I-NDIM*(I/NDIM)
      IF(J.GT.NL) NL=J
      ENDIF
      ENDDO
      RETURN
      END
