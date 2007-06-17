      SUBROUTINE LOCDAT(MAXPT,X,Y,WORV,NPT,IF,SUBCHK)
      REAL X(MAXPT),Y(MAXPT)
      COMMON/DAT1/FL,W,WS,WV,NF
      COMMON/DAT3/FLMAX
      DIMENSION FL(1000),W(1000)
       LOGICAL SUBCHK
       SUBCHK = .TRUE.
      IF=0
      NFMAX=1000
      IF(NPT.EQ.0) THEN
        SUBCHK = .FALSE.
        IF=1
        WRITE (*,
     :  '(''   ELFOPT:  DIPSO current arrays are empty'')')
        RETURN
      ENDIF
C
      IF(NPT.LE.NFMAX) THEN
        FLMAX=Y(1)
        DO I=1,NPT
          W(I)=X(I)
          IF(Y(I).GT.FLMAX) FLMAX=Y(I)
        ENDDO
        DO I=1,NPT
          FL(I)=100*Y(I)/FLMAX
        ENDDO
        WS=X(1)
        NF=NPT
        WV=WORV
      ELSE
*        WRITE (*,
*     :  '(''   ELFOPT:  number of points ('',
*     :  I<(INT(LOG10(REAL(NPT)))+1)>,
*     :  '') exceeds maximum allowed ('',
*     :  I<(INT(LOG10(REAL(NFMAX)))+1)>,'')'')') NPT, NFMAX
        SUBCHK = .FALSE.
        IF=1
      ENDIF
      RETURN
      END
