      SUBROUTINE FCDEL(VARRAY,MX)
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/KERRS/DELBS,DELPS,DSS
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION FUNS(20),PS(10,10),NPARS(10),IABST(20)
      DIMENSION FLXS(20,10),FMX(10),WSS(10),VARRAY(MX),IFLAG(10)
      DIMENSION DELBS(80,10),DELPS(10,10),DSS(20,10)
      SAVE /KERRS/
      DO I=1,MAXFC
        IFLAG(I)=0
      ENDDO
C  ERROR CHECKING
C
      IF(NUMFIT.EQ.0) THEN
        WRITE(6,103)
  103   FORMAT(3X,'ELFDELC:  fit coefficient stack is empty')
        RETURN
      ENDIF
      DO I=1,MX
        II=NINT(VARRAY(I))
        IF(II.NE.1001) THEN
          IF(II.GT.0.AND.II.LE.MAXFC) THEN
            IF(II.LE.NUMFIT) THEN
              IF(IFLAG(II).EQ.0) THEN
                IFLAG(II)=1
              ELSE
                WRITE(6,102) II
  102           FORMAT(3X,'ELFDELC:  entry',I5,'  is a duplicate')
              ENDIF
            ELSE
              WRITE(6,101) II
  101         FORMAT(3X,'ELFDELC:  entry',I5,'  is not filled')
            ENDIF
          ELSE
            WRITE(6,100) II
  100   FORMAT(3X,'ELFDELC:  entry',I5,'  is not a valid stack number')
          ENDIF
        ENDIF
      ENDDO
C  COMPRESS ARRRAYS
C
      NC=0
      DO I=1,NUMFIT
        IF(IFLAG(I).EQ.0) THEN
          NC=NC+1
          FUNS(NC)=FUNS(I)
          NLS(NC)=NLS(I)
          NPARS(NC)=NPARS(I)
          IABST(NC)=IABST(I)
          FMX(NC)=FMX(I)
          WSS(NC)=WSS(I)
          DO K=1,80
            DO J=1,2
              AS(K,NC,J)=AS(K,I,J)
              INDXS(K,NC,J)=INDXS(K,I,J)
              IRELS(K,NC,J)=IRELS(K,I,J)
            ENDDO
            DELBS(K,NC)=DELBS(K,I)
          ENDDO
          DO K=1,10
            PS(K,NC)=PS(K,I)
            DELPS(K,NC)=DELPS(K,I)
          ENDDO
          DO K=1,20
            FLXS(K,NC)=FLXS(K,I)
            DSS(K,NC)=DSS(K,I)
          ENDDO
        ENDIF
      ENDDO
      NUMFIT=NC
      RETURN
      END
