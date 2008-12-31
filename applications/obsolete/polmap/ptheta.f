      SUBROUTINE PTHETA(NZONES,CONT_ST,CONT_EN,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,
     &                  STAT_Q_MEAN,STAT_U_MEAN,OUT_LU)
C+
C
C Subroutine: 
C
C     P T H E T A
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NZONES (<), CONT_ST (<), CONT_EN (<), NPTS (<), STOKES_I (<),
C STOKES_Q (<), STOKES_QV (<), STOKES_U (<), STOKES_UV (<), LAMBDA (<),
C STAT_Q_MEAN (>), STAT_U_MEAN (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C This routine calculates the polarization in the pre-defined continuum
C bins.
C
C
C
C-
C
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL STAT_Q_MEAN,STAT_U_MEAN
C
C The current spectrum
C
      INTEGER NPTS
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
C
C The continuum zones
C
      INTEGER NZONES
      REAL CONT_ST(*)
      REAL CONT_EN(*)
C
C Misc.
C
      REAL P,THETA
      REAL PE,TE,X
      REAL ITOT,QTOT,UTOT,QVTOT,UVTOT
      REAL QQ,UU,QQE,UUE
      REAL Q_MEAN,QE_MEAN,U_MEAN,UE_MEAN,I_MEAN
      INTEGER ST,EN,I,J
C
C Formats
C
10     FORMAT(1X,I2,' P(%) = ',F7.3,' +-',F6.3,
     &        ' THETA =  ',F5.1,' +-',F4.1)
20     FORMAT(3X,' Q(%) = ',F7.3,' +-',F6.3,' U(%) = ',F7.3,' +-',F6.3)

      IF (NZONES.EQ.0) THEN
       CALL WR_ERROR('No bins defined',OUT_LU)
       GOTO 666
      ENDIF
C
      Q_MEAN = 0.
      QE_MEAN = 0.
      U_MEAN = 0.
      UE_MEAN = 0.
      I_MEAN = 0.
C
      DO I = 1,NZONES
       CALL LOCATE(LAMBDA,NPTS,CONT_ST(I),ST)
       CALL LOCATE(LAMBDA,NPTS,CONT_EN(I),EN)
       ITOT = 0.
       QTOT = 0.
       QVTOT = 0.
       UTOT = 0.
       UVTOT = 0.
       DO J = ST,EN
        ITOT = ITOT+STOKES_I(J)
        QTOT = QTOT+STOKES_Q(J)
        QVTOT = QVTOT+STOKES_QV(J)
        UTOT = UTOT+STOKES_U(J)
        UVTOT = UVTOT+STOKES_UV(J)
       ENDDO
       THETA  =  ATAN2(UTOT,QTOT) * 90.0/3.1415926
       IF (THETA .LT. 0.0) THETA  =  THETA + 180.0  
       QQ  =  100.0*QTOT/ITOT
       UU  =  100.0*UTOT/ITOT
       QQE  =  100.0*SQRT(QVTOT)/ITOT
       UUE  =  100.0*SQRT(UVTOT)/ITOT
       P  =  SQRT(QQ*QQ+UU*UU)
       THETA  =  ATAN2(UU,QQ) * 90.0/3.1415926
       IF (THETA .LT. 0.0) THETA  =  THETA + 180.0  
       IF (P .GT. 0.0) THEN
             X  =  QQE*QQE*QQ*QQ + UUE*UUE*UU*UU
             PE  =  SQRT(X)/P  
             X  =  QQ*QQ*UUE*UUE + UU*UU*QQE*QQE
             X  =  0.5*SQRT(X)
             X  =  X/(P*P)
             TE  =  ABS(X*57.2958)
         ELSE
             PE  =  0.0
             TE  =  0.0
       ENDIF
       WRITE(OUT_LU,10) I,P,PE,THETA,TE
       WRITE(OUT_LU,20) QQ,QQE,UU,UUE
       WRITE(OUT_LU,*) ' '
C       
       I_MEAN=I_MEAN+ITOT
       Q_MEAN=Q_MEAN+QTOT
       QE_MEAN=QE_MEAN+QVTOT
       U_MEAN=U_MEAN+UTOT
       UE_MEAN=UE_MEAN+UVTOT
      ENDDO
      CALL GET_ERRORS(I_MEAN,Q_MEAN,SQRT(QE_MEAN),U_MEAN,
     &                SQRT(UE_MEAN),
     &                P,PE,THETA,TE)
      WRITE(*,*) ' Mean over all bins:'
30     FORMAT(3X,' P(%) = ',F7.3,' +-',F6.3,
     &        ' THETA =  ',F5.1,' +-',F4.1)
      WRITE(OUT_LU,30) P,PE,THETA,TE
      WRITE(OUT_LU,20) 100.*Q_MEAN/I_MEAN,100.*SQRT(QE_MEAN)/I_MEAN,
     &                 100.*U_MEAN/I_MEAN,100.*SQRT(UE_MEAN)/I_MEAN
      STAT_Q_MEAN=Q_MEAN/I_MEAN
      STAT_U_MEAN=U_MEAN/I_MEAN
666   CONTINUE
      END







