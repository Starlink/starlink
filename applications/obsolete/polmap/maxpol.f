      SUBROUTINE MAXPOL(NZONES,CONT_ST,CONT_EN,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,OUT_LU)
C+
C
C Subroutine: 
C
C      M A X P O L
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NZONES (<), CONT_ST (<), CONT_EN (<), NPTS (<), STOKES_I (<),
C STOKES_Q (<), STOKES_QV (<), STOKES_U (<), STOKES_UV (<),
C LAMBDA (<) ,OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C   Finds the maximum polarization in the continuum bins.
C
C
C
C-


C
      IMPLICIT NONE
      INTEGER OUT_LU
C
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
      REAL PMAX,PMAXE,IMAX,QMAX,QMAXE,UMAX,UMAXE
      REAL THETAMAX,THETAMAXE
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
      DO I = 1,NZONES
       CALL LOCATE(LAMBDA,NPTS,CONT_ST(I),ST)
       CALL LOCATE(LAMBDA,NPTS,CONT_EN(I),EN)
       UVTOT = 0.
       PMAX = 0.
       DO J = ST,EN
        ITOT = STOKES_I(J)
        QTOT = STOKES_Q(J)
        QVTOT = STOKES_QV(J)
        UTOT = STOKES_U(J)
        UVTOT = STOKES_UV(J)
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
        IF (P.GT.PMAX) THEN
         PMAX=P
         PMAXE=PE
         IMAX=ITOT
         QMAX=100.*QTOT/ITOT
         UMAX=100.*UTOT/ITOT
         QMAXE=100.*SQRT(QVTOT)/IMAX
         UMAXE=100.*SQRT(UVTOT)/IMAX
         THETAMAX=THETA
         THETAMAXE=TE
        ENDIF
       ENDDO

       WRITE(OUT_LU,10) I,PMAX,PMAXE,THETAMAX,THETAMAXE
       WRITE(OUT_LU,20) QMAX,QMAXE,UMAX,UMAXE
       WRITE(OUT_LU,*) ' '
C       
      ENDDO
666   CONTINUE
      END







