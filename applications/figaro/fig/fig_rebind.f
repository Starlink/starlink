      SUBROUTINE FIG_REBIND(IMODE,IQUAD,WDATA,NPIX,RBIN,NRBIN,NADD,
     :                           SSKEW,CFLUX,WAVES,WAVESR,LOGW,LOGWR)
C
C See comments for FIG_REBIN.  This routine is exactly the same,
C except that the wavelength arrays (WAVES, WAVESR) are double precision.
C
C KS / AAO 12th Sept 1985
C
C Modified: 30th Dec 1985 KS/AAO Same change as for FIG_REBIN for data
C           outside the range of the input array.
C           28th Sep 1992 HME / UoE, Starlink.  TABs removed.
C           26th Jul 1994 Make common blocks SAVE. HME / UoE, Starlink.
C
C Declaration of arguments.
      LOGICAL CFLUX,LOGW,LOGWR
      REAL*4 WDATA(NPIX),RBIN(NRBIN)
      DOUBLE PRECISION WAVES(NPIX),WAVESR(NRBIN)
C Declaration of variables.
      REAL*4 WDATA1,WDATA2,WDATA3
      REAL*8 DX,A,B,C,D,DD,DDD,Y,RX2,X1,X2
      LOGICAL LSTART
C For communication with FIG_TFORMD
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
      SAVE /REBIN_INFO/
C
C     Copy arguments into common for FIG_TFORMD
C
      LOGS=LOGW
      LOGSR=LOGWR
      NBIN=NPIX
      NBINR=NRBIN
C
      LSTART=.TRUE.
      IF(IMODE.NE.0) THEN
            UP=WAVES(NPIX).GT.WAVES(1)
            IF (UP) THEN
               REVFAC=1.
            ELSE
               REVFAC=-1.
            END IF
            IX=1
            RX2=0.5
            CALL FIG_TFORMD(RX2,WAVES,WAVESR,IX,X1)
            NSGN=1
            IF (WAVESR(NRBIN).LT.WAVESR(1)) NSGN=-1
            NSTOP=NRBIN
      ELSE
            X1=-SSKEW+0.5
            DX=NADD
            NSGN=1
            NSTOP=(NPIX-1)/NADD+1
            REVFAC=1.
      END IF
      J1=NINT(X1)
      DO K=1,NSTOP
         IF(IMODE.NE.0) THEN
               RX2=RX2+1.
               CALL FIG_TFORMD(RX2,WAVES,WAVESR,IX,X2)
               DX=X2-X1
         ELSE
               X2=X1+NADD
         END IF
         J2=NINT(X2)
         D=0
         IF(IQUAD.NE.0) THEN
               IF(LSTART) THEN
                     LSTART=.FALSE.
                     IF (((J1-1).LE.0).OR.((J1-1).GT.NPIX)) THEN
                        WDATA1=0.0
                     ELSE
                        WDATA1=WDATA(J1-1)
                     END IF
                     IF ((J1.LE.0).OR.(J1.GT.NPIX)) THEN
                        WDATA2=0.0
                     ELSE
                        WDATA2=WDATA(J1)
                     END IF
                     IF (((J1+1).LE.0).OR.((J1+1).GT.NPIX)) THEN
                        WDATA3=0.0
                     ELSE
                        WDATA3=WDATA(J1+1)
                     END IF
                     A=(WDATA1+WDATA3)*0.5D0
                     B=(A-WDATA1)*0.5D0
                     C=(13.0D0/12.0D0)*WDATA2-A/12.0D0
                     A=(A-WDATA2)/3.0D0
                     Y=X1-J1
                     DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)+
     X                                         A*0.125D0+C*0.5D0
               END IF
               IF (((J2-1).LE.0).OR.((J2-1).GT.NPIX)) THEN
                  WDATA1=0.0
               ELSE
                  WDATA1=WDATA(J2-1)
               END IF
               IF ((J2.LE.0).OR.(J2.GT.NPIX)) THEN
                  WDATA2=0.0
               ELSE
                  WDATA2=WDATA(J2)
               END IF
               IF (((J2+1).LE.0).OR.((J2+1).GT.NPIX)) THEN
                  WDATA3=0.0
               ELSE
                  WDATA3=WDATA(J2+1)
               END IF
               A=(WDATA1+WDATA3)*0.5D0
               B=(A-WDATA1)*0.5D0
               C=1.083333333333333D0*WDATA2-A*
     X                                  0.08333333333333333D0
               A=(A-WDATA2)*0.3333333333333333D0
               Y=X2-J2
               D=D-DD
               DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)
               DDD=A*0.125D0+C*0.5D0
               D=D+DD-DDD
               DD=DD+DDD
         ELSE
               IF(LSTART) THEN
                     LSTART=.FALSE.
                     IF ((J1.LE.0).OR.(J1.GT.NPIX)) THEN
                        DD=0.0
                     ELSE
                        DD=(NSGN*(J1-X1)-0.5D0)*WDATA(J1)
                     END IF
               END IF
               IF ((J2.LE.0).OR.(J2.GT.NPIX)) THEN
                  DDD=0.0
               ELSE
                  DDD=WDATA(J2)
               END IF
               D=D+DD
               DD=(NSGN*(J2-X2)-0.5D0)*DDD
               D=D-DD-DDD
         END IF
         DO KK=J1,J2,NSGN
            IF ((KK.GE.1).AND.(KK.LE.NPIX)) D=D+WDATA(KK)
         END DO
         IF (.NOT.CFLUX) THEN
            RBIN(K)=D/ABS(DX)*REVFAC
         ELSE
            RBIN(K)=D*REVFAC
         END IF
         X1=X2
         J1=J2
      END DO
C
      END
