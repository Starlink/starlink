

      FUNCTION SIGMA(SUM,SUMSQ,N)

*
*                  S I G M A
*
*       SIGMA IS GIVEN THE SUM AND THE SUM OF SQUARES OF N MEASUREMENTS
*       OF A RANDOM VARIABLE AND IT THEN CALCULATES THE BEST ESTIMATE
*       OF THE STANDARD DEVIATION.
*

      IMPLICIT NONE

      DOUBLE PRECISION SIGMA,SUM,SUMSQ

      INTEGER N

      SIGMA=SQRT((SUMSQ-((SUM*SUM)/N))/(N*(N-1)))

      END


C+
      SUBROUTINE POL_INIT
C
C                         P O L _ I N I T
C
C  Routine name:
C     POL_INIT
C
C  Function:
C     Initialize arrays for polarimetry reduction
C
C  Description:
C     Zero the arrays used to hold the cumulative polarization data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL POL_INIT
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C  Support:
C     Jeremy Bailey (JAC)
C
C  Version date:
C     28/3/1989
C
C-
C
C  Subroutine / function details:
C
C  History:
C     28/3/89   Initial Version   JAB (JACH)
C


      IMPLICIT NONE

      INCLUDE 'POL_PAR'

      INTEGER I,CHAN

      DO CHAN = 1,MAXCHANS
          DO I=1,3
              STOKES(I,CHAN)=0.0D0
              STOKESQ(I,CHAN)=0.0D0
              STOKESC(I,CHAN)=0.0D0
              NCYC=0
          ENDDO
      ENDDO

      END


C+
      SUBROUTINE POL_ADD(NCHANS,RAW,FACTOR)
C
C                         P O L _ A D D
C
C  Routine name:
C     POL_ADD
C
C  Function:
C     Add a new set of polarization data.
C
C  Description:
C     The Stokes parameters for the new raw data set are calculated and
C     added into the cumulative arrays.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL POL_ADD(NCHANS,RAW,FACTOR)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C     (>)  NCHANS         (Integer,ref) Number of channels of data.
C     (>)  RAW(8,NCHANS)  (Double,ref) Array of raw data values for each
C                          of eight waveplate positions.
C     (>)  FACTOR         (Double,ref) Normalization factor for stokes
C                          parameters.
C
C
C  Support:
C     Jeremy Bailey (JAC)
C
C  Version date:
C     28/3/1989
C
C-
C
C  Subroutine / function details:
C
C  History:
C     28/3/89   Initial Version   JAB (JACH)
C

      IMPLICIT NONE

      INTEGER NCHANS
      DOUBLE PRECISION RAW(8,NCHANS)
      DOUBLE PRECISION FACTOR

      INCLUDE 'POL_PAR'

      DOUBLE PRECISION I,Q,U
      INTEGER CHAN
      INTEGER NC

      NC = MIN(NCHANS,MAXCHANS)
      NCYC=NCYC+1
      DO CHAN = 1,NC
         I=RAW(1,CHAN)+RAW(2,CHAN)+RAW(3,CHAN)+RAW(4,CHAN)+
     :       RAW(5,CHAN)+RAW(6,CHAN)+RAW(7,CHAN)+RAW(8,CHAN)
         I=I/8.0D0
         Q=RAW(1,CHAN)-RAW(3,CHAN)+RAW(5,CHAN)-RAW(7,CHAN)
         Q=Q/(4D0*FACTOR)
         U=RAW(2,CHAN)-RAW(4,CHAN)+RAW(6,CHAN)-RAW(8,CHAN)
         U=U/(4D0*FACTOR)

*  Cumulative results

         STOKES(1,CHAN)=STOKES(1,CHAN)+I
         STOKES(2,CHAN)=STOKES(2,CHAN)+Q
         STOKES(3,CHAN)=STOKES(3,CHAN)+U
         STOKESQ(1,CHAN)=STOKESQ(1,CHAN)+(I*I)
         STOKESQ(2,CHAN)=STOKESQ(2,CHAN)+(Q*Q)
         STOKESQ(3,CHAN)=STOKESQ(3,CHAN)+(U*U)

*  Cycle results

         STOKESC(1,CHAN)=I
         STOKESC(2,CHAN)=Q
         STOKESC(3,CHAN)=U

         CSTOKES(1,CHAN,NCYC)=I
         CSTOKES(2,CHAN,NCYC)=Q
         CSTOKES(3,CHAN,NCYC)=U

      ENDDO
      END

C+
      SUBROUTINE POL_SUBTRACT(NCHANS,RAW,FACTOR)
C
C                         P O L _ S U B T R A C T
C
C  Routine name:
C     POL_SUBTRACT
C
C  Function:
C     Subtract a set of polarization data.
C
C  Description:
C     The Stokes parameters for the raw data set are calculated and
C     subtracted from the cumulative arrays. The routine can be used to
C     remove a data set that has previously been added.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL POL_SUBTRACT(NCHANS,RAW,FACTOR)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C     (>)  NCHANS         (Integer,ref) Number of channels of data.
C     (>)  RAW(8,NCHANS)  (Double,ref) Array of raw data values for each
C                          of eight waveplate positions.
C     (>)  FACTOR         (Double,ref) Normalization factor for stokes
C                          parameters.
C
C
C  Support:
C     Jeremy Bailey (JAC)
C
C  Version date:
C     18/4/1989
C
C-
C
C  Subroutine / function details:
C
C  History:
C     18/4/89   Initial Version   JAB (JACH)
C

      IMPLICIT NONE

      INTEGER NCHANS
      DOUBLE PRECISION RAW(8,NCHANS)
      DOUBLE PRECISION FACTOR

      INCLUDE 'POL_PAR'

      DOUBLE PRECISION I,Q,U
      INTEGER CHAN
      INTEGER NC

      NC = MIN(NCHANS,MAXCHANS)
      NCYC=NCYC-1
      DO CHAN = 1,NC
         I=RAW(1,CHAN)+RAW(2,CHAN)+RAW(3,CHAN)+RAW(4,CHAN)+
     :       RAW(5,CHAN)+RAW(6,CHAN)+RAW(7,CHAN)+RAW(8,CHAN)
         I=I/8.0D0
         Q=RAW(1,CHAN)-RAW(3,CHAN)+RAW(5,CHAN)-RAW(7,CHAN)
         Q=Q/(4D0*FACTOR)
         U=RAW(2,CHAN)-RAW(4,CHAN)+RAW(6,CHAN)-RAW(8,CHAN)
         U=U/(4D0*FACTOR)

*  Cumulative results

         STOKES(1,CHAN)=STOKES(1,CHAN)-I
         STOKES(2,CHAN)=STOKES(2,CHAN)-Q
         STOKES(3,CHAN)=STOKES(3,CHAN)-U
         STOKESQ(1,CHAN)=STOKESQ(1,CHAN)-(I*I)
         STOKESQ(2,CHAN)=STOKESQ(2,CHAN)-(Q*Q)
         STOKESQ(3,CHAN)=STOKESQ(3,CHAN)-(U*U)

      ENDDO
      END



C+
      SUBROUTINE POL_REDUCE(NCHANS,COUNT,COUNTERR,MAG,MAGERR,Q,QERR,U,
     :                      UERR,POL,POLERR,PA,PAERR)
C
C                         P O L _ R E D U C E
C
C  Routine name:
C     POL_REDUCE
C
C  Function:
C     Reduce polarization data
C
C  Description:
C     Using the accumulated stokes parameters, calculate the final results
C     and errors.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL POL_REDUCE(NCHANS,COUNT,COUNTERR,MAG,MAGERR,Q,QERR,U,UERR,POL,
C             POLERR,PA,PAERR)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>)  NCHANS            (Integer,ref) Number of channels.
C     (<)  COUNT             (Double,ref) The total counts.
C     (<)  COUNTERR          (Double,ref) The error on the counts.
C     (<)  MAG               (Double,ref) The instrumental magnitude.
C     (<)  MAGERR            (Double,ref) The error on the magnitude.
C     (<)  Q                 (Double,ref) The percentage Q stokes parameter.
C     (<)  QERR              (Double,ref) The error on the Q stokes parameter.
C     (<)  U                 (Double,ref) The percentage U stokes parameter.
C     (<)  UERR              (Double,ref) The error on the U stokes parameter.
C     (<)  POL               (Double,ref) The percentage polarization.
C     (<)  POLERR            (Double,ref) The error on the polarization.
C     (<)  PA                (Double,ref) The position angle in degrees.
C     (<)  PAERR             (Double,ref) The error on the position angle.
C
C
C
C  Support:
C     Jeremy Bailey (JAC)
C
C  Version date:
C     28/3/1989
C
C-
C
C  Subroutine / function details:
C
C  History:
C     28/3/89   Initial Version   JAB (JACH)
C


      IMPLICIT NONE

      INTEGER NCHANS
      DOUBLE PRECISION COUNT(NCHANS),COUNTERR(NCHANS),MAG(NCHANS),
     :          MAGERR(NCHANS),Q(NCHANS),QERR(NCHANS),U(NCHANS),
     :          UERR(NCHANS),POL(NCHANS),POLERR(NCHANS),
     :          PA(NCHANS),PAERR(NCHANS)


      INCLUDE 'POL_PAR'

      DOUBLE PRECISION INTENS(3),INTENSERR(3),X,XX
      INTEGER J
      INTEGER CHAN
      INTEGER NC

*  Function
      DOUBLE PRECISION SIGMA

      REAL DEGRAD

      DEGRAD = 45.0/ATAN(1.0)
      NC = MIN(NCHANS,MAXCHANS)
      DO CHAN = 1,NC
         DO J=1,3
            INTENS(J)=STOKES(J,CHAN)/NCYC
            IF (NCYC .EQ. 1) THEN
               INTENSERR(J)=0.0D0
            ELSE
               INTENSERR(J)=SIGMA(STOKES(J,CHAN),STOKESQ(J,CHAN),NCYC)
            ENDIF
         ENDDO

            IF (INTENS(1) .LE. 0.0D0) THEN
               Q(CHAN)=0.0D0
               QERR(CHAN)=0.0D0
               U(CHAN)=0.0D0
               UERR(CHAN)=0.0D0
            ELSE
               Q(CHAN)=(INTENS(2)/INTENS(1))*100.D0
               U(CHAN)=(INTENS(3)/INTENS(1))*100.D0
               X=(INTENSERR(1)*INTENSERR(1))/(INTENS(1)*INTENS(1))
               IF (INTENS(2) .NE. 0.0D0) THEN
                 XX=X+((INTENSERR(2)*INTENSERR(2))/
     :                    (INTENS(2)*INTENS(2)))
               ELSE
                 XX=0.0D0
               ENDIF
               IF (XX .GT. 0.0D0) THEN
                 QERR(CHAN)=ABS(Q(CHAN)*SQRT(XX))
               ELSE
                 QERR(CHAN)=0.0D0
               ENDIF
               IF (INTENS(3) .NE. 0.0D0) THEN
                 XX=X+((INTENSERR(3)*INTENSERR(3))/
     :                    (INTENS(3)*INTENS(3)))
               ELSE
                 XX=0.0D0
               ENDIF
               IF (XX .GT. 0.0D0) THEN
                 UERR(CHAN)=ABS(U(CHAN)*SQRT(XX))
               ELSE
                 UERR(CHAN)=0.0D0
               ENDIF
            ENDIF


            POL(CHAN)=SQRT((Q(CHAN)*Q(CHAN))+(U(CHAN)*U(CHAN)))
            IF (Q(CHAN) .NE. 0.0) THEN
               PA(CHAN)=DEGRAD*ATAN2(U(CHAN),Q(CHAN))
               IF (PA(CHAN) .LT. 0.0D0) PA(CHAN)=PA(CHAN)+360.0D0
               PA(CHAN)=PA(CHAN)/2.0D0
            ELSE
               PA(CHAN)=0.0D0
            ENDIF
            IF (POL(CHAN) .GT. 1D-4) THEN
               X=(Q(CHAN)*Q(CHAN)*QERR(CHAN)*QERR(CHAN))+
     :           (U(CHAN)*U(CHAN)*UERR(CHAN)*UERR(CHAN))
               POLERR(CHAN)=SQRT(X)/POL(CHAN)
               X=(Q(CHAN)*Q(CHAN)*UERR(CHAN)*UERR(CHAN))+
     :           (U(CHAN)*U(CHAN)*QERR(CHAN)*QERR(CHAN))
               X=0.5D0*SQRT(X)
               X=X/(POL(CHAN)*POL(CHAN))
               PAERR(CHAN)=ABS(X*57.2958D0)
            ELSE
               POLERR(CHAN)=0.0D0
               PAERR(CHAN)=0.0D0
            ENDIF

            COUNT(CHAN)=INTENS(1)
            COUNTERR(CHAN)=INTENSERR(1)

            IF (INTENS(1) .GT. 0) THEN
               MAG(CHAN)=-2.5*LOG10(INTENS(1))
               X=INTENS(1)+INTENSERR(1)
               XX=INTENS(1)-INTENSERR(1)
               IF (XX .GT. 0.0D0) THEN
                  MAGERR(CHAN)=1.25*LOG10(X/XX)
               ELSE
                  MAGERR(CHAN)=0.0D0
               ENDIF
            ELSE
               MAG(CHAN)=99.99D0
               MAGERR(CHAN)=99.99D0
            ENDIF
         ENDDO
      END


C+
      SUBROUTINE POL_CYCLE(NCHANS,COUNT,MAG,Q,U,POL,PA)
C
C                         P O L _ C Y C L E
C
C  Routine name:
C     POL_CYCLE
C
C  Function:
C     Reduce polarization data for last cycle
C
C  Description:
C     Using the last cycles stokes parameters, calculate the final results.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL POL_CYCLE(NCHANS,COUNT,MAG,Q,U,POL,PA)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>)  NCHANS            (Integer,ref) Number of channels.
C     (<)  COUNT             (Double,ref) The total counts.
C     (<)  MAG               (Double,ref) The instrumental magnitude.
C     (<)  Q                 (Double,ref) The percentage Q stokes parameter.
C     (<)  U                 (Double,ref) The percentage U stokes parameter.
C     (<)  POL               (Double,ref) The percentage polarization.
C     (<)  PA                (Double,ref) The position angle in degrees.
C
C
C
C  Support:
C     Jeremy Bailey (JAC)
C
C  Version date:
C     17/4/1989
C
C-
C
C  Subroutine / function details:
C
C  History:
C     17/4/89   Initial Version   JAB (JACH)
C


      IMPLICIT NONE

      INTEGER NCHANS
      DOUBLE PRECISION COUNT(NCHANS),MAG(NCHANS),
     :          Q(NCHANS),U(NCHANS),
     :          POL(NCHANS),PA(NCHANS)


      INCLUDE 'POL_PAR'

      DOUBLE PRECISION INTENS(3)
      INTEGER J
      INTEGER CHAN
      INTEGER NC
      REAL DEGRAD

      DEGRAD = 45.0/ATAN(1.0)
      NC = MIN(NCHANS,MAXCHANS)
      DO CHAN = 1,NC
         DO J=1,3
            INTENS(J)=STOKESC(J,CHAN)
         ENDDO

            IF (INTENS(1) .LE. 0.0D0) THEN
               Q(CHAN)=0.0D0
               U(CHAN)=0.0D0
            ELSE
               Q(CHAN)=(INTENS(2)/INTENS(1))*100.D0
               U(CHAN)=(INTENS(3)/INTENS(1))*100.D0
            ENDIF


            POL(CHAN)=SQRT((Q(CHAN)*Q(CHAN))+(U(CHAN)*U(CHAN)))
            IF (Q(CHAN) .NE. 0.0) THEN
               PA(CHAN)=ATAN2(U(CHAN)/DEGRAD,Q(CHAN)/DEGRAD)
               IF (PA(CHAN) .LT. 0.0D0) PA(CHAN)=PA(CHAN)+360.0D0
               PA(CHAN)=PA(CHAN)/2.0D0
            ELSE
               PA(CHAN)=0.0D0
            ENDIF

            COUNT(CHAN)=INTENS(1)

            IF (INTENS(1) .GT. 0) THEN
               MAG(CHAN)=-2.5*LOG10(INTENS(1))
            ELSE
               MAG(CHAN)=99.99D0
            ENDIF
         ENDDO
      END




      SUBROUTINE POL_DESPIKE(NCHANS,NSIGMA)

      IMPLICIT NONE
      INTEGER NCHANS
      REAL NSIGMA

      INCLUDE 'POL_PAR'

      INTEGER J,K
      DOUBLE PRECISION I,IE,Q,QE,U,UE

      DOUBLE PRECISION SIGMA

      DO J=1,NCHANS
          I = STOKES(1,J)/NCYC
          IE = SIGMA(STOKES(1,J),STOKESQ(1,J),NCYC)
          Q = STOKES(2,J)/NCYC
          QE = SIGMA(STOKES(2,J),STOKESQ(2,J),NCYC)
          U = STOKES(3,J)/NCYC
          UE = SIGMA(STOKES(3,J),STOKESQ(3,J),NCYC)
          DO K=1,NCYC
              IF ((ABS(CSTOKES(2,J,K) - Q) .GT. NSIGMA*QE) .OR.
     :            (ABS(CSTOKES(3,J,K) - U) .GT. NSIGMA*UE)) THEN
                  PRINT *,'Removing spike from channel ',J,' ',K
                  STOKES(1,J) = STOKES(1,J) - CSTOKES(1,J,K)
                  STOKES(1,J) = STOKES(1,J) * (REAL(NCYC)/(NCYC-1.0))
                  STOKESQ(1,J)=STOKESQ(1,J) -
     :                  CSTOKES(1,J,K)*CSTOKES(1,J,K)
                  CSTOKES(1,J,K) = STOKES(1,J)/NCYC
                  STOKESQ(1,J)=STOKESQ(1,J) * (REAL(NCYC)/(NCYC-1.0))
                  STOKES(2,J) = STOKES(2,J) - CSTOKES(2,J,K)
                  STOKES(2,J) = STOKES(2,J) * (REAL(NCYC)/(NCYC-1.0))
                  STOKESQ(2,J)=STOKESQ(2,J) -
     :                  CSTOKES(2,J,K)*CSTOKES(2,J,K)
                  CSTOKES(2,J,K) = STOKES(2,J)/NCYC
                  STOKESQ(2,J)=STOKESQ(2,J) * (REAL(NCYC)/(NCYC-1.0))
                  STOKES(3,J) = STOKES(3,J) - CSTOKES(3,J,K)
                  STOKES(3,J) = STOKES(3,J) * (REAL(NCYC)/(NCYC-1.0))
                  STOKESQ(3,J)=STOKESQ(3,J) -
     :                  CSTOKES(3,J,K)*CSTOKES(3,J,K)
                  CSTOKES(3,J,K) = STOKES(3,J)/NCYC
                  STOKESQ(3,J)=STOKESQ(3,J) * (REAL(NCYC)/(NCYC-1.0))
              ENDIF
          ENDDO
      ENDDO
      END
