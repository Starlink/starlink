C+
      SUBROUTINE PTHETA(STATUS)
C
C            P T H E T A
C
C     Command name:
C        PTHETA
C
C     Function:
C        Output the P and Theta values for a polarization spectrum
C
C     Description:
C        The polarization and position angle corresponding to a
C        specified wavelength range of a polarization spectrum are
C        calculated and output.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D) - The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C    (2) LSTART     (Real)    - The starting wavelength for the section
C                               to be used.
C    (3) LEND       (Real)    - The end wavelength for the section.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 15/6/1988
C
C-
C
C  History:
C    15/6/1988   Original Version.   JAB/AAO
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INTEGER STATUS
      INTEGER IPTR,QPTR,UPTR,QEPTR,UEPTR,LPTR
      INTEGER SIZE,DIMS(3),ACTDIM
      CHARACTER*(DAT__SZLOC) LOC,IDLOC,QDLOC,UDLOC,QELOC,UELOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,T1LOC,T2LOC,LLOC
      REAL LSTART,LEND

      INTEGER ICH_LEN

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(1)

*  Get the Stokes parameter objects

      CALL TSP_GET_STOKES(LOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(LOC,'U',ULOC,STATUS)

*  Map the data

      CALL TSP_MAP_DATA(LOC,'READ',IPTR,IDLOC,STATUS)
      CALL TSP_MAP_DATA(QLOC,'READ',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_VAR(QLOC,'READ',QEPTR,QELOC,STATUS)
      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)
      CALL TSP_MAP_VAR(ULOC,'READ',UEPTR,UELOC,STATUS)

*  Map the wavelength array

      CALL TSP_MAP_LAMBDA(LOC,'READ',LPTR,LLOC,STATUS)

*  Get the wavelength range

      CALL PAR_GET0R('LSTART',LSTART,STATUS)
      CALL PAR_GET0R('LEND',LEND,STATUS)

*  Calculate the results

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_PTHETA(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(QEPTR),
     :   %VAL(UPTR),%VAL(UEPTR),LSTART,LEND,%VAL(LPTR),STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL TSP_UNMAP(QELOC,STATUS)
      CALL TSP_UNMAP(UELOC,STATUS)
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END


      SUBROUTINE TSP_PTHETA(SIZE,I,Q,QE,U,UE,LSTART,LEND,L,STATUS)
*
*  Subroutine to calculate the polarization and position angle
*
      IMPLICIT NONE
      INTEGER SIZE
      REAL I(SIZE),Q(SIZE),QE(SIZE),U(SIZE),UE(SIZE),L(SIZE)
      REAL LSTART, LEND
      REAL II,QQ,UU,QQE,UUE,P,PE,THETA,TE,X
      INTEGER NP
      INTEGER STATUS
      INTEGER CHAN

      NP = 0
      II = 0.0
      QQ = 0.0
      UU = 0.0
      QQE = 0.0
      UUE = 0.0
      DO CHAN = 1,SIZE
          IF (L(CHAN) .GE. LSTART .AND. L(CHAN) .LE. LEND) THEN
              NP = NP+1
              QQ = QQ + Q(CHAN)
              QQE = QQE + QE(CHAN)
              UU = UU + U(CHAN)
              UUE = UUE + UE(CHAN)
              II = II + I(CHAN)
          ENDIF
      ENDDO
      IF (NP .GE. 1) THEN
         QQ = 100.0*QQ/II
         UU = 100.0*UU/II
         QQE = 100.0*SQRT(QQE)/II
         UUE = 100.0*SQRT(UUE)/II
         P = SQRT(QQ*QQ+UU*UU)
         THETA = ATAN2(UU,QQ) * 90.0/3.1415926
         IF (THETA .LT. 0.0) THETA = THETA + 180.0
         IF (P .GT. 0.0) THEN
             X = QQE*QQE*QQ*QQ + UUE*UUE*UU*UU
             PE = SQRT(X)/P
             X = QQ*QQ*UUE*UUE + UU*UU*QQE*QQE
             X = 0.5*SQRT(X)
             X = X/(P*P)
             TE = ABS(X*57.2958)
         ELSE
             PE = 0.0
             TE = 0.0
         ENDIF
         CALL MSG_SETR('Q',QQ)
         CALL MSG_SETR('U',UU)
         CALL MSG_SETR('QE',QQE)
         CALL MSG_SETR('UE',UUE)
         CALL MSG_OUT(' ',' Q = ^Q (^QE)  U = ^U (^UE)',STATUS)
         CALL MSG_SETR('P',P)
         CALL MSG_SETR('PE',PE)
         CALL MSG_SETR('THETA',THETA)
         CALL MSG_SETR('TE',TE)
         CALL MSG_OUT(' ',' P = ^P (^PE)  THETA = ^THETA (^TE)',STATUS)
      ELSE
         CALL MSG_OUT(' ','No Data in Specified Wavelength Range',
     :       STATUS)
      ENDIF
      END



