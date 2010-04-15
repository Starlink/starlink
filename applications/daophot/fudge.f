      SUBROUTINE  FUDGE (FILE, F, MAXCOL)
C
C=======================================================================
C
C This subroutine permits the user to fudge the input image by inserting
C brightness values into rectangular subarrays, or by interpolating
C polynomials inward from the edge of the regions.
C
C             OFFICIAL DAO VERSION:  1991 April 18
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXCOL, MAXPOL, MTERM
      PARAMETER  (MAXPOL=3, MTERM=10)
C
C Parameters
C
C MAXCOL is the width of the largest picture that can be accomodated.
C
C MAXPOL is the largest permitted polynomial order (0 = constant,
C         1 = linear = plane, 2 = quadratic, ...)
C
C MTERM = (MAXPOL+1)*(MAXPOL+2)/2
C
      REAL F(MAXCOL,*), DATA(2)
      REAL C(MTERM,MTERM), V(MTERM), T(MTERM), A(MTERM)
C
      REAL AMAX1, AMIN1
      INTEGER MIN0, MAX0
C
      CHARACTER*30 FILE, NEWFIL, SWITCH
      CHARACTER CASE*1
      REAL X, DX, DY, DYSQ, W
      INTEGER I, J, K, L, LBX, LBY, MBX, MBY, ISTAT, NBORD, NPOLY
      INTEGER NCOL, NROW, N1, N2, LX, LY, NX, NY, NTERM, MX, MY, M
      INTEGER IRMIN
C
      COMMON /SIZE/ NCOL, NROW
C
C-----------------------------------------------------------------------
C
      CALL TBLANK
      NEWFIL=SWITCH(FILE, CASE('f'))
      CALL GETNAM ('Name for output picture:', NEWFIL)
      IF (NEWFIL .EQ. 'END OF FILE') RETURN        ! CTRL-Z was entered
C     CALL COPPIC (NEWFIL, F, NCOL, NROW, ISTAT)
      CALL COPPIC (NEWFIL, ISTAT)
      IF (ISTAT .NE. 0) RETURN                  ! Error creating picture
      CALL TBLANK
      CALL GETDAT ('Border (pixels):', X, 1)
      NBORD = MAX0(0, NINT(X))
      IF (NBORD .GT. 0) THEN
         CALL GETDAT (
     .        'Polynomial order (0 = constant, 1 = plane, etc.):',
     .        X, 1)
         NPOLY = MAX0(0, MIN0(MAXPOL, NINT(X) ) )
      END IF
 1000 CALL TBLANK
 1005 DATA(2)=-1.
      CALL GETDAT ('First, last column number:', DATA, 2)
      IF (DATA(1) .LE. 0.) GO TO 9000
      IF (DATA(2) .LE. 0.) DATA(2)=DATA(1)
      LX=MAX0(1, NINT(AMIN1(DATA(1), DATA(2))) )
      MX=MIN0(NCOL, NINT(AMAX1(DATA(1), DATA(2))) )
 1010 DATA(2)=-1.
      CALL GETDAT ('First, last row number:', DATA, 2)
      IF (DATA(1) .LE. 0.) GO TO 1005
      IF (DATA(2) .LE. 0.) DATA(2)=DATA(1)
      LY=MAX0(1, NINT(AMIN1(DATA(1), DATA(2))) )
      MY=MIN0(NROW, NINT(AMAX1(DATA(1), DATA(2))) )
      IF (NBORD .GT. 0) GO TO 5000
      CALL GETDAT ('Brightness value:', DATA, 1)
      IF (DATA(1) .LE. VAL__MINR) GO TO 1010
      NX = MX-LX+1
      DO I=1,NX
         F(I,1)=DATA(1)
      END DO
      DO 1100 J=LY,MY
      NY=1
 1100 CALL WRARAY ('COPY', LX, J, NX, NY, MAXCOL, F, ISTAT)
      GO TO 1000
C
C-----------------------------------------------------------------------
C
 5000 CONTINUE
      LBX = MAX0(1, LX-NBORD)
      MBX = MIN0(NCOL, MX+NBORD)
      LBY = MAX0(1, LY-NBORD)
      MBY = MIN0(NROW, MY+NBORD)
      NX = MBX-LBX+1
      NY = MBY-LBY+1
      CALL RDARAY ('COPY', LBX, LBY, NX, NY, MAXCOL, F(LBX,LBY), ISTAT)
      NTERM = (NPOLY+2)*(NPOLY+1)/2
C
C Loop over pixels in designated defect.
C
      DO J=LY,MY
         DO I=LX,MX
C
            DO L=1,NTERM
               V(L) = 0.
               DO K=1,NTERM
                  C(K,L) = 0.
               END DO
            END DO
C
C Determine the distance to the NEAREST border pixel, and multiply by
C five.
C
            IF (LX .GT. 1) IRMIN = I-LX
            IF (LX .LT. NCOL) IRMIN = MIN0(IRMIN,MX-I)
            IF (LY .GT. 1) IRMIN = MIN0(IRMIN,J-LY)
            IF (LY .LT. NROW) IRMIN = MIN0(IRMIN, MY-J)
            IRMIN = 5*(IRMIN+1)
C
C Lower y.
C
            IF (LBY .LT. LY) THEN
               DO L=LBY,LY-1
                  DY = REAL(L-J)
                  DYSQ = DY**2
                  DO K=MAX0(LBX,I-IRMIN),MIN0(MBX,I+IRMIN)
                     DX = REAL(K-I)
                     M = 0
                     DO N1=0,NPOLY
                        DO N2=0,N1
                           M = M+1
                           T(M) = 1.
                           IF (N1-N2 .GT. 0) T(M) = DX**(N1-N2)
                           IF (N2 .GT. 0) T(M) = T(M)*DY**N2
                        END DO
                     END DO
C
                     W = 1./(DX**2 + DYSQ)**2
                     DO N1=1,M
                        V(N1) = V(N1) + W * T(N1) * F(K,L)
                        DO N2=1,M
                           C(N1,N2) = C(N1,N2) + W * T(N1) * T(N2)
                        END DO
                     END DO
                  END DO
               END DO
            END IF
C
            DO L=MAX0(LY,J-IRMIN),MIN0(MY,J+IRMIN)
               DY = REAL(L-J)
               DYSQ = DY**2
C
C Lower x
C
               IF (LBX .LT. LX) THEN
                  DO K=LBX,LX-1
                     DX = REAL(K-I)
                     M = 0
                     DO N1=0,NPOLY
                        DO N2=0,N1
                           M = M+1
                           T(M) = 1.
                           IF (N1-N2 .GT. 0) T(M) = DX**(N1-N2)
                           IF (N2 .GT. 0) T(M) = T(M)*DY**N2
                        END DO
                     END DO
C
                     W = 1./(DX**2 + DYSQ)**2
                     DO N1=1,M
                        V(N1) = V(N1) + W * T(N1) * F(K,L)
                        DO N2=1,M
                           C(N1,N2) = C(N1,N2) + W * T(N1) * T(N2)
                        END DO
                     END DO
                  END DO
               END IF
C
C Upper x.
C
               IF (MBX .GT. MX) THEN
                  DO K=MX+1,MBX
                     DX = REAL(K-I)
                     M = 0
                     DO N1=0,NPOLY
                        DO N2=0,N1
                           M = M+1
                           T(M) = 1.
                           IF (N1-N2 .GT. 0) T(M) = DX**(N1-N2)
                           IF (N2 .GT. 0) T(M) = T(M)*DY**N2
                        END DO
                     END DO
C
                     W = 1./(DX**2 + DYSQ)**2
                     DO N1=1,M
                        V(N1) = V(N1) + W * T(N1) * F(K,L)
                        DO N2=1,M
                           C(N1,N2) = C(N1,N2) + W * T(N1) * T(N2)
                        END DO
                     END DO
                  END DO
               END IF
            END DO
C
C Upper y.
C
            IF (MBY .GT. MY) THEN
               DO L=MY+1,MBY
                  DY = REAL(L-J)
                  DYSQ = DY**2
                  DO K=MAX0(LBX,I-IRMIN),MIN0(MBX,I+IRMIN)
                     DX = REAL(K-I)
                     M = 0
                     DO N1=0,NPOLY
                        DO N2=0,N1
                           M = M+1
                           T(M) = 1.
                           IF (N1-N2 .GT. 0) T(M) = DX**(N1-N2)
                           IF (N2 .GT. 0) T(M) = T(M)*DY**N2
                        END DO
                     END DO
C
                     W = 1./(DX**2 + DYSQ)**2
                     DO N1=1,M
                        V(N1) = V(N1) + W * T(N1) * F(K,L)
                        DO N2=1,M
                           C(N1,N2) = C(N1,N2) + W * T(N1) * T(N2)
                        END DO
                     END DO
                  END DO
               END DO
            END IF
C
            CALL INVERS (C, MTERM, NTERM, ISTAT)
            CALL VMUL (C, MTERM, NTERM, V, A)
            F(I,J) = A(1)
         END DO
      END DO
      NX = MX-LX+1
      NY = MY-LY+1
      CALL WRARAY ('COPY', LX, LY, NX, NY, MAXCOL, F(LX,LY), ISTAT)
      GO TO 1000
C
 9000 CONTINUE
      CALL CLPIC ('COPY')
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      RETURN
      END!
