      SUBROUTINE KURV1S (N,X,Y,SLOP1,SLOPN,XP,YP,TEMP,S,SIGMA,ISLPSW)
C
C
C DIMENSION OF           X(N),Y(N),XP(N),YP(N),TEMP(N)
C ARGUMENTS
C
C LATEST REVISION        JUNE 1979
C
C PURPOSE                KURV1S DETERMINES THE PARAMETERS NECESSARY TO
C                        COMPUTE A SPLINE UNDER TENSION PASSING THROUGH
C                        A SEQUENCE OF PAIRS
C                        (X(1),Y(1)),...,(X(N),Y(N)) IN THE PLANE.
C                        THE SLOPES AT THE TWO ENDS OF THE CURVE MAY BE
C                        SPECIFIED OR OMITTED.  FOR ACTUAL COMPUTATION
C                        OF POINTS ON THE CURVE IT IS NECESSARY TO CALL
C                        THE SUBROUTINE KURV2.
C
C USAGE                  CALL KURV1S(N,X,Y,SLP1,SLPN,XP,YP,TEMP,S,SIGMA)
C
C ARGUMENTS
C
C ON INPUT               N
C                          IS THE NUMBER OF POINTS TO BE INTERPOLATED
C                          (N .GE. 2).
C
C                        X
C                          IS AN ARRAY CONTAINING THE N X-COORDINATES
C                          OF THE POINTS.
C
C                        Y
C                          IS AN ARRAY CONTAINING THE N Y-COORDINATES
C                          OF THE POINTS.
C
C                        SLOP1 AND SLOPN
C                          CONTAIN THE DESIRED VALUES FOR THE SLOPE OF
C                          THE CURVE AT (X(1),Y(1)) AND (X(N),Y(N)),
C                          RESPECTIVELY.  THESE QUANTITIES ARE IN
C                          DEGREES AND MEASURED COUNTER-CLOCKWISE
C                          FROM THE POSITIVE X-AXIS.  THE POSITIVE
C                          FROM THE POSITIVE X-AXIS.  IF ISLPSW IS NON-
C                          ZERO, ONE OR BOTH OF SLP1 AND SLPN MAY BE
C                          DETERMINED INTERNALLY BY KURV1S.
C
C                        XP AND YP
C                          ARE ARRAYS OF LENGTH AT LEAST N.
C
C                        TEMP
C                          IS AN ARRAY OF LENGTH AT LEAST N WHICH IS
C                          USED FOR SCRATCH STORAGE.
C
C                        SIGMA
C                          CONTAINS THE TENSION FACTOR.  THIS IS
C                          NON-ZERO AND INDICATES THE CURVINESS DESIRED.
C                          IF ABS(SIGMA) IS VERY LARGE (E.G., 50.) THE
C                          RESULTING CURVE IS VERY NEARLY A POLYGONAL
C                          LINE.  A STANDARD VALUE FOR SIGMA IS ABOUT 2.
C
C                        ISLPSW
C                          IS AN INTEGER INDICATING WHICH END SLOPES
C                          HAVE BEEN USER PROVIDED AND WHICH MUST BE
C                          COMPUTED BY KURV1S.  FOR ISLPSW
C                            = 0  INDICATES BOTH SLOPES ARE PROVIDED,
C                            = 1  ONLY SLOP1 IS PROVIDED,
C                            = 2  ONLY SLOPN IS PROVIDED,
C                            = 3  NEITHER SLOP1 NOR SLOPN IS PROVIDED.
C                            = 4  NEITHER SLOP1 NOR SLOPN IS PROVIDED,
C                                 BUT SLOP1=SLOPN.  IN THIS CASE X(1)=
C                                 X(N), Y(1)=Y(N) AND N.GE.3.
C ON OUTPUT              XP AND YP
C                          CONTAIN INFORMATION ABOUT THE CURVATURE OF
C                          THE CURVE AT THE GIVEN NODES.
C
C                        S
C                          CONTAINS THE POLYGONAL ARCLENGTH OF THE
C                          CURVE.
C
C                        N, X, Y, SLP1, SLPN, SIGMA AND ISLPSW ARE
C                        UNCHANGED.
C
C ENTRY POINTS           KURV1S
C
C SPECIAL CONDITIONS     NONE
C
C COMMON BLOCKS          NONE
C
C I/O                    NONE
C
C PRECISION              SINGLE
C
C REQUIRED ULIB          NONE
C ROUTINES
C
C SPECIALIST             RUSSELL K. REW, NCAR, BOULDER, COLORADO  80302
C
C LANGUAGE               FORTRAN
C
C HISTORY                ORIGINALLY WRITTEN BY A. K. CLINE, MARCH 1972.
C
C
C
C
      SAVE
      INTEGER         N
      REAL            X(N)       ,Y(N)       ,XP(N)      ,YP(N)      ,
     1                TEMP(N)    ,S          ,SIGMA
C
      NN = N
      JSLPSW = ISLPSW
      SLP1 = SLOP1
      SLPN = SLOPN
      DEGRAD = 3.1415926535897932/180.
      NM1 = NN-1
      NP1 = NN+1
      DELX1 = X(2)-X(1)
      DELY1 = Y(2)-Y(1)
      DELS1 = SQRT(DELX1*DELX1+DELY1*DELY1)
      DX1 = DELX1/DELS1
      DY1 = DELY1/DELS1
C
C DETERMINE SLOPES IF NECESSARY
C
      IF (JSLPSW .NE. 0) GO TO  70
   10 SLPP1 = SLP1*DEGRAD
      SLPPN = SLPN*DEGRAD
C
C SET UP RIGHT HAND SIDES OF TRIDIAGONAL LINEAR SYSTEM FOR XP
C AND YP
C
      XP(1) = DX1-COS(SLPP1)
      YP(1) = DY1-SIN(SLPP1)
      TEMP(1) = DELS1
      SS = DELS1
      IF (NN .EQ. 2) GO TO  30
      DO  20 I=2,NM1
         DELX2 = X(I+1)-X(I)
         DELY2 = Y(I+1)-Y(I)
         DELS2 = SQRT(DELX2*DELX2+DELY2*DELY2)
         DX2 = DELX2/DELS2
         DY2 = DELY2/DELS2
         XP(I) = DX2-DX1
         YP(I) = DY2-DY1
         TEMP(I) = DELS2
         DELX1 = DELX2
         DELY1 = DELY2
         DELS1 = DELS2
         DX1 = DX2
         DY1 = DY2
C
C ACCUMULATE POLYGONAL ARCLENGTH
C
         SS = SS+DELS1
   20 CONTINUE
   30 XP(NN) = COS(SLPPN)-DX1
      YP(NN) = SIN(SLPPN)-DY1
C
C DENORMALIZE TENSION FACTOR
C
      SIGMAP = ABS(SIGMA)*FLOAT(NN-1)/SS
C
C PERFORM FORWARD ELIMINATION ON TRIDIAGONAL SYSTEM
C
      S = SS
      DELS = SIGMAP*TEMP(1)
      EXPS = EXP(DELS)
      SINHS = .5*(EXPS-1./EXPS)
      SINHIN = 1./(TEMP(1)*SINHS)
      DIAG1 = SINHIN*(DELS*.5*(EXPS+1./EXPS)-SINHS)
      DIAGIN = 1./DIAG1
      XP(1) = DIAGIN*XP(1)
      YP(1) = DIAGIN*YP(1)
      SPDIAG = SINHIN*(SINHS-DELS)
      TEMP(1) = DIAGIN*SPDIAG
      IF (NN .EQ. 2) GO TO  50
      DO  40 I=2,NM1
         DELS = SIGMAP*TEMP(I)
         EXPS = EXP(DELS)
         SINHS = .5*(EXPS-1./EXPS)
         SINHIN = 1./(TEMP(I)*SINHS)
         DIAG2 = SINHIN*(DELS*(.5*(EXPS+1./EXPS))-SINHS)
         DIAGIN = 1./(DIAG1+DIAG2-SPDIAG*TEMP(I-1))
         XP(I) = DIAGIN*(XP(I)-SPDIAG*XP(I-1))
         YP(I) = DIAGIN*(YP(I)-SPDIAG*YP(I-1))
         SPDIAG = SINHIN*(SINHS-DELS)
         TEMP(I) = DIAGIN*SPDIAG
         DIAG1 = DIAG2
   40 CONTINUE
   50 DIAGIN = 1./(DIAG1-SPDIAG*TEMP(NM1))
      XP(NN) = DIAGIN*(XP(NN)-SPDIAG*XP(NM1))
      YP(NN) = DIAGIN*(YP(NN)-SPDIAG*YP(NM1))
C
C PERFORM BACK SUBSTITUTION
C
      DO  60 I=2,NN
         IBAK = NP1-I
         XP(IBAK) = XP(IBAK)-TEMP(IBAK)*XP(IBAK+1)
         YP(IBAK) = YP(IBAK)-TEMP(IBAK)*YP(IBAK+1)
   60 CONTINUE
      RETURN
   70 IF (NN .EQ. 2) GO TO 100
C
C IF NO SLOPES ARE GIVEN, USE SECOND ORDER INTERPOLATION ON
C INPUT DATA FOR SLOPES AT ENDPOINTS
C
      IF (JSLPSW .EQ. 4) GO TO  90
      IF (JSLPSW .EQ. 2) GO TO  80
      DELNM1 = SQRT((X(NN-2)-X(NM1))**2+(Y(NN-2)-Y(NM1))**2)
      DELN = SQRT((X(NM1)-X(NN))**2+(Y(NM1)-Y(NN))**2)
      DELNN = DELNM1+DELN
      C1 = (DELNN+DELN)/DELNN/DELN
      C2 = -DELNN/DELN/DELNM1
      C3 = DELN/DELNN/DELNM1
      SX = C3*X(NN-2)+C2*X(NM1)+C1*X(NN)
      SY = C3*Y(NN-2)+C2*Y(NM1)+C1*Y(NN)
C
      SLPN = ATAN2(SY,SX)/DEGRAD
   80 IF (JSLPSW .EQ. 1) GO TO  10
      DELS2 = SQRT((X(3)-X(2))**2+(Y(3)-Y(2))**2)
      DELS12 = DELS1+DELS2
      C1 = -(DELS12+DELS1)/DELS12/DELS1
      C2 = DELS12/DELS1/DELS2
      C3 = -DELS1/DELS12/DELS2
      SX = C1*X(1)+C2*X(2)+C3*X(3)
      SY = C1*Y(1)+C2*Y(2)+C3*Y(3)
C
      SLP1 = ATAN2(SY,SX)/DEGRAD
      GO TO  10
   90 DELN = SQRT((X(NM1)-X(NN))**2+(Y(NM1)-Y(NN))**2)
      DELNN = DELS1+DELN
      C1 = -DELS1/DELN/DELNN
      C2 = (DELS1-DELN)/DELS1/DELN
      C3 = DELN/DELNN/DELS1
      SX = C1*X(NM1)+C2*X(1)+C3*X(2)
      SY = C1*Y(NM1)+C2*Y(1)+C3*Y(2)
      IF (SX.EQ.0. .AND. SY.EQ.0.) SX = 1.
      SLP1 = ATAN2(SY,SX)/DEGRAD
      SLPN = SLP1
      GO TO  10
C
C IF ONLY TWO POINTS AND NO SLOPES ARE GIVEN, USE STRAIGHT
C LINE SEGMENT FOR CURVE
C
  100 IF (JSLPSW .NE. 3) GO TO 110
      XP(1) = 0.
      XP(2) = 0.
      YP(1) = 0.
      YP(2) = 0.
C
      SLP1 = ATAN2(Y(2)-Y(1),X(2)-X(1))/DEGRAD
      SLPN = SLP1
      RETURN
C
  110 IF (JSLPSW .EQ. 2)
     1    SLP1 = ATAN2(Y(2)-Y(1)-SLPN*(X(2)-X(1)),
     2                                X(2)-X(1)-SLPN*(Y(2)-Y(1)))/DEGRAD
C
      IF (JSLPSW .EQ. 1)
     1    SLPN = ATAN2(Y(2)-Y(1)-SLP1*(X(2)-X(1)),
     2                                X(2)-X(1)-SLP1*(Y(2)-Y(1)))/DEGRAD
      GO TO  10
      END
