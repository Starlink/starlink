      SUBROUTINE KURV2S (T,XS,YS,N,X,Y,XP,YP,S,SIGMA,NSLPSW,SLP)
C
C
C
C DIMENSION OF           X(N),Y(N),XP(N),YP(N)
C ARGUMENTS
C
C LATEST REVISION        JUNE 1979
C
C PURPOSE                KURV2 PERFORMS THE MAPPING OF POINTS IN THE
C                        INTERVAL (0.,1.) ONTO A CURVE IN THE PLANE.
C                        THE SUBROUTINE KURV1 SHOULD BE CALLED EARLIER
C                        TO DETERMINE CERTAIN NECESSARY PARAMETERS.
C                        THE RESULTING CURVE HAS A PARAMETRIC
C                        REPRESENTATION BOTH OF WHOSE COMPONENTS ARE
C                        SPLINES UNDER TENSION AND FUNCTIONS OF THE
C                        POLYGONAL ARCLENGTH PARAMETER.
C
C ACCESS CARDS           *FORTRAN,S=ULIB,N=KURV
C
C
C USAGE                  CALL KURV2S (T,XS,YS,N,X,Y,XP,YP,S,SIGMA)
C
C ARGUMENTS
C
C ON INPUT               T
C                          CONTAINS A REAL VALUE OF ABSOLUTE VALUE LESS
C                          THAN OR EQUAL TO 1. TO BE MAPPED TO A POINT
C                          ON THE CURVE.  THE SIGN OF T IS IGNORED AND
C                          THE INTERVAL (0.,1.) IS MAPPED ONTO THE
C                          ENTIRE CURVE.  IF T IS NEGATIVE, THIS
C                          INDICATES THAT THE SUBROUTINE HAS BEEN CALLED
C                          PREVIOUSLY (WITH ALL OTHER INPUT VARIABLES
C                          UNALTERED) AND THAT THIS VALUE OF T EXCEEDS
C                          THE PREVIOUS VALUE IN ABSOLUTE VALUE.  WITH
C                          SUCH INFORMATION THE SUBROUTINE IS ABLE TO
C                          MAP THE POINT MUCH MORE RAPIDLY.  THUS IF THE
C                          USER SEEKS TO MAP A SEQUENCE OF POINTS ONTO
C                          THE SAME CURVE, EFFICIENCY IS GAINED BY
C                          ORDERING THE VALUES INCREASING IN MAGNITUDE
C                          AND SETTING THE SIGNS OF ALL BUT THE FIRST
C                          NEGATIVE.
C
C                        N
C                          CONTAINS THE NUMBER OF POINTS WHICH WERE
C                          INTERPOLATED TO DETERMINE THE CURVE.
C
C                        X AND Y
C                          ARRAYS CONTAINING THE X- AND Y-COORDINATES
C                          OF THE INTERPOLATED POINTS.
C
C                        XP AND YP
C                          ARE THE ARRAYS OUTPUT FROM KURV1 CONTAINING
C                          CURVATURE INFORMATION.
C
C                        S
C                          CONTAINS THE POLYGONAL ARCLENGTH OF THE
C                          CURVE.
C
C                        SIGMA
C                          CONTAINS THE TENSION FACTOR (ITS SIGN IS
C                          IGNORED).
C
C                        NSLPSW
C                          IS AN INTEGER SWITCH WHICH TURNS ON OR OFF
C                          THE CALCULATION OF SLP
C                          NSLPSW
C                                 = 0 INDICATES THAT SLP WILL NOT BE
C                                     CALCULATED
C                                 = 1 SLP WILL BE CALCULATED
C
C                        THE PARAMETERS N, X, Y, XP, YP, S AND SIGMA
C                        SHOULD BE INPUT UNALTERED FROM THE OUTPUT OF
C                        KURV1S.
C
C ON OUTPUT              XS AND YS
C                          CONTAIN THE X- AND Y-COORDINATES OF THE IMAGE
C                          POINT ON THE CURVE.
C
C                        SLP
C                          CONTAINS THE SLOPE OF THE CURVE IN DEGREES AT
C                          THIS POINT.
C
C                        T, N, X, Y, XP, YP, S AND SIGMA ARE UNALTERED.
C
C ENTRY POINTS           KURV2S
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
      REAL            T          ,XS         ,YS         ,X(N)       ,
     1                Y(N)       ,XP(N)      ,YP(N)      ,S          ,
     2                SIGMA      ,SLP
C
C DENORMALIZE SIGMA
C
      SIGMAP = ABS(SIGMA)*FLOAT(N-1)/S
C
C STRETCH UNIT INTERVAL INTO ARCLENGTH DISTANCE
C
      TN = ABS(T*S)
C
C FOR NEGATIVE T START SEARCH WHERE PREVIOUSLY TERMINATED,
C OTHERWISE START FROM BEGINNING
C
      IF (T .LT. 0.) GO TO  10
      DEGRAD = 3.1415926535897932/180.
      I1 = 2
      XS = X(1)
      YS = Y(1)
      SUM = 0.
      IF (T .LT. 0.) RETURN
C
C DETERMINE INTO WHICH SEGMENT TN IS MAPPED
C
   10 DO  30 I=I1,N
         DELX = X(I)-X(I-1)
         DELY = Y(I)-Y(I-1)
         DELS = SQRT(DELX*DELX+DELY*DELY)
         IF (SUM+DELS-TN)  20, 40, 40
   20    SUM = SUM+DELS
   30 CONTINUE
C
C IF ABS(T) IS GREATER THAN 1., RETURN TERMINAL POINT ON
C CURVE
C
      XS = X(N)
      YS = Y(N)
      RETURN
C
C SET UP AND PERFORM INTERPOLATION
C
   40 DEL1 = TN-SUM
      DEL2 = DELS-DEL1
      EXPS1 = EXP(SIGMAP*DEL1)
      SINHD1 = .5*(EXPS1-1./EXPS1)
      EXPS2 = EXP(SIGMAP*DEL2)
      SINHD2 = .5*(EXPS2-1./EXPS2)
      EXPS = EXPS1*EXPS2
      SINHS = .5*(EXPS-1./EXPS)
      XS = (XP(I)*SINHD1+XP(I-1)*SINHD2)/SINHS+
     1     ((X(I)-XP(I))*DEL1+(X(I-1)-XP(I-1))*DEL2)/DELS
      YS = (YP(I)*SINHD1+YP(I-1)*SINHD2)/SINHS+
     1     ((Y(I)-YP(I))*DEL1+(Y(I-1)-YP(I-1))*DEL2)/DELS
      I1 = I
      IF (NSLPSW .EQ. 0) RETURN
      COSHD1 = .5*(EXPS1+1./EXPS1)*SIGMAP
      COSHD2 = .5*(EXPS2+1./EXPS2)*SIGMAP
      XT = (XP(I)*COSHD1-XP(I-1)*COSHD2)/SINHS+
     1     ((X(I)-XP(I))-(X(I-1)-XP(I-1)))/DELS
      YT = (YP(I)*COSHD1-YP(I-1)*COSHD2)/SINHS+
     1     ((Y(I)-YP(I))-(Y(I-1)-YP(I-1)))/DELS
      SLP = ATAN2(YT,XT)/DEGRAD
      RETURN
      END
