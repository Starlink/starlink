C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRN (RLAT,RLON,U,V)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,SINO,COSO,SINR,COSR,PHOC
      COMMON /MAPCM8/ P,Q,R
      COMMON /MAPCMB/ IIER
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C Define required constants.  DTOR is pi over 180, DTRH is half of DTOR
C or pi over 360, and TOPI is 2 over pi.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA RTOD / 57.2957795130823 /
      DATA TOPI / .636619772367581 /
C
C Set up U and V for the fast paths.  U is a longitude, in degrees,
C between -180. and +180., inclusive, and V is a latitude, in degrees.
C
      TMP1=RLON-PHOC
      U=TMP1-SIGN(180.,TMP1+180.)+SIGN(180.,180.-TMP1)
      V=RLAT
C
C Take fast paths for simple cylindrical projections.
C
      IF (IPRJ-10) 101,116,112
C
C No fast path.  Sort out the Lambert conformal conic from the rest.
C
  101 IF (IPRJ-1) 901,102,103
C
C Lambert conformal conic.
C
  102 P=U
      CHI=90.-SINO*RLAT
      IF (CHI.GE.179.9999) GO TO 118
      R=TAN(DTRH*CHI)**COSO
      U=U*COSO*DTOR
      V=-R*SINO*COS(U)
      U=R*SIN(U)
      GO TO 117
C
C Not Lambert conformal conic.  Calculate constants common to most of
C the other projections.
C
  103 TMP1=U*DTOR
      TMP2=V*DTOR
      SINPH=SIN(TMP1)
      SINLA=SIN(TMP2)
      COSPH=COS(TMP1)
      COSLA=COS(TMP2)
      TCOS=COSLA*COSPH
      COSA=AMAX1(-1.,AMIN1(+1.,SINLA*SINO+TCOS*COSO))
      SINA=SQRT(1.-COSA*COSA)
      IF (SINA.LT..0001) THEN
        SINA=0.
        IF (IPRJ.GE.7.OR.COSA.LT.0.) GO TO 118
        U=0.
        V=0.
        GO TO 116
      END IF
      SINB=COSLA*SINPH/SINA
      COSB=(SINLA*COSO-TCOS*SINO)/SINA
C
C Jump to code appropriate for the chosen projection.
C
      GO TO (104,105,106,107,108,109,110,111) , IPRJ-1
C
C Stereographic.
C
  104 IF (ABS(SINA).LT..0001) THEN
        R=SINA/2.
      ELSE
        R=(1.-COSA)/SINA
      END IF
      GO TO 115
C
C Orthographic or satellite-view, depending on the value of SALT.
C
  105 IF (ABS(SALT).LE.1.) THEN
        IF (COSA.GT.0.) THEN
          R=SINA
        ELSE
          IF (SALT.GE.0.) GO TO 118
          R=2.-SINA
        END IF
        GO TO 115
      ELSE
        IF (COSA.GT.1./ABS(SALT)) THEN
          R=SRSS*SINA/(ABS(SALT)-COSA)
        ELSE
          IF (SALT.GE.0.) GO TO 118
          R=2.-SRSS*SINA/(ABS(SALT)-COSA)
        END IF
        IF (ALFA.EQ.0.) GO TO 115
        UTM1=R*(SINB*COSR+COSB*SINR)
        VTM1=R*(COSB*COSR-SINB*SINR)
        UTM2=UTM1*CBET+VTM1*SBET
        VTM2=VTM1*CBET-UTM1*SBET
        UTM3=SRSS*UTM2/(UTM2*SALF+SRSS*CALF)
        VTM3=SRSS*VTM2*CALF/(UTM2*SALF+SRSS*CALF)
        U=UTM3*CBET-VTM3*SBET
        V=VTM3*CBET+UTM3*SBET
        GO TO 116
      END IF
C
C Lambert equal area.
C
  106 IF (ABS(COSA+1.).LT.1.E-6) GO TO 118
      R=(1.+COSA)/SINA
      R=2./SQRT(1.+R*R)
      GO TO 115
C
C Gnomonic.
C
  107 IF (COSA.LE..0001) GO TO 118
      R=SINA/COSA
      GO TO 115
C
C Azimuthal equidistant.
C
  108 IF (ABS(COSA+1.).LT.1.E-6) GO TO 118
      R=ACOS(COSA)
      GO TO 115
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  109 U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*RTOD
      V=90.-ACOS(COSA)*RTOD
      GO TO 116
C
C Mercator, arbitrary pole and orientation.
C
  110 U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)
      V=ALOG((1.+COSA)/SINA)
      GO TO 116
C
C Mollweide, arbitrary pole and orientation.
C
  111 U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*TOPI
      P=U
      V=COSA
      U=U*SINA
      GO TO 117
C
C Fast-path cylindrical projections (with PLAT=ROTA=0).
C
  112 IF (IPRJ-12) 113,114,901
C
C Fast-path Mercator.
C
  113 IF (ABS(RLAT).GT.89.9999) GO TO 118
      U=U*DTOR
      V=ALOG(TAN((RLAT+90.)*DTRH))
      GO TO 116
C
C Fast-path Mollweide.
C
  114 U=U/90.
      V=SIN(RLAT*DTOR)
      P=U
      U=U*SQRT(1.-V*V)
      GO TO 117
C
C Common terminal code for certain projections.
C
  115 U=R*(SINB*COSR+COSB*SINR)
      V=R*(COSB*COSR-SINB*SINR)
C
  116 P=U
C
  117 Q=V
C
C Normal exit.
C
      RETURN
C
C Projection of point is invisible or undefined.
C
  118 U=1.E12
      P=U
      RETURN
C
C Error exit.
C
  901 IF (IIER.NE.0) GO TO 118
      IIER=16
      CALL SETER (' MAPTRN - ATTEMPT TO USE NON-EXISTENT PROJECTION',
     +              IIER,1)
      GO TO 118
C
      END
