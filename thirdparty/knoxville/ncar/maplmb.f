C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPLMB
C
C The routine MAPLMB is called by MAPGRD and/or MAPLOT to draw the limb
C lines.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,SINO,COSO,SINR,COSR,PHOC
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C Define required constants.  SIN1 and COS1 are respectively the sine
C and cosine of one degree.
C
      DATA SIN1 / .017452406437283 /
      DATA COS1 / .999847695156390 /
      DATA PI   / 3.14159265358979 /
C
C The arithmetic statement functions FLOOR and CLING give, respectively,
C the "floor" of X - the largest integer less than or equal to X - and
C the "ceiling" of X - the smallest integer greater than or equal to X.
C
      FLOOR(X)=AINT(X+1.E4)-1.E4
      CLING(X)=-FLOOR(-X)
C
C Reset the intensity, dotting, and dash pattern for limb lines.
C
      CALL MAPCHI (4,0,IOR(ISHIFT(32767,1),1))
C
C Draw limb lines, the nature of which depends on the projection.
C
      GO TO (101,110,104,105,110,106,110,110,107,110,110,107) , IPRJ
C
C Lambert conformal conic with two standard parallels.
C
  101 DLAT=GRDR
      RLON=PHIO+179.9999
      K=CLING(180./DLAT)
      DO 103 I=1,2
        RLAT=-90.
        CALL MAPIT (RLAT,RLON,0)
        DO 102 J=1,K-1
          RLAT=RLAT+DLAT
          CALL MAPIT (RLAT,RLON,1)
  102   CONTINUE
        RLAT=RLAT+DLAT
        CALL MAPIT (RLAT,RLON,2)
        RLON=PHIO-179.9999
  103 CONTINUE
      GO TO 110
C
C Orthographic (or satellite-view).
C
  104 IF (ABS(SALT).LE.1..OR.ALFA.EQ.0.) THEN
        URAD=1.
        RVTU=1.
      ELSE
        DNOM=SALT*SALT*CALF*CALF-1.
        URAD=SSMO*CALF/DNOM
        RVTU=SQRT(DNOM)/SRSS
      END IF
      GO TO 108
C
C Lambert equal area.
C
  105 URAD=2.
      RVTU=1.
      GO TO 108
C
C Azimuthal equidistant.
C
  106 URAD=PI
      RVTU=1.
      GO TO 108
C
C Mollweide.
C
  107 URAD=2.
      RVTU=0.5
C
  108 UCIR=URAD
      VCIR=0.
      IVIS=-1
      DO 109 I=1,361
        IF (IPRJ.NE.3.OR.ABS(SALT).LE.1..OR.ALFA.EQ.0.) THEN
          U=UCIR
          V=RVTU*VCIR
        ELSE
          UTMP=UCIR-SRSS*SALF/DNOM
          VTMP=RVTU*VCIR
          U=UTMP*CBET-VTMP*SBET
          V=VTMP*CBET+UTMP*SBET
        END IF
        IF (.NOT.ELPF.AND.
     +      (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
          END IF
          IVIS=0
        ELSE IF (ELPF.AND.
     +           (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
          END IF
          IVIS=0
        ELSE
          IF (IVIS.LT.0) THEN
            DATL=0.
            CALL FRSTD (U,V)
            IVIS=1
          ELSE
            IF (IVIS.EQ.0) THEN
              IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
              IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
              DATL=0.
              CALL FRSTD (UOLD,VOLD)
              IVIS=1
            END IF
            CALL MAPVP (UOLD,VOLD,U,V)
          END IF
        END IF
        UOLD=U
        VOLD=V
        UTMP=UCIR
        VTMP=VCIR
        UCIR=UTMP*COS1-VTMP*SIN1
        VCIR=UTMP*SIN1+VTMP*COS1
  109 CONTINUE
C
C Restore the original intensity, dotting, and dash pattern.
C
  110 CALL MAPCHI (-4,0,0)
C
C Done.
C
      RETURN
C
      END
