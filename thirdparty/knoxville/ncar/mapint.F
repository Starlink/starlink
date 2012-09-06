      SUBROUTINE MAPINT
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,SINO,COSO,SINR,COSR,PHOC
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,BLAG,SLAG,BLOG,SLOG,PNTS(200)
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      COMMON /MAPCMB/ IIER
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C Set up alternate names for some of the variables in common.
C
      EQUIVALENCE     (PHIA,FLT1),(ROTA,FLT2)
C
      EQUIVALENCE     (PLA1,AUMN),(PLA2,AUMX),
     +                (PLA3,AVMN),(PLA4,AVMX)
C
C Ensure that the block data routine will load, so that variables will
C have the proper default values.
C
      EXTERNAL MAPBD
C
C IMPLEMENTATION-DEPENDENT VARIABLES
C
C VARIABLES USED IN LOCATING SYSTEM DEPENDENT DATA FILE
C
      CHARACTER*1024 TRANS, FNAME  
      INTEGER IOS,I1,I2
C
C
C END OF IMPLEMENTATION-DEPENDENT VARIABLES
C
C Define the necessary constants.
C
      DATA RESL / 10. /
      DATA DTOR / .017453292519943 /
      DATA OV90 / .011111111111111 /
      DATA PI   / 3.14159265358979 /
      DATA RTOD / 57.2957795130823 /
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPINT','VERSION  1')
*
*  Open data file
*
      OPEN(UNIT=ITPN,FILE='/star/etc/ezmap.dat', IOSTAT=IOS,
     :     FORM='UNFORMATTED',
#if HAVE_FC_OPEN_ACTIONREAD
     :     ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :     READONLY,
#endif     
     :     STATUS='OLD')
      IF (IOS .NE. 0) THEN
           CALL GETENV('PATH', TRANS)
           LPATH = INDEX(TRANS, ' ')
           I1 = 1
95         CONTINUE
           I2 = INDEX(TRANS(I1:),':')
           IF (I2 .EQ. 0) THEN
                FNAME = TRANS(I1:LPATH-1)//'/../etc/'//'ezmap.dat'
           ELSE
                FNAME = TRANS(I1:I1+I2-2)//'/../etc/'//'ezmap.dat'
           ENDIF
           OPEN(UNIT=ITPN,FILE=FNAME, IOSTAT=IOS,
     :          FORM='UNFORMATTED',
#if HAVE_FC_OPEN_ACTIONREAD
     :          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :     READONLY,
#endif     
     :     STATUS='OLD')
           IF (IOS .EQ. 0) GOTO 96
           IF (I2 .EQ. 0) THEN
               CALL SETER
     +         ('MAPINT - CANNOT LOCATE DATA FILE', 1, 2) 
               RETURN
           ELSE
               I1 = I1 + I2
               GOTO 95
           ENDIF
      ENDIF

96    CONTINUE
C
C Check for an error in the projection specifier.
C
      IF (JPRJ.LE.0.OR.JPRJ.GE.10) GO TO 901
C
C IPRJ equals JPRJ until we find out if fast-path projections are to be
C used.  PHOC is just a copy of PHIO.
C
      IPRJ=JPRJ
      PHOC=PHIO
C
      IF (IPRJ.EQ.1) THEN
C
C Compute constants for the Lambert conformal conic.
C
        SINO=SIGN(1.,.5*(FLT1+FLT2))
        CHI1=(90.-SINO*FLT1)*DTOR
        IF (FLT1.EQ.FLT2) THEN
          COSO=COS(CHI1)
        ELSE
          CHI2=(90.-SINO*FLT2)*DTOR
          COSO=ALOG(SIN(CHI1)/SIN(CHI2))/ALOG(TAN(.5*CHI1)/TAN(.5*CHI2))
        END IF
C
      ELSE
C
C Compute constants required for all the other projections.
C
        TMP1=ROTA*DTOR
        TMP2=PHIA*DTOR
        SINR=SIN(TMP1)
        COSR=COS(TMP1)
        SINO=SIN(TMP2)
        COSO=COS(TMP2)
C
C Compute constants required only by the cylindrical projections.
C
        IF (IPRJ.GE.7) THEN
C
C See if fast-path transformations can be used.  (PLAT = 0 and ROTA = 0
C or 180.)
C
          IF (ABS(PHIA).GE..0001.OR.(ABS(ROTA).GE..0001.AND.
     +                               ABS(ROTA).LE.179.9999)) THEN
C
C No.  Compute constants for the ordinary cylindrical projections.
C
            SINT=COSO*COSR
            COST=SQRT(1.-(SINT)**2)
            TMP1=SINR/COST
            TMP2=SINO/COST
            PHIO=PHIO-ATAN2(TMP1,-COSR*TMP2)*RTOD
            PHOC=PHIO
            SINR=TMP1*COSO
            COSR=-TMP2
            SINO=SINT
            COSO=COST
C
          ELSE
C
C Yes.  The fast paths are implemented as three additional projections.
C
            IPRJ=IPRJ+3
C
            IF (ABS(ROTA).LT..0001) THEN
              SINO=1.
            ELSE
              SINO=-1.
              PHIO=PHIO+180.
              PHOC=PHIO
            END IF
C
            COSO=0.
            SINR=0.
            COSR=1.
C
          END IF
C
        END IF
C
      END IF
C
C Now, set UMIN, UMAX, VMIN, and VMAX to correspond to the maximum
C useful area produced by the projection.
C
      GO TO (101,102,101,102,102,103,104,103,105,104,103,105) , IPRJ
C
C Lambert conformal conic and orthographic.
C
  101 IF (IPRJ.NE.3.OR.ABS(SALT).LE.1..OR.ALFA.EQ.0.) THEN
        UMIN=-1.
        UMAX=1.
        VMIN=-1.
        VMAX=1.
      ELSE
        TMP1=SALT*SALT*CALF*CALF-1.
        TMP2=CALF*SQRT(SALT*SALT*(1.-SALF*SALF*SBET*SBET)-1.)
        UMIN=SRSS*(-SALF*CBET-TMP2)/TMP1
        UMAX=SRSS*(-SALF*CBET+TMP2)/TMP1
        TMP2=CALF*SQRT(SALT*SALT*(1.-SALF*SALF*CBET*CBET)-1.)
        VMIN=SRSS*(-SALF*SBET-TMP2)/TMP1
        VMAX=SRSS*(-SALF*SBET+TMP2)/TMP1
      END IF
C
      GO TO 106
C
C Stereographic, Lambert equal area, and Gnomonic.
C
  102 UMIN=-2.
      UMAX=2.
      VMIN=-2.
      VMAX=2.
      GO TO 106
C
C Azimuthal equidistant and Mercator.
C
  103 UMIN=-PI
      UMAX=PI
      VMIN=-PI
      VMAX=PI
      GO TO 106
C
C Cylindrical equidistant.
C
  104 UMIN=-180.
      UMAX=180.
      VMIN=-90.
      VMAX=90.
      GO TO 106
C
C Mollweide.
C
  105 UMIN=-2.
      UMAX=2.
      VMIN=-1.
      VMAX=1.
C
C Compute the quantities used by MAPIT in checking for cross-over.
C
  106 UEPS=.75*(UMAX-UMIN)
      VEPS=.75*(VMAX-VMIN)
C
C As always, the conical projection is the oddball.  Cross-over is not
C detected in u and v, but in longitude, so the value has to be set
C differently.
C
      IF (IPRJ.EQ.1) UEPS=180.
C
C Now, jump to the appropriate limit-setting code.
C
      GO TO (600,200,300,400,500) , ILTS
C
C ILTS=2    Points (PL1,PL2) and (PL3,PL4) are on opposite corners
C ------    of the plot.
C
  200 E=0.
  201 CALL MAPTRN (PLA1,PLA2+E,TMP1,TMP3)
      CALL MAPTRN (PLA3,PLA4-E,TMP2,TMP4)
      IF (IPRJ.GE.7.AND.TMP1.GE.TMP2.AND.E.EQ.0.) THEN
        E=.0001
        GO TO 201
      END IF
      UMIN=AMIN1(TMP1,TMP2)
      UMAX=AMAX1(TMP1,TMP2)
      VMIN=AMIN1(TMP3,TMP4)
      VMAX=AMAX1(TMP3,TMP4)
      IF (UMAX.GE.1.E12) GO TO 904
      GO TO 600
C
C ILTS=3    Four edge points are given.
C ------
C
  300 E=0.
  301 CALL MAPTRN (PLA1,PLB1+E,TMP1,TMP5)
      CALL MAPTRN (PLA2,PLB2-E,TMP2,TMP6)
      IF (IPRJ.GE.7.AND.TMP1.GE.TMP2.AND.E.EQ.0.) THEN
        E=.0001
        GO TO 301
      END IF
      CALL MAPTRN (PLA3,PLB3,TMP3,TMP7)
      CALL MAPTRN (PLA4,PLB4,TMP4,TMP8)
      UMIN=AMIN1(TMP1,TMP2,TMP3,TMP4)
      UMAX=AMAX1(TMP1,TMP2,TMP3,TMP4)
      VMIN=AMIN1(TMP5,TMP6,TMP7,TMP8)
      VMAX=AMAX1(TMP5,TMP6,TMP7,TMP8)
      IF (UMAX.GE.1.E12) GO TO 904
      GO TO 600
C
C ILTS=4    Angular distances are given.
C ------
C
  400 CUMI=COS(AUMN*DTOR)
      SUMI=SIN(AUMN*DTOR)
      CUMA=COS(AUMX*DTOR)
      SUMA=SIN(AUMX*DTOR)
      CVMI=COS(AVMN*DTOR)
      SVMI=SIN(AVMN*DTOR)
      CVMA=COS(AVMX*DTOR)
      SVMA=SIN(AVMX*DTOR)
C
      GO TO (904,401,402,403,404,405,406,407,408,406,407,408) , IPRJ
C
C Stereographic.
C
  401 IF (SUMI.LT..0001) THEN
        IF (CUMI.GT.0.) UMIN=0.
      ELSE
        UMIN=-(1.-CUMI)/SUMI
      END IF
      IF (SUMA.LT..0001) THEN
        IF (CUMA.GT.0.) UMAX=0.
      ELSE
        UMAX=(1.-CUMA)/SUMA
      END IF
      IF (SVMI.LT..0001) THEN
        IF (CVMI.GT.0.) VMIN=0.
      ELSE
        VMIN=-(1.-CVMI)/SVMI
      END IF
      IF (SVMA.LT..0001) THEN
        IF (CVMA.GT.0.) VMAX=0.
      ELSE
        VMAX=(1.-CVMA)/SVMA
      END IF
      GO TO 600
C
C Orthographic.
C
  402 IF (ABS(SALT).LE.1.) THEN
        IF (AMAX1(AUMN,AUMX,AVMN,AVMX).GT.90.) GO TO 902
        UMIN=-SUMI
        UMAX=SUMA
        VMIN=-SVMI
        VMAX=SVMA
      ELSE
        IF (AMAX1(AUMN,AUMX,AVMN,AVMX).GE.90.) GO TO 902
        UTMP=SRSS*SALF/CALF
        VTMP=0.
        UCEN=UTMP*CBET-VTMP*SBET
        VCEN=VTMP*CBET+UTMP*SBET
        UMIN=UCEN-SRSS*CALF*SUMI/CUMI
        UMAX=UCEN+SRSS*CALF*SUMA/CUMA
        VMIN=VCEN-SRSS*CALF*SVMI/CVMI
        VMAX=VCEN+SRSS*CALF*SVMA/CVMA
      END IF
      GO TO 600
C
C Lambert equal area.
C
  403 IF (SUMI.LT..0001) THEN
        IF (CUMI.GT.0.) UMIN=0.
      ELSE
        UMIN=-2./SQRT(1.+((1.+CUMI)/SUMI)**2)
      END IF
      IF (SUMA.LT..0001) THEN
        IF (CUMA.GT.0.) UMAX=0.
      ELSE
        UMAX=2./SQRT(1.+((1.+CUMA)/SUMA)**2)
      END IF
      IF (SVMI.LT..0001) THEN
        IF (CVMI.GT.0.) VMIN=0.
      ELSE
        VMIN=-2./SQRT(1.+((1.+CVMI)/SVMI)**2)
      END IF
      IF (SVMA.LT..0001) THEN
        IF (CVMA.GT.0.) VMAX=0.
      ELSE
        VMAX=2./SQRT(1.+((1.+CVMA)/SVMA)**2)
      END IF
      GO TO 600
C
C Gnomonic.
C
  404 IF (AMAX1(AUMN,AUMX,AVMN,AVMX).GE.89.9999) GO TO 902
      UMIN=-SUMI/CUMI
      UMAX=SUMA/CUMA
      VMIN=-SVMI/CVMI
      VMAX=SVMA/CVMA
      GO TO 600
C
C Azimuthal equidistant.
C
  405 UMIN=-AUMN*DTOR
      UMAX=AUMX*DTOR
      VMIN=-AVMN*DTOR
      VMAX=AVMX*DTOR
      GO TO 600
C
C Cylindrical equidistant.
C
  406 UMIN=-AUMN
      UMAX=AUMX
      VMIN=-AVMN
      VMAX=AVMX
      GO TO 600
C
C Mercator.
C
  407 IF (AMAX1(AVMN,AVMX).GE.89.9999) GO TO 902
      UMIN=-AUMN*DTOR
      UMAX=AUMX*DTOR
      VMIN=-ALOG((1.+SVMI)/CVMI)
      VMAX=ALOG((1.+SVMA)/CVMA)
      GO TO 600
C
C Mollweide.
C
  408 UMIN=-AUMN*OV90
      UMAX=AUMX*OV90
      VMIN=-SVMI
      VMAX=SVMA
      GO TO 600
C
C ILTS=5    Values in the u/v plane are given.
C ------
C
  500 UMIN=PLA1
      UMAX=PLA2
      VMIN=PLA3
      VMAX=PLA4
C
C Compute the width and height of the plot.
C
  600 DU=UMAX-UMIN
      DV=VMAX-VMIN
C
C Error if map has zero area.
C
      IF (DU.LE.0..OR.DV.LE.0.) GO TO 903
C
C Position the map on the plotter frame.
C
      IF (DU/DV.LT.(XROW-XLOW)/(YTOW-YBOW)) THEN
        ULOW=.5*(XLOW+XROW)-.5*(DU/DV)*(YTOW-YBOW)
        UROW=.5*(XLOW+XROW)+.5*(DU/DV)*(YTOW-YBOW)
        VBOW=YBOW
        VTOW=YTOW
      ELSE
        ULOW=XLOW
        UROW=XROW
        VBOW=.5*(YBOW+YTOW)-.5*(DV/DU)*(XROW-XLOW)
        VTOW=.5*(YBOW+YTOW)+.5*(DV/DU)*(XROW-XLOW)
      END IF
C
C Error if map has essentially zero area.
C
      IF (AMIN1(UROW-ULOW,VTOW-VBOW)*PLTR.LT.RESL) GO TO 903
C
C Do the required SET call.
C
      CALL SET (ULOW,UROW,VBOW,VTOW,UMIN,UMAX,VMIN,VMAX,1)
C
C Compute the quantities used by MAPIT to see if points are far enough
C apart to draw the line between them and the quantities used by MAPVP
C to determine the number of dots to interpolate between two points.
C
      DSCA=(UROW-ULOW)*PLTR/DU
      DPSQ=DPLT*DPLT
      DSSQ=DSCA*DSCA
      DBTD=DDTS/DSCA
C
C Set parameters required if an elliptical perimeter is being used.  The
C ellipse is made to be just a little bigger than an inscribed ellipse
C so as to avoid round-off problems when drawing the limb of certain
C projections.
C
      UCEN=.5*(UMIN+UMAX)
      VCEN=.5*(VMIN+VMAX)
      URNG=.50005*(UMAX-UMIN)
      VRNG=.50005*(VMAX-VMIN)
C
C Now, compute the latitude/longitude limits which will be required by
C MAPGRD and MAPLOT, if any.
C
      IF (GRID.GT.0..OR.NOUT.NE.0) THEN
C
C At first, assume the whole globe will be projected.
C
        SLAM=-90.
        BLAM=+90.
        SLOM=PHIO-180.
        BLOM=PHIO+180.
C
C Jump if it's obvious that really is the case.
C
        IF (ILTS.EQ.1.AND.(JPRJ.EQ.4.OR.JPRJ.EQ.6.OR.JPRJ.EQ.7.OR.
     +                                             JPRJ.EQ.9)) GO TO 700
C
C Otherwise, the whole globe is not being projected.  The first thing
C to do is to find a point (CLAT,CLON) whose projection is known to be
C on the map.  First, try the pole of the projection.
C
        CLAT=PHIA
        CLON=PHIO
        CALL MAPTRN (CLAT,CLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 611
C
C If that didn't work, try a point based on the limits specifier.
C
        IF (ILTS.EQ.2) THEN
          CLAT=.5*(PLA1+PLA3)
          CLON=.5*(PLA2+PLA4)
        ELSE IF (ILTS.EQ.3) THEN
          TMP1=AMIN1(PLA1,PLA2,PLA3,PLA4)
          TMP2=AMAX1(PLA1,PLA2,PLA3,PLA4)
          TMP3=AMIN1(PLB1,PLB2,PLB3,PLB4)
          TMP4=AMAX1(PLB1,PLB2,PLB3,PLB4)
          CLAT=.5*(TMP1+TMP2)
          CLON=.5*(TMP3+TMP4)
        ELSE
          GO TO 700
        END IF
        CALL MAPTRN (CLAT,CLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 611
        GO TO 700
C
C Once we have the latitudes and longitudes of a point on the map, we
C find the minimum and maximum latitude and the minimum and maximum
C longitude by running a search point about on a fine lat/lon grid.
C
C Find the minimum latitude.
C
  611   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  612   RLAT=RLAT-SRCH
        IF (RLAT.LE.-90.) GO TO 621
  613   CALL MAPTRN (RLAT,RLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLON=SRCH
          GO TO 612
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 613
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 613
        SLAM=RLAT
C
C Find the maximum latitude.
C
  621   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  622   RLAT=RLAT+SRCH
        IF (RLAT.GT.90.) GO TO 631
  623   CALL MAPTRN (RLAT,RLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLON=SRCH
          GO TO 622
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 623
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 623
        BLAM=RLAT
C
C Find the minimum longitude.
C
  631   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  632   RLON=RLON-SRCH
        IF (RLON.LE.CLON-360.) GO TO 651
  633   CALL MAPTRN (RLAT,RLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLAT=SRCH
          GO TO 632
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 633
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 633
        SLOM=RLON-SIGN(180.,RLON+180.)+SIGN(180.,180.-RLON)
C
C Find the maximum longitude.
C
  641   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  642   RLON=RLON+SRCH
        IF (RLON.GE.CLON+360.) GO TO 651
  643   CALL MAPTRN (RLAT,RLON,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLAT=SRCH
          GO TO 642
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 643
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 643
        BLOM=RLON-SIGN(180.,RLON+180.)+SIGN(180.,180.-RLON)
        IF (BLOM.LE.SLOM) BLOM=BLOM+360.
        GO TO 700
C
  651   SLOM=PHIO-180.
        BLOM=PHIO+180.
C
      END IF
C
C Zero the error flag and turn off the initialization-required flag.
C
  700 IIER=0
      INTF=.FALSE.
C
C Done.
C
      RETURN
C
C Error returns.
C
  901 IIER=5
      CALL SETER (' MAPINT - ATTEMPT TO USE NON-EXISTENT PROJECTION',
     1              IIER,1)
      RETURN
C
  902 IIER=6
      CALL SETER (' MAPINT - ANGULAR LIMITS TOO GREAT',IIER,1)
      RETURN
C
  903 IIER=7
      CALL SETER (' MAPINT - MAP HAS ZERO AREA',IIER,1)
      RETURN
C
  904 IIER=8
      CALL SETER (' MAPINT - MAP LIMITS INAPPROPIATE',IIER,1)
      RETURN
C
      END
