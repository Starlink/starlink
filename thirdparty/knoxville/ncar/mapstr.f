C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPSTR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      COMMON /MAPCMB/ IIER
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPSTR','VERSION  1')
C
      IF (WHCH(1:2).EQ.'DD') THEN
        DDTS=RVAL
        DBTD=DDTS/DSCA
      ELSE IF (WHCH(1:2).EQ.'GD') THEN
        GRDR=AMAX1(.001,AMIN1(10.,RVAL))
      ELSE IF (WHCH(1:2).EQ.'GR') THEN
        GRID=RVAL
      ELSE IF (WHCH(1:2).EQ.'MV') THEN
        DPLT=RVAL
        DPSQ=DPLT*DPLT
      ELSE IF (WHCH(1:2).EQ.'RE') THEN
        PLTR=RVAL
        DSCA=(UROW-ULOW)*PLTR/(UMAX-UMIN)
        DSSQ=DSCA*DSCA
        DBTD=DDTS/DSCA
      ELSE IF (WHCH(1:2).EQ.'SA') THEN
        SALT=RVAL
        IF (ABS(SALT).GT.1.) THEN
          SSMO=SALT*SALT-1.
          SRSS=SQRT(SSMO)
        END IF
      ELSE IF (WHCH(1:2).EQ.'S1') THEN
        ALFA=RVAL
        SALF=SIN(.017453292519943*ALFA)
        CALF=COS(.017453292519943*ALFA)
      ELSE IF (WHCH(1:2).EQ.'S2') THEN
        BETA=RVAL
        SBET=SIN(.017453292519943*BETA)
        CBET=COS(.017453292519943*BETA)
      ELSE IF (WHCH(1:2).EQ.'SR') THEN
        SRCH=AMAX1(.001,AMIN1(10.,RVAL))
      ELSE
        GO TO 901
      END IF
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 IIER=15
      CALL MAPCEM (' MAPSTR - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      RETURN
C
      END
