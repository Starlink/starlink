C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPSTI (WHCH,IVAL)
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
      COMMON /MAPNTS/ INTS(7)
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPSTI','VERSION  1')
C
      IF (WHCH(1:2).EQ.'DA') THEN
        IDSH=IVAL
      ELSE IF (WHCH(1:2).EQ.'DD') THEN
        DDTS=IVAL
        DBTD=DDTS/DSCA
      ELSE IF (WHCH(1:2).EQ.'DL') THEN
        IDTL=IVAL
      ELSE IF (WHCH(1:2).EQ.'DO') THEN
        IDOT=IVAL
      ELSE IF (WHCH(1:2).EQ.'EL') THEN
        ELPF=IVAL.NE.0
      ELSE IF (WHCH(1:2).EQ.'GR') THEN
        GRID=IVAL
      ELSE IF (WHCH(1:2).EQ.'I1') THEN
        INTS(1)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I2') THEN
        INTS(2)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I3') THEN
        INTS(3)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I4') THEN
        INTS(4)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I5') THEN
        INTS(5)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I6') THEN
        INTS(6)=IVAL
      ELSE IF (WHCH(1:2).EQ.'I7') THEN
        INTS(7)=IVAL
      ELSE IF (WHCH(1:2).EQ.'LA') THEN
        LBLF=IVAL.NE.0
      ELSE IF (WHCH(1:2).EQ.'LS') THEN
        ILCW=IVAL
      ELSE IF (WHCH(1:2).EQ.'MV') THEN
        DPLT=IVAL
        DPSQ=DPLT*DPLT
      ELSE IF (WHCH(1:2).EQ.'PE') THEN
        PRMF=IVAL.NE.0
      ELSE IF (WHCH(1:2).EQ.'RE') THEN
        PLTR=IVAL
        DSCA=(UROW-ULOW)*PLTR/(UMAX-UMIN)
        DSSQ=DSCA*DSCA
        DBTD=DDTS/DSCA
      ELSE IF (WHCH(1:2).EQ.'SA') THEN
        SALT=IVAL
        IF (ABS(SALT).GT.1.) THEN
          SSMO=SALT*SALT-1.
          SRSS=SQRT(SSMO)
        END IF
      ELSE IF (WHCH(1:2).EQ.'S1') THEN
        ALFA=IVAL
        SALF=SIN(.017453292519943*ALFA)
        CALF=COS(.017453292519943*ALFA)
      ELSE IF (WHCH(1:2).EQ.'S2') THEN
        BETA=IVAL
        SBET=SIN(.017453292519943*BETA)
        CBET=COS(.017453292519943*BETA)
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
  901 IIER=13
      CALL MAPCEM (' MAPSTI - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      RETURN
C
      END
