C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPGTI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      COMMON /MAPCMB/ IIER
      COMMON /MAPNTS/ INTS(7)
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPGTI','VERSION  1')
C
      IF (WHCH(1:2).EQ.'DA') THEN
        IVAL=IDSH
      ELSE IF (WHCH(1:2).EQ.'DD') THEN
        IVAL=DDTS
      ELSE IF (WHCH(1:2).EQ.'DL') THEN
        IVAL=IDTL
      ELSE IF (WHCH(1:2).EQ.'DO') THEN
        IVAL=IDOT
      ELSE IF (WHCH(1:2).EQ.'EL') THEN
        IVAL=0
        IF (ELPF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'ER') THEN
        IVAL=IIER
      ELSE IF (WHCH(1:2).EQ.'GR') THEN
        IVAL=GRID
      ELSE IF (WHCH(1:2).EQ.'IN') THEN
        IVAL=0
        IF (INTF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'I1') THEN
        IVAL=INTS(1)
      ELSE IF (WHCH(1:2).EQ.'I2') THEN
        IVAL=INTS(2)
      ELSE IF (WHCH(1:2).EQ.'I3') THEN
        IVAL=INTS(3)
      ELSE IF (WHCH(1:2).EQ.'I4') THEN
        IVAL=INTS(4)
      ELSE IF (WHCH(1:2).EQ.'I5') THEN
        IVAL=INTS(5)
      ELSE IF (WHCH(1:2).EQ.'I6') THEN
        IVAL=INTS(6)
      ELSE IF (WHCH(1:2).EQ.'I7') THEN
        IVAL=INTS(7)
      ELSE IF (WHCH(1:2).EQ.'LA') THEN
        IVAL=0
        IF (LBLF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'LS') THEN
        IVAL=ILCW
      ELSE IF (WHCH(1:2).EQ.'MV') THEN
        IVAL=DPLT
      ELSE IF (WHCH(1:2).EQ.'PE') THEN
        IVAL=0
        IF (PRMF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'PN') THEN
        IVAL=PHIO
      ELSE IF (WHCH(1:2).EQ.'PT') THEN
        IVAL=PHIA
      ELSE IF (WHCH(1:2).EQ.'P1') THEN
        IVAL=PLA1
      ELSE IF (WHCH(1:2).EQ.'P2') THEN
        IVAL=PLA2
      ELSE IF (WHCH(1:2).EQ.'P3') THEN
        IVAL=PLA3
      ELSE IF (WHCH(1:2).EQ.'P4') THEN
        IVAL=PLA4
      ELSE IF (WHCH(1:2).EQ.'P5') THEN
        IVAL=PLB1
      ELSE IF (WHCH(1:2).EQ.'P6') THEN
        IVAL=PLB2
      ELSE IF (WHCH(1:2).EQ.'P7') THEN
        IVAL=PLB3
      ELSE IF (WHCH(1:2).EQ.'P8') THEN
        IVAL=PLB4
      ELSE IF (WHCH(1:2).EQ.'RE') THEN
        IVAL=PLTR
      ELSE IF (WHCH(1:2).EQ.'RO') THEN
        IVAL=ROTA
      ELSE IF (WHCH(1:2).EQ.'SA') THEN
        IVAL=SALT
      ELSE IF (WHCH(1:2).EQ.'S1') THEN
        IVAL=ALFA
      ELSE IF (WHCH(1:2).EQ.'S2') THEN
        IVAL=BETA
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
  901 IIER=2
      CALL MAPCEM (' MAPGTI - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      IVAL=0
      RETURN
C
      END
