C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPROJ (ARG1,ARG2,ARG3,ARG4)
C
      CHARACTER*(*) ARG1
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCM5/ DDCT(5),LDCT(5),PDCT(10)
      CHARACTER*2     DDCT,LDCT,PDCT
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPROJ','VERSION  1')
C
C Transfer the parameters defining the projection.
C
      I=IDICTL(ARG1,PDCT,10)
      IF (I.EQ.0) GO TO 901
C
      JPRJ=I
C
      IF (JPRJ.EQ.3) THEN
        CALL MAPSTR ('SA',0.)
      ELSE IF (JPRJ.EQ.10) THEN
        JPRJ=3
        IF (ABS(SALT).LE.1.) CALL MAPSTR ('SA',6.631)
      END IF
C
      PHIA=ARG2
      PHIO=ARG3
      ROTA=ARG4
C
C Set the flag to indicate that initialization is now required.
C
      INTF=.TRUE.
C
C Done.
C
      RETURN
C
C Error exit.
C
  901 IIER=9
      CALL MAPCEM (' MAPROJ - UNKNOWN PROJECTION NAME ',ARG1,IIER,1)
      RETURN
C
      END
