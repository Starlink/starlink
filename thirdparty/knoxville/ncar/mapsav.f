C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPSAV (IFNO)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,BLAG,SLAG,BLOG,SLOG,PNTS(200)
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
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPSAV','VERSION  1')
C
C Write a record containing all the user-settable parameters.
C
      WRITE (IFNO,ERR=901) NOUT,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,
     +                     PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,
     +                     PLTR,GRID,IDSH,IDOT,LBLF,PRMF,ELPF,
     +                     XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,
     +                     ILCW,DPLT,DDTS,SALT,SSMO,SRSS,ALFA,
     +                     BETA,SALF,CALF,SBET,CBET,
     +                     (INTS(I),I=1,7)
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 IIER=22
      CALL SETER ('MAPSAV - ERROR ON WRITE',IIER,1)
      RETURN
C
      END
