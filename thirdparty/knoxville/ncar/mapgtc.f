C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPGTC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,BLAG,SLAG,BLOG,SLOG,PNTS(200)
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPCM5/ DDCT(5),LDCT(5),PDCT(10)
      CHARACTER*2     DDCT,LDCT,PDCT
      COMMON /MAPCMB/ IIER
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,SALF,CALF,SBET,CBET
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPGTC','VERSION  1')
C
      IF (WHCH(1:2).EQ.'AR') THEN
        CVAL=LDCT(ILTS)
      ELSE IF (WHCH(1:2).EQ.'OU') THEN
        CVAL=DDCT(NOUT+1)
      ELSE IF (WHCH(1:2).EQ.'PR') THEN
        CVAL=PDCT(JPRJ)
        IF (JPRJ.EQ.3.AND.ABS(SALT).GT.1.) CVAL=PDCT(10)
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
  901 IIER=1
      CALL MAPCEM (' MAPGTC - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      CVAL=' '
      RETURN
C
      END
