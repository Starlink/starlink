      SUBROUTINE GK2DSC(INDEC)
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set colour representation
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85  GGT  Original version stabilized
*     10/01/86  GGT  Adapted for ACW
*     24/02/87  GGT  Rewrite to convert RGB to HLS
*     21/01/88   AA  Added GKMC.PAR now needed by GKHP.PAR
*  ARGUMENTS
*  ---------
*     INP INDEC    Colour index

      INCLUDE '../../include/check.inc'
*
      INTEGER INDEC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
*
*  LOCALS
*  ------
*
      REAL R,G,B,M,H,L,S,SM,SR,SG,SB
      INTEGER IH,IL,IS,NLEFT
*
      CHARACTER*20 BUFFER

*
*  COMMENTS
*  --------
*	RGB values are converted to HLS for ReGIS
*
* --------------------------------------------------------------

      R=QHP(KHPXR(KCTBPT(1,KWKIX))+INDEC)
      G=QHP(KHPXR(KCTBPT(2,KWKIX))+INDEC)
      B=QHP(KHPXR(KCTBPT(3,KWKIX))+INDEC)
*
      IF (KWKDAT(MODE,KWKIX).EQ.0) THEN
*        Monochrome
         L=0.3*R + 0.59*G + 0.11*B
         IL=INT(L*100+0.5)
         WRITE(BUFFER,100)INDEC,IL
  100    FORMAT('S(M',I1.1,'(L',I3.3,'))')
      ELSE
*        Colour
         M=MAX(R,G,B)
         SM=MIN(R,G,B)
*
         IF(M.NE.SM) THEN
            SR=(M-R)/(M-SM)
            SG=(M-G)/(M-SM)
            SB=(M-B)/(M-SM)
         ENDIF
*
*	Calculate Lightness
*
         L=0.5*(M+SM)
*
*	Calculate Saturation
*
         S=0.0
         IF(M.NE.SM) THEN
            S=(M-SM)/(M+SM)
            IF(L.GT.0.5) S=(M-SM)/(2.0-M-SM)
         ENDIF
*
*	Calculate Hue
*
         H=0.0
         IF(S.EQ.0.0) GOTO 10
         H=2.0+SB-SG
         IF(R.EQ.M) GOTO 10
         H=4.0+SR-SB
         IF(G.EQ.M) GOTO 10
         H=6.0+SG-SR
   10    CONTINUE
         H=AMOD(H*60.0,360.0)
*
*	Convert to range 0-100 for ReGIS
*
         IH=INT(H+0.5)
         IL=INT(100.0*L+0.5)
         IS=INT(100.0*S+0.5)
*
*	and output the command
*
         WRITE(BUFFER,101)INDEC,IH,IL,IS
  101    FORMAT('S(M',I1.1,'(AH',I3.3,'L',I3.3,'S',I3.3,'))')
      ENDIF
*
      CALL GKIOCO(KIOPB,BUFFER,NLEFT)
*
      RETURN
      END

