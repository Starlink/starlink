

      SUBROUTINE GK1TSH(IENT)
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S UTILITY
*  Author:             AS
*                      GGT this version
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Segment entrypoint handler for Tektronix 4107
*
*  MAINTENANCE LOG
*  ---------------
*     08/02/85  GGT   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IENT   - Entrypoint code
*
      INTEGER IENT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
      INTEGER ISGNO,NLEFT, NB, IPRI, IX, IY, MANT, EXP
      INTEGER HIX, HIY, LOX, LOY, IEB, I, J, IB(5)
      INTEGER ICREAT(9), IRENAM(9), IDELET(6), ITRANS(30)
      INTEGER IVISIB(7), IHIGHL(7), IPRIOR(9), ICLOSE(3)
      INTEGER IRENEW(4)
      REAL    SEGPRI, SX, SY, ANG, DX, DY, PI
      REAL    PTORX,PTORY
*
      PARAMETER (PTORX=6.0, PTORY=6.0)
*
      DATA ICREAT /27,83,80,27,83,79,0,0,0/
      DATA ICLOSE /27,83,67/
      DATA IRENAM /27,83,82,0,0,0,0,0,0/
      DATA IDELET /27,83,75,0,0,0/
      DATA ITRANS /27,83,73,27*0/
      DATA IVISIB /27,83,86,0,0,0,0/
      DATA IHIGHL /27,83,72,0,0,0,0/
      DATA IPRIOR /27,83,83,0,0,0,0,0,0/
      DATA IRENEW /27,75,78,48/
*

*
*  COMMENTS
*  --------
*

*       This routine is based on the CSS routine GKSGWK
*
*---------------------------------------------------------------------


      PI=4.0*ATAN(1.0)


* Conditional GOTO on entrypoint code

      GOTO (410,420,430,440,450,460,470,480,490,500,510) IENT-KCRSG+1

      GOTO 9999


*------------------------------------------------------------------
* Create segment
*------------------------------------------------------------------
  410 CONTINUE
       NB=7
       CALL GK1TTI(KWI1,ICREAT,NB)
       CALL GKIOBO(KIOPB,NB-1,ICREAT,NLEFT)
      GOTO 9999

*------------------------------------------------------------------
* Close segment
*------------------------------------------------------------------
  420 CONTINUE
      CALL GKIOBO(KIOPB,3,ICLOSE,NLEFT)
      GOTO 9999



*------------------------------------------------------------------
* Rename segment
*------------------------------------------------------------------
  430 CONTINUE
      NB=4
      CALL GK1TTI(KWI1,IRENAM,NB)
      CALL GK1TTI(KWI2,IRENAM,NB)
      CALL GKIOBO(KIOPB,NB-1,IRENAM,NLEFT)
      GOTO 9999



*------------------------------------------------------------------
* Delete segment
*------------------------------------------------------------------
  440 CONTINUE
      KWI2 = GPRSNT
      NB=4
      CALL GK1TTI(KWI1,IDELET,NB)
      CALL GKIOBO(KIOPB,NB-1,IDELET,NLEFT)
      GOTO 7777



*------------------------------------------------------------------
* Begin segment
*------------------------------------------------------------------
  450 CONTINUE
      GOTO 9999



*------------------------------------------------------------------
* End segment
*------------------------------------------------------------------
  460 CONTINUE
      GOTO 9999



*------------------------------------------------------------------
* Set segment transformation
*------------------------------------------------------------------
  470 CONTINUE
      IF (KWI5.EQ.2) THEN
        SX=SQRT(QWR1*QWR1 + QWR4*QWR4)
        SY=SQRT(QWR2*QWR2 + QWR5*QWR5)
        ANG=ACOS(QWR1/SX)*(180.0/PI)
        CALL GKTND(1,QWR3,QWR6,DX,DY)
        KERROR=0
        IX=INT(DX*PTORX+0.5)
        IY=INT(DY*PTORY+0.5)
*
        NB=4
        CALL GK1TTI(KWI1,ITRANS,NB)
        CALL GK1TTR(SX,MANT,EXP)
        CALL GK1TTI(MANT,ITRANS,NB)
        CALL GK1TTI(EXP,ITRANS,NB)
*
        CALL GK1TTR(SY,MANT,EXP)
        CALL GK1TTI(MANT,ITRANS,NB)
        CALL GK1TTI(EXP,ITRANS,NB)
*
        CALL GK1TTR(ANG,MANT,EXP)
        CALL GK1TTI(MANT,ITRANS,NB)
        CALL GK1TTI(EXP,ITRANS,NB)
*
        HIX=IX/128
        HIY=IY/128
        LOX=IX-HIX*128
        LOY=IY-HIY*128
        I=LOX
        J=LOY
        LOX=LOX/4
        LOY=LOY/4
        IEB=4*(J-LOY*4)+(I-LOX*4)
        IB(1)=HIY + 32
        IB(2)=IEB + 96
        IB(3)=LOY + 96
        IB(4)=HIX + 32
        IB(5)=LOX + 64
*
        DO 475 I=1,5
          ITRANS(NB)=IB(I)
          NB=NB+1
  475     CONTINUE
*
        CALL GKIOBO(KIOPB,NB-1,ITRANS,NLEFT)
        GOTO 7777
        ENDIF



*------------------------------------------------------------------
* Set visibility
*------------------------------------------------------------------
  480 CONTINUE
      NB=4
      CALL GK1TTI(KWI1,IVISIB,NB)
      CALL GK1TTI(KWI4,IVISIB,NB)
      CALL GKIOBO(KIOPB,NB-1,IVISIB,NLEFT)
      IF(KWI4.EQ.0) THEN
        GOTO 7777
      ELSE
        GOTO 8888
      ENDIF


*------------------------------------------------------------------
* Set highlighting
*------------------------------------------------------------------
  490 CONTINUE
      NB=4
      CALL GK1TTI(KWI1,IHIGHL,NB)
      CALL GK1TTI(KWI5,IHIGHL,NB)
      CALL GKIOBO(KIOPB,NB-1,IHIGHL,NLEFT)
      GOTO 8888


*------------------------------------------------------------------
* Set segment priority
*------------------------------------------------------------------
  500 CONTINUE
      NB=4
      CALL GK1TTI(KWI1,IPRIOR,NB)
      IPRI=INT(QWR1*32767.0 +0.5)
      CALL GK1TTI(IPRI,IPRIOR,NB)
      CALL GKIOBO(KIOPB,NB-1,IPRIOR,NLEFT)
      GOTO 9999



*------------------------------------------------------------------
* Set detectability
*------------------------------------------------------------------
  510 CONTINUE
      GOTO 7777


*----------------------------------------------------
* Renew the view
*----------------------------------------------------
 7777 CALL GKIOBO(KIOPB,4,IRENEW,NLEFT)


 8888 CONTINUE
      IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
* If regenerate immediate then suppress the playback
        KRPCC = KRPNO
        KRGN  = .TRUE.
        KWRGN(KWKIX) = .TRUE.
      ELSE
        KNFAUP(KWKIX) = GYES
      ENDIF


 9999 CONTINUE
      CALL GKIOBO(KIOSN,1,NLEFT,NLEFT)
      END
