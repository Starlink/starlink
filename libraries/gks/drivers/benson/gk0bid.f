
****************************************************************






*======================================================================
*                BENSON UTILITY ROUTINES
*======================================================================
      SUBROUTINE GK0BID
*
* (C) COPYRIGHT ICL & SERC 1985
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of Workstation Driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BENSON Initialise Device
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/85  DRJF  Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    KFRX,   KFRY,   KFRORX,   KFRORY,   KORDER
      PARAMETER (KFRX=1, KFRY=2, KFRORX=3, KFRORY=4, KORDER=5)
      INTEGER    KMAXX,  KXACFR,   KADJFR,   IXPEN,   IYPEN
      PARAMETER (KMAXX=6,KXACFR=7, KADJFR=8, IXPEN=9, IYPEN=10)
      INTEGER    KPLWID
      PARAMETER (KPLWID=17800)
      INTEGER     KXACR
      PARAMETER  (KXACR=1)
      INTEGER    NCHAR,   LENBUF
      PARAMETER (NCHAR=28,LENBUF=41)
      INTEGER NLEFT
*
*     Record size
*
      INTEGER KRECSZ(1)
*
*     FF83 - Paper limit
*
      INTEGER IFF83(4)
*
*     Maximum displacement size of one frame in mm. Used in
*     setting up the FF83 order
*
      INTEGER MFRDSZ
*
*     FF86 - Header information
*
      INTEGER IBUFF(LENBUF)
*
*     End of record buffer
*
      INTEGER ENDREC(4)
*
*     Inter frame gap
*
      INTEGER IGAP
      CHARACTER*(NCHAR) HEADER
      CHARACTER*3  MONTHS(1:12)
      INTEGER    CENT
      PARAMETER (CENT=1900)
      INTEGER YEAR,MONTH,DAY
      INTEGER HOUR,MIN,SEC
      DATA KRECSZ/128/
      DATA IFF83/255,131,0,0/
      DATA MFRDSZ/3000/
      DATA IBUFF/255,134,10*0,NCHAR,NCHAR*0/
      DATA ENDREC/255,32,0,0/
      DATA IGAP/10/
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     :            'SEP','OCT','NOV','DEC'/
*
*----------------------------------------------------------------------


*
*     Initialise buffer counters
*
      CALL GKIOFO(KIOIT,1,KRECSZ,NLEFT)
*
*     The colour table defined in the WDT file contains the
*     background colour and the pen colours available on the
*     plotter. Hence the maximum pen number that can be used in
*     plotting is this total minus 1
*
      IBUFF(6)=(KPCI(KWKIX)-1)*16
*
*     Set order to null
*
      KWKDAT(KORDER,KWKIX)=0
*
*     Taken from Gino Driver (Benson documentation says these bytes
*     are reserved?)
*
      IBUFF(9)=69
      IBUFF(10)=136
      CALL GKDATE(YEAR,MONTH,DAY)
      YEAR=CENT+YEAR
      CALL GKTIME(HOUR,MIN,SEC)
      WRITE(HEADER,100) DAY,MONTHS(MONTH),YEAR,HOUR,MIN,SEC
  100 FORMAT ('RAL GKS ',I2,1X,A3,1X,I4,1X,I2,':',I2,':',I2)
      IF (HOUR.LE.9) HEADER(21:21)='0'
      IF (MIN .LE.9) HEADER(24:24)='0'
      IF (SEC .LE.9) HEADER(27:27)='0'
      CALL GKNTOA(NCHAR,HEADER,IBUFF(14))
      CALL GKIOFO(KIOPB,LENBUF,IBUFF,NLEFT)
*
*     Set up the FF83 order
*
      IFF83(3)=(MFRDSZ+IGAP)/256
      IFF83(4)=MOD((MFRDSZ+IGAP),256)
      CALL GKIOFO(KIOPB,4,IFF83,NLEFT)
*
*     The following code is dependent on the workstation
*     types being used. If new workstation types are
*     introduced the following code may have to be changed
*
*     Determine whether DC X or DC Y lie across the paper
*
      IF (MOD(KWKTYP,10).EQ.5) THEN
*
*       Workstation is of type ADJUST
*
        KWKDAT(KADJFR,KWKIX)=1
        IF (KDSRY(KWKIX).GT.KDSRX(KWKIX)) THEN
*
*         Portrait
*
          KWKDAT(KXACFR,KWKIX)=1
        ELSE
*
*         Landscape
*
          KWKDAT(KXACFR,KWKIX)=0
        ENDIF
      ELSE
*
*       Workstation of type any other than ADJUST
*
        IF (KDSRY(KWKIX).GT.KDSRX(KWKIX)) THEN
*
*         Portrait
*
          KWKDAT(KXACFR,KWKIX)=MOD(KWKTYP-1,2)
        ELSE
*
*         Landscape
*
          KWKDAT(KXACFR,KWKIX)=MOD(KWKTYP,2)
        END IF
      END IF
*
*     Set frame variables (relative to X axis of the plotter
*     (along the roll) and the Y axis of the plotter (across
*     the roll)
*
      IF (KWKDAT(KXACFR,KWKIX).EQ.KXACR) THEN
*
*       DC Xacross
*
*       Initialise Y origin to cause new row of frames in GK0BNF
*
        KWKDAT(KFRORY,KWKIX)=KPLWID
      ELSE
*
*       DC Yacross
*
*       Initialise Y origin to cause new row of frames in GK0BNF
*
        KWKDAT(KFRORY,KWKIX)=0
      END IF
*
*     Initialise X origin
*
      KWKDAT(KFRORX,KWKIX)=0
*
*     Initialise frame widths
*
      KWKDAT(KFRY,KWKIX)=0
      KWKDAT(KFRX,KWKIX)=0
*
*     Initialise maximum displacement of plotter along the
*     roll so that the first frame origin is positioned
*     correctly on the paper
*
      KWKDAT(KMAXX,KWKIX)=0
*
*     Initial pen position is absolute (0,0)
*
      KWKDAT(IXPEN,KWKIX)=0
      KWKDAT(IYPEN,KWKIX)=0
*
*     Set up end of record to FF20 order
*
      CALL GKIOFO(KIOEB,4,ENDREC,NLEFT)
*
      END
