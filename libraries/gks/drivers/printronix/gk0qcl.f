





***************************************************************************
***************************************************************************



*==========================================================================
*              PRINTRONIX UTILITY ROUTINES
*==========================================================================



      SUBROUTINE GK0QCL
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*                    AJC  Modification for Printronix P300 Lineprinter
*                    PLP  Modifications for PRIME
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Clear display suface (output bitmap to file)
*
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'


*     Intrinsic functions declaration

      INTRINSIC MOD
*
*     External functions declaration
*
      INTEGER GKNA1,GK0QLC
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NCOL,NROW
      PARAMETER (NCOL=2,NROW=3)
*
      INTEGER IX,IY,NCHR,IGINO(6),I
      INTEGER LPLOT,LF
      PARAMETER (LPLOT=5, LF=138)
      INTEGER INTA(134),IFU1,NINTA,IOUT1
*
      DATA IGINO/1,1,177,160,1,0/
*
*  ALGORITHM
*  ---------
*
*     Write bitmap to Printronix-format external file row by row.
*     Each row contains up to 130 characters of graphics data plus
*     PLOT control and LineFeed characters, each embedded with one
*     blank(of which 160 is decimal ASCII equivalent on the PRIME).

*--------------------------------------------------------------------

*     Initialise  record and buffer counters in GKIOFO

      NCHR=KWKDAT(NROW,KWKIX)+2
      IF(MOD(NCHR,2).NE.0)NCHR=NCHR+1
      INTA(1)=NCHR+2
      NINTA=1
      IFU1=KIOIT
      CALL GKIOFO(IFU1, NINTA, INTA, IOUT1)

*     Set End-Of-Record character to LF

      INTA(1)=LF
      INTA(2)=160
      NINTA=2
      IFU1=KIOEB
      CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)

*     Now do as GINO does - insert form controlling header

      DO 10 I=1,3
         NINTA=2
         INTA(1)=IGINO(2*I-1)
         INTA(2)=IGINO(2*I)
         IFU1=KIOPB
         CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)
*
*        We don't want to continue if an error occured while
*        writing to the file( KERROR=304).
*
         IF(KERROR.NE.0)RETURN
*
         IFU1=KIOER
         CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)
   10 CONTINUE
*
*     Do the actual transfer from the bitmap to external file via GKIOFO
*
      DO 100 IY=KWKDAT(NCOL,KWKIX)-1,0,-1
         DO 200 IX=0,KWKDAT(NROW,KWKIX)-1
            INTA(IX+1)=GKNA1(CHP(GK0QLC(IX*6,IY)))
  200    CONTINUE

*        Padd with zero if row contains odd number of characters

         NINTA=KWKDAT(NROW,KWKIX)
         IF(NINTA+2.NE.NCHR)THEN
            INTA(NINTA+1)=0
         ENDIF
         NINTA=NCHR

*        Insert PLOT MODE control character at the end of the record

         INTA(NINTA-1)=0
         INTA(NINTA)=LPLOT

*        Send one row to the buffer

         IFU1=KIOPB
         CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)
*
*        We don't want to continue if an error occured while
*        writing to the file( KERROR=304).
*
         IF(KERROR.NE.0)RETURN
*
*        End this record and start new one
*

         IFU1=KIOER
         CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)

  100 CONTINUE

*     Flush the last buffer

      IFU1=KIOSN
      CALL GKIOFO(IFU1,NINTA,INTA,IOUT1)

      END
