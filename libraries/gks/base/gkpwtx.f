C# IL>= a, OL>= 0
         SUBROUTINE GKPWTX(NC,ITXT,XBVCH,YBVCH,XWKHFD,XWKPLN)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To perform text minimal simulation (i.e. draw boundary)
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/86  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP NC       Number of entries in ITXT
*     INP ITXT     Integer array of character codes
*     INP XBVCH    Baseline vector of hardware char
*     INP YBVCH    Baseline vector of hardware char
*     INP XWKHFD   Routine to obtain Hardware text details
*     INP XWKPLN   Routine to draw polyline
*
      INTEGER NC, ITXT(NC)
      REAL XBVCH,YBVCH
      EXTERNAL XWKHFD,XWKPLN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   QWR1       : X coordinate of text (NDC)
*     Read   QWR2       : Y coordinate of text (NDC)
*     Set    QWR3..QWR4 : Width vector from WCA (NDC)
*     Set    QWR5..QWR6 : Height vector from WCA (NDC)
*     Set    QWR7..QWR8 : Concatenation point (set by utility)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     XNDC     X-text extent (NDC)
*     YNDC     Y-text extent (NDC)
*     XDC      X-text extent (DC)
*     YDC      Y-text extent (DC)
*
      REAL XNDC(5),YNDC(5)
      REAL XDC(5),YDC(5)
*
*---------------------------------------------------------------------

* Set width vector
      QWR3=QICHWX
      QWR4=QICHWY
* Set height vector
      QWR5=QICHHX
      QWR6=QICHHY

* Invoke utility appropriate to text precision
      IF(KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NC,ITXT,XNDC,YNDC)
      ELSE
        CALL GKXQXC(NC,ITXT,XBVCH,YBVCH,XNDC,YNDC,XWKHFD)
      ENDIF

* Join first to last point
      XNDC(5)=XNDC(1)
      YNDC(5)=YNDC(1)

* Transform, clip and draw
      IF(KERROR.EQ.0) THEN
        CALL GKTWD(5,XNDC,YNDC,XDC,YDC)
        CALL GKLCLP(5,XDC,YDC, .FALSE., 1.0,
     :     QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),QWCLYT(KWKIX),
     :     XWKPLN)
      ENDIF

      END
