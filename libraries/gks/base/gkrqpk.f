      SUBROUTINE GKRQPK (PKAP,XPPSUB,XCSSUB,XFTSUB)
*-----------------------------------------------------------------
*
      INCLUDE '../include/check.inc'

*  PURPOSE
*  -------
*     Emulate pick using locator
*
*  MAINTENANCE LOG
*  ---------------
*     28/04/89  KEVP  Original version stabilised
*     16/11/89  RMK   Removed unused local variables.
*     03/05/90  RMK   Added check on KERROR after GKRQIP call.
*     11/04/91  KEVP  Corrected Comments
*
*  ARGUMENTS
*  ---------
*     INP  PKAP    Pick Aperture in DC units
*     INP  XPPSUB  Device Primitive Pick Routine
*     INP  XCSSUB  Device Cursor Routine
*     INP  XFTSUB  Device Font Details Routine
*
      REAL PKAP
      EXTERNAL XPPSUB, XCSSUB, XFTSUB
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkinp.par'
*
      INCLUDE '../include/gkwca.cmn'
*
*     /WCA/  Read and modified as indicated
* --> Data expected (1st entry)
*     KWI1   : device number
*     KWI2   : KNIL indicating 1st entry
*
* --> Data expected (echo/unecho actioned)
*     KWI1   : device number
*     KWI2   : actioned request (ECHO,UNECHO)
*
* <-- Data returned (pick complete)
*     KWI1   : KNIL indicating pick completed
*     KWI2   : segment name
*     KWI3   : pick identifier
*     KWI4   : status
*
* <-- Data returned (echo/unecho request)
*     KWI1   : echoplay request (ECHO,UNECHO)
*     KWI2   : segment name
*     KWI3   : pick identifier
*     KWI4   : primitive identifier
*     KWI5   : echo specification (SEGMENT,PICKID,PRIMITIVE)
*     KWI6   : primitive type (KPL,KPM,KTX,KFA,KCA,KGDP)
*
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/gkpc.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwdt.cmn'
*
*
*  LOCALS
*  ------
*     DOPICK True, if pick is to requested on this entry
*     INDEV  TEK device code
*     INTA   Integer data
*     ITYPE  Primitive type
*     IREQ   Request for next entry
*     NDEV   Pick Device Number
*     NPKID  Pick ID
*     NPRIM  Primitive Number
*     NSEG   Segment name
*     PKAP   Pick aperture
*     REALA  Real data
*     XP,YP  Pick point in DC

      INTEGER INTA(9), NSEG, NPKID, NPRIM
      INTEGER ITYPE, NDEV, IREQ
      REAL REALA(6), XP,YP
      LOGICAL DOPICK

*
*  COMMENTS
*  --------
*     This utility uses CSS segments for PICK. To speed up the
*     process the bounding box facility of the segment list
*     GKSLxx utilities can be used. To do so you call GKSGWB
*     instead of GKSGWK in the workstation driver.
*
*-------------------------------------------------------------------

*   Get Pick device information (KWI1 is the Pick Device Number)
      NDEV = KWI1
      CALL GKRQIP(GPICK,NDEV,9,4,INTA,REALA)


*   KERROR set to 140 if input device NCDS not present on workstation
      IF (KERROR.NE.0) GOTO 739

*   Request Pick, only if action KWI2 = KNIL and go to echo
*                                            if req'd, else finish.
*                 else if action KWI2 = KPECHO go to unecho.
*                 else if action KWI2 = KPUNEC finish and
*                                              restore WCA.
*                                KWI2 = KPSCAN not expected.

      DOPICK = (KWI2 .EQ. KNIL)
      IF(INTA(KIPE) .EQ. GECHO)THEN
         IF(KWI2 .EQ. KNIL)THEN
            IREQ = KPECHO
            KWI4 = KNIL
         ELSEIF(KWI2 .EQ. KPECHO)THEN
            IREQ = KPUNEC
            KWI4 = KPCPI
         ELSEIF(KWI2 .EQ. KPUNEC)THEN
            IREQ = KNIL
            KWI4 = GOK
         ENDIF
      ELSE
         IREQ = KNIL
         KWI4 = GOK
      ENDIF

      IF (DOPICK) THEN
*      Obtain initial position of cursor
         NSEG = INTA(KPCISG)
         NPKID = INTA(KPCINI)
         XP = 0.5
         YP = 0.5
         CALL GKPNTP (NSEG,NPKID,KNIL,XP,YP)
*  -- CHANGE to NDC ---

  737    CONTINUE
*      Obtain Locator
         KWI1 = NDEV
         CALL GKRQLC(XCSSUB, XP,YP)
*
*      If locator OK continue with emulating Pick
         IF(KWI1 .EQ. GOK)THEN

*         Obtain Pick values from locator value.
            CALL GKPIKP(XP,YP,PKAP,XFTSUB,XPPSUB,
     :                  NSEG,NPKID,NPRIM,ITYPE)


*           If no segment is picked quit with no pick
            IF(NSEG .EQ. KNIL)THEN
               KWI1 = KNIL
               KWI4 = GNPICK
            ELSE
*           else leave with usual data
               KWI1 = IREQ
               KWI2 = NSEG
               KWI3 = NPKID
               IF(IREQ .EQ. KNIL)THEN
                 KWI4 = GOK
               ELSE
                 KWI4 = NPRIM
                 KWI6 = ITYPE
               ENDIF
            ENDIF
         ELSE
*        locator not OK
            KWI1 = KNIL
            KWI4 = GNONE
         ENDIF
      ELSE
         KWI1 = IREQ
         KWI2 = KPCSG
         KWI3 = KPCID
         IF(IREQ .EQ. KNIL)THEN
            KWI4 = GOK
         ELSE
           KWI4 = KPCPI
           KWI6 = KPCPT
         ENDIF
      ENDIF
      KWI5 = KPID

  739 CONTINUE

      END
