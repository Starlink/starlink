C# IL>=a, OL>=1
      SUBROUTINE GKPCRD(IENT,NR,NI,NC,NRP,NIP,NCP,MORE)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM

*  Type of routine:    W/S
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Pick echoplay "read" interface to CSS.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT IENT  - Entrypoint code
*     OUT NI    - Size of integer data
*     OUT NIP   - Stack pointer to Integer data
*     OUT NR    - Size of real data
*     OUT NRP   - Stack pointer to Real data
*     OUT NC    - Size of character data
*     OUT NCP   - Stack pointer to Character data
*     OUT MORE  - MORE in this segment item
*
      INTEGER IENT, NI, NIP, NR, NC, NRP, NCP, MORE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkpc.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     IENTRY  Entry point code for current item
*
      INTEGER IENTRY
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*   Primitive type (not used) is in KPCPT.
*
*---------------------------------------------------------------------

*     continue if previous item was sliced primitive
      IF(KPCMOR.EQ.GMORE) GOTO 888

*     stop replay when requested
      IF(KPCRQ.EQ.KPSTOP) THEN
        IENT=KENSG
        NI=0
        NR=0
        NC=0
        NIP=KNIL
        NRP=KNIL
        NCP=KNIL
        MORE=GNMORE
        GOTO 999
      ENDIF

*     loop to examine next item of segment
 10   CONTINUE
      IF(KERROR.EQ.0) THEN

*       begin item
        CALL GKPCBI(IENTRY)

*       look for primitives
        IF(KPL.LE.IENTRY .AND. IENTRY.LE.KGDP) THEN

*         update count of primitives
          KPCNP=KPCNP+1

*         now decide if this primitive is a required item
          IF(KPCPET.NE.KPSG) THEN

            IF(KPCPET.EQ.KPP) THEN

              IF(KPCPI.EQ.KPCNP) THEN
*               set to stop echoplay after this item
                KPCRQ=KPSTOP
              ELSE
*               skip this primitive
                CALL GKPCSI
                GOTO 10
              ENDIF

            ELSEIF(KPCPET.EQ.KPID .AND. KPCID.NE.KPCNID) THEN
*             skip this primitive
              CALL GKPCSI
              GOTO 10

            ENDIF
          ENDIF
        ENDIF
      ENDIF

*     read and return this item
 888  CONTINUE
      CALL GKCSRD(IENT,NR,NI,NC,NRP,NIP,NCP,MORE)
      KPCMOR=MORE

*     record any change in pick identifier
      IF(IENT.EQ.KSPKID) KPCNID=KSS1


 999  CONTINUE

      END
