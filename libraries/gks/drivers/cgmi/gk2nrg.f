      SUBROUTINE GK2NRG(RGBVAL,MMSET)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine gets an RGB value from the CGM,
*  returning it as three real values.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*        RGBVAL : Output Real array
*        MMSET  : Flag showing whether the min & max colour vals are
*                 set
*
      REAL RGBVAL(3)
      LOGICAL MMSET
*
*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gkdt.par'

      INCLUDE '../../include/gkwca.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*  LOCALS
*  ------
*        INPINT : Input integer to be converted
*        I      : Loop counter
*        N      : Loop counter

      INTEGER INPINT, I, N

*---------------------------------------------------------------------

*   Initialise array
      DO 5 N=1,3
         RGBVAL(N) = 0
    5 CONTINUE

*   Loop for each byte
      DO 50 I=1,KCOLPR(KWKIX) /2
         CALL GK2NNC(INPINT,.TRUE.)

*   Get each bit (split byte into 2 groups of 3 bits; ignoring b7 &8)
         DO 10 N=3,1,-1
            RGBVAL(N) = (RGBVAL(N)*4) + MOD(INPINT,2)
            INPINT=INPINT/2
   10    CONTINUE
         DO 20 N=3,1,-1
            RGBVAL(N) = RGBVAL(N) + (MOD(INPINT,2)*2)
            INPINT=INPINT/2
   20    CONTINUE
  50  CONTINUE
      IF(MOD(KCOLPR(KWKIX) ,2).EQ.1) THEN
         CALL GK2NNC(INPINT,.TRUE.)
         DO 60 N=3,1,-1
            INPINT = INPINT/8
            RGBVAL(N) = (RGBVAL(N)*2) + MOD(INPINT,2)
            INPINT = INPINT/2
  60     CONTINUE
      ENDIF

*   Convert to range of 0.0 to 1.0
      IF (MMSET) THEN
         DO 70 N=1,3
            IF ( QMXCOL(N)-QMNCOL(N) .LE. 1E-3) THEN
               RGBVAL(N) = QMNCOL(N)
            ELSE
               RGBVAL(N) = (RGBVAL(N)-QMNCOL(N))/(QMXCOL(N)-QMNCOL(N))
            ENDIF
  70     CONTINUE
      ENDIF
      RETURN
      END
