C# IL>=a, OL>=0
      SUBROUTINE GKSOTW(IWK,IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND - W/S
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation interface routine called by GKS front end to send
*     control to one specific workstation,identified by
*     workstation type.
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/92  KEVP  Created from GKSONW to enable ESCAPE to be
*                     called for a workstation type.
*
*  ARGUMENTS
*  ---------
*     INP IWK  - workstation type
*     INP IENT - entrypoint code
*     INP NID  - number of elements in array IDAT
*     I/O IDAT - integer data passed to or from workstation driver
*     INP NRD  - number of elements in array RX and RY
*     I/O RX   - real x-coordinates passed to or from workstation driver
*     I/O RY   - real y-coordinates passed to or from workstation driver
*     INP NCD  - length of character array
*     I/O STR  - character array
*
      INTEGER IWK,IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKDT /    KWK,KMXWKT
*     Read   /GKDT /    KLAWKT
*     Read   /GKWKE/    KQWKCA,KQPXAD
*     Read   /GKWCB/    KWKID,KWTYIX
*     Read   /GKSTK/    KIUSED
*     Modify /GKWCA/    KWKTYP,KWKIX
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IWKTYP,ITEMP,I
      CHARACTER*9 NAME
      PARAMETER (NAME='GKSOTW')
*
*  ERRORS
*  ------
*     22   Specified workstation type is invalid
*     23   Specified workstation type does not exist
*
*---------------------------------------------------------------------


      IF (IWK.LE.0) THEN

        KERROR = 22
        GOTO 999

      ELSE

* Determine, if workstation type exists

        DO 30 I=1,KMXWKT
          IF (KLAWKT(I).EQ.IWK) GOTO 40
   30   CONTINUE

        KERROR = 23
        GOTO 999

   40   CONTINUE
        KWKTYP = IWK
        IWKTYP = I

* Set workstation index

        DO 50 I=1,KNOPWK
          IF (KWTYIX(I).EQ.IWKTYP) THEN
            KWKIX = I
            GOTO 60
          ENDIF
   50   CONTINUE
          KWKIX = KNIL
   60   CONTINUE

      ENDIF

* Call workstation

      INCLUDE 'cdrive.inc'

  999 CONTINUE
      END
