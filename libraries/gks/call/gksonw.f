C# IL>=a, OL>=0
      SUBROUTINE GKSONW(IWK,IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND - W/S
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Workstation interface routine called by GKS front end to send
*     control to one specific workstation,identified by workstation
*     identifier or workstation type.
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  AS    Original version stabilized
*     18/03/83  AS    Define KWKIX for workstation type entries
*     21/03/83  AS    Change logical name for include on CDRIVE.INC
*     24/06/83  AS    Change error checking
*     16/08/83  AS    Add character array to argument list
*     11/11/83  AS    Bug fix - change KNOPWK to KWK
*
*  ARGUMENTS
*  ---------
*     INP IWK  - identifier or type - depends on entrypoint code
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
      PARAMETER (NAME='GKSONW')
*
*  ERRORS
*  ------
*     20   Specified workstation identifier is invalid
*     22   Specified workstation type is invalid
*     23   Specified workstation type does not exist
*     25   Specified workstation is not open
*
*---------------------------------------------------------------------


* Workstation identifier

      IF (IENT.LT.KQWKCA) THEN

        IF (IWK.LE.0) THEN

          KERROR = 20
          GOTO 999

        ELSE

* Find workstation index

          DO 10 I=1,KWK
            IF (KWKID(I).EQ.IWK) GOTO 20
   10     CONTINUE

          KERROR = 25
          GOTO 999

   20     CONTINUE
          KWKIX = I
          IWKTYP = KWTYIX(KWKIX)
          KWKTYP = KLAWKT(IWKTYP)

        ENDIF

      ELSE

* Workstation type

        IF (IWK.LE.0) THEN

          KERROR = 22
          GOTO 999

        ELSE

* If workstation type exists

          DO 30 I=1,KMXWKT
            IF (KLAWKT(I).EQ.IWK) GOTO 40
   30     CONTINUE

          KERROR = 23
          GOTO 999

   40     CONTINUE
          KWKTYP = IWK
          IWKTYP = I

* Set workstation index

          DO 50 I=1,KNOPWK
            IF (KWTYIX(I).EQ.IWKTYP) THEN
              KWKIX = I
              GOTO 60
            ENDIF
   50     CONTINUE
          KWKIX = KNIL
   60     CONTINUE

        ENDIF

      ENDIF

* Call workstation

      INCLUDE 'cdrive.inc'


  999 CONTINUE
      END
