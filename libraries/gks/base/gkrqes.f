C# IL>=a, OL>=0
      SUBROUTINE GKRQES(ICURS)
*
* (C) COPYRIGHT SERC  1988
*

*---------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:     Workstation utility
*  Author:              RMK
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To handle locator or choice input when locator device 1 has
*     been associated with choice device 2 by a GESC call.
*
*  MAINTENANCE LOG
*  ---------------
*     23/06/88  RMK   Original version stabilized.
*
*  ARGUMENTS
*  ---------
*     INP  ICURS  Workstation driver cursor input routine
*
*  EXTERNALS
*  ---------
      EXTERNAL ICURS
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
*     ICHFLG  Stored choice data flag: can be GYES, GNO or KNIL
*             if there is no associated device
*     ICHSTA  Choice status  }  stored choice data
*     ICHNUM  Choice number  }
*     ILCFLG  Stored locator data flag: can be GYES, GNO or KNIL
*             if there is no associated device
*     ILCSTA  Locator status         }
*     ILCX    Locator X coord        }  stored locator data
*     ILCY    Locator Y coord        }
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace ....
*     ICODE       ASCII code of key kit
*     XDC, YDC    Cursor position in device coordinates
*     XNDC, YNDC  Cursor position in normalised device coordinates
*
*     ... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace ....
      INTEGER ICODE, ICTRLZ, IRETN, ISPACE, IDEL
      PARAMETER(ICTRLZ=26, IRETN=13, ISPACE=32, IDEL=127)
      REAL XDC, YDC, XNDC, YNDC
*
*  ERRORS
*  ------
*
*
*---------------------------------------------------------------------

   50 CONTINUE
*     Read key hit and cursor
      CALL ICURS(ICODE, XDC,YDC)
      IF (ICODE.EQ.ICTRLZ) THEN
*        Break is CTRL-Z
         KWKDAT(ICHSTA, KWKIX) = GNONE
         KWKDAT(ICHNUM, KWKIX) = KNIL
         KWKDAT(ILCSTA, KWKIX) = GNONE
         QWKDAT(ILCX,   KWKIX) = QNIL
         QWKDAT(ILCY,   KWKIX) = QNIL
      ELSE
         CALL GKTDN(1, XDC, YDC, XNDC, YNDC)
*        If point is not within workstation window, try again
         IF (KERROR.NE.0) THEN
            KERROR = 0
            GOTO 50
         ENDIF
         QWKDAT(ILCX, KWKIX) = XNDC
         QWKDAT(ILCY, KWKIX) = YNDC
         IF (ICODE .EQ. IRETN) THEN
*           Have <RETURN>, which is nochoice
            KWKDAT(ICHSTA, KWKIX) = GNCHOI
            KWKDAT(ICHNUM, KWKIX) = KNIL
            KWKDAT(ILCSTA, KWKIX) = GOK
         ELSE IF (ICODE.LT.ISPACE .OR. ICODE.GT.IDEL) THEN
*           Choice break
            KWKDAT(ICHSTA, KWKIX) = GNONE
            KWKDAT(ICHNUM, KWKIX) = KNIL
            KWKDAT(ILCSTA, KWKIX) = GOK
         ELSE
*           OK
            KWKDAT(ICHSTA, KWKIX) = GOK
            KWKDAT(ICHNUM, KWKIX) = ICODE - ISPACE + 1
            KWKDAT(ILCSTA, KWKIX) = GOK
         ENDIF
      ENDIF
*     Set stored data flags
      KWKDAT(ICHFLG, KWKIX) = GYES
      KWKDAT(ILCFLG, KWKIX) = GYES

      END
