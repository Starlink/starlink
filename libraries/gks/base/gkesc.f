C# IL>=a, OL>=0
      SUBROUTINE GKESC
*
* (C) COPYRIGHT SERC  1988
*

*---------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:     Workstation utility
*  Author:             RMK
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To handle RAL GKS Escapes.  To be called by workstation driver
*     ESCAPE entrypoint.
*
*  MAINTENANCE LOG
*  ---------------
*     23/06/88  RMK   Original version stabilized.
*     06/12/91  DLT   Return error 180 for escapes other than -1 and -2
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
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
*
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
*  ERRORS
*  ------
*
*     180   Specified function is not supported
*     182   Contents of escape data record are invalid
*
*  COMMENTS
*  --------
*     This routine handles escapes -1 and -2 which associate/disassociate
*     two input_class/input_device_number pairs, allowing two request
*     input calls to be triggered by a single operator action.  See
*     GIPN 39 for details.
*
*     NOTE: At present, this routine is restricted to dealing with
*     Request Locator (with locator device 1) and Request Choice (with
*     choice device 2).
*
*---------------------------------------------------------------------

*     Check escape fuction
      IF (KWI1.LT.-2 .OR. KWI1.GT.-1) THEN
         KERROR = 180
         GO TO 9999
      ENDIF

*     Check values which were passed in data record from user
*     Only allow locator device 1 and choice device 2
      IF (KWI2.EQ.GLOCAT .AND. KWI3.EQ.1) THEN
         IF (KWI4.NE.GCHOIC .OR. KWI5.NE.2) KERROR = 182
      ELSE IF (KWI2.EQ.GCHOIC .AND. KWI3.EQ.2) THEN
         IF (KWI4.NE.GLOCAT .OR. KWI5.NE.1) KERROR = 182
      ELSE
         KERROR = 182
      ENDIF
      IF (KERROR.NE.0) GOTO 9999

      IF (KWI1.EQ.-1) THEN
*        Have associate escape, initialise stored data flags
         KWKDAT(ICHFLG, KWKIX) = GNO
         KWKDAT(ILCFLG, KWKIX) = GNO
      ELSE
*        KWI1=-2, have disassociate escape, set stored data
*        flags to KNIL to indicate there is not an associated device
         KWKDAT(ICHFLG, KWKIX) = KNIL
         KWKDAT(ILCFLG, KWKIX) = KNIL
      ENDIF
*     In either case, set stored data to KNIL/QNIL
      KWKDAT(ICHSTA, KWKIX) = KNIL
      KWKDAT(ICHNUM, KWKIX) = KNIL
      KWKDAT(ILCSTA, KWKIX) = KNIL
      QWKDAT(ILCX, KWKIX) = QNIL
      QWKDAT(ILCY, KWKIX) = QNIL

 9999 CONTINUE
      END
