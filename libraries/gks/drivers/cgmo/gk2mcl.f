      SUBROUTINE GK2MCL
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This does all the resetting etc for each GKS
*  clear workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gaspct.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'

      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkplb.cmn'
      INCLUDE '../../include/gkpmb.cmn'
      INCLUDE '../../include/gktxb.cmn'
      INCLUDE '../../include/gkfab.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*
*  LOCALS
*  ------
* Do Loop Variables & temporary variable
      INTEGER I,OLD,NEW

*   Buffer variables
      CHARACTER INCHAR*81, MANTIS*9, EXPONE*9

* Parameters
      INTEGER    REALPT
      PARAMETER (REALPT=1,OLD=41,NEW=42)
      CHARACTER*5  STSTRT, STTERM
      PARAMETER (STSTRT = '1B58 ', STTERM = '1B5C ')

*---------------------------------------------------------------------

*   End Picture except first time
      IF ( KCLEAR ) THEN
         CALL GK2MHA('3024 ')
      ELSE
         KCLEAR = .TRUE.
      ENDIF

*   Set up state list variables
      DO 10 I=1,13
         KSETAS(OLD,I,KWKIX)=0
  10  CONTINUE
      DO 20 I=1,16
         QSTLST(OLD,I,KWKIX)=0.0
         KSTLST(OLD,I,KWKIX)=1
  20  CONTINUE
      KWKDAT(1,KWKIX)=1
      KWKDAT(2,KWKIX)=1
      KWKDAT(3,KWKIX)=1
      KWKDAT(4,KWKIX)=1
      KSTLST(OLD,3,KWKIX)=3
      KSTLST(OLD,8,KWKIX)=0
      KSTLST(OLD,10,KWKIX)=0
      KSTLST(OLD,11,KWKIX)=0
      KSTLST(OLD,12,KWKIX)=0
      KSTLST(OLD,13,KWKIX)=0
*  Set linewidth
      QSTLST(OLD,1,KWKIX)=1.0
*  Set marker size
      QSTLST(OLD,2,KWKIX)=1.0
*  Set character expansion factor
      QSTLST(OLD,3,KWKIX)=1.0
      QSTLST(OLD,17,KWKIX)=1.0
      QSTLST(OLD,18,KWKIX)=1.0
      QSTLST(OLD,19,KWKIX)=0.0
      QSTLST(OLD,20,KWKIX)=0.0
      QSTLST(OLD,21,KWKIX)=1.0
      QSTLST(OLD,22,KWKIX)=1.0

*   Begin Picture
      KCHANO(KWKIX)=201
      CALL GK2MHA('3022 ')
      CALL GK2MHA(STSTRT)
      CALL GK2MEA(8,'PICTURE ')
      WRITE (INCHAR,35) KWKDAT(5,KWKIX)
  35  FORMAT(I8)
      DO 40 I=1,8
         IF(INCHAR(I:I).NE.' ')GOTO 50
  40  CONTINUE
  50  CONTINUE
      CALL GK2MEA(8-I+1,INCHAR(I:8))
      KWKDAT(5,KWKIX)=KWKDAT(5,KWKIX)+1
      CALL GK2MHA(STTERM)

*   VDC Extent
*   If settings have changed then update the statelist & output changes
      IF( (QSTLST(OLD,19,KWKIX).NE.QSTLST(NEW,19,KWKIX)).OR.
     +   (QSTLST(OLD,20,KWKIX).NE.QSTLST(NEW,20,KWKIX)).OR.
     +   (QSTLST(OLD,21,KWKIX).NE.QSTLST(NEW,21,KWKIX)).OR.
     +   (QSTLST(OLD,22,KWKIX).NE.QSTLST(NEW,22,KWKIX)))THEN
         CALL GK2MHA('3225 ')
         DO 60 I=19,22
            QSTLST(OLD,I,KWKIX)=QSTLST(NEW,I,KWKIX)
            CALL GK2MRH(QSTLST(OLD,I,KWKIX),MANTIS,EXPONE,REALPT)
            CALL GK2MBU(MANTIS)
            CALL GK2MBU(EXPONE)
  60     CONTINUE
      ENDIF

*   Background Colour
      CALL GK2MHA('3226 ')
      CALL GK2MRG( QCLREP(0,1,KWKIX), QCLREP(0,2,KWKIX),
     +             QCLREP(0,3,KWKIX), KCOLPR(KWKIX) )

*   Begin Picture Body
      CALL GK2MHA('3023 ')

*   If Set colour representation has been used then output items again
      DO 80 I=1,256
         IF(QCLREP(I,1,KWKIX).GT.-1)THEN
*   write index to CGM
            CALL GK2MHA('3630 ')
            CALL GK2MIW(1,I)

*   Specify Bitstream Format
            CALL GK2MIW(1,0)

*   Write RGB values to CGM
            CALL GK2MRG( QCLREP(I,1,KWKIX), QCLREP(I,2,KWKIX),
     +                   QCLREP(I,3,KWKIX), KCOLPR(KWKIX) )

         ENDIF
  80  CONTINUE

*   Clip rectangle
*   If settings have changed then update the statelist & output changes
      IF( (QSTLST(OLD,15,KWKIX).NE.QSTLST(NEW,15,KWKIX)).OR.
     +   (QSTLST(OLD,16,KWKIX).NE.QSTLST(NEW,16,KWKIX)).OR.
     +   (QSTLST(OLD,17,KWKIX).NE.QSTLST(NEW,17,KWKIX)).OR.
     +   (QSTLST(OLD,18,KWKIX).NE.QSTLST(NEW,18,KWKIX)))THEN
         CALL GK2MHA('3324 ')
         DO 70 I=15,18
            QSTLST(OLD,I,KWKIX)=QSTLST(NEW,I,KWKIX)
            CALL GK2MRH(QSTLST(OLD,I,KWKIX),MANTIS,EXPONE,REALPT)
            CALL GK2MBU(MANTIS)
            CALL GK2MBU(EXPONE)
  70     CONTINUE
      ENDIF

      RETURN
      END
