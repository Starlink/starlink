      SUBROUTINE GK2MRH(INREAL,CHAMAN,CHAEXP,TYPPNT)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine converts real numbers to hex,
*  returning the number as a mantissa & exponent.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
* in     INREAL : Input parameter containing real number
* out    CHAMAN : The mantissa of a real (in HEX)
* out    CHAEXP : The exponent of a real (in HEX)
* in     TYPPNT : Flag showing what type of real
*
      REAL INREAL
      CHARACTER CHAMAN*9, CHAEXP*9
      INTEGER TYPPNT
*
*  COMMON BLOCKS USAGE
*  -------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'
*
*  LOCALS
*  ------
*        INTOUT : The mantissa of a real
*        EXP    : The exponent of a real
*        PREC   : Current precision

*        NEGNUM : Flag showing whether the number is negative
*        EXPTRU : Flag showing whether Exponent is present

*        REALIN : Contains the real Number to be converted
*        MINREL : Smallest real


      INTEGER INTOUT, EXP, PREC, CUREXP, TEMP
      DOUBLE PRECISION REALIN, MINREL
      LOGICAL NEGNUM,EXPTRU

*---------------------------------------------------------------

      EXPTRU = .TRUE.
      EXP=0
      REALIN=INREAL
      INTOUT = 0

*   Set precision according to whether point is VDC or not
      IF (TYPPNT.EQ.1) THEN
         PREC=KMXRPR(KWKIX)-1
         MINREL=(2.0**KMNRPR(KWKIX))
         CUREXP=KDEFEX(KWKIX)
      ELSE
         PREC=KVMXRP(KWKIX)-1
         MINREL=(2.0**KVMNRP(KWKIX))
         IF (TYPPNT.EQ.2) THEN
            CUREXP=KVDEFX(KWKIX)

*   If points list then set default exponent to last X or Y as approp
         ELSEIF (TYPPNT.EQ.3) THEN
            CUREXP=KXEXP(KWKIX)
         ELSEIF (TYPPNT.EQ.4) THEN
            CUREXP=KYEXP(KWKIX)
         ENDIF
      ENDIF

*        If input negative, then make it positive & set negative
*        input flag to true
      IF (REALIN.LT.0.0) THEN
         REALIN = -REALIN
         NEGNUM = .TRUE.
      ELSE
         NEGNUM = .FALSE.
      ENDIF

*        If input zero, then no point in doing lots of tests.
*        Also no point in exponent
      IF (REALIN.LT.MINREL) THEN
         EXP=CUREXP
         INTOUT = 0
         GOTO 200
      ENDIF

 100  CONTINUE

*        If explicit exponent allowed and present, then work out
*        mantissa & exponent
      IF((KEXPAL(KWKIX).EQ.0).AND.EXPTRU) THEN
         IF(REALIN.GE.(2.0**PREC)) THEN
            REALIN = REALIN / 2
            EXP = EXP + 1
            GOTO 100
         ELSEIF(REALIN.LT.(2.0**(PREC-1))) THEN
            REALIN = REALIN * 2
            EXP = EXP -1
            GOTO 100
         ENDIF
         INTOUT = REALIN

*   If last bits are zero, see if we can fit the default exponent
         IF(EXP.LT.CUREXP) THEN
            TEMP=MOD(INTOUT,(2**(CUREXP-EXP)))
            IF(TEMP.EQ.0)THEN
               INTOUT=INTOUT/(2**(CUREXP-EXP))
               EXP=CUREXP
            ENDIF
         ENDIF

*   Store the current X or Y exponent
         IF(TYPPNT.EQ.3)THEN
            KXEXP(KWKIX)=EXP
         ELSEIF(TYPPNT.EQ.4)THEN
            KYEXP(KWKIX)=EXP
         ENDIF
      ELSE
 150     CONTINUE

*   If default exponent is used, then multiply or divide the
*   mantissa to suit the exponent
         IF (EXP.LT.PREC+1) THEN
            REALIN = REALIN / 2
            EXP = EXP + 1
            GOTO 150
         ELSEIF (EXP.GT.PREC+1) THEN
            REALIN = REALIN * 2
            EXP = EXP - 1
            GOTO 150
         ENDIF
         INTOUT = REALIN
      ENDIF
 200  CONTINUE

*        Reassign the real mantissa to an integer; if flag for
*        a negative number is true, then make integer negative
      IF (NEGNUM) INTOUT = -INTOUT

*        Call routine to convert mantissa into hex, and if necessary
*        call routine to convert exponent into hex.

*  If exponent = default prec then set exponent follows flag to false
      IF(EXP.EQ.CUREXP) THEN
         EXPTRU=.FALSE.
         CHAEXP(1:1)=CHAR(0)
      ELSEIF (EXPTRU) THEN

*  Get exponent
         CALL GK2MIH(EXP,CHAEXP,0,.FALSE.)
      ENDIF

*  Get mantissa
      CALL GK2MIH(INTOUT,CHAMAN,KEXPAL(KWKIX),EXPTRU)

 350  CONTINUE
      RETURN
      END
