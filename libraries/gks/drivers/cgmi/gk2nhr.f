      SUBROUTINE GK2NHR(RELVAL,RELTYP)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation Driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine gets a real from the CGM.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*   OUT  RELVAL : Real value returned
*   INP  RELTYP : Contains flag showing which exponent to use
*                 (1=Default Real, 2=Normal VDC,
*                  3=X VDC, 4=Y VDC,
*                  5=X in Point List, 6=Y in Point List)
*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*  LOCALS
*  ------
*        INTVAL : Temporary variable
*        INPINT : Input integer to be converted (actual parameter)
*        RTEMP  : Temporary variable

*        NEGNUM : Flag showing whether a number is negative
*        EXPALL : Flag to show if Exponents are allowed
*        EXPFOL : Flag to show if Exponent follows

      INTEGER INTVAL, INPINT, RELTYP
      REAL RELVAL,RTEMP
      LOGICAL NEGNUM, EXPALL, EXPFOL

*---------------------------------------------------------------------

      IF( RELTYP.EQ.1 ) THEN
*  Read a real
         EXPALL = (KEXPAL(KWKIX).EQ.0)
      ELSE
*   Check if VDC Type is integer
         IF( KVDCTY(KWKIX).EQ.0 )THEN
            CALL GK2NHI(INTVAL,.TRUE.)
            RELVAL = REAL(INTVAL)
            GOTO 50
         ELSE
            EXPALL = (KVXALL(KWKIX).EQ.0)
         ENDIF
      ENDIF

      NEGNUM = .FALSE.

*   Get first byte of real
      CALL GK2NNC(INTVAL,.TRUE.)
      RTEMP=MOD(INTVAL,16)
      INPINT=INTVAL/8

*   If explicit exponent is allowed
      IF ( EXPALL ) THEN
         RTEMP=MOD(INTVAL,8)

*   Set EXPFOL accordingly
         IF (MOD(INPINT,2).EQ.1) THEN
            EXPFOL=.TRUE.
         ELSE
            EXPFOL=.FALSE.
         ENDIF
      ELSE
         EXPFOL=.FALSE.
      ENDIF
      INPINT=INPINT/2

*   If sign bit is set, then set NEGNUM to true
      IF(MOD(INPINT,2).EQ.1) THEN
         NEGNUM = .TRUE.
      ENDIF
      INPINT=INPINT/2

  10  CONTINUE

*   If extension flag is set, get next byte
      IF(INPINT.EQ.3) THEN
         RTEMP=RTEMP*32
         CALL GK2NNC(INPINT,.TRUE.)

*   Add the last 5 bits to the running total
         RTEMP=RTEMP+MOD(INPINT,32)
         INPINT=INPINT/32
         GOTO 10
      ELSE

*   When extension flag not set (ie last byte), make number
*   positive or negative, according to the sign bit
         IF(NEGNUM) THEN
            RELVAL=-RTEMP
         ELSE
            RELVAL=RTEMP
         ENDIF
      ENDIF

*   Get exponent if present
      IF ( EXPFOL ) THEN
         CALL GK2NHI(INTVAL,.TRUE.)
         RTEMP=(2.0**INTVAL)
         RELVAL=RELVAL*RTEMP

*   Store X or Y exponent if necessary
         IF(RELTYP.EQ.5)THEN
            KXEXP(KWKIX)=INTVAL
         ELSEIF(RELTYP.EQ.6)THEN
            KYEXP(KWKIX)=INTVAL
         ENDIF
      ELSE
*  else If exponent is not present
*  Calculate value using appropriate default exponent
         IF(RELTYP.EQ.1) THEN
*  Real Value
            RELVAL=RELVAL*(2.0**KDEFEX(KWKIX))
         ELSEIF(RELTYP.GE.2.AND.RELTYP.LE.4) THEN
*  Normal VDC
            RELVAL=RELVAL*(2.0**KVDEFX(KWKIX))
         ELSEIF(RELTYP.EQ.5) THEN
*  VDC in Points list
            RELVAL=RELVAL*(2.0**KXEXP(KWKIX))
         ELSEIF(RELTYP.EQ.6) THEN
            RELVAL=RELVAL*(2.0**KYEXP(KWKIX))
         ENDIF
      ENDIF

  50  CONTINUE
      IF ( RELTYP.GT.2 ) THEN
* Normalise VDC if necessary
         IF( RELTYP.EQ.3 ) THEN
            RELVAL = (RELVAL-QVDCOX(KWKIX))*QVDCSX(KWKIX)
         ELSE IF( RELTYP.EQ.4 ) THEN
            RELVAL = (RELVAL-QVDCOY(KWKIX))*QVDCSY(KWKIX)
         ELSE IF( RELTYP.EQ.5 ) THEN
*   Points list VDC (difference) scale only
            RELVAL = RELVAL*QVDCSX(KWKIX)
         ELSE IF( RELTYP.EQ.6 ) THEN
            RELVAL = RELVAL*QVDCSY(KWKIX)
         ENDIF
      ENDIF

      RETURN
      END
