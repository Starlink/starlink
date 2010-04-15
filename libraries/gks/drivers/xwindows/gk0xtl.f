*---------------------------------------------------------------------


      SUBROUTINE GK0XTL( NAME, IVAL)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Finds the user preferred value for a parameter by translating a
*     logical name of the form GKS_<workstation type>_<name>
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP NAME   - parameter name
*     OUTP IVAL  - parameter value (zero if no valid translation)
*
      CHARACTER*(*) NAME
      INTEGER IVAL
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
*!#if VMS
*!      INCLUDE '($syssrvnam)'
*!      INCLUDE '($lnmdef)'
*!#endif
*
*  LOCALS
*  ------
*     LOGNAM   Logical name
*     ISTAT    System service status
*     EQUNAM   Equivalence name
*     LEQU     Length of equivalence name
*
      CHARACTER LOGNAM*29, EQUNAM*10
      INTEGER ISTAT, LEQU
*!#if VMS
*!      STRUCTURE /ITEMLIST/
*!         INTEGER*2 BUFLEN
*!         INTEGER*2 CODE
*!         INTEGER BUFADR
*!         INTEGER RETLEN
*!         INTEGER ZERO
*!      END STRUCTURE
*!      RECORD /ITEMLIST/ ITMLST
*!#endif
*
*  COMMENTS
*  --------
*     This routine assumes that the workstation type is a four digit
*     number
*
*---------------------------------------------------------------------
      WRITE (LOGNAM, '(A4,I4,A1,A)') 'GKS_', KWKTYP, '_', NAME

*!#if VMS
*!      ITMLST.CODE = LNM$_STRING
*!      CALL LIB$ANALYZE_SDESC( EQUNAM,  ITMLST.BUFLEN, ITMLST.BUFADR)
*!      ITMLST.RETLEN = %loc(LEQU)
*!      ITMLST.ZERO = 0
*!      EQUNAM = ' '
*!      ISTAT = SYS$TRNLNM( LNM$M_CASE_BLIND, 'LNM$FILE_DEV',
*!     1 LOGNAM, , ITMLST)
*!      IF (ISTAT .AND. (LEQU.GT.0)) THEN
*!#else
      CALL GETENV(LOGNAM, EQUNAM)

      IF (EQUNAM.NE.'          ') THEN
*!#endif
         READ(EQUNAM, '(BN,I10)', ERR=10) IVAL
      ELSE
         IVAL = 0
      END IF
      GO TO 20

   10 CONTINUE
      IVAL = 0

   20 CONTINUE
      END
