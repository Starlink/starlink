      SUBROUTINE GK2MBU(INCHAR)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine writes a character string into
*  the string buffer; when this is full, it writes
*  the buffer to the CGM file.

*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        INCHAR : Character to write to CGM
*
      CHARACTER INCHAR*81
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*
*  LOCALS
*  ------
*        I      : Loop counter
*        J      : Loop counter
*        RECLEN : Record Length

      INTEGER I, J, RECLEN
      PARAMETER (RECLEN=80)
*
*---------------------------------------------------------------------

      J=1
  10  DO 20 I = KCHANO(KWKIX), RECLEN

*   If not end of string then write character to buffer
         IF(ICHAR(INCHAR(J:J)).NE.0)THEN

*  Check for character substitution for '7E' after BEGMF
            IF ( INCHAR(J:J) .EQ. CHAR(126) .AND.
     :           KWKDAT(5,KWKIX) .GT. 0 ) THEN
               COUTCH(KWKIX)(I:I)=CHAR(126)
               INCHAR(J:J) = CHAR( 62 )
*  Check for character substitution for '1B' after BEGMF (not yet)
*           ELSE IF ( INCHAR(J:J) .EQ. CHAR(27) .AND.
*    :           KWKDAT(5,KWKIX) .GT. 0 ) THEN
*              COUTCH(KWKIX)(I:I)=CHAR(126)
*              INCHAR(J:J) = CHAR( 91 )
            ELSE
               COUTCH(KWKIX)(I:I)=INCHAR(J:J)
               J=J+1
            ENDIF
         ELSE
            GOTO 30
         ENDIF
  20  CONTINUE

*   If buffer full & still not end of string, then write buffer to
*   CGM, clear buffer & continue writing to buffer
      IF(ICHAR(INCHAR(J:J)).NE.0)THEN
*  Possible new routine to output Characters to file
*        CALL GKFOCO(KIOPB, COUTCH(KWKIX), ILEFT)
         WRITE(KWCID(KWKIX),1000) COUTCH(KWKIX)(1:RECLEN)

*   Reinitialise buffer
         DO 25 I=1,RECLEN
            COUTCH(KWKIX)(I:I)=CHAR(0)
  25     CONTINUE
         KCHANO(KWKIX)=1
         GOTO 10
      ENDIF
  30  CONTINUE

*   Set current character No in Common Block
      KCHANO(KWKIX) = I
1000  FORMAT(A)
      RETURN
      END
