      SUBROUTINE GK2NBU (ICODE)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             CIA
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*  This routine gets the next ASCII character from
*  the file and converts it into an integer code.
*  It also checks whether character substitution has
*  taken place & if so, it returns the replaced
*  character & screens out any ignored characters.
*
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*     out   ICODE : Integer value of a hex character
*
      INTEGER ICODE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index
*     Modify /GKYWKD/    Derive workstation I/O channel i.d.
*     Modify /GKZIO/     CMBUFF
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'

      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkerr.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'
*
*  LOCALS
*  ------
*
*        I      : Loop variable
*        ESCAPE : Set .TRUE. if substitution character found

      INTEGER I
      LOGICAL ESCAPE
*
*  Parameters
*
*        X40, X60, XFF, X7E :  Hexadecimal constants

      INTEGER X40, X60, XFF, X7E
      PARAMETER (X40 = 64, X60 = 96 ,XFF = 127, X7E = 126)
*
*---------------------------------------------------------------------

      ESCAPE=.FALSE.
 5    CONTINUE

*   Get code from metafile defaults replacement if necessary
      IF (DOREP(KWKIX)) THEN
         IF (KDFPTR(KWKIX).GT.KLENRC) THEN
            KDFPTR(KWKIX)=1
            KDFREC(KWKIX)=KDFREC(KWKIX)+1
         ENDIF
         ICODE=ICHAR(CMFDEF(KDFREC(KWKIX))
     +      (KDFPTR(KWKIX):KDFPTR(KWKIX)))
         KDFPTR(KWKIX)=KDFPTR(KWKIX)+1
      ELSE

*   Otherwise read from the metafile as required
         IF (KCHANI(KWKIX).GT.KLENRC) THEN
            CALL GKIOFI(KIORB,KLENRC,I,CMBUFF(KWKIX))
            IF ( KERROR .NE. 0 ) GOTO 99
            KCHANI(KWKIX)=1
         ENDIF
         ICODE=ICHAR(CMBUFF(KWKIX)(KCHANI(KWKIX):KCHANI(KWKIX)))
         KCHANI(KWKIX)=KCHANI(KWKIX)+1

*   See whether this character is to be ignored
         DO 100 I=1,KNSUBS(KWKIX)
            IF ((KSUBAR(I,KWKIX).GE.X40).AND.
     +         (ICODE.EQ.KSUBAR(I,KWKIX)-X40)) THEN
*              KCDPTR(KWKIX)=KCDPTR(KWKIX)+1
               GOTO 5
            ELSEIF ( KSUBAR(I,KWKIX).EQ.(X40-1) .AND.
     +          ICODE.EQ.XFF ) THEN
*              KCDPTR(KWKIX)=KCDPTR(KWKIX)+1
               GOTO 5
            ENDIF
  100    CONTINUE

*   Set escape flag or ignore escape character where relevant
         IF (ICODE.EQ.X7E)THEN
            IF((BEGMET(KWKIX)).OR.(KNSUBS(KWKIX).GT.0))THEN
               ESCAPE=.TRUE.
               GOTO 5
            ENDIF
         ENDIF

*   Find the character that has been escaped
         IF(ESCAPE)THEN
            DO 200 I=1,KNSUBS(KWKIX)
               IF(ICODE.EQ.KSUBAR(I,KWKIX))THEN
                  IF ( ICODE.GE.X40 .AND. ICODE.LE.X60 ) THEN
                     ICODE=ICODE-X40
                  ELSEIF (ICODE.LT.X40) THEN
                     ICODE=ICODE+X40
                  ENDIF
                  GOTO 300
               ENDIF
 200        CONTINUE

*   If character isnt in the list & its the beginning of the metafile,
*   then add it to the list.
            IF(BEGMET(KWKIX))THEN

               KNSUBS(KWKIX)=KNSUBS(KWKIX)+1
               KSUBAR(KNSUBS(KWKIX),KWKIX)=ICODE
               ESCAPE=.FALSE.
               GOTO 5
            ENDIF
         ENDIF
      ENDIF

 300  CONTINUE

*   Put code into metafile defaults replacement buffer if necessary
      IF(METDEF(KWKIX))THEN
         IF(KDFPTR(KWKIX).GT.KLENRC)THEN
            KDFPTR(KWKIX)=1
            KDFREC(KWKIX)=KDFREC(KWKIX)+1
         ENDIF
         CMFDEF(KDFREC(KWKIX))(KDFPTR(KWKIX):KDFPTR(KWKIX))=
     +      CHAR(ICODE)
         KDFPTR(KWKIX)=KDFPTR(KWKIX)+1
      ENDIF


*   If end of output data record, start next one
      IF (KCDPTR(KWKIX).GT.KLENRC) THEN
         KCDPTR(KWKIX)=1
         KCDREC(KWKIX)=KCDREC(KWKIX)+1
      ENDIF


*   Put the code into the buffer (which is read by GK2NNC)
      CMFDR(KCDREC(KWKIX))(KCDPTR(KWKIX):KCDPTR(KWKIX))=CHAR(ICODE)
      KCDPTR(KWKIX)=KCDPTR(KWKIX)+1

   99 CONTINUE
      RETURN
      END
