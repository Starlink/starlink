C# IL>=a, OL>=0
*---------------------------------------------------------------
      SUBROUTINE GKEDST(IESIZE,IEDAT,IBUFSZ,IDPOS,IDSIZE,IDAT)
*---------------------------------------------------------------
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Edit an input string, interpreting imbedded <BACKSPACE>s and
*     removing control characters
*
*  MAINTENANCE LOG
*  ---------------
*     28/03/85  RSK  Original version
*     31/10/85  RSK  Fix VME compilation bug for IDAT declaration, ie
*                     use IBUFSZ not IDSIZE to dimension IDAT
*     12/03/86  RSK  Re-code removal of leading spaces.
*
*  ARGUMENTS
*  ---------
*     INP     IESIZE Size of Edit string
*     INP     IEDAT  INTEGER array of Edit string chars
*                     (hold edit chars + real chars)
*     INP     IBUFSZ Maximium Output string size
*     INP     IDPOS  Position in Output string from where to start editing
*     INP/OUT IDSIZE Size of Output string
*     INP/OUT IDAT   INTEGER array of Output string chars after editing

      INTEGER IESIZE,IBUFSZ,IDPOS,IDSIZE
      INTEGER IEDAT(IESIZE),IDAT(IBUFSZ)
*
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ICHARN       Loop variable
*     IBSPAC       ASCII BackSpace character ADE
*     ISPACE       ASCII Space character ADE
*     IDEL         ASCII Del character ADE
*
      INTEGER      ICHARN
      INTEGER      IBSPAC,ISPACE,IDEL
      PARAMETER   (IBSPAC=8,ISPACE=32,IDEL=127)
*
*  ALGORITHM
*  ---------
*
* --------------------------------------------------------------


*   Edit the input string interpreting any BackSpaces input
      DO 860, ICHARN = 1, IESIZE
*   Have we got a BACKSPACE?
        IF (IEDAT(ICHARN) .EQ. IBSPAC)
     :  THEN
*   Yes. Decrement IDAT index IDPOS if not at start already
          IF (IDPOS .GT. 1)
     :    THEN
            IDPOS = IDPOS - 1
          ENDIF
*   Have we got a control character ( eg DEL, etc)
        ELSE
          IF ((IEDAT(ICHARN) .LT. ISPACE)   .OR.
     :        (IEDAT(ICHARN) .GE. IDEL))
     :    THEN
*   Yes. So ignore it
            GOTO 860
          ELSE
*   We've should now have a real character to store the character away.
            IDAT(IDPOS) = IEDAT(ICHARN)
*   Check we won't exceed the buffer length for this String device
            IF ((IDPOS  .LT. IBUFSZ) .AND.
     :          (ICHARN .LT. IESIZE))
     :      THEN
*   Increment IDAT index IDPOS
              IDPOS = IDPOS + 1
            ELSE
*   Buffer for this STRING device filled up, so leave the rest
              GOTO 880
            ENDIF
          ENDIF
        ENDIF
 860  CONTINUE
 880  CONTINUE
*   Remove leading spaces
      IDSIZE = 0
      DO 890, ICHARN = 1, IDPOS
        IF (IDAT(ICHARN) .NE. ISPACE)
     :  THEN
          IF (ICHARN .EQ. 1)
     :    THEN
*   No leading spaces so return whole edited string
            IDSIZE       = IDPOS
            GOTO 895
          ELSE
*   Reached Non-Space characters so copy them to start of string
            IDSIZE       = IDSIZE + 1
            IDAT(IDSIZE) = IDAT(ICHARN)
          ENDIF
        ENDIF
 890  CONTINUE
 895  CONTINUE

      RETURN
      END
