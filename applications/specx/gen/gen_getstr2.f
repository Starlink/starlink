*     31 July 2000 (ajc):
*        Re-write illegal concatenation
*        Unused GEN_IENDCH
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_GETSTR2 (LV, PROMPT, SDEF,
     &                              FDEF, STRING, JDEF)

      IMPLICIT  NONE

C   Routine to put a prompt message to the terminal and receive a
C   single string value returned in STRING. JDEF as for GETCH
C   SDEF is the default value for STRING, FDEF is a format qualifier
C   e.g. A3, describing how the default is to be presented by the
C   prompt. A null string for FDEF will suppress the presentation
C   of the default value.

C   As for GEN_GETSTR but has extra option of LEVEL in call

*     Formal parameters

      INTEGER*4 LV           ! Level at which to fetch next "item"
      CHARACTER PROMPT*(*)
      CHARACTER FDEF*(*)
      CHARACTER STRING*(*)
      CHARACTER SDEF*(*)
      INTEGER*4 JDEF

*     Local variables

      INTEGER*4 I
      INTEGER*4 ILS, ISS
      INTEGER*4 ILP, IDS
      INTEGER*4 ILDEF
      INTEGER*4 ILSTR
      INTEGER*4 LCOPY
      CHARACTER DEFSTR*80
      CHARACTER PSTRING*512
      CHARACTER STR*256

*     Functions

      INTEGER*4 GEN_ILEN

      GEN_GETSTR2 = .TRUE.

      JDEF  = 0
      ILDEF = GEN_ILEN (SDEF)
      ILSTR = LEN      (STRING)
      LCOPY = MIN      (ILDEF, ILSTR)

*     Set up default string. Note that if default string is not
*     properly initialized it will be set to zero bytes (null) rather
*     than blanks --- correct this at this stage.

      STRING = SDEF(:LCOPY)//' '
      DO I = 1, ILDEF
        IF (STRING(I:I) .EQ. CHAR(0)) STRING(I:I) = ' '
      END DO

      IF (GEN_ILEN (FDEF).EQ.0)   THEN
        CALL GEN_INPUT  (LV, PROMPT, STR, ILS, JDEF)
      ELSE
        STR = '('//FDEF(:GEN_ILEN(FDEF))//')'
        WRITE (DEFSTR, STR) SDEF(:GEN_ILEN(SDEF))
        IDS = GEN_ILEN (DEFSTR)
        ILP = GEN_ILEN (PROMPT)
        PSTRING = PROMPT(:ILP)
        PSTRING(ILP+1:) = ' ['
        PSTRING(ILP+3:) = DEFSTR(:IDS)
        PSTRING(ILP+IDS+4:) = '] '
        CALL GEN_INPUT (LV, PSTRING,
     &                  STR, ILS, JDEF)
      END IF

      IF (JDEF.NE.0)   RETURN

C  Strip off leading blanks

      ISS = 1
      DO WHILE (STR(ISS:ISS).EQ.' ')
        ISS = ISS + 1
      END DO

      ILS = GEN_ILEN (STR)

C  Terminate with blanks

      IF (ILS .GE. ISS) THEN
        STRING = STR (ISS:ILS)//' '
      ELSE
        STRING = ' '
      END IF

      RETURN
      END
