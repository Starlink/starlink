      SUBROUTINE GRGENV(NAME, VALUE, L)
*+
*     - - - - - - - -
*       G R G E N V     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Get value of PGPLOT environment parameter or PATH
*
*   Given
*      NAME     c   The name of the parameter to evaluate.
*
*   Returned
*      VALUE    c   receives the value of the parameter, truncated or extended
*                   with blanks as necessary. If the parameter is undefined,
*                   a blank string is returned.
*      L        i   receives the number of characters in VALUE, excluding
*                   trailing blanks. If the parameter is undefined, zero is
*                   returned.
*
*    In Unix, environment parameters are environment variables; e.g. parameter
*    ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
*    recursive and is case-sensitive.
*
*    The strings DEV is considered a special case and a default
*    value is supplied if no translation exists.

*    [For historical compatibility, if name PGPLOT_XX is not found, this
*    routine will also look for PLT$XX.]
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      CHARACTER*(*) NAME, VALUE
      INTEGER L

      INCLUDE 'SAE_PAR'

      INTEGER I, LIN, IER, CHR_LEN
      CHARACTER*32 TEST

*  Mark error stack
      CALL ERR_MARK


*  Construct environment variable name
      IF (NAME.NE.'PATH') THEN
         TEST = 'PGPLOT_'//NAME
      ELSE
         TEST = NAME
      ENDIF
      LIN = CHR_LEN(TEST)

*  Translate name
      IER = SAI__OK
      CALL PSX_GETENV(TEST(:LIN),VALUE,IER)

*  If it fails, try old form
      IF (IER.NE.SAI__OK) THEN
          CALL ERR_ANNUL(IER)
          IF (NAME.EQ.'PATH') THEN
              TEST = 'PLT$'//NAME
              LIN = CHR_LEN(TEST)
              CALL PSX_GETENV(TEST(:LIN),VALUE,IER)
          ENDIF
      END IF

      IF (IER.NE.SAI__OK .OR. VALUE.EQ.' ') THEN

*      Failure
          IF (IER.NE.SAI__OK) CALL ERR_ANNUL(IER)
          IF (NAME.EQ.'DEV') THEN
             VALUE = 'xwindows'
             L = 8
          ELSE
             L = 0
             VALUE = ' '
          END IF
      ELSE

*      Success - trim trailing blanks
          DO 10 I=LEN(VALUE),1,-1
              L = I
              IF (VALUE(I:I).NE.' ') GOTO 20
   10     CONTINUE

*      String was all blanks
          L = 0
   20     CONTINUE
      END IF

*  Release error stack
      CALL ERR_RLSE
      END

