      SUBROUTINE GRUSER(STRING, L)
*+
*     - - - - - - - -
*       G R U S E R     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Get user name
*
*   Returned
*      STRING   c   receives user name, truncated or extended with
*                   blanks as necessary.
*      L        i   receives the number of characters in STRING, excluding
*                   trailing blanks.
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER L

      INCLUDE 'SAE_PAR'


      INTEGER I, IER

      STRING = ' '

*   Get username
      IER = SAI__OK
      CALL PSX_CUSERID(STRING,IER)
      IF (IER.NE.SAI__OK .OR. STRING.EQ.' ') THEN

*      Failed
          L = 0
          STRING = ' '
      ELSE

*      Success - remove trailing blanks
          DO 10 I=LEN(STRING),1,-1
              L = I
              IF (STRING(I:I).NE.' ') GOTO 20
   10     CONTINUE

*      Username was all blanks
          L = 0
   20     CONTINUE
      END IF
      END
