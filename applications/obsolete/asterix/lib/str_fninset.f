*+  STR_FNINSET - Find index of first character in STR not in a set SET
      INTEGER FUNCTION STR_FNINSET( STR, SET )
*
*    Description :
*
*
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     26 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*(*)             STR			! String of interest
      CHARACTER*(*)             SET			! The set of characters
*
*    Function declarations :
*
      INTEGER                   CHR_SIZE
*
*    Local variables :
*
      INTEGER			LEN			! Declared length of STR
      INTEGER			IC			! Cursor over STR
      INTEGER                   POS			! Function return value
*-

*    Initialise return value
      POS = 0

*    Get declared length of string
      LEN = CHR_SIZE( STR )

*    Loop until end of string found or character in the set is found
      IC = 1
      DO WHILE ( (IC.LE.LEN) .AND. (POS.EQ.0) )
        IF ( INDEX(SET,STR(IC:IC)) .EQ. 0 ) POS = IC
        IC = IC + 1
      END DO

*    Set function return value
      STR_FNINSET = POS

      END
