
      SUBROUTINE snx_CHSET (I)

*+
*
*  - - - - - -
*   C H S E T
*  - - - - - -
*
*  Select character set
*
*  Given:
*     I      i      character set: 1='duplex', 2='complex'
*
*  This routine sets the variable MODE in the NCAR labelled
*  COMMON block /PUSER/.
*
*  P T Wallace   Starlink   April 1986
*
*+

      IMPLICIT NONE

      INTEGER I

*  NCAR labelled COMMON block
      INTEGER MODE
      COMMON /PUSER/ MODE


      MODE=I

      END
