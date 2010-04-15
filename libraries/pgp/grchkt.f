      INTEGER FUNCTION GRCHKT()
*++
*  GRCHKT -- determine whether the process has a terminal attached
*
* Returns:
*  GRCHKT (integer): a TRUE value (odd) if there is a terminal device,
*       a FALSE value otherwise.
*--
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      CHARACTER*40 NAME
      INTEGER STATUS, LNAME

*  Defer error reporting
      CALL ERR_MARK

      STATUS = SAI__OK
      CALL GNS_GTN( NAME, LNAME, STATUS)
      IF (LNAME.GT.0) THEN
          GRCHKT = 1
      ELSE
          GRCHKT = 4828 ! "input device is not a terminal"
      END IF

*  Clear errors and re-enable reporting
      CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE
      END
