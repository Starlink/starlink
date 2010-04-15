

      SUBROUTINE ELF1_RAND(TYPE,SEED,VALUE,STATUS)

*+
*  Name:
*     ELF1_RAND

*  Purpose:
*     Provide random numbers in the range 0-1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_RAND(TYPE,SEED,VALUE,STATUS)

*  Description:
*      Crude and simple random number generator based
*      upon a NIST routine supplied by Malcolm Currie.

*  Arguments:
*     TYPE = INTEGER (Given)
*        Seed or request?
*     SEED = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-Oct-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER TYPE                    ! Seed or request?
      INTEGER SEED                    ! Seed value.

*  Arguments Returned:
      REAL VALUE                      ! RAndom number

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION R
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION TWO28

      DATA FACTOR /41475557.0D0/, TWO28 /268435456.0D0/
      SAVE R
*.


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   First value only.
      IF(TYPE.EQ.0) R=DBLE(FLOAT(SEED))/TWO28

*   Evaluate.
      R=DMOD(R*FACTOR,1.0D0)
      VALUE=SNGL(R)

      END


      SUBROUTINE ELP1_RAND(TYPE,SEED,VALUE,STATUS)

*+
*  Name:
*     ELP1_RAND

*  Purpose:
*     Provide random numbers in the range 0-1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELP1_RAND(TYPE,SEED,VALUE,STATUS)

*  Description:
*      Crude and simple random number generator based
*      upon a NIST routine supplied by Malcolm Currie.

*  Arguments:
*     TYPE = INTEGER (Given)
*        Seed or request?
*     SEED = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-Oct-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER TYPE                    ! Seed or request?
      INTEGER SEED                    ! Seed value.

*  Arguments Returned:
      REAL VALUE                      ! RAndom number

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION R
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION TWO28

      DATA FACTOR /41475557.0D0/, TWO28 /268435456.0D0/
      SAVE R
*.



*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   First value only.
      IF(TYPE.EQ.0) R=DBLE(FLOAT(SEED))/TWO28

*   Evaluate.
      R=DMOD(R*FACTOR,1.0D0)
      VALUE=SNGL(R)

      END
