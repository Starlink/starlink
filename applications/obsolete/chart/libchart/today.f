      SUBROUTINE TODAY(DATE, STATUS )
*+
*   Subroutine TODAY will return,in date,the
*   current Epoch in Decimal form e.g. 1979.76543
*   Subroutine IIDATE gives today's Day Number in the 20th
*   century (i.e. 1st Jan. 1900 =1)
*   JDATE is todays Julian Date ( 1 Jan 1900 = J.D. 2415020)
*   Note : Fundamental Epoch 1900.0 = J.D. 2415020.313
*
*   Gets
*   ----
*      DATE  - Current Epoch
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*     15-MAR-1993 (AJJB):
*        Function TRULEN removed from the end if this file as it's in
*        it's own file, TRULEN.

*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL IIDATE(N, STATUS )
      TJDATE = FLOAT(N) + 2415019.0
      DATE = 1900.0 + (TJDATE-2415020.313)/365.2422
      END
