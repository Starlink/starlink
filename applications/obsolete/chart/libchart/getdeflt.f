      SUBROUTINE GETDEFLT(PARAMS,NVALS,PAR,DEF,NPOS, STATUS )
*+
*   Subroutine GETDEFLT
*
*   It selects the default value for a selected parameter
*   from an array of all the parameter records.
*
*   Gets
*   ----
*      PARAMS		character array of parameter records
*      NVALS		maximum number of records found
*      PAR		the name of the required parameter
*
*   Returns
*   -------
*      DEF		the default value of the required
*			parameter
*      NPOS		the position of this parameter
*			in the list. (0 if not found)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   Written by K F Hartley at RGO on 23-12-82

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*(*) PARAMS(25),PAR,DEF

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      L=2+LEN(PAR)
      DEF=' '
      NPOS=0
      DO I=1,NVALS
         IF (PAR.EQ.PARAMS(I)(3:L)) THEN
            DEF=PARAMS(I)(21:70)
            NPOS=I
         END IF
      END DO
      END
