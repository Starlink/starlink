      SUBROUTINE IDENT(IDLIST,ARR,N, STATUS )

*+
*   IDENT Finds which of the Bits in the Byte Array
*   IDLIST is set.
*
*   Gets
*   ----
*      IDLIST - BYTE Array
*      ARR    - Integer Array Holding the N Bit Numbers
*
*   Returns
*   -------
*      N      - The Number of Bits Set in IDLIST

*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

      LOGICAL SET
      BYTE IDLIST(*)
      INTEGER ARR(*)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 0
      DO I=1,8*IDBYTE
         ARR(I)=0
      ENDDO
      DO 100 NBYTE=1,IDBYTE
         IF (IDLIST(NBYTE).EQ.0) GOTO 100
         DO 110 K=1,8
*
*   Ignore First One & Last 2 Bits
*
            IF ((NBYTE.EQ.IDBYTE.AND.K.GT.3).OR.(NBYTE.EQ.1.AND.K.EQ.1)
     :         .OR.(NBYTE.EQ.1.AND.K.EQ.8)) GOTO 110
            IF (SET(IDLIST(NBYTE),K)) THEN
               N = N+1
               ARR(N) = (NBYTE-1)*8 + K
            ENDIF
110      CONTINUE
100   CONTINUE
      END
