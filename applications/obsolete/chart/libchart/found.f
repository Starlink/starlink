      LOGICAL FUNCTION FOUND(IDLIST)
*+
*   Function Returns Value 'TRUE' if
*   1. All the Bits Listed in 'YESCAT' are Set
*   2. All the Bits Listed in 'NOCAT' are not Set
*   Where the Bit Numbers are Referring to the IDBYTE Length
*   Array of LOGICAL*1 Variables
*
*   Gets
*   ----
*      IDLIST - The LOGICAL*1 Array of IDBYTES Holding the
*               Catalogue Indicators
*
*   Returns
*   -------
*      IDLIST - Set to TRUE or FALSE (see above)
*-
      INCLUDE 'MAIN'
      LOGICAL SET
      LOGICAL*1 IDLIST(IDBYTE)

      FOUND = .TRUE.
      IF (YESCAT(1).EQ.0) GOTO 105
*
*   Check that All Bits Specified in YESCAT are all set
*
      DO K=1,50
         IF (YESCAT(K).EQ.0) GOTO 105
         N=YESCAT(K)
         NBYTE = (N+7)/8
         FOUND = SET(IDLIST(NBYTE),N-((NBYTE-1)*8)).AND.FOUND
      ENDDO
*
*   Check that None of the Bits Specified in NOCAT are set
*
105   CONTINUE
      IF (NOCAT(1).EQ.0) GOTO 205
      DO K=1,50
         IF (NOCAT(K).EQ.0) GOTO 205
         N=NOCAT(K)
         NBYTE = (N+7)/8
         FOUND = (.NOT.SET(IDLIST(NBYTE),N-((NBYTE-1)*8))).AND.FOUND
      ENDDO
205   CONTINUE
      END
