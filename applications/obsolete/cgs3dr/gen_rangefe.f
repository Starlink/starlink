C+
      SUBROUTINE GEN_RANGEFE(ARRAY, QDATA, QUAL, FLAGS, FBAD,
     :                       IST, IEN, VMAX, VMIN)
C
C     G E N _ R A N G E F E
C
C     Finds the maximum and minimum values in a real array, ignoring
C     data values which are either flagged as bad by a quality array
C     or which contain a specified "magic" bad value.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) QDATA     (Byte) Associated quality array,, checked if FLAGS is
C                   False. If magic value checking is used, the calling
C                   routine may simply specify a dummy argument for QDATA,
C                   as long as the code in this routine never accesses
C                   QDATA when FLAGS is True.
C     (>) QUAL      (Logical) True if quality is to be used.
C     (>) FLAGS     (Logical) True if magic values are used. Note that
C                   EITHER quality OR magic values may be checked, but not
C                   both
C     (>) FBAD      (Real) Magic value.
C     (>) IST       (Integer) The first element of ARRAY to be
C                   examined.
C     (>) IEN       (Integer) The last element of ARRAY to be
C                   examined.
C     (<) VMAX      (Real) The maximum value of those examined.
C     (<) VMIN      (Real) The minimum value of those examined.
C
C                                      KS / CIT  2nd Jan 1983
C
C     Modified :
C
C     6th Sept 1989. JFL / ROE. Quality added, based on routines
C                               supplied by JM.
C     1st Nov 1989.  SMB / ROE. QDATA_OK parameter used.
C     20th Nov 1989. SMB / ROE. Modified to be much similar to JM's
C                               routines (which follow the example
C                               in the FIGARO programmers' guide).
C                               DO WHILE used to eliminate GOTO
C                               statements.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL QUAL, FLAGS
      REAL FBAD
      INTEGER IST,IEN
      REAL ARRAY(IEN),VMIN,VMAX
      BYTE QDATA(IEN)
C
C     Local constants
C
      INTEGER GOOD
      PARAMETER ( GOOD = 0 )
C
C     Local variables
C
      INTEGER I, II
C
C     The code executed depends upon the bad pixel mechanism being used.
C     If QUAL is true, then a quality array is being used.
C     Otherwise, if FLAGS is True, then a magic value is being used.
C     If both QUAL and FLAGS are False, then no bad pixel mechanism
C     is being used. (Note that both QUAL and FALSE should not be True).
C
      IF ( QUAL ) THEN
C
C        The quality array is significant
C        Locate the first good element in the specified section of the array.
C
         II = IST
         DO WHILE ( ( II .LE. IEN ) .AND.
     :              ( QDATA(II) .NE. GOOD ) )

            II = II + 1
         END DO

         IF ( II .GT. IEN ) THEN
C
C           The whole of the specified region is bad. Return arbitrary values.
C
            VMIN = 0.0
            VMAX = VMIN
         ELSE
C
C           A good value has been found.
C           Initialise VMIN and VMAX to the first good value in the array.
C
            VMAX = ARRAY(II)
            VMIN = VMAX
C
C           Search the remaining good elements in the array, and update
C           VMIN and VMAX accordingly
C
            DO I = II+1, IEN

               IF ( QDATA(I) .EQ. GOOD ) THEN

                  VMIN=MIN(VMIN,ARRAY(I))
                  VMAX=MAX(VMAX,ARRAY(I))
               ENDIF
            END DO
         END IF

      ELSE IF (FLAGS) THEN
C
C        The quality array is not significant and a magic value is being used.
C        Locate the first good element in the specified section of the array.
C
         II = IST
         DO WHILE ( ( II .LE. IEN ) .AND.
     :              ( ARRAY(II) .EQ. FBAD ) )

            II = II + 1
         END DO

         IF ( II .GT. IEN ) THEN
C
C           The whole of the specified region is bad. Return the magic value.
C
            VMIN = FBAD
            VMAX = FBAD
         ELSE
C
C           A good value has been found.
C           Initialise VMIN and VMAX to the first good value in the array.
C
            VMAX = ARRAY(II)
            VMIN = VMAX
C
C           Search the remaining good elements in the array, and update
C           VMIN and VMAX accordingly
C
            DO I = II+1, IEN

               IF ( ARRAY(I) .NE. FBAD ) THEN

                  VMIN=MIN(VMIN,ARRAY(I))
                  VMAX=MAX(VMAX,ARRAY(I))
               ENDIF
            END DO
         END IF

      ELSE
C
C        Neither a quality array nor bad values are being used.
C        Determine the maximum and minimum values of all the elements.
C
         VMIN=ARRAY(IST)
         VMAX=VMIN
         IF (IST.LT.IEN) THEN
            DO I=IST+1,IEN
               VMIN=MIN(VMIN,ARRAY(I))
               VMAX=MAX(VMAX,ARRAY(I))
            END DO
         END IF

      ENDIF

      END
