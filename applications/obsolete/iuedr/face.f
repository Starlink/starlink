      SUBROUTINE FACE( NAXIS1, NAXIS2, RDATA, QUAL, STATUS )
*+
*   Name:
*      SUBROUTINE FACE
*
*   Description :
*      Define Rotation Details from Face plate specification.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          03-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The image subset is bounded by a circle and a rectangle which
*      overlaps it. The minimum subset is taken.
*      The Data Limits (DLIM) are determined at the same time.
*      The face plate specification is taken from CMFACE and the Rotation
*      details are stored in CMROTS.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Constants:
      REAL*8 RADIAN     ! radians per degree
      PARAMETER ( RADIAN = 0.01745329d0 )
*      PARAMETER ( RADIAN = 0.017453292520d0)

*   Import:
      INTEGER NAXIS1    ! size of axis 1 (sample)
      INTEGER NAXIS2    ! size of axis 2 (line)

      INTEGER*2 RDATA(NAXIS1, NAXIS2) ! image

      BYTE QUAL(NAXIS1, NAXIS2)       ! quality

*   Export:
      INTEGER STATUS    ! status return

*   External references:
      INTEGER DQ_AND    ! data quality AND

*   Global variables:
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'

*   Local variables:
      REAL*8 DSDL
      REAL*8 DSDR

      INTEGER IDL
      INTEGER IDS
      INTEGER IL        ! loop index
      INTEGER IL1
      INTEGER IL2
      INTEGER IS        ! loop index
      INTEGER IQ
      INTEGER ISCF
      INTEGER ISCL
      INTEGER ISF
      INTEGER IS1
      INTEGER IS2
      INTEGER NR
      INTEGER SMN       ! local Smin
      INTEGER SMX       ! local Smax

*   Check validity
      IF ( NOFACE ) THEN
         CALL ERROUT( 'Error: no face-plate\\', STATUS )
         RETURN

      ELSE IF ( NOROT ) THEN
         CALL ERROUT( 'Error: no rotation\\', STATUS )
         RETURN
      END IF

*   Restrict L-limits for face-plate circle
      LMIN = MAX(1, CENTRE(2) - RADIUS)
      LMAX = MIN(NL, CENTRE(2) + RADIUS)

      IF ( LMIN .GT. LMAX ) THEN
         CALL ERROUT( 'Error: image subset empty\\', STATUS )
         RETURN
      END IF

*   Define a pair of parallel lines which subset the image
      DSDL = TAN(ANGLE * RADIAN)
      DSDR = ABS(1.0 / COS(ANGLE * RADIAN))
      ISCF = CENTRE(1) + NINT(REAL(RLIM(1)) * DSDR)
      ISCL = CENTRE(1) + NINT(REAL(RLIM(2)) * DSDR)
      NR = ISCL - ISCF + 1

      ISF = ISCF - NINT(REAL(CENTRE(2)) * DSDL)

      DO IL = LMIN, LMAX

*      Parallel band subset
         SMN = ISF - 1 + NINT(REAL(IL - 1) * DSDL)
         SMX = SMN + NR - 1
         SMN = MAX(SMN, 1)
         SMX = MIN(SMX, NS)

*      Circle subset
         IF ( SMN .LE. SMX ) THEN
            IDL = ABS(IL - CENTRE(2))

            IF ( IDL .GE. RADIUS ) THEN
               SMN = NL + 1
               SMX = 0

            ELSE
               IDS = SQRT(REAL(RADIUS * RADIUS) - REAL(IDL * IDL))
               SMN = MAX(SMN, CENTRE(1) - IDS)
               SMX = MIN(SMX, CENTRE(1) + IDS)
            END IF
         END IF

*      Blank data subset
         IF ( SMN .LE. SMX ) THEN

*         Blank data at start
            IS1 = SMN

            DO IS = IS1, SMX
               IF ( RDATA(IS, IL) .NE. DBLANK ) GO TO 40
               IQ = QUAL(IS,IL)
               IF ( DQ_AND(IQ, 1) .EQ. 0 ) GO TO 40
               SMN = IS + 1
            END DO
 40         CONTINUE

*         Blank data at end of record
            IF ( SMN .LE. SMX ) THEN
               IS2 = SMX

               DO IS = IS2, SMN, -1
                  IF ( RDATA(IS, IL) .NE. DBLANK) GO TO 100
                  IQ = QUAL(IS, IL)
                  IF ( DQ_AND(IQ, 1) .EQ. 0 ) GO TO 100

                  SMX = IS - 1
               END DO

 100           CONTINUE
            END IF
         END IF

*      Save results
         SMIN(IL) = SMN
         SMAX(IL) = SMX
      END DO

*   Update Lmin to exclude empty lines
      IL1 = LMIN

      DO IL = IL1, LMAX
         IF ( SMIN(IL) .LE. SMAX(IL) ) GO TO 400
         LMIN = IL + 1
      END DO
 400  CONTINUE

*   Update Lmax to exclude empty lines
      IF ( LMIN .LE. LMAX ) THEN
         IL2 = LMAX
         DO IL = IL2, LMIN, -1
            IF ( SMIN(IL) .LE. SMAX(IL) ) GO TO 500
            LMAX = IL - 1
         END DO
 500     CONTINUE

      ELSE
         CALL ERROUT( 'Error: image subset is empty\\', STATUS )
         RETURN
      END IF

*   Check that there are some records
      IF ( LMIN .GT. LMAX ) THEN
         CALL ERROUT( 'Error: image subset is empty\\', STATUS )
         RETURN
      END IF

*   Data limits (and blank out remainder of image)
      DO IL = 1, LMIN - 1
         DO IS = 1, NAXIS1
            RDATA(IS, IL) = DBLANK
         END DO
      END DO

      DLIM(1) = +32769
      DLIM(2) = -32769

      DO IL = LMIN, LMAX
         DO IS = 1, SMIN(IL) - 1
            RDATA(IS, IL) = DBLANK
         END DO
         DO IS = SMIN(IL), SMAX(IL)
            IF ( RDATA(IS, IL) .NE. DBLANK ) THEN
               IF ( RDATA(IS, IL) .GT. DLIM(2) )
     :            DLIM(2) = RDATA(IS, IL)
               IF ( RDATA(IS, IL) .LT. DLIM(1) )
     :            DLIM(1) = RDATA(IS, IL)
            END IF
         END DO
         DO IS = SMAX(IL) + 1, NAXIS1
            RDATA(IS, IL) = DBLANK
         END DO
      END DO

      DO IL = LMAX + 1, NAXIS2
         DO IS = 1, NAXIS1
            RDATA(IS, IL) = DBLANK
         END DO
      END DO

      END
