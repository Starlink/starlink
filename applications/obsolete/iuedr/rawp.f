      SUBROUTINE RAWP( NSAMP, NLINE, RAW, PHOT )

*+
*
*   Name:
*      SUBROUTINE RAWP
*
*   Description:
*      The RAW image (stored as BYTE) is copied into PHOT form (SHORT).
*
*   History:
*      Jack Giddings     01-AUG-82     AT4 version
*      Paul Rees         03-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NSAMP                    ! number of samples
      INTEGER NLINE                    ! number of lines

      BYTE RAW(NSAMP, NLINE)           ! RAW BYTE array

*   Export:
      INTEGER*2 PHOT(NSAMP, NLINE)     ! photometric image array

*   Local variables:
      INTEGER IL                       ! line index
      INTEGER IS                       ! sample index

      DO IL = 1, NLINE
         DO IS = 1, NSAMP
            IF ( RAW(IS, IL) .LT. 0) THEN
               PHOT(IS, IL) = RAW(IS, IL) + 256
            ELSE
               PHOT(IS, IL) = RAW(IS, IL)
            END IF
         END DO
      END DO
      END
