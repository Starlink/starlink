      SUBROUTINE MSC_MESUB(MAXL, LMIN, LMAX, SMIN, SMAX, LMN, LMX, SMN,
     :                     SMX)

*+
*
*   Name:
*      SUBROUTINE MSC_MESUB
*
*   Description:
*      The subset (LMN,LMX,SMN,SMX) is modified to exclude pixels
*      not in (LMIN,LMAX,SMIN,SMAX).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER MAXL           ! maximum number of image lines
      INTEGER LMIN           ! minimum image line contained
      INTEGER LMAX           ! maximum image line contained
      INTEGER SMIN(MAXL)     ! minimum image sample per line
      INTEGER SMAX(MAXL)     ! maximum image sample per line

*   Import-Export:
      INTEGER LMN            ! minimum image line contained
      INTEGER LMX            ! maximum image line contained
      INTEGER SMN(MAXL)      ! minimum image sample per line
      INTEGER SMX(MAXL)      ! maximum image sample per line

*   Local variables:
      INTEGER IL             ! line index
      INTEGER IL1            ! line lower limit
      INTEGER IL2            ! line upper limit

*.

*   Modify line limit immediately
      LMN = MAX(LMN, LMIN)
      LMX = MIN(LMX, LMAX)

*   For each image line in modifiable subset
      IF (LMN.LE.LMX) THEN
         DO 50 IL = LMN, LMX
            SMN(IL) = MAX(SMN(IL), SMIN(IL))
            SMX(IL) = MIN(SMX(IL), SMAX(IL))
 50      CONTINUE
      END IF

*   Update lmin to exclude empty lines
      IL1 = LMN

      DO 100 IL = IL1, LMX
         IF (SMN(IL).LE.SMX(IL)) GO TO 200
         LMN = IL + 1
 100  CONTINUE

*   Update lmx to exclude empty lines
 200  CONTINUE

      IF (LMN.LE.LMX) THEN
         IL2 = LMX
         DO 250 IL = IL2, LMN, -1
            IF (SMN(IL).LE.SMX(IL)) GO TO 300
            LMX = IL - 1
 250     CONTINUE
      END IF
 300  CONTINUE

      END
