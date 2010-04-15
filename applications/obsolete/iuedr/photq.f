      SUBROUTINE PHOTQ(NX, NY, Z, DQ, STATUS)

*+
*
*   Name:
*      SUBROUTINE PHOTQ
*
*   Description:
*      Convert IUEPI pixels into FN and DQ components.
*
*   History:
*      Jack Giddings      04-NOV-81     AT4 version
*      Paul Rees          03-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The pixels from an IUE Photometric Image (PI) are decoded
*      into FN and DQ values
*      The FN values are 0.5 times the official FN values so that
*      they can be held, uniformly, in the range -32767 to 32767.
*      The IUEPI specific data quality information is stored in
*      DQ bits 5-8 set.
*      Pixels that have raw DN values are voided totally.
*      In addition, the standard pixel DQ bits are set as appropriate.
*      Pixels which have NO value are set to -32768 which is the BLANK
*      value.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Constants:
      INTEGER IUEPIXEXT       ! IUE extrapolated pixel
      INTEGER IUEPIXSAT       ! IUE saturated pixel
      INTEGER IUEPIXTRN       ! IUE truncated ITF pixel
      INTEGER OK              ! OK status

      PARAMETER (IUEPIXEXT = 5, IUEPIXSAT = 7, IUEPIXTRN = 6, OK = 0)

*   Import:
      INTEGER NX              ! number of x-pixels
      INTEGER NY              ! number of y-pixels

*   Import-Export:
      INTEGER*2 Z(NX, NY)     ! image array

*   Export:
      BYTE DQ(NX, NY)         ! data quality array

      INTEGER STATUS

*   Local variables:
      INTEGER TN              ! pixel value from tape
      INTEGER QSAT            ! data quality value for saturation
      INTEGER QEXT            ! data quality value for ITF extrapolation
      INTEGER IX              ! loop index
      INTEGER IY              ! loop index

*   Form data quality value for saturation
      QSAT = 0
      CALL DQ_WRPK(1, 2, 1, QSAT)
      CALL DQ_WRPK(1, IUEPIXSAT, 1, QSAT)

*   Data quality value for ITF extrapolation
      QEXT = 0
      CALL DQ_WRPK(1, IUEPIXEXT, 1, QEXT)

*   Decode pixel array
      DO IY = 1, NY

         DO IX = 1, NX
            TN = Z(IX, IY)

            IF (TN.GE.256 .AND. TN.LE.32767) THEN
               Z(IX, IY) = TN - 2000
               DQ(IX, IY) = 0
            ELSE IF (TN.GE. -32767 .AND. TN.LE. -2049) THEN
               Z(IX, IY) = -TN
               DQ(IX, IY) = QSAT
            ELSE IF (TN.GE. -2048 .AND. TN.LE. -1) THEN

               IF (TN.NE. - 2048) THEN
                  Z(IX, IY) = -16*TN
               ELSE
                  Z(IX, IY) = 32767
               END IF

               DQ(IX, IY) = QEXT
            ELSE IF (TN.GE.0 .AND. TN.LE.255) THEN
               Z(IX, IY) = -32768
               DQ(IX, IY) = 0
            ELSE
               Z(IX, IY) = -32768
               DQ(IX, IY) = 0
            END IF

         END DO

      END DO

      STATUS = OK

      END
