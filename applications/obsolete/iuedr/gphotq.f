      SUBROUTINE GPHOTQ( TNMAX, NX, NY, Z, DQ, STATUS )
*+
*  Name:
*     SUBROUTINE GPHOTQ
*
*  Description:
*     Convert IUEGP pixels into FN and DQ components.
*
*  History:
*     Jack Giddings      04-NOV-81     AT4 version
*     Paul Rees          03-NOV-88     IUEDR Vn. 2.0
*
*  Method:
*     The pixels from an IUE GPHOT Image (GPI) are decoded
*     into FN and DQ values
*     The IUEGP specific data quality information is stored in
*     DQ bits 5-8 set.
*     If TNMAX lies in the range 0-32766 then pixels at that value are
*     set ITF truncated and those above that value are set ITF
*     extrapolated.
*     In addition, the standard pixel DQ bits are set as appropriate.
*     A special case is that of TN<=0 which occurs for regions outside
*     the faceplate. In this case, to avoid large numbers of
*     pixels with non-zero data quality, the DATA_BLANK=-32768
*     facility is used.
*
*-

*  Implicit:
      IMPLICIT NONE

*  Constants:
      INTEGER IUEPIXEXT       ! IUE extrapolated pixel
      INTEGER IUEPIXSAT       ! IUE saturated pixel
      INTEGER IUEPIXTRN       ! IUE truncated ITF pixel
      PARAMETER ( IUEPIXEXT = 5, IUEPIXSAT = 7, IUEPIXTRN = 6 )

*  Import:
      INTEGER TNMAX           ! maximum tape pixel value in ITF table
      INTEGER NX              ! number of x-pixels
      INTEGER NY              ! number of y-pixels

*  Import-Export:
      INTEGER*2 Z(NX, NY)     ! image array

*  Export:
      BYTE DQ(NX, NY)         ! data quality array

      INTEGER STATUS

*  Local variables:
      INTEGER IX              ! loop index
      INTEGER IY              ! loop index
      INTEGER QEXT            ! data quality value for ITF extrapolation
      INTEGER QSAT            ! data quality value for saturation
      INTEGER QTRUN           ! data quality value for ITF truncation
      INTEGER TN              ! pixel value from tape
*.

*  Form data quality value for saturation.
      QSAT = 0
      CALL DQ_WRPK(1, IUEPIXSAT, 1, QSAT)

*  Data quality value for ITF truncation.
      QTRUN = 0
      CALL DQ_WRPK(1, IUEPIXTRN, 1, QTRUN)

*  Data quality value for ITF extrapolation.
      QEXT = 0
      CALL DQ_WRPK(1, IUEPIXEXT, 1, QEXT)

      CALL LINE_WRITI('%p ITFMAX=%i used to detect ITF saturation.\\',
     :                TNMAX)
      CALL PRTBUF( STATUS )

*  Decode pixel array.
      DO IY = 1, NY

         DO IX = 1, NX
            TN = Z(IX, IY)

            IF (TN.EQ. -32768 .OR. TN.EQ.32767) THEN
               Z(IX, IY) = 32767
               DQ(IX, IY) = QSAT

            ELSE IF (TN.LE.0) THEN
               Z(IX, IY) = -32768
               DQ(IX, IY) = 0

            ELSE IF (TN.EQ.TNMAX) THEN
               Z(IX, IY) = TN - 2000
               DQ(IX, IY) = QTRUN

            ELSE IF (TN.GT.TNMAX) THEN
               Z(IX, IY) = TN - 2000
               DQ(IX, IY) = QEXT

            ELSE
               Z(IX, IY) = TN - 2000
               DQ(IX, IY) = 0
            END IF

         END DO

      END DO

      END
