      SUBROUTINE VIC_MEZ1(RECLEN, DATA, STATUS)

*+
*
*   Name:
*      SUBROUTINE VIC_MEZ1
*
*   Description:
*      Translate common part of IUE Record Zero.
*
*   History:
*      Jack Giddings      02-DEC-81     AT4 version
*      Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The basic contents of IUE Record Zero are read into the common
*      block CMUEZ1.
*      The values of RECLEN, Norder and Nrecrd are used to determine
*      the software version, Swver, and the resolution, Resol.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER RECLEN             ! size of record in short words

      INTEGER*2 DATA(RECLEN)     ! record zero data

*   Export:
      INTEGER STATUS             ! status return

*   External references:
      LOGICAL STR_SIMLR          ! caseless string equality

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMUEZ1'
      INCLUDE 'CMSAVE'

*   Local variables:
      INTEGER I                  ! loop index
      INTEGER IORDER             ! order index
      INTEGER J                  ! loop index
      INTEGER SWVER              ! software version

*   Software version
      IF (RECLEN .EQ. 602) THEN
         SWVER = 1

      ELSE IF (RECLEN .EQ. 1024) THEN
         SWVER = 2

      ELSE
         CALL ERRSTR('record length \\')
         CALL ERRINT(RECLEN)
         CALL ERROUT(': illegal\\', STATUS)
         RETURN
      END IF

*   Camera
      IF (DATA(6).EQ.1) THEN
         CALL STR_MOVE('LWP\\', 16, CAMERA)

      ELSE IF (DATA(6).EQ.2) THEN
         CALL STR_MOVE('LWR\\', 16, CAMERA)

      ELSE IF (DATA(6).EQ.3) THEN
         CALL STR_MOVE('SWP\\', 16, CAMERA)

      ELSE IF (DATA(6).EQ.4) THEN
         CALL STR_MOVE('SWR\\', 16, CAMERA)

      ELSE
         CALL STR_TERM(0, 16, CAMERA)
      END IF

*   Image number
      IMAGE = DATA(7)

*   Number of orders
      NORDER = DATA(5)
      IF (NORDER.LT.1 .OR. NORDER.GT.100) THEN
         CALL ERRSTR('Number of orders \\')
         CALL ERRINT(NORDER)
         CALL ERROUT(': illegal\\', STATUS)
         RETURN
      END IF

*   Number of records per order
      NRECRD = DATA(8)
      IF (NRECRD.LT.3 .OR. NRECRD.GT.8) THEN
         CALL ERRSTR('Number of records per order \\')
         CALL ERRINT(NRECRD)
         CALL ERROUT(': illegal\\', STATUS)
         RETURN
      END IF

*   Aperture (optional item)
      IF (DATA(17).EQ.1) THEN
         CALL STR_MOVE('LAP\\', 16, APER)

      ELSE IF (DATA(17).EQ.2) THEN
         CALL STR_MOVE('SAP\\', 16, APER)

      ELSE
         CALL STR_TERM(0, 16, APER)
      END IF

*   Data quality has safe scale factors
      DATMIN(2) = -32767
      DATMAX(2) = 32767
      DATSCL(2) = 1.0

*   Global wavelength limits and scale factor
      DATMIN(1) = DATA(3)
      DATMAX(1) = DATA(4)

      IF (STR_SIMLR('LORES\\', RESOL)) THEN
         DATSCL(1) = 0.2

      ELSE
         DATSCL(1) = 0.002

      END IF

*   Limits and scale factor for other records
      DO I = 3, NRECRD
         J = (I - 3)*4 + 20
         DATMIN(I) = DATA(J + 1)
         DATMAX(I) = DATA(J + 2)
         DATSCL(I) = DBLE(DATA(J + 3)) / 2.0**DATA(J + 4)
      END DO

*   Wavelength offset, order number, number of points and slit
*   for each order
      DO IORDER = 1, NORDER
         WOFFS(IORDER) = DATA(IORDER + 102)
         ORDERS(IORDER) = DATA(IORDER + 202)
         NWAVS(IORDER) = DATA(IORDER + 302)
         SLITS(IORDER) = DATA(IORDER + 402) / 100.0
      END DO

      END
