*-----------------------------------------------------------------------

      SUBROUTINE ALLOCATE_DEVICE (DEVICE, IFAIL)

*   Routine to open channel to required MONGO device

      IMPLICIT  NONE

      INTEGER   DEVICE
      INTEGER   IFAIL

      INCLUDE   'FLAGCOMM'

*  Ok, go...

      IFAIL    = 0

*     Type *, '--- allocate_device: called with DEVICE =', device

      IF (DEVICE.EQ.0) THEN
        WRITE (ILOUT, *) 'Null plot device selected'
        IFAIL = 75

      ELSE IF (DEVICE.GE.1 .AND. DEVICE.LE.9) THEN
*       Type *,'Plotting on two-screen terminal'
        CALL SXGDEVICE (TERMDEV, TTNN, .FALSE., IFAIL)

      ELSE IF (DEVICE.EQ.10) THEN
*       Type *,'Plotting on VT240, Device = ',TTNN
        CALL SXGDEVICE (TERMDEV, TTNN, .FALSE., IFAIL)

      ELSE IF (DEVICE.EQ.11) THEN
*       Type *,'Plotting on Tek4010, Device = ',TTNN
        CALL SXGDEVICE (TERMDEV, TTNN, .FALSE., IFAIL)

      ELSE IF (DEVICE.GE.12 .AND. DEVICE.LE.19) THEN
*       Type *,'Plotting on workstation/image display, Device = ',TTNN
        CALL SXGDEVICE (TERMDEV, TTNN, .FALSE., IFAIL)

      ELSE IF (DEVICE.GE.20) THEN
        WRITE (ILOUT, *) 'Plotting on hardcopy device'
        CALL SXGDEVICE (PRINTDEV, ' ', ANDXLG, IFAIL)

      ELSE
        IFAIL = 18
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
