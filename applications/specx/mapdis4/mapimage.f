*--------------------------------------------------------------------------

      SUBROUTINE MAPIMAGE (FILENAME, IPTR, NMAPS, IMX, IMY, ISTAT)

*  Routine to read an ANM type SMP file (as produced by SPECX in this
*  instance) and to copy the data to virtual memory.

      IMPLICIT   NONE

      INCLUDE 'CNF_PAR'

*     Formal parameters:

      CHARACTER  FILENAME*(*)   ! Filename for SMP file
      INTEGER    IPTR           ! Returned pointer to virtual memory
      INTEGER    NMAPS          ! Number of (same sized) maps in data
      INTEGER    IMX            ! Returned X-size of image (words)
      INTEGER    IMY            ! Returned Y-size of image (words)
      INTEGER    ISTAT          ! Status return

*     Local variables:

      INTEGER    IOSTAT
      INTEGER    UNIT
      INTEGER    NBYTES

*     Functions:

      INTEGER    IGETVM
      INTEGER    IGETLUN
      INTEGER    IFREELUN

*  Ok, go...

      ISTAT = 0

      ISTAT = IGETLUN (UNIT, 'contour_map4', .FALSE.)
      IF (ISTAT.ne.0) RETURN

      OPEN (UNIT,
     &      FILE   = FILENAME,
     &      STATUS = 'OLD',
     &      FORM   = 'UNFORMATTED',
     &      ACCESS = 'SEQUENTIAL',
     &      IOSTAT =  IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT *, ' --- mapimage ---'
        PRINT *, '     failed to open file ', filename
        ISTAT = IOSTAT
        RETURN
      END IF

*     Read the basic header

      READ (UNIT, ERR=999) NMAPS, IMX, IMY
CD    PRINT *, '--- mapimage ---'
CD    PRINT *, '    file contains (# of maps) ', nmaps
CD    PRINT *, '    each has size (x by y) = ', imx, imy

*     Get virtual memory for the data array

      NBYTES = 4*NMAPS*IMX*IMY

      ISTAT  = IGETVM (NBYTES, .TRUE., 'MAP_IMAGE', IPTR)
      IF (ISTAT.NE.0) THEN
        PRINT *, '--- mapimage ---'
        PRINT *, '    IGETVM return status = ', ISTAT
        RETURN
      ELSE
CD      PRINT *, '    virtual memory got (bytes)  = ', NBYTES
      END IF

*     Read the data into the image array

      CALL VREAD (UNIT, NMAPS*IMX*IMY, %VAL(CNF_PVAL(IPTR)), ISTAT)

*     Close the file and release the logical unit

  999 CONTINUE
      CLOSE (UNIT)
      ISTAT = IFREELUN (UNIT)

      RETURN
      END

*--------------------------------------------------------------------------

      SUBROUTINE VWRITE (UNIT, NDAT, DATA, ISTAT)

*  Routine to write data array as single record to unit UNIT from
*  data array DATA

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     UNIT
      INTEGER     NDAT
      REAL        DATA(NDAT)
      INTEGER     ISTAT

*  Ok, go...

      WRITE (UNIT, IOSTAT=ISTAT) DATA
      IF (ISTAT.NE.0) THEN
        PRINT *, '--- vwrite ---'
        PRINT *, '    FORTRAN i/o error # ', ISTAT
      END IF

      RETURN
      END

*--------------------------------------------------------------------------

      SUBROUTINE VREAD (UNIT, NDAT, DATA, ISTAT)

*  Routine to read data array as single record from unit UNIT into
*  data array DATA

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     UNIT
      INTEGER     NDAT
      REAL        DATA(NDAT)
      INTEGER     ISTAT

*  Ok, go...

      READ (UNIT, IOSTAT=ISTAT) DATA
      IF (ISTAT.NE.0) THEN
        PRINT *, '--- vread ---'
        PRINT *, '    FORTRAN i/o error # ', ISTAT
      END IF

      RETURN
      END

*--------------------------------------------------------------------------
