*  History:
*     31 Jan 1994 (hme):
*        Fix the INQUIRE statement. Remove second declaration of IFILE.
*        Disuse <> in formats.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused LNAME, MAP_USED, IOPEN, K, IERR, IMAP, MAPLOC, NDATA
*-----------------------------------------------------------------------

      SUBROUTINE MAKE_LINE4 (BUF1, BUF2, IFAIL)

*   Routine to make six maps, of the peak temperature, velocity of the peak,
*   line equivalent width and integrated intensity

      IMPLICIT NONE

*     Formal parameters:

      REAL*4    BUF1(*), BUF2(*)  ! Workspace
      INTEGER*4 IFAIL             ! SPECX error return

*     Include files

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'
      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Functions

      INTEGER*4 IGETVM
      INTEGER*4 IFREEVM
      INTEGER*4 IGETLUN
      INTEGER*4 IFREELUN
      INTEGER*4 GEN_ILEN

*     Other parameters

      LOGICAL*4 IEXIST
      LOGICAL*4 INTERP
      INTEGER*4 I,  J,  N
      INTEGER*4 IFILE
      INTEGER*4 IOSTAT
      INTEGER*4 IPTR(6)
      INTEGER*4 IPTR_MAP
      INTEGER*4 ISTAT
      INTEGER*4 IXN, IXU
      INTEGER*4 LOCATION
      INTEGER*4 NBYTES
      INTEGER*4 SAVLINK(3)
      REAL*4    TEMP(2)
      CHARACTER XTITLE*10, XUNITS*6, PROMPT*256

*     For investigation of Sun compiler errors; do not delete
*     INTEGER   TMEM
*     COMMON /TEMPORARY/ TMEM

*  Ok, go...

      IFAIL = 0

*     Save the existing LINK array and set to 1,2,3 for these maps

      DO J = 1,3
        SAVLINK(J) = LINK(J)
        LINK(J)    = J
      END DO

*     Check cube loaded -- error return if not

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

*     Find out what range of the x-axis to calculate line parameters for:

      XTITLE = XAXIS_NAME
      IXN    = GEN_ILEN (XTITLE)
      XUNITS = XAXIS_UNITS
      IXU    = GEN_ILEN (XUNITS)

      TEMP(1) = QBEG(3)
      TEMP(2) = QEND(3)
      CALL GEN_GETR4A (XTITLE(:IXN)//' range ('//XUNITS(:IXU)//') ?',
     &                  TEMP, 2, 'F8.3,'','',F8.3', TEMP, ISTAT)
      QBEG(3) = TEMP(1)
      QEND(3) = TEMP(2)

*     Find out which parameters we need to plot

      N = 0
      DO I = 1, 6
        IF (IPLP(I).ne.0) N = N + 1
      END DO
      IF (N.eq.0) N = 1

      WRITE (PROMPT, '(128I2)') (IPLP(I),I=1,N)

      PROMPT =    '"'' Which parameters do you want to plot?''/'
     &         // ''' Tmax (1), Vmax (2), Integrated Intensity (3),''/'
     &         // ''' Equivalent width (4), Centroid (5), '
     &         // ' or 2nd Moment (6) ''/'' ['
     &         // PROMPT
      PROMPT = PROMPT(:GEN_ILEN(PROMPT))//']  ''"'
      CALL GEN_GETI4A (PROMPT, IPLP, 6, ' ', IPLP, ISTAT)

*     Push existing data onto the stack and fetch the prototype header

      CALL PUSH
      CALL EXTRACT_HEADER (SCAN_HEADER)

      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, MAP_RA, MAP_DEC)

*     Now find windowing function for each axis (uses information set by
*     SET-MAP-SCALES and contained in map header to find appropriate windows
*     for a normal map - so third axis information not relevant at this stage)
*     Note: Don't now need to read in map details, since map and prototype
*           headers and INDEX array are now maintained in memory.

      INDEX_PTR = CURRENT_INDEX_ADDRESS
      CALL PLOT2D_RANGE (QBEG, QEND, PBEG, PEND, 1, 2, 3)

*     Find plot windowing function

      NPTS1 = NPTS(1)
      CALL MWINDO (BUF1, NPTS1, IFAIL)

      IF (NAX(1).LE.1 .OR. NAX(2).LE.1) THEN
        IFAIL = 49
        RETURN
      END IF

*     Get virtual memory for 6 input maps

      NBYTES = 4*NAX(1)*NAX(2)
      ISTAT= IGETVM (6*NBYTES, .TRUE., 'MAKE_LINE4', IPTR_MAP)
      IF (ISTAT.ne.0) THEN
        PRINT *,'Trouble getting virtual memory for map arrays'
        IFAIL = 51
        GO TO 999
      END IF

*     Extract appropriate maps from data

      IPTR(1) = IPTR_MAP
      IPTR(2) = IPTR_MAP +    NBYTES
      IPTR(3) = IPTR_MAP +  2*NBYTES
      IPTR(4) = IPTR_MAP +  3*NBYTES
      IPTR(5) = IPTR_MAP +  4*NBYTES
      IPTR(6) = IPTR_MAP +  5*NBYTES

      CALL MAP_WINDOW (BUF1, NPTS1, IFAIL)
      IF (IFAIL.NE.0) THEN
        IFAIL = 48
        GO TO 999
      END IF

      WRITE (ILOUT,*) '--- Make_line4 --- '
      WRITE (ILOUT,*) '    extracting maps from data...'

      CALL GETPARS (BUF1, BUF2, %VAL(CNF_PVAL(IPTR(1))), 
     :              %VAL(CNF_PVAL(IPTR(2))),
     &              %VAL(CNF_PVAL(IPTR(3))), %VAL(CNF_PVAL(IPTR(4))), 
     :              %VAL(CNF_PVAL(IPTR(5))),
     &              %VAL(CNF_PVAL(IPTR(6))), INTERP_WAIT, IFAIL)
      IF (IFAIL.NE.0) GO TO 999

*     Invert the maps (top to bottom) to make right for graphics

      DO I = 1, 6
        LOCATION = IPTR (I)
        CALL SWAP_ARR (4*NAX(1), %VAL(CNF_PVAL(LOCATION)), NAX(2))
      END DO

*     Interpolate and/or smooth the maps as required...

      LXPIX = 2
      LYPIX = 2
      NAXX  = NAX(1)
      NAXY  = NAX(2)
      NMAPS = 6

      INTERP = MAP_INTERPOLATED .OR. SMOOTH
      IF (INTERP) THEN
        CALL INTERPOLATE_MAP (NMAPS, NAXX, NAXY, IPTR_MAP,
     &                        CELL_XSIZE, CELL_YSIZE,
     &                        INTERP, 0.0, 0.0, 0.0, 0.0,
     &                        SMOOTH, INTERP_X, INTERP_Y, LXPIX, LYPIX,
     &                        BADPIX_VAL, IFAIL)
      END IF

      IF (IFAIL.NE.0) GO TO 999

*     Restore the LINK array

      DO J = 1,3
        LINK(J) = SAVLINK(J)
      END DO

*     Check for existence of mapplane.tmp and delete it if it is there

      INQUIRE (FILE='mapplane.tmp', EXIST=IEXIST, IOSTAT=ISTAT)
      IF (IEXIST) THEN
        ISTAT = IGETLUN (IFILE, 'make_line4', .TRUE.)
        OPEN (IFILE, FILE='mapplane.tmp', STATUS='OLD',
     &        ACCESS='SEQUENTIAL', IOSTAT=ISTAT)
        CLOSE (IFILE, STATUS='DELETE', IOSTAT=ISTAT)
        ISTAT = IFREELUN (IFILE)
      END IF

*     Write map to a file

      ISTAT = IGETLUN (IFILE, 'make_line4', .TRUE.)
      OPEN  (IFILE,
     &       FILE   = 'mapplane.tmp',
     &       STATUS = 'NEW',
     &       FORM   = 'UNFORMATTED',
     &       ACCESS = 'SEQUENTIAL',
     &       IOSTAT =  ISTAT)

      IF (ISTAT.NE.0) THEN
        PRINT *, ' --- make_line4 ---'
        PRINT *, '     error opening mapplane.tmp'
        CALL GEN_ERMSG (ISTAT)
        IFAIL = 18
      ELSE
*       PRINT *, 'Writing file from address ', IPTR_MAP
        WRITE (IFILE) NMAPS, NAXX, NAXY
        CALL VWRITE (IFILE, NMAPS*NAXX*NAXY, %VAL(CNF_PVAL(IPTR_MAP)), 
     :               ISTAT)
      END IF

      CLOSE (IFILE, IOSTAT=IOSTAT)
      ISTAT = IFREELUN (IFILE)

*     Release virtual memory

  999 CONTINUE

*     TMEM  = IPTR_MAP
*     PRINT *, 'TMEM = ', TMEM

      ISTAT = IFREEVM (IPTR_MAP)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble freeing virtual memory for map'
        IFAIL = 51
      END IF

 1000 CALL POP

      RETURN
      END

C-----------------------------------------------------------------------
