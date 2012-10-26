*  History:
*     01 Jan 1994 (rp):
*        modified to open MAPPLANE using OPENUF
*     15 Jan 1994 (rp):
*        OPENUF changed to OPEN
*     31 Jan 1994 (hme):
*        Fix the INQUIRE statement.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused REPEAT, IOPEN, I, J, LUN, IERR, LOCATION
C---------------------------------------------------------------------------

      SUBROUTINE MAKE_MAP4 (BUF, XSCALE, IFAIL)

C   Routine to produce contour map of data in current map file.
C   The call to GETMAP extracts the appropriate
C   plane from the data, windowed as desired, and puts it into
C   the array BUF which is then contoured.

      IMPLICIT  NONE

C     Formal parameters:

      REAL      BUF(*)
      REAL      XSCALE(*)
      INTEGER   IFAIL

C     Include files:

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'PLOT2D'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

C     Functions

      INTEGER   IGETVM,  IFREEVM
      INTEGER   IGETLUN, IFREELUN

C     Local variables

      LOGICAL   IEXIST
      LOGICAL   INTERP
      LOGICAL   PUSHED

*      INTEGER   I
      INTEGER   IX, IY, IZ
      INTEGER   IFILE
      INTEGER   IOSTAT
      INTEGER   IPTR
      INTEGER   ISTAT
      REAL      TWID(3)
      REAL      TEXT(3)

C  Ok, go...

      IFAIL = 0
      PUSHED = .FALSE.

C  Check cube loaded - error return if not

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

C  Push the stack and extract the prototype header to the stack

      CALL PUSH
      PUSHED = .TRUE.

      CALL EXTRACT_HEADER (SCAN_HEADER)

C  Set up orientation of slice(s) through cube

      IX   = LINK(1)
      IY   = LINK(2)
      IZ   = LINK(3)

C  First call to MWINDO - for chosen axes establishes the cell width
C  for each coordinate (needed to set defaults sensibly)
C  OK if it fails this time - we are only asking, not using the results

      NPTS1 = NPTS(1)
      CALL MWINDO (XSCALE, NPTS1, IFAIL)

C  Set range of integration for Z-coordinate and evaluate all limits

      CALL SET_ZRANGE
      INDEX_PTR = CURRENT_INDEX_ADDRESS
      CALL PLOT2D_RANGE (QBEG, QEND, PBEG, PEND, IX, IY, IZ)


*     PRINT '('' initial axis limits: ''/ 3(1X, I1, 2(2X,F10.4)/))',
*    &     (I, QBEG(I),  QEND(I),I=1,3)
*     PRINT '('' final axis limits:   ''/ 3(1X, I1, 2(2X,F10.4)/))',
*    &     (I, PBEG(I),  PEND(I),I=1,3)

C  Given the final ranges of each axis, find windowing function
C  (map and prototype headers and index block are maintained in memory,
C  so don't now need to get them explicitly).

      CALL MAP_WINDOW (XSCALE, NPTS1, IFAIL)
      IF (IFAIL.NE.0) THEN
        IFAIL = 48
        GO TO 998
      END IF

*     PRINT *,'-- Make Map --'
*     PRINT *,'     Raw 2D map has size ',NAX(IX),NAX(IY)

      IF (NAX(IX).LE.1 .OR. NAX(IY).LE.1) THEN
        IFAIL = 49
        RETURN
      END IF

C  Get virtual memory for map: Note change to ADAM type status return

      ISTAT = IGETVM (4*NAX(IX)*NAX(IY), .TRUE., 'MAKE_MAP4', IPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble getting virtual memory for map array'
        IFAIL = 51
        GO TO 999
      END IF

C  Extract appropriate plane from data

      CALL GETMAP2 (BUF, XSCALE, INTERP_WAIT,
     &              %VAL(CNF_PVAL(IPTR)), IFAIL)
      IF (IFAIL.NE.0) GO TO 999

C  Flip 2-D array top/bottom to make right for MONGO

      CALL SWAP_ARR (4*NAX(IX), %VAL(CNF_PVAL(IPTR)), NAX(IY))

C  Interpolate and/or smooth map as necessary

      NXMAP = 1
      NYMAP = 1
      NMAPS = 1

      LXPIX = 2
      LYPIX = 2
      NAXX  = NAX(IX)
      NAXY  = NAX(IY)

      INTERP = MAP_INTERPOLATED.OR.INTERP_WAIT
      IF (INTERP) THEN
        TWID(1) = 0.0
        TWID(2) = 0.0
        TWID(3) = VWIDHM
        TEXT(1) = 0.0
        TEXT(2) = 0.0
        TEXT(3) = VFNMAX
      END IF

      IF (INTERP.OR. SMOOTH) THEN
*       PRINT *, 'Interpolating 2-D map'
        CALL INTERPOLATE_MAP (NMAPS, NAXX, NAXY, IPTR,
     &                        ABS(PFAC(IX)), ABS(PFAC(IY)), INTERP,
     &                        TWID(IX), TEXT(IX), TWID(IY), TEXT(IY),
     &                        SMOOTH, INTERP_X, INTERP_Y, LXPIX, LYPIX,
     &                        BADPIX_VAL, IFAIL)
      END IF
      IF (IFAIL.NE.0) GO TO 999

C     Write map parameters to common so CONTOUR knows what it is a map of...

      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, MAP_RA, MAP_DEC)
      MAP_NAME     = MAP_ID

C     Check for existence of mapplane.tmp and delete it if it is there

      INQUIRE (FILE='mapplane.tmp', EXIST=IEXIST, IOSTAT=ISTAT)
      IF (IEXIST) THEN
        ISTAT = IGETLUN (IFILE, 'make_map4', .TRUE.)
        OPEN (IFILE, FILE='mapplane.tmp', STATUS='OLD',
     &        ACCESS='SEQUENTIAL', IOSTAT=ISTAT)
        CLOSE (IFILE, STATUS='DELETE', IOSTAT=ISTAT)
        ISTAT = IFREELUN (IFILE)
      END IF

C     Write map to a file

      ISTAT = IGETLUN (IFILE, 'make_map', .TRUE.)
      OPEN  (IFILE,
     &       FILE   = 'mapplane.tmp',
     &       STATUS = 'NEW',
     &       FORM   = 'UNFORMATTED',
     &       ACCESS = 'SEQUENTIAL',
     &       IOSTAT =  ISTAT)

      IF (ISTAT.NE.0) THEN
        PRINT *, ' --- make_map4 ---'
        PRINT *, '     error opening mapplane.tmp'
        CALL GEN_ERMSG (ISTAT)
        IFAIL = 18
      ELSE
        WRITE (IFILE) NMAPS, NAXX, NAXY
        CALL VWRITE (IFILE, NAXX*NAXY, %VAL(CNF_PVAL(IPTR)), ISTAT)
      END IF

      CLOSE (IFILE, IOSTAT=IOSTAT)
      ISTAT = IFREELUN (IFILE)

C   Release virtual memory

  999 CONTINUE

      ISTAT = IFREEVM (IPTR)
      IF (ISTAT.ne.0) THEN
        PRINT *,'Trouble freeing virtual memory for map'
        IFAIL = 51
      END IF

  998 IF (PUSHED) CALL POP

      RETURN
      END

*----------------------------------------------------------------------
