*  History:
*     31 Jan 1994 (hme):
*        Fix the INQUIRE statement.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused IOPEN, J, IERR, MAPLOC, LOCATION
*---------------------------------------------------------------------------

      SUBROUTINE MAKE_CHAN4 (BUF, XSCALE, IFAIL)

*   Routine to produce channel maps of data in current map file.
*   The call to GETMAP extracts the appropriate
*   plane from the data.

      IMPLICIT  NONE

*     Formal parameters:

      REAL      BUF(*)
      REAL      XSCALE(*)
      INTEGER   IFAIL

*     Include files

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'
      INCLUDE 'CUBE'
      INCLUDE 'PLOT2D'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Local variables:

      LOGICAL   PUSHED
      LOGICAL   IEXIST
      LOGICAL   INTERP
      INTEGER   BYTEOFFSET
      INTEGER   I
      INTEGER   IFILE
      INTEGER   ISTAT
      INTEGER   IPTR_MAP
      INTEGER   IX,     IY,      IZ
      REAL      TWID(3),    TEXT(3)

*     Misc common blocks

      INTEGER   NMAP_CM
      INTEGER   NB,   NE
      REAL      BEGZ, ENDZ
      COMMON /CHANMAP/ NMAP_CM, NB, NE, BEGZ, ENDZ

*     Functions

      INTEGER   IGETVM,   IFREEVM
      INTEGER   IGETLUN,  IFREELUN

*  Ok, go...

      IFAIL  = 0
      PUSHED = .FALSE.

*     Check cube loaded and fail if not

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

*     Push the stack and fetch the prototype header

      CALL PUSH
      PUSHED = .TRUE.
      CALL EXTRACT_HEADER (SCAN_HEADER)

*     Set up these indices for general use (describe which axis is which)

      IX   = LINK(1)
      IY   = LINK(2)
      IZ   = LINK(3)

*     Get integration range parameters - need to make a first call to
*     MWINDO to establish the cell sizes for each axis for the chosen
*     set of coordinates - these may have changed since PFAC last evaluated.
*     We don't mind if it fails this time - we are only asking, not using
*     the answers for the plot yet.

      NPTS1 = NPTS(1)
      CALL MWINDO (XSCALE, NPTS1, IFAIL)

      CALL SET_ZRANGE
      INDEX_PTR = CURRENT_INDEX_ADDRESS
      CALL PLOT2D_RANGE (QBEG, QEND, PBEG, PEND, IX, IY, IZ)

*     Then get remaining questions over and done with

      CALL GEN_GETR4 ('Channel width? ('//AXTIT(IZ)//')',
     &                 ZWID, 'F10.3', ZWID, ISTAT)
      ZZERO = QBEG(IZ)            ! First value given to SET_ZRANGE
      ZWID  = ABS(ZWID)

      IF (ZWID.EQ.0.0) THEN
        PRINT *,'Cannot make channel maps with zero channel width!'
        IFAIL = 16
        RETURN
      END IF

*     Now find windowing function for each axis (uses information set by
*     SET-MAP-SCALES and contained in map header to find appropriate windows
*     for a normal map - so third axis information not relevant at this stage)
*     Note: Don't now need to read in map details, since map and prototype headers
*           and INDEX array are now maintained in memory.

      CALL MAP_WINDOW (XSCALE, NPTS1, IFAIL)
      IF (IFAIL.NE.0) THEN
        IFAIL = 48
        GO TO 1000
      END IF

CD    PRINT *,'Channel maps have size ',NAX(IX),NAX(IY)

      IF (NAX(IX).LE.1 .OR. NAX(IY).LE.1) THEN
        IFAIL = 49
        GO TO 1000
      END IF

*     Save the range values of the Z-dimension and set up temporary values

      BEGZ = PBEG(IZ)
      ENDZ = PEND(IZ)

*     Work out how many channel maps to be plotted and range.

      NB = (ZZERO-BEGZ)/ZWID
      NE = (ENDZ-ZZERO)/ZWID
      PBEG(IZ) = ZZERO - NB*ZWID
      PEND(IZ) = PBEG(IZ) + SIGN (ZWID,(ENDZ-BEGZ))
      NMAP_CM  = ABS (NB+NE)
      NMAPS    = NMAP_CM               ! Copy in PLOT2D common block

*     Initialize smoothing

      LXPIX = 2
      LYPIX = 2
      NAXX  = NAX(IX)
      NAXY  = NAX(IY)

*     Get virtual memory for maps

      ISTAT = IGETVM (NMAP_CM*4*NAXX*NAXY, .TRUE.,
     &               'MAKE_CHAN4', IPTR_MAP)
      IF (ISTAT.ne.0) THEN
        PRINT *, 'Trouble getting virtual memory for map arrays'
        PRINT *, ' -- FORTRAN i/o status return = ', ISTAT
        IFAIL = 51
        GO TO 999
      END IF

*     Iterate over channels

      PRINT *,'Generating maps from cube - please be patient!'

      DO I = 1, NMAP_CM

*       Set up pointer to this (uninterpolated) channel map

        BYTEOFFSET = 4*(I-1)*NAX(IX)*NAX(IY)

*       Extract appropriate plane from data (currently using MAP_WINDOW,
*       but should be able to abbreviate this to a simpler routine)

        CALL MAP_WINDOW (XSCALE, NPTS1, IFAIL)
        IF (IFAIL.NE.0) THEN
          IFAIL = 48
          GO TO 999
        END IF

        CALL GETMAP2 (XSCALE, BUF, INTERP_WAIT,
     &                %VAL(CNF_PVAL(IPTR_MAP)+BYTEOFFSET), IFAIL)
        IF (IFAIL.NE.0) GO TO 999

*       Invert the map (top to bottom) to make right for MONGO

        CALL SWAP_ARR (4*NAX(IX), %VAL(CNF_PVAL(IPTR_MAP)+BYTEOFFSET),
     :       NAX(IY))

*       Update velocity interval for next map..

        PBEG(IZ) = PEND(IZ)
        PEND(IZ) = PEND(IZ) + SIGN (ZWID,(ENDZ-BEGZ))

      END DO

*     Interpolate/smooth maps

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
        CALL INTERPOLATE_MAP (NMAP_CM, NAXX, NAXY, IPTR_MAP,
     &                        ABS(PFAC(IX)), ABS(PFAC(IY)), INTERP,
     &                        TWID(IX), TEXT(IX), TWID(IY), TEXT(IY),
     &                        SMOOTH, INTERP_X, INTERP_Y, LXPIX, LYPIX,
     &                        BADPIX_VAL, IFAIL)
      END IF

      IF (IFAIL.NE.0) GO TO 999

*     Write map parameters to common so PLOT_CHANN knows what it is a map of...

      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, MAP_RA, MAP_DEC)
      MAP_NAME     = MAP_ID

*     Done with the scan header - release the stack

      CALL POP
      PUSHED = .FALSE.

C     Check for existence of mapplane.tmp and delete it if it is there

      INQUIRE (FILE='mapplane.tmp', EXIST=IEXIST, IOSTAT=ISTAT)
      IF (IEXIST) THEN
        ISTAT = IGETLUN (IFILE, 'make_chan4', .TRUE.)
        OPEN (IFILE, FILE='mapplane.tmp', STATUS='OLD',
     &        ACCESS='SEQUENTIAL', IOSTAT=ISTAT)
        CLOSE (IFILE, STATUS='DELETE', IOSTAT=ISTAT)
        ISTAT = IFREELUN (IFILE)
      END IF

C     Write map to a file

      ISTAT = IGETLUN (IFILE, 'make_chan4', .TRUE.)
      OPEN  (IFILE,
     &       FILE   = 'mapplane.tmp',
     &       STATUS = 'NEW',
     &       FORM   = 'UNFORMATTED',
     &       ACCESS = 'SEQUENTIAL',
     &       IOSTAT =  ISTAT)

      IF (ISTAT.NE.0) THEN
        PRINT *, ' --- make_chan4 ---'
        PRINT *, '     error opening mapplane.tmp'
        IFAIL = 18
      ELSE
        WRITE (IFILE) NMAPS, NAXX, NAXY
        CALL VWRITE (IFILE, NMAPS*NAXX*NAXY, %VAL(CNF_PVAL(IPTR_MAP)), 
     :               ISTAT)
      END IF

      CLOSE (IFILE, IOSTAT=ISTAT)
      ISTAT = IFREELUN (IFILE)

      PRINT *,'Maps now generated, going to contour them...'

C   Release virtual memory

  999 CONTINUE

      ISTAT = IFREEVM (IPTR_MAP)
      IF (ISTAT .ne. 0) THEN
        PRINT *,'Trouble freeing virtual memory for map'
        IFAIL = 51
      END IF

 1000 IF (PUSHED) CALL POP

      RETURN
      END

C-----------------------------------------------------------------------
