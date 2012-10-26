*  History:
*     31 Jan 1994 (hme):
*        Remove second declarations of IFAIL, AX1REQ, AX2REQ. The latter
*        two are REAL*4 in PLOT2D.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused FILESIZE, SAVMAP, IEXIST, IGETVM
*     13 Sep 2005 (timj):
*        Initialise variable NCONT to stop random addressing in array
*        lookup when contours are disabled.
C-----------------------------------------------------------------------

      SUBROUTINE CONTOUR_MAP4 (IFAIL)

*  Routine to take the 2D image stored in the file "mapplane.tmp"
*  and produce a contour plot on the current plot device using
*  the currently defined contouring set. If plot device is OK for
*  interactive plotting then it loops on the cursor until told to quit.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL             ! status return

*     Global variables/common blocks:

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'PLOT2D'
      INCLUDE 'CNF_PAR'

*     Local constants
      INTEGER   MAX_CONTOURS
      PARAMETER ( MAX_CONTOURS = 32 )

*     Local variables:

      INTEGER   IERR
      INTEGER   IMX, IMY
      INTEGER   IPTR
      INTEGER   IX, IY, IZ
      INTEGER   ISTAT
      INTEGER   NC
      INTEGER   NCONT1,  NCONT2
      INTEGER   NZ

      LOGICAL   REPEAT
      LOGICAL   CONTOURS
      LOGICAL   GREYSCALE
      LOGICAL   FIRST
      LOGICAL   SXGGREYOK
      LOGICAL   SXGCOLOROK

      REAL      ARRMAX
      REAL      ARRMIN
      REAL      DZ
      REAL      XAXLEN
      REAL      YAXLEN
      REAL      ZC(MAX_CONTOURS)
      REAL      ZMAX, ZMIN
      REAL      GRLIMITS(2)
      CHARACTER OPTIONS*64

      REAL       XMARGIN
      PARAMETER (XMARGIN = 25.)
      REAL       YMARGIN
      PARAMETER (YMARGIN = 40.)

*     Functions

      INTEGER   IFREEVM

*  Ok, go...

      IFAIL = 0

*  Default to no contours
      DATA ZC / MAX_CONTOURS * 0.0 /
      NZ = 0
      DZ = 0.0
      NCONT1 = 1

      IX   = LINK(1)
      IY   = LINK(2)
      IZ   = LINK(3)

      CALL MAPIMAGE ('mapplane.tmp', IPTR, NMAPS, IMX, IMY, ISTAT)
      IF (ISTAT.ne.0) THEN
        IFAIL = 67
        RETURN
      END IF

      FIRST = .TRUE.

C   Greyscale?

      CONTOURS  = PLOTCONT .OR. OVERCONT
      IF (PLOTGREY .AND..NOT. SXGGREYOK()) THEN
        PRINT *,'Sorry, greyscaling not available!'
        GREYSCALE = .FALSE.
      ELSE
        GREYSCALE = PLOTGREY
      END IF
      IF (.NOT.(GREYSCALE.OR.CONTOURS)) RETURN

C   Initialize Graphics (needed so that SET_DISPLAY_SIZE can work)

      CALL ALLOCATE_DEVICE (IDEV,IFAIL)
      IF (IFAIL.NE.0) THEN
        GO TO 999
      END IF

C     Copy the default plot values to arrays in /PLOT2D/ for starters

      XLIM(1) = PBEG(IX)
      XLIM(2) = PEND(IX)
      YLIM(1) = PEND(IY)
      YLIM(2) = PBEG(IY)

      AX1REQ  = AX1LEN
      AX2REQ  = AX2LEN

      CHANGE_PLOT = .FALSE.

C  Initial values of axis lengths (changed inside I2DOPT interactively
C  through call to NEW_SCALES if requested)

      XAXLEN = ABS (XLIM(2)-XLIM(1))
      YAXLEN = ABS (YLIM(2)-YLIM(1))

      CALL SET_DISPLAY_SIZE (AX1LEN, AX2LEN, XAXLEN,  YAXLEN,
     &                       AXLENX, AXLENY, XMARGIN+5., YMARGIN+8.)

C  Minimum and maximum on map

      CALL MAP_MAXMIN (%VAL(CNF_PVAL(IPTR)), NAXX, NAXY, XLIM, YLIM,
     &                 BADPIX_VAL, ZMIN, ZMAX)
      PRINT *, ' -- contour_map --'
      PRINT *, '    Min and Max on map = ', ZMIN, ZMAX

      ARRMIN = ZMIN
      ARRMAX = ZMAX

C   Plot the map on the output device

      REPEAT  = .TRUE.
      OPTIONS = ' H C ? L R T B N X S D M V I E Q W'
      IF (IZ.EQ.3) OPTIONS = ' G'//OPTIONS
      IF (GREYSCALE)    OPTIONS = ' 0 1 3'//OPTIONS
      IF (SXGCOLOROK()) OPTIONS = ' 2 4 5'//OPTIONS

      DO WHILE (REPEAT)

C  Work out contour levels

        IF (GREYSCALE) THEN
          CALL SETGSCAL (ZMIN, ZMAX, GRLIMITS)
        END IF

        IF (CONTOURS) THEN
          CALL SETCLEVS (ZMIN, ZMAX, ZC, NZ, DZ, NCONT1, NCONT2)
          IF (IDEV.LT.10 .OR. IDEV.GE.20) THEN
            CALL SXGTIDLE
            WRITE (ILOUT, *) '----------------------------------------'
            WRITE (ILOUT, *) 'Contour levels used:'
            WRITE (ILOUT, '((8(1X,F9.2)))', IOSTAT=IERR)
     &                    (ZC(NC),NC=NCONT1,NCONT2)
            WRITE (ILOUT, *) '----------------------------------------'
            CALL SXGTTGRAPH
          END IF
        ELSE
          NZ = 0
        END IF

        IF (PLOTGREY .AND. FIRST) THEN
          FIRST = .FALSE.
          CALL SETCOL5     (C5START, C5ROTAT, C5EXP)
          CALL COLOUR_PLOT (COLOUR_TABLE, IFAIL)
          CALL SET_COLOURS
        END IF

        CALL PLOT_MAP (%VAL(CNF_PVAL(IPTR)), ZC(NCONT1), DZ, NZ,
     &                 GRLIMITS, ARRMAX, ARRMIN,
     &                 CONTOURS, GREYSCALE, XMARGIN, YMARGIN, IFAIL)

        REPEAT = .FALSE.
        IF (TERMINAL .AND. INTERACTIVE) THEN
          CALL I2DOPT (%VAL(CNF_PVAL(IPTR)), OPTIONS, REPEAT,
     &                  CONTOURS, GREYSCALE, GRLIMITS)
        END IF
      END DO

C   Close the plot

      CALL ENDPLOT (IDEV, INTERACTIVE, .TRUE.)

C   Release virtual memory

  999 CONTINUE
      ISTAT = IFREEVM (IPTR)
      IF (ISTAT.NE.0) THEN
        PRINT *, '--- contour_map4 ---'
        PRINT *, '    error freeing virtual memory: ', ISTAT
      END IF

      RETURN
      END

*--------------------------------------------------------------------------
