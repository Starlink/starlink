*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     01 Jan 1994 (rp):
*        Replace with new version from VAX SPECX V6.3, make change as above
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Default PLOT_INTERP to .FALSE.
*        Unused DELMAP, K
C--------------------------------------------------------------

      SUBROUTINE GRID_MAP (BUF, XSCALE, IFAIL)

C   Routine to draw grid of spectra corresponding to spectra
C   in open .MAP file.
C   Parameters for individual spectra as given by DEF-PLOT and
C   S-PLOT-SCALES, those for overall windowing etc as given by
C   DEF-MAP and SET-MAP-SCALES

      IMPLICIT  NONE

*     Formal parameters

      REAL      BUF(*)
      REAL      XSCALE(*)
      INTEGER   IFAIL

*     Include files

      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Local variables

      LOGICAL   PLOT_INTERP
      LOGICAL   INTERP
      LOGICAL   STATUS
      LOGICAL   PUSHED

      INTEGER   I,  J
      INTEGER   IL, IR
      INTEGER   JT, JB
      INTEGER   II, JJ
      INTEGER   ISTAT
      INTEGER   NI, NJ
      INTEGER   NT, NV
      INTEGER   NXY
      INTEGER   SAVLINK(3)

      REAL      XARRAY(2*LSPMAX)
      REAL      YARRAY(2*LSPMAX)
      REAL      TEMP(2)
      REAL      DT,     DV
      REAL      DX,     DY
      REAL      DXC,    DYC
      REAL      PL,     PR
      REAL      PT,     PB
      REAL      T1,     T2
      REAL      V1,     V2
      REAL      XL,     XH
      REAL      YL,     YH
      REAL      XLIMG(2),YLIMG(2)
      REAL      XPOS,   YPOS
      REAL      XAXLEN, YAXLEN
      REAL      XMARGIN
      REAL      YMARGIN

      DATA      XMARGIN /15.0/
      DATA      YMARGIN /40.0/

      CHARACTER VCHAR*1

*     Axis titles etc

      LOGICAL*4 INVERT_AXIS
      COMMON /GOOD_PT/ INVERT_AXIS(3)

*     Functions:

      INTEGER   GEN_ILEN

*  Ok, go...

      IFAIL  = 0
      PUSHED = .FALSE.

C  Save the existing LINK array and set to 1,2,3 for these maps

      DO J = 1,3
        SAVLINK(J) = LINK(J)
        LINK(J)    = J
      END DO

*     Check cube is loaded - error return if not

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

*     Set up plot-limits for individual spectra
*     Note that QBEG(3), QEND(3) not stored consecutively, so have to copy...

      TEMP(1) = QBEG(3)
      TEMP(2) = QEND(3)
      CALL GEN_GETR4A ('X-axis range in '//MAPTIT(3)(:4)//'? ('//
     &                 AXTIT(3)(:6)//') ',
     &                 TEMP, 2, 'F9.3'',''F9.3', TEMP, ISTAT)
      IF (ISTAT.EQ.0) THEN
        QBEG(3) = TEMP(1)
        QEND(3) = TEMP(2)
      END IF

      CALL GEN_GETR4A ('Y-axis range in Kelvins?',
     &                  TLIM, 2, 'F9.3'',''F9.3', TLIM, ISTAT)

      CALL GEN_YESNO ('Also plot interpolated spectra?',
     &                 .FALSE., PLOT_INTERP, ISTAT)

*     Make sure this routine uses uninterpolated, unrotated map (if
*     interpolated spectra are required they are interpolated on demand).

      INDEX_PTR = INDEX_ADDRESS
      INTERP    = (MAP_INTERPOLATED.OR.INTERP_WAIT).AND.PLOT_INTERP

*     Then ascertain (R.A. - Dec.) limits of map, assuming that we have
*     X and Y as display axes (i.e. ignore SET-MAP-SCALES selection of axes,
*     but keep limits for each axis).

      CALL PLOT2D_RANGE (QBEG, QEND, PBEG, PEND, 1, 2, 3)

CD    TYPE '('' initial axis limits: ''/ 3(1X, I1, 2(2X,F10.4)/))',
CD   &     (I, QBEG(I),  QEND(I),I=1,3)
CD    TYPE '('' final axis limits:   ''/ 3(1X, I1, 2(2X,F10.4)/))',
CD   &     (I, PBEG(I),  PEND(I),I=1,3)

*     Now find windowing function for each axis.
*     Note that NPTS1 is now stored in the map header, but need to page
*     in prototype header to make SETX work

*     Push the stack and fetch the prototype header

      CALL PUSH
      PUSHED = .TRUE.
      CALL EXTRACT_HEADER (SCAN_HEADER)

      CALL MWINDO (XSCALE, NPTS1, IFAIL)
      IF (IFAIL.NE.0) THEN
        IFAIL = 48
        GO TO 99
      END IF

      NAXX = NAX(1)
      NAXY = NAX(2)

*     Find appropriate tick intervals for axes

      V1 = PBEG(3)
      V2 = PEND(3)
      T1 = TLIM(1)
      T2 = TLIM(2)

      IF (T1-T2.EQ.0.0 .OR. V1-V2.EQ.0.0) THEN
        IFAIL = 41
        GO TO 99
      END IF

      CALL AUTORANGE (V1, V2, NV)
      DV = 2. * ABS(V2-V1)/FLOAT(NV)
      CALL AUTORANGE (T1,T2,NT)
      DT = 2. *ABS(T2-T1)/FLOAT(NT)

*     Debug info

CD    PRINT *, ' -- grid_map --'
CD    PRINT *, '    nax(1),  nax(2)  = ', nax(1), nax(2)
CD    PRINT *, '    ioff(1), ioff(2) = ', ioff(1), ioff(2)

*     Initialize plot device (needed for SET_DISPLAY_SIZE to work)

      CALL ALLOCATE_DEVICE (IDEV,IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *,'Device not available?'
        GO TO 99
      END IF

*     Increment of X & Y associated with each pixel (L->R, B->T)

      DXC = SIGN (CELL_XSIZE, PEND(1)-PBEG(1))
      DYC = SIGN (CELL_YSIZE, PBEG(2)-PEND(2))

*     Overall size of montage in arcseconds. First need to find
*     out how many pixels fit within the specified area, given that
*     they must align with (0,0). Note -- for stored map format
*     implied cell size is negative in both co-ordinates.

      CALL SETPIX (PBEG(1), PEND(1), -CELL_XSIZE, MSTEP,
     &             PL, PR, IL, IR, NI)
      CALL SETPIX (PBEG(2), PEND(2), -CELL_YSIZE, NSTEP,
     &             PT, PB, JT, JB, NJ)

      XAXLEN = ABS (CELL_XSIZE*(NI+1))
      YAXLEN = ABS (CELL_YSIZE*(NJ+1))

      CALL SET_DISPLAY_SIZE (AX1LEN, AX2LEN, XAXLEN,  YAXLEN,
     &                       AXLENX, AXLENY, XMARGIN, YMARGIN)

*     Size of individual windows on screen (mm)

      DX = ABS (AXLENX/(NI+1))
      DY = ABS (AXLENY/(NJ+1))

*     Put a label to the screen

      MAP_NAME = MAP_ID
      CALL COPY_MAPPOS (RA, DEC, RAM, DECM, MAP_RA, MAP_DEC)

      CALL SXGLWEIGHT    (LWEIGHT)
      CALL SXGEXPAND     (CHARHT/3.)
      CALL SXGFONT       (1)

      IF (SHOW_2DHEAD) THEN
        CALL LABEL_MAP_SOURCE (MAP_RA, MAP_DEC)
      END IF

*     Get spectra one at a time and window onto map area.

      DO J = 1, NJ
        DO I = 1, NI

*         Set up window in millimetres

          XL = XMARGIN + (FLOAT(I-1)+0.5)*DX
          XH = XL+DX
          YL = YMARGIN + (FLOAT(NJ-J)+0.5)*DY
          YH = YL+DY

*         Locate the spectrum in the file

          II = IL+(I-1)
          IF (INVERT_AXIS(1)) II = IL-(I-1)
          JJ = JT+(J-1)
          IF (INVERT_AXIS(2)) JJ = JT-(J-1)

*         Notes: XARRAY is used as workspace only in this call.
*         Make sure requested data lies within boundaries of dataset.

          IF (II.GE.1 .AND. II.LE.MSTEP
     &     .AND. JJ.GE.1 .AND. JJ.LE.NSTEP) THEN
            CALL GET_CUBE_DATA (II, JJ, NPTS1, XARRAY,
     &                          INTERP, 1+IOFF(3), NAX(3)+IOFF(3),
     &                          YARRAY, %VAL(CNF_PVAL(INDEX_PTR)), 
     :                          MSTEP, NSTEP,
     &                          STATUS)
          ELSE
            STATUS = .FALSE.
          END IF

*         If it exists plot it!

          IF (STATUS) THEN

*           Copy xdata to buffer and make a histogram if necessary

            NXY  = NAX(3)
            CALL XCOPY (4*NXY, XSCALE(IOFF(3)+1), XARRAY)
            IF (HISTOGRAM) CALL MKHIS (XARRAY,YARRAY,NXY,XFAC(1))

*           Plot spectrum and put it in a box

            CALL SXGVWINDOW   (XL,XH,YL,YH)
            CALL SXGLIMITS    (PBEG(3), PEND(3), TLIM(1), TLIM(2))
            CALL SXGCONNECT   (XARRAY,YARRAY,NXY)
            CALL SXGTICKSIZE  (DV,DV,DT,DT)
            CALL SXGEXPAND    (0.0)
            CALL SXGBOX       (0,0)

          END IF

*         Put labels on box in bottom left corner

          IF (I.EQ.1 .AND. J.EQ.NJ) THEN
            CALL SXGVWINDOW   (XL,XH,YL,YH)
            CALL SXGLIMITS    (PBEG(3), PEND(3), TLIM(1), TLIM(2))
            CALL SXGTICKSIZE  (DV,DV,DT,DT)
            CALL SXGFONT      (1)
            CALL SXGEXPAND    (0.5*CHARHT/3.)
            CALL SXGBOX       (1,2)
          END IF
        END DO
      END DO

*     Put a big box around the whole thing to show position

      XLIMG(1) = PL - DXC
      XLIMG(2) = PR + DXC
      YLIMG(1) = PB - DYC
      YLIMG(2) = PT + DYC

      CALL SXGVWINDOW   (XMARGIN, XMARGIN+AXLENX,
     &                   YMARGIN, YMARGIN+AXLENY)
      CALL SXGLIMITS    (XLIMG(1), XLIMG(2), YLIMG(1), YLIMG(2))
      CALL SXGEXPAND    (CHARHT/4.)
      CALL SXGTICKSIZE  (0.,0.,0.,0.)
      CALL MAP_COORDS   (DMS, .TRUE., 1, 2, XLIMG, YLIMG,
     &                   MAP_RA, MAP_DEC)
      IF (SHOW_2DHEAD) THEN
        CALL SXGPLOTID  (' ',NAMEMP(:GEN_ILEN(NAMEMP)))
      END IF

*     Save the screen for VT330...

      IF (TERMINAL .AND.
     &        (TERMDEV.EQ.'VT330' .OR. TERMDEV.EQ.'VT240')) THEN
        CALL SXGCURSOR (XPOS, YPOS, VCHAR)
      END IF

*     Restore the LINK array

      DO J = 1,3
        LINK(J) = SAVLINK(J)
      END DO

*     Reset expansion and font

      CALL SXGFONT   (1)
      CALL SXGEXPAND (1.)

*     Release the plot device

      CALL ENDPLOT (IDEV, .FALSE., .TRUE.)

*     Done with the scan header - release the stack

   99 CONTINUE

      IF (PUSHED) CALL POP

      RETURN
      END

*-----------------------------------------------------------------------
