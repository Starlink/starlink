*  History:
*     14 Dec 1993 (hme):
*        Do not mix up INTEGER and LOGICAL w.r.t. I{GET|FREE}VM.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE DRAW_MAP (ZC, NZ, MAP, NAXX, NAXY, XM1, XM2, YM1, YM2,
     &                     NXCELL, NYCELL, NX, NY, LTPOS, LTZ, LTNEG,
     &                     PLOTCONT, PLOTGREY, GREYLIM,
     &                     BADPIX_VAL, SOME_PLOT)

C  Subroutine to draw the map contained in the array at MAP with dimensions
C  NAXX x NAXY. Puts in contour levels contained in ZC(NZ). Uses information
C  contained in INDEX to exclude pixels which do not have valid data at all
C  four corners. Within each pixel the data are interpolated to a NX by NY
C  subgrid which is independently plotted. Data window is at (mm) positions
C  (XOFF,YOFF) at the bottom left hand corner and has size AX1LEN x AX2LEN.

C  Formal parameters:

      REAL*4    ZC(NZ)             ! Array of contour levels
      INTEGER*4 NZ                 ! Number of contours
      REAL*4    MAP(NAXX, NAXY)    ! The data
      INTEGER*4 NAXX,   NAXY       ! Dimensions of MAP
      REAL*4    XM1, XM2, YM1, YM2 ! Limits of map array
      INTEGER*4 NXCELL, NYCELL     ! # of independent cells for each axis
      INTEGER*4 NX,     NY         ! Number of points in each subarray
      INTEGER*4 LTPOS, LTZ, LTNEG  ! Line types for +ve, zero and -ve contours
      REAL*4    BADPIX_VAL         ! "magic" value for map
      LOGICAL*4 PLOTCONT           ! Plot contour levels
      LOGICAL*4 PLOTGREY           ! Plot greyscale
      REAL*4    GREYLIM(2)         ! Min and max values on greyscale
      LOGICAL*4 SOME_PLOT          ! Something got plotted.

C  Other declarations

*     CALL SXGTIDLE
*     Print *,'X-limits:',XM1,XM2
*     Print *,'Y-limits:',YM1,YM2
*     Print *,'NAXX, NAXY',NAXX,NAXY
*     Print *,'NXCELL,NYCELL',NXCELL,NYCELL
*     Print *,'NX, NY',NX, NY
*     Print *,'Badpixel value (in DRAW_MAP): ', BADPIX_VAL
*     Print *,'Contour line types, +/0/- ',LTPOS, LTZ, LTNEG
*     CALL SXGTTGRAPH

      IF (PLOTGREY) THEN
        CALL SXGGREY  (MAP, NAXX, NAXY, XM1, XM2, YM1, YM2, GREYLIM)
      END IF

      IF (PLOTCONT) THEN
        DO J = 1,NZ
          LTYPE = LTPOS
          IF (ZC(J).EQ.0.) LTYPE = LTZ
          IF (ZC(J).LT.0.) LTYPE = LTNEG
          CALL SXGLTYPE   (LTYPE)

          IF (PLOTGREY) THEN
            CALL SETCCOL (ZC(J), GREYLIM(1), GREYLIM(2), ICOL)
            CALL SXGSCI  (ICOL)
          END IF

          CALL SXGCONB (MAP, NAXX, NAXY, XM1, XM2, YM1, YM2,
     &                  ZC(J), 1, BADPIX_VAL)
        END DO
      END IF

      CALL SXGSCI (1)
      SOME_PLOT = .TRUE.

      RETURN
      END

C-----------------------------------------------------------------------

