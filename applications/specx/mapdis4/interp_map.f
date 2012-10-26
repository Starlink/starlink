*  History:
*     14 Dec 1993 (hme):
*        Do not mix up INTEGER and LOGICAL w.r.t. I{GET|FREE}VM.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused J
*-----------------------------------------------------------------------

      SUBROUTINE INTERPOLATE_MAP (NMAP, NAXX, NAXY, IPTR,
     &                            CELL_XSIZE, CELL_YSIZE,
     &                            INTERP, FWHMX, WMAXX, FWHMY, WMAXY,
     &                            SMOOTH, INTERP_X, INTERP_Y,
     &                            LXPIX, LYPIX, BADPIX_VAL, IFAIL)

*  Routine to apply interpolation (to convolve and fill up missing
*  pixels) and smoothing (to make smooth contours) to maps in
*  virtual memory.
*  Note that we could do both interpolation and smoothing with the
*  same routine (INTERPARR), but we don't. The width of the convolving
*  gaussian may be very low, and then we get funny results when we plot
*  the contours. Better to convolve first, then interpolate with tapered
*  sin(x)/x (in CONVARR).

      IMPLICIT   NONE

      INCLUDE 'CNF_PAR'

*     Formal parameters:

      INTEGER   NMAP         ! Number of maps in array
      INTEGER   NAXX         ! Input length of map row.
      INTEGER   NAXY         ! Input length of map column
      INTEGER   IPTR         ! Address of first element of map
      REAL      CELL_XSIZE   ! X-dimension of input cell, arcseconds
      REAL      CELL_YSIZE   ! Y-dimension of input cell, arcseconds
      LOGICAL   INTERP       ! Map is to be interpolated
      REAL      FWHMX        ! FWHM of X-convolving gaussian
      REAL      WMAXX        ! Maximum extent of Y-convolving gaussian
      REAL      FWHMY        ! FWHM of Y-convolving gaussian
      REAL      WMAXY        ! Maximum extent of Y-convolving gaussian
      LOGICAL   SMOOTH       ! Map is to be smoothed
      INTEGER   INTERP_X     ! Number of interpolating pixels in X direction
      INTEGER   INTERP_Y     ! Number of interpolating pixels in Y direction
      INTEGER   LXPIX        ! Number of points along single pixel , X
      INTEGER   LYPIX        ! Number of points along single pixel , Y
      REAL      BADPIX_VAL   ! Value set for pixels where value not known
      INTEGER   IFAIL        ! Error return

*     Functions

      INTEGER   IGETVM, IFREEVM

*     Local variables:

      INTEGER   I
      INTEGER   ISTAT
      INTEGER   IOFF, IOFF1
      INTEGER   NAXX1, NAXY1
      INTEGER   MAPSIZEIN
      INTEGER   MAPSIZEOUT
      INTEGER   IPTR2

*  Ok, go..

      IFAIL     = 0

      NAXX1     = NAXX
      NAXY1     = NAXY
      MAPSIZEIN = 4*NAXX*NAXY

*     Diagnostics

*     PRINT *, ' --- interpolate_map ---'
*     PRINT *, '     map size in (x,y) = ', NAXX, NAXY
*     PRINT *, '     number of maps    = ', NMAP
*     PRINT *, '     memory/map (bytes)= ', MAPSIZEIN

*     If map is to be interpolated do that first...
*     Get some new virtual memory, and interpolate each map

      IF (INTERP) THEN

        ISTAT = IGETVM (NMAP*MAPSIZEIN, .TRUE.,
     &                 'INTERPOLATE_MAP', IPTR2)
        IF (ISTAT.ne.0) THEN
          PRINT *,'Trouble getting virtual memory for interpolated map'
          IFAIL = 51
          GO TO 999
        ELSE
*         PRINT *,'Got ', NMAP*MAPSIZEIN, ' bytes for interpolated map'
        END IF

        DO I = 1, NMAP
          IOFF = (I-1)*MAPSIZEIN
          CALL INTERPARR  (%VAL(CNF_PVAL(IPTR)+IOFF),  
     :                     NAXX1,      NAXY1,
     &                     %VAL(CNF_PVAL(IPTR2)+IOFF), 
     :                     NAXX1,      NAXY1,
     &                     BADPIX_VAL,       FWHMX,      WMAXX,
     &                     FWHMY,            WMAXY,
     &                     CELL_XSIZE,       CELL_YSIZE, IFAIL)
          IF (IFAIL.NE.0)  GO TO 999
        END DO

        ISTAT = IFREEVM (IPTR)
        IF (ISTAT.ne.0) THEN
          PRINT *,'Trouble freeing VM for original map array'
          IFAIL = 51
          GO TO 999
        ELSE
*         PRINT *,'.. and released ', NMAP*MAPSIZEIN, ' bytes'
        END IF

        IPTR   = IPTR2

      END IF

*     If smoothing required, now do that for each map. This is independent
*     of units of each axis.

      IF (SMOOTH) THEN

        LXPIX = 2+INTERP_X
        LYPIX = 2+INTERP_Y
        NAXX  = (NAXX1-1)*(INTERP_X+1)+1
        NAXY  = (NAXY1-1)*(INTERP_Y+1)+1
        MAPSIZEOUT = 4*NAXX*NAXY

        ISTAT = IGETVM (NMAP*MAPSIZEOUT, .TRUE.,
     &                 'INTERPOLATE_MAP', IPTR2)
        IF (ISTAT.ne.0) THEN
          PRINT *,'Trouble getting virtual memory for smoothed maps'
          IFAIL = 51
          GO TO 999
        ELSE
*         PRINT *, 'Got ', NMAP*MAPSIZEOUT, ' bytes for smoothed maps'
        END IF

        DO I = 1, NMAP
          IOFF  = (I-1)*MAPSIZEIN
          IOFF1 = (I-1)*MAPSIZEOUT
          CALL CONVARR    (%VAL(CNF_PVAL(IPTR)+IOFF),   NAXX1,   NAXY1,
     &                     %VAL(CNF_PVAL(IPTR2)+IOFF1), NAXX,    NAXY,
     &                     BADPIX_VAL,        IFAIL)
          IF (IFAIL.NE.0)  GO TO 999
        END DO

        ISTAT = IFREEVM (IPTR)
        IF (ISTAT.ne.0) THEN
          PRINT *,'Trouble freeing VM for unsmoothed map array'
          IFAIL = 51
          GO TO 999
        ELSE
*         PRINT *, '... released ', NMAP*MAPSIZEIN, ' bytes for old maps'
        END IF

        IPTR   = IPTR2

      END IF

*     PRINT *, '     map size out (x,y)= ', NAXX, NAXY
*     PRINT *, '     memory/map (bytes)= ', MAPSIZEOUT
*     PRINT *, '     memory address    = ', IPTR2

      RETURN

*     Error return

  999 CONTINUE
      RETURN

      END

*-----------------------------------------------------------------------
