      subroutine ndp_image_plot
     &  (array,nx,ny,stapix,endpix,start,end,high,low,label,control,
     &   ximv,yimv)
C+------------------------------------------------------------------------------
C
C   ---------------------------
C   N D P _ I M A G E _ P L O T
C   ---------------------------
C
C   Description
C   -----------
C   Displays an image on the current PGPLOT device. High and low data
C   values may be selected. Calibrated axes and a title may be plotted.
C   A ramp of the LUT may be displayed to the right of the image.
C
C
C   Parameters
C   ----------
C   ARRAY    (> real array). Image data array.
C   NX       (> integer) Array X dimension.
C   NY       (> integer) Array Y dimension.
C   STAPIX   (> real array). Start pixel numbers being displayed.
C   ENDPIX   (> real array). End pixel numbers being displayed.
C   START    (> real array). Start axis values being displayed.
C   END      (> real array). End axis values being displayed.
C   HIGH     (> real). Highest data value to be displayed.
C   LOW      (> real). Lowest data value to be displayed.
C   LABEL    (> character). Image label.
C   CONTROL  (> character). Option control characters.
C   XIMV     (> real array). X coordinates of viewport in inches.
C   YIMV     (> real array). Y coordinates of viewport in inches.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library NDP:
C     NDP_IMAGE_RAMP
C
C   PGPLOT:
C     PGBOX
C     PGLABEL
C     PGPIXL
C     PGVSIZE
C     PGWINDOW
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Jim Lewis    RGO  (CAVAD::JRL or JRL@UK.AC.CAM.AST-STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   14-JUN-1900   - Changed calling parameters so that ARRAY isn't dimensions with
C                   elements of another array.  (JRL)
C   21-JAN-1992   - Input array is now an integer array of colour indices.
C                   The original data should have been processed with
C                   NDP_IMAGE_INDEX.  Now makes use of PGPIXL which give more
C                   control over colour plotting. (JRL)
C
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
c   Parameters.
c
      character*(*) label,control
      integer       stapix(2),endpix(2),nx,ny
      integer       array(nx,ny)
      real          start(2),end(2),high,low,ximv(2),yimv(2)
c
c   Local variables.
c
      logical       axes
      logical       ramp
c
c   Interpret control instructions.
c
      axes=index(control,'A').ne.0
      ramp=index(control,'R').ne.0
c
c   Define image viewport.
c
      call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
c
c   Set world coordinates to axis units.
c
      call pgwindow(start(1),end(1),start(2),end(2))
c
c   Plot image.
c
      call pgpixl(array,nx,ny,stapix(1),endpix(1),stapix(2),
     : endpix(2),start(1),end(1),start(2),end(2))
c      call pgpixl(array,nx,ny,stapix(1),endpix(1),stapix(2),
c     : endpix(2),start(1),end(1),end(2),start(2))
c
c   Plot axes if required.
c
      if(axes)then
        call pgbox('BCINST',0.0,0,'BCINST',0.0,0)
      else
        call pgbox('BC',0.0,0,'BC',0.0,0)
      end if
c
c   Plot label.
c
      call pglabel(' ',' ',label)
c
c   Plot colour or grey scale ramp if required. Afterwards restore the image
c   viewport.
c
      if(ramp)then
        call ndp_image_ramp(ximv,yimv,low,high)
        call pgvsize(ximv(1),ximv(2),yimv(1),yimv(2))
      end if
c
      end
