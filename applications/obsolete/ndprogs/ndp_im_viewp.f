      subroutine ndp_image_viewport
     &  (stapix,endpix,mag,control,ximv,yimv,square)
C+
C
C   -----------------------------------
C   N D P _ I M A G E _ V I E W P O R T
C   -----------------------------------
C
C   Description
C   -----------
C   Sets the PGPLOT viewport for image display. The image size may be
C   selected as a fraction of the whole display durface. The location on the
C   surface may be any combination of Top/Centre/Bottom paired with
C   Left/Centre/Right, e.g. 'TL' is top left. A central position is always
C   returned unless otherwise specified, e.g. 'L' gives left of screen,
C   vertically centred.
C
C
C   Parameters
C   ----------
C   STAPIX  (> integer array). Start pixel numbers to be displayed.
C   ENDPIX  (> integer array). End pixel numbers to be displayed.
C   MAG     (> real). Magnification. 1.0 = fit to full display surface.
C   CONTROL (> character). Option control characters.
C   XIMV    (< real array). X coordinates of image viewport in inches.
C           (coords of image area only, does not include axes and titles).
C   YIMV    (< real array). Y coordinates of image viewport in inches.
C           (coords of image area only, does not include axes and titles).
C   SQUARE  (< real). Size of largest possible square viewport in inches
C           (size of image area only, does not include axes and titles).
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library ICH:
C     ICH_LEN
C
C   Starlink PGPLOT:
C     PGQVP
C     PGVSTAND
C
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / Names > 6 characters
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
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   06-OCT-1994   - Removed unused variables. (GJP)
CC
C
C+-----------------------------------------------------------------------------
c
      implicit none
c
c
      character*(*) control
      integer       stapix(2),endpix(2)
      real          mag,ximv(2),yimv(2),square
c
      logical       bottom
      logical       left
      integer       ncol
      integer       nrow
      real          ratio
      logical       right
      logical       rlcent
      logical       tbcent
      logical       top
      real          xmax
      real          xmax1
      real          xmax2
      real          xmin
      real          xmin1
      real          xmin2
      real          ymax
      real          ymax1
      real          ymax2
      real          ymin
      real          ymin1
      real          ymin2
c
c   Default is right-left and top-bottom centering.
c
      rlcent=.true.
      tbcent=.true.
c
c   Interpret control instructions.
c
      right=index(control,'R').ne.0
      left=index(control,'L').ne.0
      top=index(control,'T').ne.0
      bottom=index(control,'B').ne.0
c
c   Work out whether right-left or top-bottom centering is required.
c
      if(right.or.left)then
        rlcent=.false.
      end if
      if(top.or.bottom)then
        tbcent=.false.
      end if
c
c   Compute device aspect ratio.
c
      call pgvstand
      call pgqvp(1,xmin,xmax,ymin,ymax)
      ratio=(ymax-ymin)/(xmax-xmin)
c
c   Compute location of largest possible square.
c
      xmin1=xmin
      xmax1=xmax
      ymin1=ymin
      ymax1=ymax
c
      if(ratio.lt.1.0)then
        if(rlcent)then
          xmin1=xmin+0.5*(xmax-ymax)
        else if(right)then
          xmin1=xmin+(xmax-ymax)
        else if(left)then
          xmin1=xmin
        end if
        xmax1=xmin1+(ymax-ymin)
        ymin1=ymin
        ymax1=ymax
        square=ymax
      else if(ratio.gt.1.0)then
        if(tbcent)then
          ymin1=ymin+0.5*(ymax-xmax)
        else if(top)then
          ymin1=ymin+(ymax-xmax)
        else if(bottom)then
          ymin1=ymin
        end if
        xmin1=xmin
        xmax1=xmax
        ymax1=ymin1+(xmax-xmin)
        square=xmax
      end if
c
c   Compute image aspect ratio.
c
      ncol=endpix(1)-stapix(1)+1
      nrow=endpix(2)-stapix(2)+1
      ratio=real(nrow)/real(ncol)
c
c   Adjust for image aspect ratio.
c
      xmin2=xmin1
      xmax2=xmax1
      ymin2=ymin1
      ymax2=ymax1
c
      if(ratio.gt.1.0)then
        if(right)then
          xmin2=xmin1+(ymax1-ymin1)*(1.0-1.0/ratio)
          xmax2=xmax1
        else if(left)then
          xmin2=xmin1
          xmax2=xmax1-(ymax1-ymin1)*(1.0-1.0/ratio)
        else if(rlcent)then
          xmin2=xmin1+0.5*(ymax1-ymin1)*(1.0-1.0/ratio)
          xmax2=xmax1-0.5*(ymax1-ymin1)*(1.0-1.0/ratio)
        end if
        ymin2=ymin1
        ymax2=ymax1
      else if(ratio.lt.1.0)then
        if(top)then
          ymin2=ymin1+(xmax1-xmin1)*(1.0-ratio)
          ymax2=ymax1
        else if(bottom)then
          ymin2=ymin1
          ymax2=ymax1-(xmax1-xmin1)*(1.0-ratio)
        else if(tbcent)then
          ymin2=ymin1+0.5*(xmax1-xmin1)*(1.0-ratio)
          ymax2=ymax1-0.5*(xmax1-xmin1)*(1.0-ratio)
        end if
        xmin2=xmin1
        xmax2=xmax1
      end if
c
c   Adjust for required magnification.
c
      ximv(1)=xmin2
      ximv(2)=xmax2
      yimv(1)=ymin2
      yimv(2)=ymax2
c
      if(right)then
        ximv(1)=xmin2+(xmax2-xmin2)*(1.0-mag)
        ximv(2)=xmax2
      else if(left)then
        ximv(1)=xmin2
        ximv(2)=xmax2-(xmax2-xmin2)*(1.0-mag)
      else if(rlcent)then
        ximv(1)=xmin2+0.5*(xmax2-xmin2)*(1.0-mag)
        ximv(2)=xmax2-0.5*(xmax2-xmin2)*(1.0-mag)
      end if
c
      if(top)then
        yimv(1)=ymin2+(ymax2-ymin2)*(1.0-mag)
        yimv(2)=ymax2
      else if(bottom)then
        yimv(1)=ymin2
        yimv(2)=ymax2-(ymax2-ymin2)*(1.0-mag)
      else if(tbcent)then
        yimv(1)=ymin2+0.5*(ymax2-ymin2)*(1.0-mag)
        yimv(2)=ymax2-0.5*(ymax2-ymin2)*(1.0-mag)
      end if
c
      end
