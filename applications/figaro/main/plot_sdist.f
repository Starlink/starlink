      subroutine plot_sdist(nl,ni,npts,mlength,xpos,ypos,nwindow)
*+
* Name:
*    PLOT_SDIST

* Invocation:
*    CALL PLOT_SDIST(NL,NI,NPTS,MLENGTH,XPOS,YPOS,NWINDOW)

* Purpose:
*  Plot the map of S-distortion (in COMB) or line curvature (in ARCSDI).

* Description:
*  Plot the map of S-distortion (in COMB) or line curvature (in ARCSDI).

* Arguments
*    NL = INTEGER (Given)
*        1st dimension of data
*    NI = INTEGER (Given)
*        2nd     "     "   "
*    NPTS(NWINDOW) = INTEGER ARRAY (Given)
*        Number of points for each line
*    MLENGTH = INTEGER (Given)
*        1st dimension of position arrays
*    XPOS(MLENGTH,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        X positions
*    YPOS(MLENGTH,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Y positions
*    NWINDOW = INTEGER (Given)
*        Number of lines (windows)
*
* Revised to remove large fixed dimension arrays T.N.Wilkins Manchester
*  8/8/88
* TNW 12-Aug 1992 IoA Workspace removed
*-
      implicit none
      integer nl
      integer ni
      integer nwindow
      integer mlength
      double precision xpos(mlength,nwindow)
      double precision ypos(mlength,nwindow)
      integer npts(nwindow)

      integer k,j

      call gr_spen(1)
*
* Set x-axis limits.
*
      call pgbbuf
      call pgenv(1.0,real(nl),1.0,real(ni),0,0)
      call pglabel('Channels','X-sections','S-Distortion Map')
*
* mark each point with a dot
*
      do k =1,nwindow
        do j = 1,npts(k)
          call pgpoint(1,real(xpos(j,k)),real(ypos(j,k)),1)
        end do
      end do
      call pgebuf
      end
