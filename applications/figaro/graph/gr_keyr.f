      subroutine gr_keyr(nlevk,nlevu,zkey,rmin,rmax,title)
*+
* Name:
*    GR_KEYR

* Invocation:
*    CALL GR_KEYR(NLEVK,NLEVU,ZKEY,RMIN,RMAX,TITLE)

* Purpose:
*   Draw key-GKS 7.4 version

* Description:
*   This subroutine draws a key to right of the grey-scale plot
*   consisting of a title and an equally spaced selection of
*   the grey levels, each of which is annotated with the mean
*   value corresponding to it. This version is for floating point
*   data.
*
* Method:
*
*
* Arguments:
*    NLEVK = INTEGER (Given)
*       number of grey levels to be plotted up to 16.
*    NLEVU = INTEGER (Given)
*      total number of grey levels used in the grey-scale plot itself.
*    ZKEY = REAL (Given)
*       the fraction (to the right) of the square zone that will plot the key
*       (0.15 for the Versatec produces acceptable results.
*    RMIN = REAL (Given)
*       value corresponding to white in the grey-scale plot
*    RMAX = REAL (Given)
*       value corresponding to black in the grey-scale plot
*    TITLE = CHARACTER*(*) (Given)
*       title, the maximum length depends on ZKEY, but for ZKEY=0.2 only the
*       first 25 characters will be displayed.
*
* Graphics routines: GKS (library prefix G)
*                 CA
*
*              SGS (library prefix SGS_)
*                 ATEXT, BTEXT, OTEXT, SARTX, SFONT, SHTX, SW, ZSHAP
*
* History:
*
*    Original  Malcolm J. Currie  RAL  1986 June 26th
*    Number made a little larger T.N.Wilkins Manchester 1988
*    Converted to GKS 7.4, TNW 28/9/93
*-
*
*         SCALE adjusts the size of the pixels in the grey-level key
*         NPIX is the number of pixels plotted in the grey-level key
*
      implicit none
      real scale
      integer npix
      parameter (scale=100.,npix=2)

      integer nlevk,nlevu,keygr(npix,npix)

      real zkey,rmin,rmax
      real xoffk,gmin,grey,gm,gmax,range,yoffk,del,xoffa,spkey
      real ytitle,ykey
      character*(*) title
      character*11 annote
      integer k,j,i,istat,izone2

*
*         Reset zone to square in the larger zone. The viewport ranges
*         are the same and large to produce square pixels
*
      call sgs_zshap(1.0,'BR',izone2,istat)
      call sgs_sw(0.,scale,0.,scale,istat)

      ytitle=0.95*scale
      ykey=0.9*scale
      xoffk=(1.0-zkey)*scale
      spkey=ykey/real(nlevk)
*
*         Set font attributes: height and bold
*
      call sgs_shtx(scale*0.02)
      call sgs_sfont(104)
      call sgs_btext(xoffk+scale*0.012,ykey+scale*0.01)
      call sgs_atext('Key:')
      call sgs_btext(xoffk+scale*0.012,ytitle+scale*0.02)
      call sgs_atext('Title:')
*
*         Adjust font for title
*
      call sgs_shtx(scale*0.013)
      call sgs_sfont(1)
      call sgs_btext(xoffk+scale*0.015,ytitle+scale*0.000)
      call sgs_atext(title)
      call sgs_otext
*
*         Adjust font for annotation
*
      call sgs_shtx(spkey*0.26)
      call sgs_sartx(0.5)

      xoffa=scale*0.25*zkey + xoffk
      del=0.005*scale
*
*         Loop for each grey level
*
      range=rmax-rmin
      gm=min(rmin,rmax)
      gmax=gm
      do k=1,nlevk
         yoffk=real(k)*spkey
*
*         Determine range of values it represents
*
         gmin=gmax
         gmax=real(k)*abs(range)/real(nlevk) + gm
         grey=0.5*(gmin+gmax)

         do j=1,npix
            do i=1,npix
               keygr(i,j)=nint( (grey-rmin)/range * real(nlevu) )
     :                    + 1
            end do
         end do
*
*         Convert real E format to character
*
         write(annote,'(e11.4)') grey
*
*         Label annotations
*
         call sgs_btext(xoffa,yoffk-0.45*spkey)
         call sgs_atext(annote)
         call sgs_otext
*
*         Draw a grey level
*
         call gca(xoffk+2.*del,yoffk-del-real(npix),
     :        xoffk+real(npix)+2.*del,yoffk-del,npix,npix,1,1,npix
     :        ,npix,keygr)
      end do

      end
