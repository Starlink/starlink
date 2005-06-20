      subroutine dja_arcurve(results,resvar,lincnt,xc,wavelength,kp1
     :     ,xsect,rc,wavep,delwave,height,width,height_er,width_er)
*+
* Name:
*    DJA_ARCURVE

* Invocation:
*    CALL DJA_ARCURVE(RESULTS,RESVAR,LINCNT,XC,WAVELENGTH,KP1,
*          XSECT,RC,WAVEP,DELWAVE,HEIGHT,WIDTH,HEIGHT_ER,WIDTH_ER)

* Purpose:
*   Plot a dispersion curve and deviation from linearity for the current
*   wavelength fit

* Description:
*   The following plots are available:
*      -  Dispersion v Channel
*      -  Residuals v line number
*      -  Residuals v line strength
*      -  Residuals v line width


*  Arguments:
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube"
*    RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Variance on above
*    LINCNT = INTEGER (Given)
*        Number of lines
*    XC(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*        Fit coefficients
*    WAVELENGTH(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*        Wavelengths of lines
*    KP1 = INTEGER (Given)
*       Order+1
*    XSECT = INTEGER (Given)
*       Currect cross-section
*    RC(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*       Residuals
*    WAVEP(LINCNT) = REAL ARRAY (Workspace)
*        Stores predicted wavelength
*    DELWAVE(LINCNT) = REAL ARRAY (Workspace)
*    HEIGHT(LINCNT) = REAL ARRAY (Workspace)
*    WIDTH(LINCNT) = REAL ARRAY (Workspace)
*    HEIGHT_ER(LINCNT) = REAL ARRAY (Workspace)
*    WIDTH_ER(LINCNT) = REAL ARRAY (Workspace)
* History:
*  Altered to get rid of use of "DIAGRAM" zones, T.N.Wilkins Manchester
*           19/11/87
*  QMENU used, TNW 1/11/89
*  Dispersion calculated using differentiation, TNW/CAVAD 20/9/90
*  Use cursor, TNW 12/10/93
*  Removed bits not needed with arcplot, TNW 10/2/94
*-
* --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
*
* integer
*
      integer lincnt
      integer xsect
      integer i
      integer idiag
      integer kp1
*
* double precision
*
      double precision xc(kp1)
      double precision wavelength(lincnt)
      double precision rc(lincnt)
      double precision difxc(10)

* number of Y subzones for SOFT

      integer NYSOFT
      parameter (NYSOFT = 2)

      integer cnv_fmtcnv,nbad,term_order
      real zones(4,NYSOFT)
*
* common
*
      integer status
      include 'arc_dims'
*
* parameter
*
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
*
      real wavep(lincnt),delwave(lincnt)
      real height(lincnt),width(lincnt),height_er(lincnt)
      real width_er(lincnt),x,y
      character key,chr_upper
      integer pgcurse
      logical loop,iloop
      integer get_parnum,h_pos,w_pos

* Divide softcopy base zone into 2 zones

      do i = 1, NYSOFT
         zones(2,i) = 0.95
         zones(1,i) = 0.1
         zones(4,i) = real(i)/real(NYSOFT) - 0.05
         zones(3,i) = zones(4,i) - 1.0/real(NYSOFT) + 0.15
      end do
*
      status = cnv_fmtcnv('double','float',wavelength,wavep,lincnt,nbad)
      status = cnv_fmtcnv('double','float',rc,delwave,lincnt,nbad)
      h_pos = get_parnum('Height_1')
      w_pos = get_parnum('Width_1')
      do i=1,lincnt
         height_er(i) = sqrt(abs(resvar(h_pos,i,xsect)))
         height(i) = results(h_pos,i,xsect)
         width(i) = results(w_pos,i,xsect)
         width_er(i) = sqrt(abs(resvar(w_pos,i,xsect)))
      end do
*
* setup angstroms per channel
*
* Differentiate the coefficients! Note that the constant term is last

      call copd2d(kp1-1,xc,difxc)
      do i = 1, kp1-1
         term_order = kp1 - i
         difxc(i) = difxc(i)*dble(term_order)
      end do

* graphics

      loop = .true.
      do while(loop)
         call gr_soft(status)
         if(status.ne.SAI__OK) goto 500
*
* Start by plotting the graphs
*
         call pgvport(zones(1,1),zones(2,1),zones(3,1),zones(4,1))
         call resid_v_ht(height,lincnt,delwave,height_er)
         call pgvport(zones(1,2),zones(2,2),zones(3,2),zones(4,2))
         call resid_v_width(width,delwave,width_er,lincnt)
         call pgmtext('T',0.5,1.0,1.0,'Hit ? for help')
         iloop = .true.
         do while(iloop)
            status = pgcurse(x,y,key) - 1
            key = chr_upper(key)
*
* Blowup - ask which plots
*
            if (key.eq.'B') then
               iloop = .false.
               call gr_soft(status)
               if(status.ne.SAI__OK) goto 500
               if(y.gt.0.5) then
                  idiag = 2
               else
                  idiag = 1
               end if

               call pgvstand
               if(idiag.eq.1) then
                  call resid_v_ht(height,lincnt,height_er,delwave)
               else if(idiag.eq.2) then
                  call resid_v_width(width,delwave,width_er,lincnt)
               end if
               call pgmtext('T',0.5,1.0,1.0,'Hit any key to proceed')
               status = pgcurse(x,y,key) - 1

*   Hardcopy plots

            else if(key.eq.'H') then
               iloop = .false.
               call gr_hard(status)
               if(status.ne.SAI__OK) goto 500
               call pgvstand
               call resid_v_ht(height,lincnt,height_er,delwave)
               call pgpage
               call pgvstand
               call resid_v_width(width,delwave,width_er,lincnt)
               call clgrap
            else if(key.eq.'Q') then
               loop = .false.
               iloop = .false.
            else if(key.eq.'?') then
               call pgenv(0.0,1.0,0.0,1.0,0,-1)
               call pgtext(0.1,0.8,'B - Blowup of 1 plot')
               call pgtext(0.1,0.7,'H - Hardcopies of all plots')
               call pgtext(0.1,0.6,'Q - Quit')
               call pgtext(0.1,0.5,'? - display this help')
               call pgtext(0.1,0.1,'Hit any key to return to plots')
               status = pgcurse(x,y,key) - 1
               iloop = .false.
            else
               call par_wruser('Invalid key hit',status)
            end if
         end do
      end do
      return
 500  continue
      call par_wruser('Error with graphics',status)
      end
