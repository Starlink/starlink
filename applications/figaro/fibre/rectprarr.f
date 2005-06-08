      subroutine rectprarr(data,xa,ya,total,xr,yr,limit,ifsoft,full,
     :                  datmin,datmax,line)
*+
* Name:
*    RECTPRARR

* Invocation:
*    CALL RECTPRARR(DATA,XA,YA,TOTAL,XR,YR,LIMIT,IFSOFT,FULL,
*                       DATMIN,DATMAX,LINE)

* Purpose:
*  To display line profiles on an rectangular array, from a sorted cube

* Description:
*  To display line profiles on an rectangular array, from a sorted cube

* Arguments:
*   WAVDIM = INTEGER (Given)
*        Wavelength dimension of array
*   SPDIM1 = INTEGER (Given)
*        X dimension of array
*   SPDIM2 = INTEGER (Given)
*        Y dimension of array
*   DATA(WAVDIM,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Intensity array
*   XA(SPDIM1) = REAL ARRAY (Given)
*        X array
*   YA(SPDIM2) = REAL ARRAY (Given)
*        Y array
*   TOTAL(SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Total values array
*   XR(2) = REAL ARRAY (Given)
*        X display limits (if limit=true)
*   YR(2) = REAL ARRAY (Given)
*        Y display limits (if limit=true)
*   LIMIT = LOGICAL (Given)
*        If to limit display
*   FULL = LOGICAL (Given)
*        If full size plot required
*   DATMIN = REAL (Given)
*        Minimum scaling for profiles
*   DATMAX = REAL (Given)
*        Maximum scaling for profiles
*   LINE = INTEGER (Given)
*        Current line
*   IFSOFT = LOGICAL (Given and returned)
*        If image etc. displayed
*
*
* Subroutines/functions referenced:
*   ADDFIT      : Draw fit over data
*   CNF_PVAL    : Full pointer to dynamically allocated memory
*   DRAWPOLY    : Draw line using PGPLOT
*   GR_SELCT    : Open device
*   RX2CHN = INTEGER (Given and returned)
*        Convert array value to array index
*
*   GEN_RANGEF        : Get range of array

* Author:
*   TNW: T.N.Wilkins Manchester until 1/89, then IoA Cambridge until
*        9/92, then Durham

* History:
*   TNW: 1988 Original version
*   TNW: 5/6/92 Made to support multiple lines
*   TNW: 17/6/93 Bug fix-off_d_xptr introduced
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'

      integer i,j,line
      real data(wavdim,spdim1,spdim2),xa(spdim1),ya(spdim2)
      real xdisp,ydisp,xmin,xmax,ymin,ymax,xr(2),yr(2)
      real total(spdim1,spdim2),datmin,datmax,xps,xpe,yps,ype
      real xws,xwe,yws,ywe,xvs,xve,yvs,yve,xvend
      logical limit,ifsoft,full,isnew
      integer xst,yst,xen,yen,rx2chn,status,off_d_xptr
      integer wavst,nwav
      real gen_elemf

      status = SAI__OK
      call gr_selct(ifsoft,status)

      call gr_spen(1)

      xdisp = (xa(spdim1)-xa(1))/real(spdim1-1)
      ydisp = (ya(spdim2)-ya(1))/real(spdim2-1)
      if(limit) then
        xmin = xr(1)
        xmax = xr(2)
        ymin = yr(1)
        ymax = yr(2)
        xst = rx2chn(xa,spdim1,xmin)
        xen = rx2chn(xa,spdim1,xmax)
        yst = rx2chn(ya,spdim2,ymin)
        yen = rx2chn(ya,spdim2,ymax)
      else
        xmin = xa(1)
        xmax = xa(spdim1)
        ymin = ya(1)
        ymax = ya(spdim2)
        xst = 1
        xen = spdim1
        yst = 1
        yen = spdim2
      end if

* Allow for half pixels at ends

      xmax = xmax + xdisp*0.5
      xmin = xmin - xdisp*0.5
      ymax = ymax + ydisp*0.5
      ymin = ymin - ydisp*0.5

* Set up X range

      if(full) then
        xvend = 0.99
      else
        xvend = 0.85
      end if
      call pgvport(0.01,xvend,0.01,0.99)
      call pgwnad(real(xst)-0.5,real(xen)+0.5,real(yst)-0.5,
     :     real(yen)+0.5)
      call pgsfs(2)
      call gr_spen(1)

      call pgqwin(xws,xwe,yws,ywe)
      call pgqvp(0,xvs,xve,yvs,yve)
      wavst = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,
     :               gen_elemf(%VAL(CNF_PVAL(d_tlptr)),line))
      call dyn_incad(d_xptr,'float',wavst-1,off_d_xptr,isnew,status)
      nwav = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,
     :              gen_elemf(%VAL(CNF_PVAL(d_trptr)),line)) - wavst + 1
      do j=yst,yen
        yps = ((real(j)-0.5-yws)/(ywe-yws))*(yve-yvs) + yvs
        ype = ((real(j)+0.5-yws)/(ywe-yws))*(yve-yvs) + yvs
        do i=xst,xen
          if(total(i,j).ne.VAL__BADR) then
            xps = ((real(i)-0.5-xws)/(xwe-xws))*(xve-xvs) + xvs
            xpe = ((real(i)+0.5-xws)/(xwe-xws))*(xve-xvs) + xvs
            call gr_spen(1)
            call pgvport(xvs,xve,yvs,yve)
            call pgwindow(xws,xwe,yws,ywe)
            call rectmark(real(i),real(j),0.5)
            call pgvport(xps,xpe,yps,ype)

*       Draw profile inside box

            call drawpoly(%VAL(CNF_PVAL(off_d_xptr)),data(wavst,i,j),
     :                    nwav,datmin,datmax)
            call addfit(i,j,%VAL(CNF_PVAL(d_rptr)),line)
          end if
        end do
      end do
      call pgvport(xvs,xve,yvs,yve)
      call pgwindow(xws,xwe,yws,ywe)
      call gr_spen(1)

* Free CNF resource.
      if ( isnew ) call cnf_unregp(off_d_xptr)
      end
