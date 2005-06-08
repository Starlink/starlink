      subroutine hexprarr(data,total,xadj,xa,ya,size,xr,yr,limit,ifsoft,
     :                  full,datmin,datmax,line)
*+
* Name:
*    HEXPRARR

* Invocation:
*    CALL HEXPRARR(DATA,TOTAL,XADJ,XA,YA,SIZE,XR,YR,LIMIT,IFSOFT,
*                       FULL,DATMIN,DATMAX,LINE)

* Purpose:
*  To display line profiles on an hexagonal array, from a sorted cube

* Description:
*  To display line profiles on an hexagonal array, from a sorted cube

* Arguments:
*   DATA(WAVDIM,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        The data
*   WAVDIM,SPDIM1,SPDIM2 = INTEGER (Given)
*        Dimensions of above
*   TOTAL(SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Total values array
*   XADJ(SPDIM2) = REAL ARRAY (Given)
*        X position adjustment
*   XA(SPDIM1) = REAL ARRAY (Given)
*        X (spatial) array
*   YA(SPDIM2) = REAL ARRAY (Given)
*        Y (spatial) array
*   XR(2) = REAL ARRAY (Given)
*        X range for display
*   YR(2) = REAL ARRAY (Given)
*        Y range for display
*   LIMIT = LOGICAL (Given)
*        If to limit range of display
*   FULL = LOGICAL (Given)
*        If full size plot required
*   DATMIN = REAL (Given)
*        Minimum scaling for profiles
*   DATMAX = REAL (Given)
*        Maximum scaling for profiles
*   LINE = INTEGER (Given)
*        Current line
*   IFSOFT = LOGICAL (Given and returned)
*        If softcopy plot required
*   SIZE = REAL (Returned)
*        Size of hexagons
*
* Functions/subroutines referenced:
*    ADDFIT      : Draw fit over data
*    CNF_PVAL    : Full pointer to dynamically allocated memory
*    DRAWPOLY    : Draw line using SGS
*    GR_SELCT = LOGICAL (Returned)
*      Open device
*    GEN_RANGEF  : Get range of array
*    GEN_ELEMF   : Get element of array

* Author:
*   TNW: T.N.Wilkins Manchester until 1/89, then IoA Cambridge until
*        9/92, then Durham

* History:
*   TNW: 11-12/7/88 Original version
*   TNW: 5/6/92 Made to support multiple lines
*   TNW: 17/6/93 Bug fix-off_d_xptr introduced
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer i,j,status,line
      include 'arc_dims'
      real total(spdim1,spdim2)
      real data(wavdim,spdim1,spdim2),xadj(spdim2),xa(spdim1)
      real xmin,xmax,ymin,ymax,xdisp,ydisp,size
      real tsize,xr(2),yr(2),ya(spdim2),x,y,datmin,datmax
      logical limit,ifsoft,full,isnew
      real xws,xwe,yws,ywe,xvs,xve,yvs,yve,xps,xpe,yps,ype,xvend
      integer wavst,nwav,rx2chn,off_d_xptr
      real gen_elemf

      status = SAI__OK
      call gr_selct(ifsoft,status)

      call gr_spen(1)

      xdisp = (xa(spdim1)-xa(1))/real(spdim1-1)
      ydisp = (ya(spdim2)-ya(1))/real(spdim2-1)

* If to limit range of display then set xmax, xmin to these

      if(limit) then
        xmin = xr(1)
        xmax = xr(2)
        ymin = yr(1)
        ymax = yr(2)
      else

*   Get range of X

        call gen_rangef(xadj,1,spdim2,xmax,xmin)
        xmin = xmin + xa(1)
        xmax = xmax + xa(spdim1)
        ymax = ya(spdim2)
        ymin = ya(1)
      end if

* Allow for half pixels at ends

      xmax = xmax + xdisp*0.5
      xmin = xmin - xdisp*0.5
      ymax = ymax + ydisp*0.57735
      ymin = ymin - ydisp*0.57735

      size = xdisp*0.57735

      call pgsfs(2)
      tsize = size*0.63
      if(full) then
        xvend = 0.99
      else
        xvend = 0.85
      end if
      call pgvport(0.01,xvend,0.01,0.99)
      call pgwnad(xmin,xmax,ymin,ymax)
      wavst = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,
     :               gen_elemf(%VAL(CNF_PVAL(d_tlptr)),line))
      call dyn_incad(d_xptr,'float',wavst-1,off_d_xptr,isnew,status)
      nwav = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,
     :              gen_elemf(%VAL(CNF_PVAL(d_trptr)),line)) - wavst + 1
      do j=1,spdim2
        y = ya(j)
        do i=1,spdim1
          if(total(i,j).ne.VAL__BADR) then
            x = xa(i)+xadj(j)

* If to limit range then check point is in range

            if((.not.limit).or.((x.ge.xr(1)).and.(x.le.xr(2)).and.
     :        (y.ge.yr(1)).and.(y.le.yr(2)))) then
              call pgvport(0.0,1.0,0.0,1.0)
              call pgwnad(xmin,xmax,ymin,ymax)
              call pgqwin(xws,xwe,yws,ywe)
              call pgqvp(0,xvs,xve,yvs,yve)
              xps = ((x-tsize-xws)/(xwe-xws))*(xve-xvs) + xvs
              xpe = ((x+tsize-xws)/(xwe-xws))*(xve-xvs) + xvs
              yps = ((y-tsize-yws)/(ywe-yws))*(yve-yvs) + yvs
              ype = ((y+tsize-yws)/(ywe-yws))*(yve-yvs) + yvs
              call gr_spen(1)
              call hexmark(x,y,size)
              call pgvport(xps,xpe,yps,ype)

*       Draw profile inside box

              call drawpoly(%VAL(CNF_PVAL(off_d_xptr)),data(wavst,i,j),
     :             nwav,datmin,datmax)
              call addfit(i,j,%VAL(CNF_PVAL(d_rptr)),line)
            end if
          end if
        end do
      end do
      call pgvport(0.0,1.0,0.0,1.0)
      call pgwnad(xmin,xmax,ymin,ymax)
      call gr_spen(1)

* Free CNF resource.
      if ( isnew ) call cnf_unregp(off_d_xptr)
      end
