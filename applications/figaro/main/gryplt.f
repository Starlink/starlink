      subroutine gryplt(z,work,nchans,nxscts,irange,ist,nlevs
     :  ,white,black,colour,logsc,slev)
*+
* Name:
*    GRYPLT

* Invocation:
*    CALL GRYPLT(Z,WORK,NCHANS,NXSCTS,IRANGE,IST,NLEVS
*       ,WHITE,BLACK,COLOUR,LOGSC,SLEV)

* Purpose:
*  To plot a greyscale plot of the array Z in the current SGS zone.

* Description:
*  To plot a greyscale plot of the array Z in the current SGS zone.

* Arguments:
*  NCHANS = INTEGER (Given)
*        Number of channels
*  NXSCTS = INTEGER (Given)
*        Number of cross-sections
*  Z(NCHANS,NXSCTS) = REAL ARRAY (Given)
*        Data
*  IRANGE = INTEGER (Given)
*        Range of channels to use
*  IST = INTEGER (Given)
*        Starting channel to use
*  NLEVS = INTEGER (Given)
*        Number of greyscale levels
*  WORK(IRANGE,NXSCTS) = REAL ARRAY (Given)
*        Work space
*  WHITE = REAL (Given)
*        White level
*  BLACK = REAL (Given)
*        Black level
*  COLOUR = LOGICAL (Given)
*        If to use colour "look-up" table
*  LOGSC = LOGICAL (Given)
*        Use log scale
*  SLEV = INTEGER (Given)
*        Starting level to use
* History:
*  T.N.Wilkins Manchester  4/88
*  TNW 1/7/88 changed to use setgry
*  TNW 13/7/88 SLEV introduced.
*  TNW 22/9/88 Upper limits of range increased by 1 (world coords)
*-
      implicit none
      include 'PRM_PAR'
      integer nchans,nxscts,irange,ist
      real work(irange,nxscts)
      integer i,j,nlevu,nlevs,slev
      real z(nchans,nxscts),white,black,temp,lowlev
      real lwhite,lblack,tr(6),x1,x2,y1,y2,disp
      logical colour,logsc

      nlevu=max(slev,nlevs-slev+1)
      call pgqwin(x1,x2,y1,y2)
      disp = (x2 - x1)/(irange-1)
      if(logsc) then
        tr(1) = x1 - disp*(0.5)
      else
        tr(1) = x1 - disp*(real(ist)-0.5)
      end if
      tr(2) = disp
      tr(3) = 0.0
      tr(4) = y1 - 0.5
      tr(5) = 0.0
      tr(6) = 1.0

      if(logsc) then
        lwhite = log(white)
        lblack = log(black)
        lowlev = min(lwhite,lblack)

*   Draw the map

        do i=1,nxscts
          do j=1,irange
            temp = z(j-1+ist,i)
            if(temp.gt.VAL__SMLR) then
              work(j,i) = log(temp)
            else
              work(j,i) = lowlev
            end if
          end do
        end do
        call pggray(work,irange,nxscts,1,irange,1,nxscts,lwhite,lblack
     :           ,tr)

* linear mapping, PGGRAY needs no help here!

      else
        call pggray(z,nchans,nxscts,ist,ist+irange-1,1,nxscts,white,
     :           black,tr)
      end if

* PGGRAY probably changes the colours, so set them after plotting
* -ok for ikon, no effect on laser printers etc.

      if(colour) then

*   Colour look-up table required, use rainbow type

        call colrain(slev,nlevu)
      else

*    Greyscale

        call setgry(slev,nlevu+slev-1)
      end if
      end
