      subroutine fibslice(data,nx,ny,nw,xpts,ypts,maxnpt,xa,ya,xadj
     :    ,ifxcut,hex,work,size)
*+
* Name:
*    FIBSLICE

* Invocation:
*    CALL FIBSLICE(DATA,NX,NY,NW,XPTS,YPTS,MAXNPT,XA,YA,XADJ
*         ,IFXCUT,HEX,WORK,SIZE)

* Description:
*   To take a slice through a 3-d data array, using the nearest points
* Purpose:
*   To take a slice through a 3-d data array, using the nearest points
* to the line selected.
*
* Arguments:
*     DATA(NW,NX,NY) = REAL ARRAY (Given)
*        Data array
*     NX = INTEGER (Given)
*        X dimension of data
*     NY = INTEGER (Given)
*        Y dimension of data
*     NW = INTEGER (Given)
*        wavelength dimension of data
*     XPTS(MAXNPT) = INTEGER ARRAY (Given)
*        X point work array
*     YPTS(MAXNPT) = INTEGER ARRAY (Given)
*        Y point work array
*     MAXNPT = INTEGER (Given)
*        Dimension of work arrays
*     XA(NX) = REAL ARRAY (Given)
*        X array
*     YA(NY) = REAL ARRAY (Given)
*        Y array
*     XADJ(NY) = REAL ARRAY (Given)
*        X adjustment array
*     IFXCUT = LOGICAL (Given)
*        If a cut in the X direction is required
*                          -otherwise Y
*     HEX = LOGICAL (Given)
*        If hexagonal array
*     WORK(NW,MAXNPT) = REAL ARRAY (Given)
*        Work array
*
*   Subroutines/functions referenced:
*     GET_GREY    : Select grey-scale device
*     HEXMARK     : Draw hexagon
*     RX2CHN      : Convert value to pixel number in array
*     SETGRY      : Set colour table to greyscale
*     ZERO_INT    : Zero integer array
*
*     PGPGRAY     : Draw greyscale image
*     PGSCR       : Set colour representation
*     PGCURSE     : cursor routine
*     GR_SPEN     : Set pen
*     PAR_QNUM    : Get number from user
*     PAR_QUEST   : Get yes/no response from user

* Author:
*      T.N.Wilkins Manchester 24/6/88
*-
      implicit none
      include 'SAE_PAR'
      integer nx,ny,nw,maxnpt,xpts(maxnpt),ypts(maxnpt)
      real data(nw,nx,ny),xa(nx),ya(ny),xadj(ny),x,y,white,black
      real size,value
      real work(nw,maxnpt),tr(6)
      integer i,j,ix,iy,npt,rx2chn,pgcurse,status
      logical ifxcut,hex,loop,qstat,par_qnum,par_quest
      character*1 ch
      include 'PRM_PAR'

*  Find cursor position

      call sync
      if(pgcurse(x,y,ch).ne.1) return
      npt = 0
      call pgscr(3,1.0,0.0,0.0)

*  X direction cut

      call pgsfs(2)
      call gr_spen(3)
      if(ifxcut) then
        if(hex) then
          iy = rx2chn(ya,ny,y)
        else
          iy = nint(y)
        end if
        do i = 1, nx
          npt = npt + 1
          xpts(npt) = i
          ypts(npt) = iy
          if(hex) then
            call hexmark((xa(i)+xadj(iy)),ya(iy),size)
          else
            call rectmark(real(i),real(iy),0.5)
          end if
        end do
      else

*  Y direction cut

        do i = 1, ny
          if(hex) then
            ix = rx2chn(xa,ny,x-xadj(i))
            call hexmark((xa(ix)+xadj(i)),ya(i),size)
          else
            ix = nint(x)
            call rectmark(real(ix),real(i),0.5)
          end if
          npt = npt + 1
          xpts(npt) = ix
          ypts(npt) = i
        end do
      end if

* If any data found then produce plot

      if(npt.ge.1) then
        loop = .true.

*  Open/select greyscale device

        status = SAI__OK
        call get_grey('device',status)
        if(status.ne.SAI__OK) return

*  Inquire number of greyscale levels available

        do while(loop)
           qstat = par_qnum('Enter value for black',VAL__MINR,VAL__MAXR
     :          ,0.0,.true.,' ',black)
          qstat = par_qnum('Enter value for white',VAL__MINR,VAL__MAXR
     :         ,100.0,.true.,' ',white)
          call zero_real(work,npt*nw)
          do j = 1,npt
            do i = 1,nw
              value = data(i,xpts(j),ypts(j))
              if(value.ne.VAL__BADR) then
                work(i,j) = value
              end if
            end do
          end do
          call pgvstand
          call pgwindow(0.5,real(nw)+0.5,0.5,real(npt)+0.5)

*      Set colour table to grey, but with pen 0 set to dark blue

          tr(1) = 0.0
          tr(2) = 1.0
          tr(3) = 0.0
          tr(4) = 0.0
          tr(5) = 0.0
          tr(6) = 1.0
          call pggray(work,nw,npt,1,nw,1,npt,white,black,tr)
          call pgscr(0,0.0,0.0,0.2)
          call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
          loop = .not.par_quest('Ok?',.true.)
        end do
      end if
      end
