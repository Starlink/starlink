      subroutine delfit(results,mxpars,nx,ny,ml,disp,xa,ya,xadj,hex)
*+
* Name:
*    DELFIT

* Invocation:
*    CALL DELFIT(RESULTS,MXPARS,NX,NY,ML,DISP,XA,YA,XADJ,HEX)
* Purpose:
*   To delete fits from the results block
*
* Description:
*   To delete fits from the results block
*
* Arguments:
*     MXPARS,NX,NY,ML = INTEGER (Given)
*        Dimension of block
*     QUICK = LOGICAL (Given)
*        If image etc. displayed
*     RESULTS(MXPARS,ML,NX,NY) = REAL ARRAY (Given and returned)
*        Results block
*
*   Subroutines/functions referenced:
*     RX2CHN      : Convert value to pixel number of array
*     PAR_WRUSER  : Write string to user
*     PAR_QNUM    : Get number from user
*     GEN_CFILL   : Fill array with a given value
*     PGCURSE     : Cursor
* Authors:
*   T.N.Wilkins Manchester 15/7/88
*-
      implicit none
      integer mxpars,nx,ny,ml,ix,iy,k,l,status,rx2chn,pgcurse
      real results(mxpars,ml,nx,ny),value
      include 'PRM_PAR'
      real xa(nx),ya(ny),xadj(ny),x,y
      logical disp,cursor,par_quest,hex,qstat,par_qnum
      character*37 chars,ch*1

      if(disp) then
        cursor = par_quest('Use cursor to locate points',.true.)
      else
        cursor = .false.
      end if

      if(cursor) then

*       Get cursor position

        if(pgcurse(x,y,ch).ne.1) goto 500
        if(hex) then
          iy = rx2chn(ya,ny,y)
          ix = rx2chn(xa,nx,x-xadj(iy))
        else
          ix = nint(x)
          iy = nint(y)
        end if

*        Check in range

        if((ix.gt.nx).or.(iy.gt.ny).or.(ix.lt.1).or.(iy.lt.1))
     :                 then
          call par_wruser('Point outside range',status)
          goto 500
        end if
      else
        qstat = par_qnum('Enter X position',1.0,real(nx),1.0,.true.,' ',
     :   value)
        ix = nint(value)
        qstat = par_qnum('Enter Y position',1.0,real(nx),1.0,.true.,' ',
     :   value)
        iy = nint(value)
      end if
      do l = 1, ml
        if(ml.gt.1) then
          write(chars,'(''Line : '',i2)')ml
          call par_wruser(chars,status)
        end if
        do k = 1, mxpars
          value = results(k,l,ix,iy)
          if(value.ne.VAL__BADR) then
            write(chars,'(''Results('',i3,'','',i3,'','',i3,'')=''
     :      ,e12.5)')k,ix,iy,value
            call par_wruser(chars,status)
          end if
        end do
      end do
      if(par_quest('Delete fit?',.true.)) then
        if(ml.eq.1) then
          l = 1
        else
          qstat = par_qnum('Line number?',1.0,real(ml),1.0,.true.,' ',
     :     value)
        l = nint(value)
        end if
        call gen_cfill(1,mxpars,VAL__BADR,results(1,l,ix,iy))
      end if
 500  continue
      end
