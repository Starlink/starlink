      subroutine fillim(z,x,nx,ny,work)
*+
* Name:
*    FILLIM

* Invocation:
*    CALL FILLIM(Z,X,NX,NY,WORK)

* Purpose:
*    To put Gaussians into an array, prompting for their parameters.
*  Also to fill in the X array, linearly increasing.


* Description:
*  To put Gaussians into an array, prompting for their parameters.
*  Also to fill in the X array, linearly increasing.
*
* Arguments:-
*    NX,NY = INTEGER (Given)
*         Dimensions of arrays as below
*    Z(NX,NY) = INTEGER ARRAY (Returned)
*         The Z data array
*    X(NX) = INTEGER ARRAY (Returned)
*         The X data array
*    WORK(NX*10) = INTEGER ARRAY (Workspace)

* History:
* T.N.Wilkins Manchester May 1987
*     "       Cambridge, 15/3/91 Made to use PRM_PAR
*-
      implicit none
      integer nx,ny,i,j,ncomp,status
      real z(nx,ny),x(nx),gaussian,value,disp,xstart,xend

* w,h,c

      real xc(4)
      real work(nx*10),add,xval
      real  efold
      include 'PRM_PAR'
      parameter (efold = 2.35482004e0)
      character*1 nums(5)
      data nums/'1','2','3','4','5'/

      call dsa_put_fits_c('data','note',
     :  'File produced by program CRIGAUSS',' ',status)

* Get parameters of gaussian

      call par_rdval('xstart',1.0,val__maxr,1.0,' ',xstart)
      call par_rdval('xend',xstart,val__maxr,1.0,' ',xend)
      disp = (xend-xstart)/(nx-1)
      call par_rdval('ncomp',1.0,5.0,1.0,' ',value)
      ncomp = nint(value)
      call par_rdval('base',val__minr,val__maxr,0.0,' ',value)


* Set Z array values to value of gaussian(s), first set work array at
* 10 times resolution, then average these to give value in each pixel of
* first row of Z array. Then copy these to other rows. We do this as we
* read the values from the user

* Set work array to base value

      call gen_cfill(1,nx*10,value,work)
      call dsa_put_fits_f('data','base',value,' ',status)
      do i = 1, ncomp
        call par_rdval('width'//nums(i),val__smlr,val__maxr,1.0,' ',
     :            value)
        call dsa_put_fits_f('data','width'//nums(i),value,' ',status)
        xc(2) = value/efold
        call par_rdval('height'//nums(i),val__minr,val__maxr,1.0,' ',
     :            value)
        call dsa_put_fits_f('data','height'//nums(i),value,' ',status)
        xc(3) = value
        call par_rdval('centre'//nums(i),val__minr,val__maxr,0.0,' ',
     :            value)
        call dsa_put_fits_f('data','centre'//nums(i),value,' ',status)
        xc(4) = value
        xc(1) = 0.0
        do j = 1,nx*10
          xval = xstart+disp*(real(j)*0.1-0.55)
          work(j) = work(j) + gaussian(xval,xc)
        end do
      end do

* Fill X array

      do i = 1, nx
        x(i) = xstart + disp*real(i-1)
      end do

* Average each 10 pixels in work array

      do i = 1, nx
        add = 0.0
        do j = 1,10
          add = add + work(((i-1)*10+j))
        end do
        z(i,1) = add*0.1
      end do

* Copy to rest of file

      do j = 2,ny
        do i = 1,nx
          z(i,j) = z(i,1)
        end do
      end do
      end
