      subroutine wr2new(dim1,dim2,dim3,nx,ny,z1,z2,lunit,miss,minleft,
     :    maxright,waves)
*+
* Name:
*    WR2NEW

* Invocation:
*    CALL WR2NEW(DIM1,DIM2,DIM3,NX,NY,Z1,Z2,LUNIT,MISS,MINLEFT,
*         MAXRIGHT,WAVES)

* Description:
*   To copy data from array Z1 to Z2, where the position of each
* Purpose:
*   To copy data from array Z1 to Z2, where the position of each
* part of Z1 in Z2 is determined by the number in the file on
* unit LUNIT.
*
* Arguments:
*     DIM1 = INTEGER (Given)
*        1st dimension of Z2
*     DIM2 = INTEGER (Given)
*        2nd dimension of Z2
*     DIM3 = INTEGER (Given)
*        3rd dimension of Z2
*     NX,NY = INTEGER (Given)
*        Dimensions of Z1
*     Z1(NX,NY) = REAL ARRAY (Given)
*        Input image
*     LUNIT = INTEGER (Given)
*        Unit number of file
*     MINLEFT = REAL (Given)
*        Start wavelength to use
*     MAXRIGHT = REAL (Given)
*        End wavelength to use
*     WAVES(NX) = REAL ARRAY (Given)
*        Wavelength array
*     Z2(DIM1,DIM2,DIM3) = REAL ARRAY (Given and returned)
*        Output cube
*     MISS(DIM2,DIM3) = INTEGER*2 ARRAY (Given and returned)
*        Missing values array
*
* Subroutines/functions referenced:
*     RX2CHN = INTEGER (Given and returned)
*        Convert 1-d array value to pixel
*                                      number
*     PAR_WRUSER = INTEGER (Given and returned)
*        Write character string to user

* History:
*    T.N.Wilkins Manchester 5/4/88
*    TNW 11/8/88 Bug fix
*-
      implicit none
      include 'SAE_PAR'
      integer dim1,dim2,dim3,nx,ny,lunit,x,y,i,j,status,rx2chn,istart
      integer iend,ii
      real z1(nx,ny),z2(dim1,dim2,dim3)
      real maxright,minleft,waves(nx)
      integer*2 miss(dim2,dim3)
      character*30 chars
      logical pass,go

* Copy data over

      istart = rx2chn(waves,nx,minleft)
      iend = rx2chn(waves,nx,maxright)
      do j=1,ny
        pass = .false.
        go = .true.
        do while(go)
          read(lunit,'(a)',iostat=status)chars
          go = .false.
          if(status.eq.SAI__OK) then
            if(chars(1:1).eq.'!') then !allow for comments in file
              call par_wruser(chars(2:30),status)
              go = .true.
            else
              read(chars,'(2i5)',iostat=status) x,y
              pass = (x.eq.0).and.(y.eq.0)
            end if
          end if
        end do
        if(.not.pass) then
          if(status.eq.SAI__OK) then
            ii = 0
            do i=istart,iend
              ii = ii+1
              z2(ii,x,y) = z1(i,j)
            end do
            if(miss(x,y).eq.1) then
              call par_wruser(
     :          'Warning: Data written twice to one point',status)
            end if
            miss(x,y) = 1
          else
            call par_wruser('Error reading from file',status)
            go to 500
          end if
        end if
      end do
 500  continue
      end
