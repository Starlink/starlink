CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMAGE.INC
C
C  alan penny                 ral                      1990-08-10

      integer ipim		! Image pointer
      integer nx		! Image X size
      integer ny		! Image Y size
      real    bs		! Image value scale
      real    bz		! Image value zero
      integer inval		! Image bad pixel value for 'INT' and 'SHORT' images
      real    rinval		! Image bad pixel value for 'REAL' images

      character*70  imtitle	! Image title
      character*6   imtype	! Image type ('REAL':'INT':'SHORT')

      common /imagecoma / ipim, nx, ny, bs, bz, inval, rinval

      common /imagecomc / imtitle,  imtype

