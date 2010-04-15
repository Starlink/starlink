c DAOSYMB   -   Fortran program to take a .COO output file and
c               write an ADAM procedure to plot symbols
c               at stars found by DAOPHOT's FIND.
c

      character dummy*80,name*60
      real x(1000),y(1000)
      integer n(1000),lun, stype,size,csize

      type *, ' '
      type *,'Enter the name of daophot find option output file '
      read(5,'(a)')name
      type *, ' '
      type *,'Plot a cross=1, circle=2, id number=3, circle/number=4 ?'
      read(5,*)stype
      type *, ' '
      if ( stype .eq. 1) then
        type *,'Input size of cross in arcseconds'
        read(5,*)size
      else if( stype .eq. 2) then
        type *,'Input diameter of circle in arcseconds'
        read(5,*)size
      else if( stype .eq. 3) then
        type *,'Input size of comment '
        read(5,*)csize
      else
        type *,'Input diameter of circle in arcseconds'
        read(5,*)size
        type *,'Input size of comment '
        read(5,*)csize
      end if
      type *, ' '

c open the file and read in the x,y coordinates

      lun = 142
      open(unit=lun,file=name,status='old',err=999)
      read(lun,'(a80)',end=998)dummy
      read(lun,'(a80)',end=998)dummy
      read(lun,'(a80)',end=998)dummy

      do i=1,1000
        read(lun,*,end=100)n(i),x(i),y(i)
        nrec=i
      end do

100   close(lun)

c open output file and write the ADAM procedure

      open(unit=lun,file='daosymb.icl',status='unknown')
      write(lun,*)'proc daosymb'

      do i=1,nrec
         if ( stype .eq. 1) then
           write(lun,500)int(x(i)+0.5),int(y(i)+0.5),size
         else if( stype .eq. 2) then
           write(lun,501)int(x(i)+0.5),int(y(i)+0.5),size
         else if( stype .eq. 3) then
           write(lun,503)int(x(i)+0.5),int(y(i)+0.5)
           write(lun,504)
           write(lun,505)
           write(lun,502)n(i),csize
         else
           write(lun,501)int(x(i)+0.5),int(y(i)+0.5),size
           write(lun,503)int(x(i)+0.5),int(y(i)+0.5)
           write(lun,504)
           write(lun,505)
           write(lun,502)n(i),csize
         end if
      end do
  500 format(' obeyw plt2d cross ',3(i4))
  501 format(' obeyw plt2d circle ',3(i4))
  502 format(' obeyw plt2d comment ',i4,' (value1) (value2) ',i4)
  503 format(' obeyw plt2d conval ',2(i4))
  504 format(' get plt2d x_cur_real (value1)')
  505 format(' get plt2d y_cur_real (value2)')

      write(lun,*)'end proc'
      close(lun)

      goto 888

  999 type *, 'Error opening ', name
      goto 888

  998 type *, 'Error reading from ', name
      close( lun)
      goto 888

  888 continue

      end
