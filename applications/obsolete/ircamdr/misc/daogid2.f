c DAOGID2 - program to get the DAOPHOT .coo ID number from a star selected
c          by cursor on the image display.
c
c

      character dummy*80,name*60,name2*60
      real x(1000),y(1000)
      real x2(1000),y2(1000)
      integer lun,id(1000),id2(1000)

      lun = 142
      open(unit=lun,file='daojunk2.dat',status='old',err=999)
      read(lun,'(a60)',end=998)name
      read(lun,'(a60)',end=998)name2
      read(lun,*,end=998)xstar
      read(lun,*,end=998)ystar
      close(lun)

c open the file and read in the x,y coordinates

      open(unit=lun,file=name,status='old',err=997)
      read(lun,'(a80)',end=996)dummy
      read(lun,'(a80)',end=996)dummy
      read(lun,'(a80)',end=996)dummy

      do i=1,1000
        read(lun,*,end=100)id(i),x(i),y(i)
        nrec=i
      end do

  100 close(lun)

      open(unit=lun,file=name2,status='old',err=997)
      read(lun,'(a80)',end=996)dummy
      read(lun,'(a80)',end=996)dummy
      read(lun,'(a80)',end=996)dummy

      do i=1,1000
        read(lun,*,end=101)id2(i),x2(i),y2(i)
        nrec2=i
      end do

  101 close(lun)

c search for the nearest star

      dclose = 1000.0
      do i=1,nrec
        dist = sqrt ( (x(i)-xstar)**2 + (y(i)-ystar)**2 )
        if(dist.lt.dclose)then
          dclose=dist
          number=id(i)
        end if
      end do

      dclose2 = 1000.0
      do i=1,nrec2
        dist = sqrt ( (x2(i)-xstar)**2 + (y2(i)-ystar)**2 )
        if(dist.lt.dclose2)then
          dclose2=dist
          number2=id2(i)
        end if
      end do

      type *, ' '
      type *,'File = ',name
      type *,'Closest star is ID = ',number
      type *,'Positional error in pixels = ',dclose
      type *, ' '
      type *,'File = ',name2
      type *,'Closest star is ID = ',number2
      type *,'Positional error in pixels = ',dclose2
      type *, ' '

      goto 888

  999 type *, 'Error opening DAOJUNK2.DAT'
      goto 888

  998 type *, 'Error reading from DAOJUNK2.DAT EOF encountered'
      close( lun)
      goto 888

  997 type *, 'Error opening ', name
      goto 888

  996 type *, 'Error reading from ',name
      close( lun)
      goto 888

  888 continue

      end
