c DAOGID - program to get the DAOPHOT .coo ID number from a star selected
c          by cursor on the image display.
c
c

      character dummy*80,name*60
      real x(1000),y(1000)
      integer lun,id(1000)

      lun = 142
      open(unit=lun,file='daojunk.dat',status='old',err=999)
      read(lun,'(a60)',end=998)name
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

c search for the nearest star

      dclose = 1000.0
      do i=1,nrec
        dist = sqrt ( (x(i)-xstar)**2 + (y(i)-ystar)**2 )
        if(dist.lt.dclose)then
          dclose=dist
          number=id(i)
        end if
      end do
      type *, ' '
      type *,'The closest star has an ID number of ',number
      type *,'in file ',name
      type *, ' '
      type *,'Positional error in pixels is ',dclose
      type *, ' '

      goto 888

  999 type *, 'Error opening DAOJUNK.DAT'
      goto 888

  998 type *, 'Error reading from DAOJUNK.DAT EOF encountered'
      close( lun)
      goto 888

  997 type *, 'Error opening ', name
      goto 888

  996 type *, 'Error reading from ',name
      close( lun)
      goto 888

  888 continue

      end
