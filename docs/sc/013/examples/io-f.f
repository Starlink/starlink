      program io
      
      implicit none
      
      real arr(10)
      integer idx
      
      arr(1) = 1.0
      do idx=2,10
         arr(idx) = arr(idx-1)*10.0
      enddo
      
      open (unit=99,file='outputf.dat',form='unformatted')
      write (99) arr
      close (99)
      
      end
