c   Given a square array m(n,n), call cadd2 on each element
      subroutine addarr (m, n)
      real m(n,n)
      integer n
      integer i,j
      real addval

      addval = m(n,n)
      do j=1,n
         do i=1,n
            call cadd2 (m(i,j), %val(addval))
         enddo
      enddo
      
      return
      end
