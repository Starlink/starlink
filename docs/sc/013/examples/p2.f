      subroutine mkidentity (matrix, dim)
      implicit none
      integer dim
      real matrix (dim,dim)
      integer m,n
      do m = 1, dim
         do n = 1, dim
            if (m.eq.n) then
               matrix(m,n) = 1.
            else
               matrix(m,n) = 0.
            endif
         enddo
      enddo
      return
      end
