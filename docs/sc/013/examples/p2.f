      subroutine mkidentity (matrix, dim)
      real matrix (dim,dim)
      integer dim
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
