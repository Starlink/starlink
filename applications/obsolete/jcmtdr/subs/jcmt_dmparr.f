      subroutine jcmt_dump_array (nx, ny, array, name, status)

      implicit none
      integer nx, ny
      real array (nx,ny)
      character*(*) name
      integer status

      integer dims (2)
      integer address, slot
      include 'CNF_PAR'

      if (status .ne. 0) return

      call dsa_named_output ('temp', name, ' ', 0, 0, status)
      dims (1) = nx
      dims (2) = ny
      call dsa_simple_output ('temp', 'D', 'float', 2, dims, status)
      call dsa_map_data ('temp', 'update', 'float', address, slot,
     :   status)
      if (status .eq. 0) then
         call gen_move (4*nx*ny, array, %val(CNF_PVAL(address)))
      end if
      call dsa_unmap (slot, status)
      call dsa_close_structure ('temp', status)

      end
