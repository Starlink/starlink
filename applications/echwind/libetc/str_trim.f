      subroutine str_trim(dest,source,length)

      implicit none

      character*(*) dest,source
      integer length,lnblnk
      character*(80) temp

      length = lnblnk(source)
      temp = source(1:length)
      dest = temp

      end
