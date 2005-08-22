      subroutine str_trim(dest,source,length)

      implicit none

      character*(*) dest,source
      integer length,str_len
      character*(80) temp
      external str_len

      length = str_len(source)
      temp = source(1:length)
      dest = temp

      end
