      program parse_option

      implicit none

      call testn ('1.2')
      call testn ('  3. 4')
      call testn ('  5.6    ')
      call testn ('  7.  ')
      call testn ('7')
      call testn (' -71 ')
      call testn (' +72')
      call testn (' 888')
      call testn ('99   ')
      call testn ('   10  ')
      call testn (' 1 2 3 4 5 6 7 8 9 10 ')

      call testc ('adjust')
      call testc ('disable')
      call testc ('help')
      call testc ('menu')
      call testc ('plot')
      call testc ('exit')
      call testc ('quit')
      call testc ('hyper')
      call testc ('ADJUST')
      call testc ('diSable')
      call testc (' disable this ' )
      call testc ('HeL')
      call testc ('meNU ')
      call testc ('   PLOT')
      call testc ('  hYpErrrrrrr   ')
      call testc ('a')
      call testc ('d')
      call testc ('h')
      call testc ('m')
      call testc ('p')
      call testc ('e')
      call testc ('q')
      call testc ('hy')
      call testc ('  disa')
      call testc ('men  ')
      call testc ('  yes  ')
      call testc ('/')
      call testc ('+')
      call testc ('!')
      call testc ('*')
      call testc ('this is wrong')
      call testc ('  also  ')
      call testc ('')
      call testc ('    ')

      end

      subroutine testn (str)

      implicit none

      character*(*) str
      character tok
      integer option, suboption

      option = 0
      suboption = 0

      call ech_parse_option (str, tok, option, suboption)
      write (*, '(a25," -> ",a1,":",2i4)'), str, tok, option, suboption

      end

      subroutine testc (str)

      implicit none

      character*(*) str
      character tok
      integer option, suboption

      call ech_parse_option (str, tok, option, suboption)
      write (*, '(a25," -> ",a1)'), str, tok

      end

