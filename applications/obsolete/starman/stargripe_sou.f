CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMANGRIPE --  Tells about comments and suggestions system for Starman
C
C         a.j.penny                ral                   1994 Dec

      subroutine starmangripe ( ierradam )

      implicit none

      integer    ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_starmangripe

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_STARGRIPE.F  Tells about comments and suggestions system (name too long)
C
C  Contains:-
C  T_STARMANGRIPE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_STARMANGRIPE -- Tells about comments and suggestions system (name too long)
C
C  alan penny                   ral            1994 Dec

      subroutine t_starmangripe ( )

      implicit none
      include 'STARMAN_INC'
C--
      integer k
      character*72 text(14)
      data text /
     + ' ',
     + '   To make a comment on any aspect of Starman, or to report',
     + '   a bug, or to make a suggestion for improvement, you can',
     + '   send an e-mail to the author, Alan Penny at RAL, at the',
     + '   address    alan.penny@rl.ac.uk     Or phone him up on',
     + '   01235-445675 in the UK, or +44-1235-445675 from outside',
     + '   You can also send a Fax, to 01235-446667.',
     + ' ',
     + '   You can also do this by using the hypertext version of ',
     + '   this program - starmanhypergripe',
     + ' ',
     + '   Help is also available with starmanhelp and ',
     + '   starmanhyperhelp.',
     + ' '/

Cbegin


      if ( ST_FAILED ) return

      do k = 1, 14
         call printo ( text(k) )
      enddo


      end
