CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is STARMAN3_MON.F
C
C  Contains:-
C
C  STARMAN3_MON         Monolith 3 for Starman programs

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN3_MON -- Monolith for Starman programs
C   See SUN 144, section 3 for this
C
C  alan penny             RAL          1994 June
*  BLY: M.J.Bly (Starlink, RAL) 28-NOV-1995
*     Split STARMAN monolith into 4 monoliths.

      subroutine starman3_mon ( status )

      implicit none
      include 'SAE_PAR'
      include 'PAR_PAR'

      integer status
C--
      character*(PAR__SZNAM) name, iname
Cbegin

*  Check STATUS and exit if not OK
      if ( status.ne.SAI__OK ) return

*  Get action name and fold to lower case
      call task_get_name ( iname, status )
      call lowcase ( iname, name )

*  Call the appropriate action routine
      if ( name.eq.'tbcalc' ) then
         call tbcalc ( status )
      elseif ( name.eq.'tbchart' ) then
         call tbchart ( status )
      elseif ( name.eq.'tbcomps' ) then
         call tbcomps ( status )
      elseif ( name.eq.'tbcut' ) then
         call tbcut ( status )
      elseif ( name.eq.'tbdes' ) then
         call tbdes ( status )
      elseif ( name.eq.'tbjoin' ) then
         call tbjoin ( status )
      elseif ( name.eq.'tbkey' ) then
         call tbkey ( status )
      elseif ( name.eq.'tblist' ) then
         call tblist ( status )
      elseif ( name.eq.'tbload' ) then
         call tbload ( status )
      elseif ( name.eq.'tbmatch' ) then
         call tbmatch ( status )
      elseif ( name.eq.'tbnative' ) then
         call tbnative ( status )
      elseif ( name.eq.'tbnmatch' ) then
         call tbnmatch ( status )
      elseif ( name.eq.'tbplot' ) then
         call tbplot ( status )
      elseif ( name.eq.'tbpmatch' ) then
         call tbpmatch ( status )
      elseif ( name.eq.'tbrenum' ) then
         call tbrenum ( status )
      endif

      end
