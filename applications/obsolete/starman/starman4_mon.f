CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is STARMAN4_MON.F
C
C  Contains:-
C
C  STARMAN4_MON         Monolith 4 for Starman programs

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN4_MON -- Monolith for Starman programs
C   See SUN 144, section 3 for this
C
C  alan penny             RAL          1994 June

      subroutine starman4_mon ( status )

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
      if ( name.eq.'tbsheet' ) then
         call tbsheet ( status )
      elseif ( name.eq.'tbsort' ) then
         call tbsort ( status )
      elseif ( name.eq.'tbstat' ) then
         call tbstat ( status )
      elseif ( name.eq.'tbtran_auto' ) then
         call tbtran_auto ( status )
      elseif ( name.eq.'tbtran_do' ) then
         call tbtran_do ( status )
      elseif ( name.eq.'tbtran_load' ) then
         call tbtran_load ( status )
      elseif ( name.eq.'tbtran_make' ) then
         call tbtran_make ( status )
      elseif ( name.eq.'tbvalue' ) then
         call tbvalue ( status )
      elseif ( name.eq.'tbweed' ) then
         call tbweed ( status )
      elseif ( name.eq.'unccd' ) then
         call unccd ( status )
      endif

      end
