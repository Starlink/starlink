CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is STARMAN2_MON.F
C
C  Contains:-
C
C  STARMAN2_MON         Monolith 2 for Starman programs

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN_MON -- Monolith for Starman programs
C   See SUN 144, section 3 for this
C
C  alan penny             RAL          1994 June
*  BLY: M.J.Bly (Starlink, RAL) 28-NOV-1995
*     Split STARMAN monolith into 4 monoliths.

      subroutine starman2_mon ( status )

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
      if ( name.eq.'imjoin' ) then
         call imjoin ( status )
      elseif ( name.eq.'imjoin' ) then
         call imjoin ( status )
      elseif ( name.eq.'imkey' ) then
         call imkey ( status )
      elseif ( name.eq.'import' ) then
         call import ( status )
      elseif ( name.eq.'imrotate' ) then
         call imrotate ( status )
      elseif ( name.eq.'imsmooth' ) then
         call imsmooth ( status )
      elseif ( name.eq.'imstat' ) then
         call imstat ( status )
      elseif ( name.eq.'imtype' ) then
         call imtype ( status )
      elseif ( name.eq.'imweed' ) then
         call imweed ( status )
      elseif ( name.eq.'interact' ) then
         call interact ( status )
      elseif ( name.eq.'measure' ) then
         call measure ( status )
      elseif ( name.eq.'profile' ) then
         call profile ( status )
      elseif ( name.eq.'simplemag' ) then
         call simplemag ( status )
      elseif ( name.eq.'sprinkle' ) then
         call sprinkle ( status )
      elseif ( name.eq.'starfind' ) then
         call starfind ( status )
      elseif ( name.eq.'starmangripe' ) then
         call starmangripe ( status )
      elseif ( name.eq.'starmanhelp' ) then
         call starmanhelp ( status )
      endif

      end
