CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is STARMAN1_MON.F
C
C  Contains:-
C
C  STARMAN1_MON         Monolith 1 for Starman programs

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARMAN1_MON -- Monolith for Starman programs
C   See SUN 144, section 3 for this
C
C  alan penny             RAL          1994 June
*  BLY: M.J.Bly (Starlink, RAL) 28-NOV-1995
*     Split STARMAN monolith into 4 monoliths.

      subroutine starman1_mon ( status )

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
      if ( name.eq.'addstars' ) then
         call addstars ( status )
      elseif ( name.eq.'automag' ) then
         call automag ( status )
      elseif ( name.eq.'average' ) then
         call average ( status )
      elseif ( name.eq.'chi' ) then
         call chi ( status )
      elseif ( name.eq.'diagram' ) then
         call diagram ( status )
      elseif ( name.eq.'dustring' ) then
         call dustring ( status )
      elseif ( name.eq.'imcalc' ) then
         call imcalc ( status )
      elseif ( name.eq.'imcube' ) then
         call imcube ( status )
      elseif ( name.eq.'imcut' ) then
         call imcut ( status )
      elseif ( name.eq.'imdes' ) then
         call imdes ( status )
      elseif ( name.eq.'imfits_dr' ) then
         call imfits_dr ( status )
      elseif ( name.eq.'imflash' ) then
         call imflash ( status )
      endif

      end
