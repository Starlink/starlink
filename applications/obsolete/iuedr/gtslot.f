      subroutine gtslot(cycle,slot,string,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the string stored in a specified RECALL slot.
*
*SOURCE
*       GTSLOT.FOR in DSCLDIR:SCLLIB.TLB
*
*METHOD
*       The value of slot actually used is wrapped round into the
*       range 1 to DSB_max.
*       An attempt is made to translate the logical name RECALL_'slot'.
*       If this is successful, the equivalence string is returned.
*       Otherwise, the ierr is set to a non zero value and a null
*       string returned.
*
*ARGUMENTS
*   INPUTS:
*       slot    integer         Slot number required
*   OUTPUTS:
*       string  character       Contents of required slot
*       ierr    integer         Error status 0 - Sucess
*                                            1 - Slot not defined
*
*SUBROUTINES CALLED
*       DSCLDIR:SCLLIB.TLB:
*              chr_ldblk
*       VMS Run Time Library:
*              lib$sys_trnlog
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/7/88
*-------------------------------------------------------------------
*
      implicit none
      include 'CMDSB'

*
* DECLARE ARGUMENTS
*
      integer   cycle,slot,ierr
      character string*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   minc            ! Statement function MOVE dummy argument
      integer   move            ! Statement function giving cyclic move-
                                ! ment round the buffer
      integer   mval            ! Statement function MOVE dummy argument

      move(mval,minc)=mod(mval+minc+100*DSB_max-1,DSB_max)+1

      if( move(slot,0) .le. DSB_top(cycle) ) then
         string=DSB_rec( move(slot,0), cycle )
         ierr =0
      else
         string = ' '
         ierr=1
      endif

      end
