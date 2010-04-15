      subroutine getinp(cycle, string,prompt,istat)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets a string of text from the users keyboard. The users is
*       given the specified prompt. DCL-like multiple command line
*       recall and command line editing is available using cursor
*       keys, etc.
*
*METHOD
*
*ARGUMENTS
*   INPUTS:
*       cycle   integer         An index for the recall history
*                               context to be used (1 or 2).
*       prompt  character       The prompt string
*   OUTPUTS:
*       string  character       The returned text string from the user
*       istat   integer         Status: 0 for success
*
*-------------------------------------------------------------------
*
      implicit none
      include 'CMDSB'

*
* DECLARE ARGUMENTS
*
      character string*(*),prompt*(*)
      integer istat,cycle

*
* DECLARE LOCAL VARIABLES
*
      integer   i
      integer   lenout          ! No. of characters entered by user
      character lognam*11       ! Logical name in which the string
                                ! obtained from user will be stored
      character lstbuf*255      ! Previous command line from user
      integer   minc            ! Statement function MOVE dummy argument
      integer   move            ! Statement function giving cyclic move-
                                ! ment round the buffer
      integer   mval            ! Statement function MOVE dummy argument

      logical clr
      logical isatty
      common /CLR_COM/ clr


*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*DSB_max-1,DSB_max)+1

*
*  INITIALISE THE RETURNED STATUS FLAG TO INDICATE SUCCESS.
*
      istat = 0


*
*  SEE IF STANDARD INPUT OUT IS A TERMINAL. IF IT ISN'T (EG IF IT IS A
*  FILE) OR IF THE USER HAS CHOSEN NOT TO USE COMMAND LINE RECALL,
*  JUST DO A NORMAL FORTRAN READ FROM UNIT 5.
*
      call psx_isatty(0, isatty, istat)
      if( .not. isatty ) then
         read( 5,'(A)',iostat=istat ) string
         goto 999
      end if

      if( .not. clr  ) then
         write( *, '(A,$)', iostat=istat ) prompt
         if( istat .eq. 0 ) read( *,'(A)',iostat=istat ) string
         goto 999
      end if


      if( cycle .lt. 1 .or. cycle .gt. DSB_ncy ) then
         write(*,*) '*** Illegal input cycle: ',cycle
         istat = 1
         goto 999
      endif

*
* IF DSB_pw is out of bounds, reset the common arrays, etc.
*
      if( DSB_pw(cycle) .le. 0 .or. DSB_pw(cycle) .gt. DSB_max ) then
         do i = 1, DSB_max
            DSB_rec( i, cycle ) = ' '
         enddo
         DSB_pw(cycle) = 1
         DSB_top( cycle ) = 0
      endif

*
* GET A TEXT STRING FROM THE USER USING ROUTINE RDKYBD TO
* PROVIDE THE COMMAND LINE RECALL FACILITY.
*
      call rdkybd(cycle,string,prompt,lenout,istat)
      if( istat .ne. 0 ) goto 999

      if( lenout .lt. len( string ) ) string( lenout+1: )=' '

*
* STORE THE RETURNED STRING AS A LOGICAL NAME IN THE NEXT SLOT
* (POINTED TO BY RECALL_DSB_pw), BUT ONLY IF:
*               A) IT IS NOT IDENTICAL TO THE PREVIOUS COMMAND LINE
*               B) IT IS NOT A NULL STRING
*
      if(lenout.gt.0) then
         call gtslot(cycle,move(DSB_pw(cycle),-1),lstbuf,istat)
         if(lstbuf.ne.string .or. istat.ne.0  ) then
            DSB_rec( DSB_pw(cycle), cycle ) = string(:lenout)

*
* IF ALL IS OK, INCREMENT THE VALUE OF RECALL_DSB_pw, CYCLING FROM MAX
* BACK TO 1 WHEN MAX IS REACHED.
*
            DSB_top( cycle ) = max( DSB_top( cycle ), DSB_pw(cycle) )
            DSB_pw(cycle)=move(DSB_pw(cycle),1)
         endif
         istat = 0
      endif

*
* FINISH
*
  999 continue

      end

