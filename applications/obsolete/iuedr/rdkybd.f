      subroutine rdkybd(cycle,string,prompt,lenout,istat)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Reads text from the keyboard. DCL-like command line recall
*       can be performed by using cursor or control keys. The recall
*       buffer is not updated (that is done by routine GETINP).
*
*SOURCE
*       RDKYBD.FOR in DSCLDIR:SCLLIB.TLB
*
*METHOD
*       This routine handles the selection of strings to display
*       when up or down arrow keys are pressed. The actual keyboard
*       read is done by routine rdkbd2.
*
*ARGUMENTS
*   INPUTS:
*       prompt  character       The user prompt
*   OUTPUTS:
*       string  character       Final string entered by user
*       lenout  integer         No. of characters entered by user
*       istat   integer         Status: 0 for success.
*
*SUBROUTINES CALLED
*       DSCLDIR:SCLLIB.TLB:
*              gtslot,rdkbd2,recall
*       INTERIM:
*              wruser
*
*VAX SPECIFICS
*       do while
*       enddo
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/2/88
*-------------------------------------------------------------------
*
      implicit none
      include 'CMDSB'

*
* DECLARE ARGUMENTS
*
      integer   cycle,lenout,istat
      character string*(*),prompt*(*)

*
* DECLARE LOCAL VARIABLES
*
      integer   chr_len  ! Used length of a string
      character clear*7  ! String holding control codes to clear a line
      integer   ctrl_d   ! Integer flag for a control-D terminator
      integer   dncurs   ! Integer flag for a cursor down terminator
      integer   lprm     ! Length of prompt in characters
      integer   minc     ! Statement function MOVE dummy argument
      logical   more     ! True if user has not finished giving input
      integer   move     ! Statement function giving cyclic movement
                         ! round the buffer
      integer   mval     ! Statement function MOVE dummy argument
      character pbuf*255 ! Temporary buffer for prompt string
      integer   pr       ! Pointer to slot from which current recall
                         ! string was read
      integer   rdkbd2   ! Read a line of text
      integer   return   ! Integer flag for a RETURN terminator
      integer   slstat   ! 0 if specified slot defined, 1 otherwise
      integer   term     ! flag for character which caused termination of user input
      integer   upcurs   ! Integer flag for a cursor up terminator

*
* SET UP PARAMETERS
*
      parameter (

     :    return = 1,
     :    upcurs = 2,
     :    dncurs = 3,
     :    ctrl_d = 4
     :          )

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*DSB_max-1,DSB_max)+1
*
* Initialise clear here due to a deficiency in Linux g77.
*
      clear  = char(13)//char(27)//'[K'//char(0)//char(0)//char(0)
*
* INITIALISE THE RETURNED STATUS TO INDICATE SUCCESS.
*
      istat = 0

*
* COPY PROMPT TO TEMPORARY BUFFER
*
      pbuf=prompt
      lprm=len(prompt)

*
* INITIALISE POINTER TO CURRENTLY RECALLED COMMAND LINE SLOT
*
      pr=DSB_pw( cycle )

*
* LOOP UNTIL THE USER TERMINATES A STRING WITH A RETURN (AS OPPOSED TO
* CURSOR UP OR DOWN KEYS)
*
      more=.true.
      do while(more)

*
* GET THE TEXT OF THE COMMAND BEING RECALLED (THE SLOT BEING WRITTEN
* TO IS CONSIDERED TO BE BLANK). IF SLOT IS NOT YET DEFINED, SLSTAT
* IS SET TO 1
*
         if(pr.ne.DSB_pw( cycle )) then
            call gtslot(cycle,pr,string,slstat)
            if( slstat .eq. 1 ) then
               string = ' '
               lenout = 0
            else
               lenout = chr_len( string )
            endif
         else
            string=' '
            lenout = 0
            slstat = 0
         endif

*
* GET A STRING FROM THE USER TERMINATED WITH EITHER RETURN, UP ARROW OR
* DOWN ARROW.
*
         istat = rdkbd2(string,pbuf,lprm,term,lenout)
         if( istat .eq. 0 ) then

*
* UPDATE THE COMMAND TO RECALL IF STRING WAS TERMINATED BY A CURSOR KEY
*
            if(term.eq.upcurs) then
               if(pr.ne.move(DSB_pw( cycle ),1).and.slstat.eq.0)
     :            pr=move(pr,-1)

            else if(term.eq.dncurs) then
               if(pr.ne.DSB_pw( cycle )) pr=move(pr,+1)

*
* IF STRING WAS TERMINATED WITH A CONTROL-D, RETURN AN ERROR STATUS.
*
            else if(term.eq.ctrl_d) then
               write(*,*) '^D'
               istat = 1
               more = .false.
*
* IF STRING WAS TERMINATED WITH ANYTHING OTHER THAN AN ARROW KEY,
* ASSUME THAT USER HAS FINISHED GIVING INPUT
*
            else

*
* CHECK WHETHER STRING ENTERED IS A RECALL COMMAND. IF SO PROCESS IT.
*
               call reca(cycle,string,pr,istat)
*
* IF IT WASN'T A VALID RECALL COMMAND COMMAND THEN EXIT
*
               if(istat.eq.1) then
                  more=.false.
                  istat = 0
               else if(istat.eq.2 ) then
                  istat = 0
               endif

            endif

*
* MODIFY THE PROMPT STRING TO INCLUDE A CARRAGE RETURN AND LINE CLEAR
* AT THE START. THIS IS SO THAT ANY FURTHER PROMPTS WILL OVERLAY THE
* ONE AND NOT GET ADDED ON THE END
*
            pbuf=clear//prompt
            lprm=len(prompt)+7

*
* IF SOMETHING WENT WRONG, GIVE A MESSAGE AND EXIT.
*
         else
            write(*,*) '*** Error ',istat,' reading input from terminal'
            more = .false.
            string = ' '
            lenout = 0

         end if

*
* LOOP ROUND TO DISPLAY NEXT RECALL STRING AND GET A NEW STRING FROM
* USER
*
      enddo

*
* FINISH
*
      end
