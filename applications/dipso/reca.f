      subroutine reca(cycle,string,pr,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Checks a string entered by the user to see if it is a RECALL
*       command. If it is, this routine processes the command and sets
*       the input string blank (RECALL commands are not stored).
*
*SOURCE
*       RECALL.FOR in DSCLDIR:SCLLIB.TLB
*
*METHOD
*       Parse the string into command, qualifier and parameter.
*       Implement REC/ALL   - gives a list of all saved commands
*                 REC fred  - returns slot starting with "fred
*                 REC 10    - Returns 10th previous string
*                 REC       - Returns last command
*       If a recall command is specified, the input string is set
*       blank on exit to avoid the string being stored in a recall
*       slot.
*
*ARGUMENTS
*   INPUTS:
*       string  character       String supplied by user
*       pr      integer         Currently displayed slot
*   OUTPUTS:
*       string  character       Set to blank if a RECALL command found
*       ierr    integer         Error status 0 - User to be re-prompted
*                                            1 - User input finished
*                                            2 - RECA/ALL given
*
*SUBROUTINES CALLED
*       DSCLDIR:SCLLIB.TLB:
*              chr_len,gtslot,chr_ldblk
*       INTERIM:
*              wruser,ctoi
*       VMS Run Time Library:
*              str$upcase,lib$get_symbol
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 30/9/88
*-------------------------------------------------------------------
*
      implicit none
      include 'SAE_PAR'
      include 'DSB_CLR'

*
* DECLARE ARGUMENTS
*
      character string*(*)
      integer   cycle,pr,ierr

*
* DECLARE LOCAL VARIABLES
*
      character buffer*255 ! Tidied up copy of the input string
      logical chr_simlr
      character com*6      ! Part of string which may contain RECALL
      integer   end        ! Useful length of tidied up input string
      integer   istat      ! Error status
      integer   lcom       ! Useful length of string in variable "com
      integer   lparm      ! Useful length of string in variable "parm
      integer   lqual      ! Useful length of string in variable "qual
      integer   minc       ! Statement function MOVE dummy argument
      integer   move       ! Statement function giving cyclic movement
                           ! round the buffer
      integer   mval       ! Statement function MOVE dummy argument
      integer   n          ! Offset from current command backwards
      character parm*40    ! Parameter from input string
      character qual*5     ! Qualifier from input string
      integer   slash      ! Position of 1st slash in the input string
      integer   slot       ! Current slot number
      integer   space      ! Position of 1st space in the input string
      integer   chr_len     ! Function giving useful length of a string
      character text*255   ! Text contained in current slot

*
* DEFINE FUNCTION WHICH RETURNS THE SLOT NUMBER WHICH IS A GIVEN
* INCREMENT AWAY FROM A STARTING SLOT. INCREMENTS CAN BE +VE OR -VE
*
      move(mval,minc)=mod(mval+minc+100*DSB_max-1,DSB_max)+1

*
* INITIALISE IERR TO INDICATE THAT THE USER HAS FINISHED GIVING INPUT
*
      ierr=1

*
* CONVERT COPY OF INPUT STRING TO UPPER CASE, STRIP LEADING BLANKS AND
* FIND ITS USEFUL LENGTH
*
      buffer = string
C     call chr_ucase(buffer)
      call chr_ldblk(buffer)
      end=chr_len(buffer)

*
* FIND POSITION OF FIRST SPACE AND FIRST SLASH
*
      space=index(buffer,' ')
      if(space.eq.0) space=100000
      slash=index(buffer,'/')
      if(slash.eq.0) slash=100000

*
* SLASH ONLY INDICATES A QUALIFIER IF IT OCCURS BEFORE THE FIRST SPACE
*
      if(slash.gt.space) slash=100000

*
* EXTRACT THE COMMAND FROM THE STRING
*
      lcom=min(end,space-1,slash-1)
      com=buffer(:lcom)

*
* SEE IF THE COMMAND WAS "RECA", AND IF SO
* SET INPUT STRING BLANK TO INHIBIT STORAGE OF RECALL COMMAND
*
      if(lcom.gt.0.and.chr_simlr('RECA',com(:lcom)) ) then
         string=' '

*
* IF IT WAS THEN EXTRACT ANY COMMAND QUALIFIER
*
         if(slash.lt.end) then
            qual=buffer(slash+1:min(end,space-1))
            call chr_ldblk(qual)
            lqual=chr_len(qual)

*
* CHECK VALIDITY OF QUALIFIER. ONLY /ALL ALLOWED
*
            if(chr_simlr('ALL',qual(:lqual)) ) then
               write(*,*) '*** Invalid RECA qualifier "/'
     :                     //qual(:lqual)//'"'
               goto 999
            endif
         else
            qual=' '
         endif

*
* NOW EXTRACT ANY COMMAND PARAMETER
*
         if(space.lt.end) then
            parm=buffer(space+1:end)
            call chr_ldblk(parm)
            lparm=chr_len(parm)
         else
            parm=' '
         endif

*
* IF BOTH A QUALIFIER AND A PARAMETER WERE GIVEN GIVE MESSAGE AND QUIT
*
         if(parm.ne.' '.and.qual.ne.' ') then
            write(*,*) '*** Too many RECA parameters - "'
     :                  //parm(:lparm)//'"'
            goto 999
         endif

*
* IF A QUALIFIER (/ALL) WAS SPECIFIED, GIVE A LIST OF ALL DEFINED
* RECALL SLOTS
*
         if(qual.ne.' ') then
            ierr=2
            text=' '
            do n=1,DSB_max-1
               write(text(:4),'(2X,I2)') n
               call gtslot(cycle,move(DSB_pw(cycle),-n),text(6:),istat)
               if(istat.ne.0) goto 999
               write(*,*) text(:chr_len(text))
            enddo

*
* IF NO QUALIFIER WAS GIVEN, PROCESS SEPERATELY THE THREE CASES WHERE
*   1) NO PARAMETER IS GIVEN
*   2) A NUMERIC PARAMETER IS GIVEN
*   3) A STRING PARAMETER IS GIVEN
*
         else

*
* IF NO PARAMETER WAS GIVEN, SET PR TO RECALL THE PREVIOUS COMMAND
*
            if(parm.eq.' ') then
               if(pr.ne.DSB_pw(cycle)+1) pr=move(pr,-1)
               ierr=0

*
* IF A NUMERIC PARAMETER (=n) WAS GIVEN, RECALL THE nTH
* PREVIOUS COMMAND
*
            else
               call err_mark
               istat=SAI__OK
               call chr_ctoi(parm,n,istat)
               if(istat.eq.SAI__OK) then
                  if(n.gt.0.and.n.lt.DSB_max) then
                     pr=move(DSB_pw(cycle),-n)
                     ierr=0
                  else
                     write(*,*) '*** Invalid value for numeric RECALL'
     :                           //' parameter'
                  endif

*
* OTHERWISE IF A STRING PARAMETER WAS GIVEN, RECALL THE LAST SLOT WHICH
* STARTS WITH THE GIVEN STRING
*
               else
                  CALL ERR_ANNUL( istat )
                  text=' '
                  do n=1,DSB_max-1
                     call gtslot(cycle,move(DSB_pw(cycle),-n),text,
     :                           istat)
C                    call chr_ucase(text)
                     call chr_ldblk(text)
                     if(index(text,parm(:lparm)).eq.1) then
                        pr=move(DSB_pw(cycle),-n)
                        ierr=0
                        goto 999
                     endif
                  enddo

*
* IF CONTROL GETS TO THIS POINT THEN NO MATCHING SLOT WAS FOUND
*
                  write(*,*) '*** RECALL string not found "'//
     :                        parm(:lparm)//'"'
               endif
               call err_rlse
            endif
         endif
      endif

*
* FINISH
*
  999 continue
      end

