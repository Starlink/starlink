      subroutine read_dict(word,icount)
*+
* Name:
*    READ_DICT

* Invocation:
*    CALL READ_DICT(WORD,ICOUNT)
*
* Description:
*   To read the dictionary of commands in the array dictionary and try
*   to find a match to the command input by the user.
* Purpose:
*   To read the dictionary of commands in the array dictionary and try
*   to find a match to the command input by the user.
* Arguments:
*     WORD = CHARACTER*(*) (Given)
*        Command as typed by user
*     ICOUNT = INTEGER (Returned)
*        Number of matches found-or -10 if command not
*                    allowed
* Author:
*   T.N.Wilkins Manchester
* History:
*   Altered TNW 20/7/88 to allow one continuation line in dictionary
*   Altered TNW 2/1/90 to allow as many continuation lines as required.
*   Check for "forbidden" commands first-this means that it doesn't
*   matter if these are left in the command list, TNW 18-JUN-1991
*- --------------------------------------------------------------------
      implicit none
      character*(*) word
      character*150 dict(400),forbid(30)*20,scr*510
      integer ndict,j,jj,chr_len,i,icount,nforbid
      integer ip,ipos,iref
      common/bat_dict/dict,forbid
      common/bat_dicti/ndict,nforbid
      icount=0

* Isolate command verb

      j=chr_len(word)
      ipos=index(word,' ') - 1
      if (ipos.gt.0) j=min(j,ipos)
      ipos=index(word,',') - 1
      if (ipos.gt.0) j=min(j,ipos)

* Check if command is allowed (i.e. useable in batch mode)

      do jj=1,nforbid
        scr=forbid(jj)
        ip=chr_len(scr)
        if ((ip.eq.j).and.(scr(:ip).eq.word(:j))) then
          icount=-10
        end if
      end do
      if(icount.eq.-10) return
      do jj=1,ndict
        scr=dict(jj)
        ip=index(scr,',')-1

* Check on icount is to prevent more than one match being displayed,
* even if the input file contains such.

        if ((ip.eq.j).and.(scr(:ip).eq.word(:j)).and.(icount.eq.0)) then
          i=chr_len(scr)
          iref = jj
          do while(iref.lt.ndict)
            iref = iref + 1
            if(dict(iref)(1:1).eq.'-') then
              scr(i+1:) = dict(iref)(2:chr_len(dict(iref)))
              i=chr_len(scr)
            else
              iref = ndict
            end if
          end do
          icount=icount+1

* Print out entry in array dict corresponding to this command.

          call print_string(scr(:i),' ')
        end if
      end do
      end
