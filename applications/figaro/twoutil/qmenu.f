      subroutine qmenu(title,dict,ndict,deflt,nums,cvals,key,narg,
     :     status)
*+
* Name:
*    QMENU

* Invocation:
*    CALL QMENU(TITLE,DICT,NDICT,DEFLT,NUMS,CVALS,KEY,NARG,
*          STATUS)

*
* Description:
*   Menu handler. This will ensure that all responses are valid.
*   Each element in the dictionary can have arguments and/or user
*   information.
* Purpose:
*   Menu handler. This will ensure that all responses are valid.
*   Each element in the dictionary can have arguments and/or user
*   information.
* Arguments:
*     TITLE = CHARACTER*(*) (Given)
*        Title for menu
*     DICT(NDICT) = CHARACTER*(*) ARRAY (Given)
*        dictionary, including information on arguments,
*                          description of meaning, etc.
*     NDICT = INTEGER (Given)
*        Number of entries in dictionary
*     DEFLT = INTEGER (Given)
*        Default entry if user types <CR>
*     NUMS(*) = REAL ARRAY (Returned)
*        Numeric arguments
*     CVALS(*) = CHARACTER*(*) ARRAY (Returned)
*        Character arguments
*     KEY = INTEGER (Returned)
*        Menu option selected
*     NARG = INTEGER (Returned)
*        Number of arguments
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    Format for input:
*      "OPTION %F %T(explanation) : Explanation"
*                        etc.
*                OPTION  - Menu option, must be in upper case
*                %f/%F   - decimal arguments
*                %t/%T   - text arguments
*      Uppercase means required if all arguments to left given, lower
*      case means optional.
* Author:
*   T.N.Wilkins, Cambridge, 7th-15th April 1992
* History:
*   T.N.Wilkins, Cambridge, 12th May 1992 Number o arguments reduced by 1,
*                returning narg rather than ncval and nnums.
*        "     , Durham, 3rd March 1992 Prompt for missing arguments
*        "           "   28th May 1993 Fill default string for par_qstr with
*                blanks before putting items into it.
*-
      implicit none
      include 'PRM_PAR'
      include 'SAE_PAR'
      integer ndict,deflt,status,nnums,ncval,key,narg
      character*(*) dict(ndict),cvals(*),string*80,title,prompt*80
      real nums(*)
      integer i,tmp1,tmp2,lmen,chr_len,pstat,menu,nodict,MAXWRD,istat
      parameter (MAXWRD = 5)
      integer start(MAXWRD),stop(MAXWRD),nword,nwordm,j,len1,colon,move
      integer oldmdf,mindif,tlen,menflg,oind,len2,okey
      character*20 words(MAXWRD),mwords(MAXWRD),chars*79,parprm
      character*1 test1,test2,chr_upper
      logical ifdef,par_batch,usedef,qstat,par_qnum
      save menflg
      data menflg/2/

* Return if an error has occurred

      if(par_batch()) status = SAI__ERROR
      if(status.ne.SAI__OK) return

* Write title

      nodict = 0
      colon = 0
      if(menflg.ge.2) then

*  Prepare title for menu, and write to terminal

        call chr_fill(' ',chars)
        len1 = 0
        call chr_putc('=========< ',chars,len1)
        tlen = len(title)
        do i = 1, tlen
          call chr_putc(title(i:i),chars,len1)
          len1 = len1 + 1
        end do
        call chr_putc('>=========',chars,len1)
        call par_wruser(' ',pstat)
        call par_wruser(chars(:len1),pstat)
        call par_wruser(' ',pstat)


* Write entries with explanations
*   -first check spacing

        do i = 1, ndict
          string = dict(i)
          tmp1 = index(string,':')
          tmp2 = tmp1
          do j = 1, tmp1-1
            if((string(j:j).eq.'%').and.
     :           (string(j+2:j+2).ne.' ')) then
              tmp2 = tmp2 - 2
            endif
          enddo
          colon = max(colon,tmp2)
        enddo
        if(menflg.eq.3) colon = colon + 6
      else
        len2 = 0
      endif

      do i = 1, ndict
        string = dict(i)
        call tnw_ssblk(string)
        len1 = chr_len(string)
        if(menflg.le.1) then
          lmen = len1
        else
          lmen = 0
        endif
        oind = 1

*   If an explanation is given for an argument, then use that instead
*   of %whatever.

        do while(len1.ne.lmen)
          lmen = len1
          tmp1 = index(string,':')
          if(tmp1.eq.0) tmp1 = len1
          tmp2 = index(string(oind:tmp1),'%')
          if(tmp2.ne.0) then

*      We have a "%", so is the 2nd character after it blank (if not there's
*      some form of explanation).

            tmp2 = oind + tmp2 + 1
            if(string(tmp2:tmp2).ne.' ') then
              do j = tmp2,len1
                string(j-2:j-2) = string(j:j)
              enddo
              string(len1-1:len1) = '  '
              len1 = len1 - 2
              oind = tmp2-1
            endif
          endif
        enddo

*    Align all the colons

        if(menflg.eq.2) then
          tmp2 = index(string,':')
          if(tmp2.eq.0) then
            move = 0
          else
            move = colon - tmp2
            if(move.gt.0) then
              do j = len1,tmp2,-1
                string(j+move:j+move) = string(j:j)
              enddo
              string(tmp2:colon-1) = ' '
              len1 = len1 + move
            endif
          endif
        endif

*         What is the minimum abbreviation?

        tmp1 = index(string,' ') - 1

        if(dict(i)(1:1).eq.'%') then
          nodict = i
          mindif = tmp1
        else

*     Get minimum abbreviation

          oldmdf = 0
          mindif = 1
          do while((mindif.gt.oldmdf).and.(mindif.lt.len1))
            oldmdf = mindif
            do j = 1, ndict
              if((i.ne.j).and.(dict(j)(1:1).ne.'%')) then
                if(string(1:mindif).eq.dict(j)(1:mindif))
     :               mindif = mindif + 1
              end if
            end do
          end do
        end if

* Now actually write out list! MENFLG gives the format

        if(menflg.eq.1) then

          call chr_lcase(string(mindif+1:tmp1))
          if(len2.ne.0) then
            call chr_putc(',',chars,len2)
            if((len2+tmp1).gt.79) then
              call par_wruser(chars(:len2),pstat)
              len2 = 0
            endif
          endif
          call chr_appnd(string(1:tmp1),chars,len2)
          if(i.eq.ndict) call par_wruser(chars(:len2),pstat)

        else if(menflg.eq.2) then

          call chr_lcase(string(mindif+1:tmp1))
          call par_wruser(string(:len1),pstat)

        else if(menflg.eq.3) then

          len2 = 0
          call chr_fill(' ',chars)
          tmp2 = index(string,':')-1
          call chr_appnd(string(:tmp2),chars,len2)
          if(dict(i)(1:1).eq.'%') then
            len2 = max(tmp2,colon-1)
          else
            len2 = max(len2+1,tmp2,(colon-mindif-3))
            call chr_appnd('[',chars,len2)
            call chr_putc(string(:mindif),chars,len2)
            call chr_appnd(']',chars,len2)
          endif
          call chr_appnd(string(tmp2:len1),chars,len2)
          call par_wruser(chars(:len2),pstat)

        endif

      enddo
      prompt = title

*  Set up default

      ifdef = deflt.ne.0
      if(ifdef) then
        lmen = index(dict(deflt),':')
        if(lmen.eq.0) then
          lmen = chr_len(dict(deflt))
        else
          lmen = lmen - 1
        endif
        call chr_dcwrd(dict(deflt)(:lmen),MAXWRD,nwordm,start,stop,
     :       mwords,istat)
        nnums = 0
        ncval = 0
        lmen = 0
        call chr_fill(' ',chars)
        do i =  1, nwordm
          if(mwords(i)(1:1).eq.'%') then
            test1 = chr_upper(mwords(i)(2:2))
            if(lmen.ne.0) lmen = lmen + 1
            if(test1.eq.'F') then
              nnums = nnums + 1
              call chr_putr(nums(nnums),chars,lmen)
            else if(test1.eq.'T') then
              ncval = ncval + 1
              chars = cvals(ncval)
              lmen = chr_len(chars)
            endif
          else
            call chr_appnd(mwords(i),chars,lmen)
          endif
        enddo
      else
        chars = ' '
        lmen = 1
      endif

      key = 0
      do while(key.le.0)

*  Prompt user

         call par_qstr(prompt(:chr_len(prompt)),chars(:lmen),ifdef,
     :       .true.,string)

*   User may wish to change menu handling, or require help, or to abort
*   program

        if(string(1:1).eq.'%') then
          read(string(2:2),'(i1)')tmp1
          if((tmp1.ge.0).and.(tmp1.le.3)) then
            menflg = tmp1
          else
            call par_wruser('Menu type must be 0-3',pstat)
          endif
          key = -10
        else if(string(1:2).eq.'??') then
          call par_wruser('%N to change "mode", N=0,1,2,3',pstat)
          call par_wruser('?? for this help',pstat)
          call par_wruser('     !! to abort program',pstat)
        else if(string(1:2).eq.'!!') then
          status = SAI__ERROR
          return
        else

*     Extract "words" from user response

          call chr_dcwrd(string,MAXWRD,nword,start,stop,words,istat)

*     If we have no words, then we'll take the default (if any)

          usedef = (nword.eq.0).and.ifdef
          if(usedef) then
            key = deflt
          else if(nword.ne.0) then
            key = menu(dict,ndict,words(1),deflt)
          else
            key = 0
          endif
          if(key.eq.-1) then
            prompt = 'Ambiguous answer-try again'
          else if((nodict.ne.0).and.(key.le.0).and.(nword.ge.1)) then
            key = nodict
          else
            prompt = 'Word not in dictionary-try again'
          endif
        endif
        if(.not.usedef) then
          nnums = 0
          ncval = 0
        endif

* Get arguments (if any). A capital letter indicates a required
* argument, and a lower case argument one not required. Note that
* if an argument is not found all future arguments are ignored,
* regardless of case of specifier.

        if((key.gt.0).and.(.not.usedef)) then

*     Extract "words" from selected item from dictionary

          call chr_dcwrd(dict(key),MAXWRD,nwordm,start,stop,mwords
     :         ,istat)
          i = 1
          istat = 0
          okey = key
          do while(i.le.nwordm)

*        Decode string, if arguments are required then set error flag
*        if not present

            if(mwords(i)(1:1).eq.'%') then

*        Decode numbers from "word", or put "word" into output character
*        array.

              test1 = mwords(i)(2:2)
              test2 = chr_upper(test1)
              if(test2.eq.'F') then
                if(nword.lt.i) istat = -2
                nnums = nnums+1
                call chr_ctor(words(i),nums(nnums),istat)
                if(istat.ne.0) then
                  if(test1.eq.'F') then
                    if(mwords(i)(3:3).eq.' ') then
                      parprm = 'Number'
                    else
                      parprm = mwords(i)(3:)
                    endif
                    qstat = par_qnum(parprm,VAL__MINR,VAL__MAXR,0.0,
     :                   .false.,' ',nums(nnums))
                  else
                    nnums = nnums-1
                    i = nwordm
                  endif
                endif
              else if(test2.eq.'T') then

*         Text string

                if(nword.lt.i) then
                  istat = -2
                  if(test1.eq.'T') then
                    ncval = ncval + 1
                    if(mwords(i)(3:3).eq.' ') then
                      parprm = 'String'
                    else
                      parprm = mwords(i)(3:)
                    endif
                    call par_qstr(parprm,' ',.false.,.false.,
     :                   cvals(ncval))
                  else
                    i = nwordm
                  endif
                else
                  ncval = ncval + 1
                  cvals(ncval) = words(i)
                  len1 = chr_len(cvals(ncval))
                  if(cvals(ncval)(1:1).eq.'"') then
                    do j = 2, len1
                      cvals(ncval)(j-1:j-1) = cvals(ncval)(j:j)
                    enddo
                    cvals(ncval)(len1:len1) = ' '
                    len1 = len1 - 1
                  endif
                  if(cvals(ncval)(len1:len1).eq.'"') then
                    cvals(ncval)(len1:len1) = ' '
                  endif
                endif
              endif
            endif
            i = i + 1
          enddo

*      Ensure correct prompt if error-if we have no menu entry and we
*      were assuming that the first entry must be the entry requiring
*      nothing before the argument, but it proved unsuitable, then
*      we'll backtrack and take it as a wrong menu entry.

          if(key.eq.-2) then
            if((okey.eq.nodict).and.(ncval.eq.0).and.(nnums.eq.0)) then
              prompt = 'Word not in dictionary-try again'
            else
              prompt = 'Missing/invalid argument(s)-try again'
            endif
          endif
        endif
      enddo
      narg = ncval + nnums
      end
