      subroutine prtout(menlis,entries,dict,menflg,title)
*+
* Name:
*    PRTOUT

* Invocation:
*    CALL PRTOUT(MENLIS,ENTRIES,DICT,MENFLG,TITLE)

* Purpose:
*    To output a menu of options.

* Description:
*    To output a menu of options. If LMENLS is greater than ENTRIES,
*    then the additional items must be at the start of MENLIS. Note that
*    if MENFLG=1, MENLIS and LMENLS are ignored.
*
* Arguments:
*      MENLIS(*) = CHARACTER*(*) ARRAY (Given)
*        List of menu options, with explanations
*      ENTRIES = INTEGER (Given)
*        Number of entries in dictionary
*      DICT(ENTRIES) = CHARACTER*(*) ARRAY (Given)
*        Dictionary being used
*      MENFLG = INTEGER (Given)
*        Flag to give format of menu
*                              1 - brief, abbreviations given by case of
*                                  letter
*                              2 - full, abbreviations given by case of
*                                  letter
*                              3 - full, abbreviations given before :
*      TITLE = CHARACTER*(*) (Given)
*        Title of menu
* Subroutines/functions referenced:
*     TNW_SSBLK,PAR_WRUSER,CHR_LEN,ICH_TIDY,CHR_APPND,CHR_FILL,
*     CHR_LCASE,CHR_PUTC

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 26-OCT-1989 Original version
*   TNW: 4-FEB-1991, Algorithm changed a bit
*   TNW: 4-OCT-1991, made to accept MENLIS as array (in addition to a
*        long string).
*   TNW: 9-DEC-1993 Only accept array for MENLIS.
*-
      implicit none
      character*(*) menlis(*)
      integer entries
      character*(*) dict(entries),title
      integer chr_len,ich_tidy
      integer ilen
      integer menflg
      logical go
      integer plen
      integer pstat
      integer ndict
      character*80 chars,tdict*20,current
      integer i,oldmdf,colind
      integer mindif,len1,tlen,ind2,shift,offset,toffset,len2

      ilen = chr_len(menlis(1))

      if(ilen.ne.0) then

         if(menflg.ge.2) then

*     Prepare title for menu, and write to terminal

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

*       What is the maximum space we require between the menu items and
*       the explanations (for parameters)? OFFSEt is the position of the
*       colon.

            offset = 0
            toffset = 0
            do i = 1, entries
               toffset = index(menlis(i),':') + chr_len(dict(i))
               offset = max(toffset,offset)
            end do
            offset = max(4,offset) + 2
            offset = min(40,offset)
            if(menflg.eq.3) offset = offset + 3

*     Now loop outputting information on options to terminal.

            ndict = 0
            go = .true.
            do while(go)

*       Extract information from string

               ndict = ndict + 1
               current = menlis(ndict)
               call chr_fill(' ',chars)
               len1 = 0

               if(ndict.gt.0) then

*         What is the minimum abbreviation?

                  oldmdf = 0
                  mindif = 1
                  tdict = dict(ndict)
                  tlen = chr_len(tdict)
                  do while((mindif.gt.oldmdf).and.(mindif.lt.tlen))
                     oldmdf = mindif
                     do i = 1, entries
                        if(i.ne.ndict) then
                           if(tdict(1:mindif).eq.dict(i)(1:mindif))
     :                          mindif = mindif + 1
                        end if
                     end do
                  end do

*         Option to give abbreviations by case

                  if(menflg.eq.2) then
                     call chr_putc(tdict(:mindif),chars,len1)
                     call chr_lcase(tdict)
                     if(tlen.gt.mindif)
     :                    call chr_putc(tdict(mindif+1:tlen),chars,len1)
                  else if(menflg.eq.3) then

*           Otherwise just put menu item into string

                     call chr_appnd(tdict,chars,len1)
                  end if
               end if

*         Anything between the end of the previous item and the colon is
*         a parameter, or possibly a non-menu item such as a line name
*         (to be dealt with separately).

               colind = index(current,':')
               ind2 = max(1,colind)

*         If ind is zero, then this is the last item in menlis,
*         otherwise it should be added to the current position to locate
*         the end of this item.

               if(ndict.gt.0) len1 = len1 + 1

*        Length of any parameters

               plen = ind2 - 1

*        If we list the abbreviations after the menu item, then allow
*        room

               if(menflg.eq.3) then
                  toffset = offset - mindif - 3
               else
                  toffset = offset
               end if
               shift = max(0,toffset-plen-len1-1)

*        Any parameters? If so write them to string now

               if(plen.gt.0) then
                  if(ndict.gt.0) len1 = len1 + shift
                  call chr_putc(current(:plen),chars,len1)
                  len1 = len1 + 1
               end if

*       Option to give abbreviations before ":" in square brackets

               if((menflg.eq.3) .and. (ndict.gt.0)) then
                  if(plen.le.0) len1 = len1 + shift + 1
                  call chr_putc('[',chars,len1)
                  call chr_putc(tdict(:mindif),chars,len1)
                  call chr_putc(']',chars,len1)
                  len1 = len1 + 1
               end if
               len1 = max(offset,len1)
               len2 = len1 + 1

*        Insert explanation of menu option, adding a colon if not
* present
*        in original

               if(colind.eq.0) call chr_putc(': ',chars,len1)
               call chr_appnd(current(ind2:),chars,len1)

*        Remove any multiple blanks in string after the ":"

               call tnw_ssblk(chars(len2:len1))
               len1 = ich_tidy(chars)

*        Finally output to user

               call par_wruser(chars(:len1),pstat)
               go = ndict.lt.entries
            end do

*     Just put a list of menu options on screen, but with abbreviations
*     indicated by case

         else if(menflg.eq.1) then

            call chr_fill(' ',chars)
            len1 = 0
            do ndict = 1, entries
               tdict = dict(ndict)
               tlen = chr_len(tdict)
               if((len1+tlen).gt.78) then
                  call par_wruser(chars(:len1),pstat)
                  len1 = 0
               end if
               if(len1.ne.0) call chr_putc(',',chars,len1)

*         What is the minimum abbreviation?

               oldmdf = 0
               mindif = 1
               do while((mindif.gt.oldmdf).and.(mindif.lt.tlen))
                  oldmdf = mindif
                  do i = 1, entries
                     if(i.ne.ndict) then
                        if(tdict(1:mindif).eq.dict(i)(1:mindif))
     :                       mindif = mindif + 1
                     end if
                  end do
               end do
               call chr_putc(tdict(:mindif),chars,len1)
               call chr_lcase(tdict)
               if(tlen.gt.mindif)
     :              call chr_putc(tdict(mindif+1:tlen),chars,len1)
            end do
            if(len1.gt.0) call par_wruser(chars(:len1),pstat)
         end if
      end if
      end
