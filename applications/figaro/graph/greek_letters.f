      subroutine greek_letters(instring,outstring,lens,upper)
*+
* Name:
*    GREEK_LETTERS

* Invocation:
*    CALL GREEK_LETTERS(INSTRING,OUTSTRING,LENS,UPPER)

* Purpose:
*   To change a Greek letter written as a word to a PGPLOT code for
*   that letter.

* Description:
*   To change a Greek letter written as a word to a PGPLOT code for
*   that letter (lowercase).
*   The word for the letter must be in uppercase and terminated by a
*   space or the end of the string. The strings cannotbe more that 80
*   characters long.
*
* Arguments:
*    INSTRING = CHARACTER*(*) (Given)
*        Input string
*    UPPER = LOGICAL (Given)
*        if uppercase
*    OUTSTRING = CHARACTER*(*) (Returned)
*        Output string
*    LENS = INTEGER (Returned)
*        length of output string
* History:
*    T.N.Wilkins Manchester, Nov 1986
*         "      Cambridge, 5 April 1990 PGPLOT version working
*         "          "      3 Oct 1991 Only use "logical" length of input
*                           string
*-
      implicit none
      character*(*) instring,outstring
      logical upper
      integer lens

      character*80 work
      character*7 letter(24)
      character*1 dummy,bs,bs2*2
      logical error
      integer i,ind,lenlet,j,chr_len,lenmax
      integer pstat,len1
      data letter/'ALPHA','BETA','GAMMA','DELTA','EPSILON','ZETA','ETA'
     :  ,'THETA','IOTA','KAPPA','LAMBDA','MU','NU','XI','OMICRON','PI',
     :  'RHO','SIGMA','TAU','UPSILON','PHI','CHI','PSI','OMEGA'/
      data bs2/'\\'/
      bs = bs2(1:1)

      call chr_fill(' ',outstring)
      lens = chr_len(instring)
      outstring = instring
      lenmax = len(outstring)
      i = 1
      do while(i.le.24)

* See if letter present

        lenlet=chr_len(letter(i))
        ind = index(outstring,letter(i)(:lenlet))
        if(ind.ne.0) then

* check followed by space or end of string

          if(ind+lenlet.gt.lens) then
            error = .false.
          else
            error = outstring((lenlet+ind):(lenlet+ind)).ne.' '
          end if

* check ETA is not realy THETA, ZETA or BETA.

          if(i.eq.7)then
            if(ind.gt.1) then
              dummy=outstring(ind-1:ind-1)
              if(dummy.eq.'B') error = .true.
              if(dummy.eq.'Z') error = .true.
              if(dummy.eq.'H') then
                if(ind.gt.2) then
                  if(outstring(ind-2:ind-2).eq.'T') error = .true.
                end if
              end if
            end if
          end if

* No confusion with ETA!!

          if(.not.error) then
            len1 = 0
            call chr_fill(' ',work)
            call chr_putc(outstring(:ind-1),work,len1)
            call chr_putc(bs,work,len1)
            call chr_putc('(',work,len1)
            if(upper) then
              j =  526 + i
            else
              j =  626 + i
            end if
            call chr_puti(j,work,len1)
            call chr_putc(')',work,len1)
            call chr_appnd(outstring(ind+lenlet:),work,len1)
            if(len1.gt.lenmax) then
              call par_wruser('String truncated',pstat)
            end if
            outstring = work(:lenmax)
            lens = len1
          end if

*   try next letter

        else
          i = i + 1
        end if

* loop over letters

      end do
      end
