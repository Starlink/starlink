      subroutine edit_list(left,right,lincnt,wavelength,line_name)
*+
* Name:
*    EDIT_LIST

* Invocation:
*    CALL EDIT_LIST(LEFT,RIGHT,LINCNT,WAVELENGTH,LINE_NAME)

* Purpose:
*   Edit line list

* Description:
*       Allows a specimen list of regions of a spectrum containing
*       lines which are to be fitted,and which have been indetified
*       in LINPICK to be modified.
*
*       There are several options:-
*
*    AD   include a new line i.e extend beyond LINCNT
*
*    CH   Change a currently defined line. It is possible
*         to enter a new wavelength but its ID will be
*         set to 'OTHER' to remind user of change.
*
*    RE   Rename line
*
*    LI   List lines
*
*    PU   Purge a defined line.
*
*    BO   Alter the boundaries of a defined line.
*
*    LEFT(NYP) = REAL ARRAY (Given)
*        Left trams
*    RIGHT(NYP) = REAL ARRAY (Given)
*        Right trams
*    LINCNT = INTEGER (Given)
*        Line count
*    WAVELENGTH(NYP) = REAL ARRAY (Given)
*        Line wavelengths
*    LINE_NAME(NYP) = CHARACTER*10 ARRAY (Given)
*        Line names

* Authors:
*    T.N.Wilkins Cambridge until 9/92, then Durham
*    ACD: A C Davenhall, Starlink, Edinburgh

* History:
*   TNW: RE option made to work, 30/6/89
*   TNW: 2/8/92 Allow full English words in menu,
*   ACD: 28/9/00 Remove character string continued across continuation
*     lines.
* __________________________________________________________________
*
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
* common
*
      integer status
      include 'arc_dims'

      real left(nyp),right(nyp)
      character*10 line_name(nyp)
      real wavelength(nyp)
      integer lincnt
*-
      logical loop,valid
*
* menus
*
      integer NDICT
      parameter (NDICT = 7)
      character*64 dictedit(NDICT)
*
* integer
*
      integer i,m,n,in,nav,ilen,nnums,pstat
      integer chr_len
*
* character
*
      character*10 purged
      character*5 other
      character*72 chars

      real values(3)
      real wave
      integer OPT_AD, OPT_CH, OPT_PU, OPT_BO, OPT_RE, OPT_LI, OPT_EX
      parameter (OPT_AD = 1, OPT_CH = 2, OPT_PU = 3, OPT_BO = 4,
     :     OPT_LI = 5, OPT_RE = 6, OPT_EX = 7)
*
* data statments
*
      data dictedit/
     :     'ADD %FW %FL %FR : Add line, wavelength W, boundaries L & R'
     :     ,'CHANGE %FN %FW : Change line N to wavelength W',
     :     'PURGE %FN   : Purge line N from list',
     :     'BOUNDARIES %FN %FL %FR : alter Boundaries of line N',
     :     'LIST         : List  lines',
     :     'RENAME %FN %T(name) : Rename line',
     :     'EXIT         : List lines and exit'/
      data purged,other/'purged    ','other'/

      loop = .true.
      status = SAI__OK

      do while( loop )
        nnums = 3
        call qmenu('Edit List',dictedit,NDICT,OPT_LI,values,chars,m,
     :       nnums,status)

*   abort

        if( status.ne.SAI__OK ) then
          loop = .false.
*
* if "exit" remove lines that have been purged
*
        else if( m .eq. OPT_EX ) then
          in = 0
          do i = 1 , lincnt
            if( line_name(i) .ne. purged ) then
              in = in+1
              line_name(in)  = line_name(i)
              wavelength(in) = wavelength(i)
              left(in)=left(i)
              right(in)=right(i)
              call shift(in,i,%VAL( CNF_PVAL(d_rptr) ),
     :               %VAL( CNF_PVAL(d_cptr) ), %VAL( CNF_PVAL(d_mptr) ))
            end if
          end do
          do i =(in+1),lincnt
            left(i) = 0.0
            right(i) = 0.0
          end do
          lincnt = in
          loop   = .false.

*   change line

        else if( m .eq. OPT_CH ) then
*
*   Evaluate numeric parameters
*
          n    = nint(values(1))
          wave = values(2)
*
* Outside defined entries
*
          if(valid(1,lincnt,n,'Slot')) then
            line_name(n)  = 'changed'
            wavelength(n )= wave
          end if
*
*  ** 'ad' section
*
        else if(m.eq.OPT_AD) then
          wave = values(1)
          n    = lincnt+1
          if(n.gt.nyp) then
            call par_wruser('table is full',pstat)
            loop = .false.
          else
            left(n)       = values(2)
            right(n)      = values(3)
            wavelength(n) = wave
            line_name(n)  = other
            lincnt        = lincnt+1
            write(chars,'(a,i4,a,f10.2)') ' line',N,' included as',
     :           wave
            call par_wruser(chars,pstat)

*       n.gt.nyp

          end if
*
*  "pu" section
*
        else if(m.eq.OPT_PU) then
*
* error
*
          n=nint(values(1))
          if(valid(1,lincnt,n,'Slot')) then
            if( line_name(n) .eq. purged ) then
              call par_wruser('Line already purged',pstat)
            else
              line_name(n)  = purged
              wavelength(n) = -1.0
              write(chars,'(a,i4,a)') 'line ',n,' purged'
              call par_wruser(chars,pstat)
            end if
          end if
*
* bo section
*
        else if(m.eq.OPT_BO) then
          n=nint(values(1))
          if(valid(1,lincnt,n,'Slot')) then
            left(n)  = values(2)
            right(n) = values(3)
            write(chars,'(a,i4,a,f6.1,1x,f6.1)') ' line',N,
     +           ' boundaries changed to ',left(n),right(n)
            call par_wruser(chars,pstat)
          end if

        else if(m.eq.OPT_RE) then

*     Rename

          n = nint(values(1))
          if(valid(1,lincnt,n,'Slot')) then
            ilen = chr_len(chars)
            if(ilen.gt.10) then
              ilen = 10
              call par_wruser('Name truncated',pstat)
            end if
            line_name(n) = chars(:ilen)
          end if

*   m

        end if
*
* "LI" SECTION
*
        call par_wruser(
     :       '     U P D A T E D   L I N E  L I S T',pstat)
        call underscore
        call par_wruser('     NUMBER    BOUNDARIES          NAME'/
     :    /'     WAVELENGTH',pstat)
        call par_wruser('              LEFT     RIGHT',pstat)
        call par_wruser(' ',pstat)
        do  i=1,lincnt
          write(chars,'(5x,i4,2(3x,f6.1),5x,a10,5x,f14.4)')
     :         i,left(i),right(i),line_name(i),wavelength(i)
          call par_wruser(chars,pstat)
        end do
        nav=nyp-lincnt
        write(chars,'(a,i4)') 'Number of free slots = ',nav
        call par_wruser(chars,pstat)

* loop

      end do
      end
