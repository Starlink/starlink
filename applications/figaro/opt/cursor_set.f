      subroutine cursor_set(gstore,ncomp,ngauss,vbase,sdata,ifchng,
     :     curupd)
*+
* Name:
*    CURSOR_SET

* Invocation:
*    CALL CURSOR_SET(GSTORE,NCOMP,NGAUSS,VBASE,SDATA,IFCHNG,
*          CURUPD)

* Purpose:
*   Get guesses using graphics cursor

* Description:
*    This obtains guesses for Gaussian parameters from the user, for use
*   as guesses for optimisation. This is for the option to get them
*   using a graphics cursor.
*
* Arguments
*    SDATA(M) = REAL ARRAY (Given)
*        X array data (for region being dealt with)
*    VBASE(M) = REAL ARRAY (Given)
*        Variable component of base
*    NGAUSS = INTEGER (Given)
*        Number of gaussians
*    GSTORE(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Given and returned)
*        Store for guesses
*    NCOMP = INTEGER (Given and returned)
*        Number of component being worked on
*    IFCHNG(0:NGAUSS) = LOGICAL ARRAY (Returned)
*        If component altered (0=base)
*    CURUPD = LOGICAL (Returned)
*        If to update plot
* Global variables:
*    MPTS = INTEGER (Returned)
*        Number of channels in line (in opt_cmn)
*    TIMES = INTEGER (Returned)
*        Number of guesses (attempts!) (in opt_cmn)
*    MAX_CMP = INTEGER (Returned)
*        Maximum number of components (in opt_cmn)
* Notes:
*   All the arguments are scaled, that is to the range used for
*   optimisation. For this the "window" containing the data is scaled
*   to the range 0-1.
*
* Authors:
*   T.N.Wilkins Manchester
* History:
*   T.N.Wilkins Cambridge 25/4/90 Base added
*        "          "     20/11/90 Allow for variable base level
*        "          "     28/8/91 Allow change of component
*        "          "      9/6/92 Change to loops
*   A C Davenhall Edinburgh 19/12/00 Corrected the order of the arguments
*                         for RESCALE_PARS.
*-
* ----------------------------------------------------------------------
      implicit none
      include 'sae_par'
      include 'opt_cmn'
      integer MAX_PARS
      parameter (MAX_PARS=4)
      real gstore(MAX_PARS,max_cmp,times)
      integer ncomp,ngauss
      logical ifchng(0:ngauss),curupd
      real base
      real x,y
      character*23 chars
      character key,chr_upper
      real pars(4)
      double precision scaled_pars(4)
      integer status,pgcurse,rx2chn,ichn,pstat
      real sdata(mpts),vbase(mpts),savpar(4)
      logical loop,oloop,mode
      real left,right,dummy,dummy1,dummy2,ymin
      logical l_set,r_set,c_set,h_set,w_set,b_set,gen_similar
      real EFOLD
      integer option,ACCEPT,AGAIN,num,saved,i
      parameter (ACCEPT = 1, AGAIN=2)
      character*28 dict(3)
      integer dumi
      real dumr
      character dumc
      save savpar,saved,mode

* convert sigma to  half-width

      parameter (EFOLD = 2.35482004)
      character*2 bss,bsn
      data bss/'\\'/
      data dict/
     :     'ACCEPT : Accept these values',
     :     'AGAIN  : Try again',
     :     'QUIT   : Quit'/
      data mode/.false./
      bsn = bss(1:1)//'n'

* Tell user which component is current

      write(chars,'(''Current component is'',i2)')ncomp
      call par_wruser(chars,pstat)

* inquire if cursor available and get y range of plot-actually we
* only want the minimum in Y

      if(.not.curupd) saved = 0
      curupd = .false.
      status = SAI__OK
      call pgqwin(dummy,dummy1,ymin,dummy2)
      call gr_curin(2,status)
      oloop = status.eq.SAI__OK
      do while(oloop)
        scaled_pars(2) = dble(gstore(2,ncomp,times)*EFOLD)
        scaled_pars(3) = dble(gstore(3,ncomp,times))
        scaled_pars(4) = dble(gstore(4,ncomp,times))
        scaled_pars(1) = dble(gstore(1,ncomp,times))
        call rescale_pars(pars,scaled_pars,4,1)
        call par_wruser('H to set height',pstat)
        call par_wruser('C to set centre',pstat)
        call par_wruser('L to set left',pstat)
        call par_wruser('R to set right',pstat)
        call par_wruser('B to set base',pstat)
        call par_wruser('Number of component to change to',pstat)
        call par_wruser('U to update plot (accepting settings)',
     :       pstat)
        call par_wruser('M Toggle mode (update on change component)',
     :       pstat)
        call par_wruser('E to end',pstat)
        b_set = .false.
        w_set = .false.
        h_set = .false.
        c_set = .false.
        l_set = .false.
        r_set = .false.
*
        loop = .true.
        do while(loop)
          status = SAI__OK

*   Get values from user

          call sync
          status = pgcurse(x,y,key) - 1
          key = chr_upper(key)
          if(status.ne.SAI__OK) then
            status = SAI__OK
            call par_wruser('Error with cursor',pstat)
            loop = .false.
            oloop = .false.
          else
*
* See which key has been hit.
*
            if(key.eq.'H') then
              pars(3) = y-ymin
              h_set = .true.
            else if(key.eq.'C') then
              pars(4) = x
              c_set = .true.
            else if(key.eq.'L') then
              left = x
              l_set = .true.
            else if(key.eq.'R') then
              right = x
              r_set = .true.
            else if(key.eq.'E') then
              loop = .false.
              oloop = .false.
            else if(key.eq.'B') then
              b_set = .true.
              ichn = rx2chn(sdata,mpts,x)

*          Make base pass through this point

              pars(1)= y - vbase(ichn)
            else if(key.eq.'Q') then

*           Undo previous changes

              if(saved.ne.0) then
                call copr2r(4,savpar,gstore(1,saved,times))
                if(b_set) then
                  do i=1,ngauss
                    gstore(1,i,times) = savpar(1)
                  end do
                end if
                call par_wruser('Changes undone',pstat)
              else
                call par_wruser('No changes to undo',pstat)
              end if
            else if(key.eq.'M') then

*           Toggle mode

              mode = .not.mode

            else if(key.eq.'U') then

*           Update plot

              loop = .false.
              oloop = .false.
              curupd = .true.

            else
              read(key,'(i1)',iostat=status) num
              if(status.ne.SAI__OK) then
                status = SAI__OK
                call par_wruser('Unrecognised key',pstat)
              else if((num.gt.0).and.(num.le.ngauss)) then
                loop = .false.
                if(.not.mode) then
                  curupd = .true.
                end if
              else
                call par_wruser('Invalid component',pstat)
              end if
            end if
          end if
          saved = 0
        end do

*   If left and right given, then check if ok, reversing order if
*  neccesary.

        if(l_set.and.r_set) then
          if(right.lt.left) then
            dummy = left
            left = right
            right = dummy
            call par_wruser('Have reversed order of left and right'
     :           ,pstat)
          else if(gen_similar(left,right)) then
            call par_wruser('Error :- left = right - ignored both',
     :           pstat)
            l_set = .false.
            r_set = .false.
          end if
          if(l_set.and.r_set) then
            pars(2) = right-left
            w_set = .true.
            if(.not.c_set) then
              pars(4) = (left+right)/2.0
              c_set = .true.
            end if
          end if
        else if(l_set) then
          pars(2) = 2.0*(pars(4)-left)
          w_set = .true.
        else if(r_set) then
          pars(3) = 2*(right-pars(4))
          w_set = .true.
        end if

* Output values to screen

        if(h_set) then
          write(chars,'(''Height = '',f12.5)')pars(3)
          call par_wruser(chars,pstat)
        end if
        if(c_set) then
          write(chars,'(''Centre = '',f12.5)')pars(4)
          call par_wruser(chars,pstat)
        end if
        if(w_set) then
          write(chars,'(''Fwhm = '',f12.5)')pars(2)
          call par_wruser(chars,pstat)
        end if
        if(b_set) then
          write(chars,'(''Base = '',f12.5)')pars(1)
          call par_wruser(chars,pstat)
        end if

*   Check user is happy with values, scale them, and return

        if(curupd) then
          option = ACCEPT
        else if(oloop) then
          call par_wruser('Hit Q to reject these',pstat)
          option = ACCEPT
        else
          call qmenu('Cursor Accept',dict,3,1,dumr,dumc,option,dumi,
     :         status)

        end if
        if(option.eq.ACCEPT) then
          call scale_pars(pars,scaled_pars,4)

*     Save values so can use Q cursor option if oloop = .true.

          call copr2r(4,gstore(1,ncomp,times),savpar)
          saved = ncomp

          if(w_set) gstore(2,ncomp,times) = real(scaled_pars(2))/EFOLD
          if(h_set) gstore(3,ncomp,times) = real(scaled_pars(3))
          if(c_set) gstore(4,ncomp,times) = real(scaled_pars(4))
          if(b_set) then
            base = real(scaled_pars(1))
            do i=1,ngauss
              gstore(1,i,times) = base
            end do
          end if

*         Set flag to say we've changed something

          if(w_set.or.h_set.or.c_set) ifchng(ncomp) = .true.
          if(b_set) ifchng(0) = .true.
          if(oloop) then
            ncomp = num
            oloop = .not.curupd
            call dsa_wruser('Component now ')
            call dsa_wruser(key)
            call dsa_wruser(bsn)
          end if
        else if(option.eq.AGAIN) then
          oloop = .true.
        end if
      end do
      end
