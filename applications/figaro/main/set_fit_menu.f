      subroutine set_fit_menu(redraw,instant,deccntr,wavdim,gpscal
     :     ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss,line_count
     :     ,errpre,inherit,status)
*+
* Name:
*    SET_FIT_MENU

* Invocation:
*    CALL SET_FIT_MENU(REDRAW,INSTANT,DECCNTR,WAVDIM,GPSCAL,PRVFIT
*           ,USEPEAK,BIMTST,TYAIC,CURMCMP,PRVPOS,MGAUSS,LINE_COUNT
*          ,ERRPRE,INHERIT,STATUS)

* Purpose:
*   Set fitting options.

* Description:
*   Change parameters to determine details of fitting. The user can
*   select the various fits type from menus. There is a main menu (on
*   which are displayed the current settings), and additional menus called from
*   this to handle specific area such as which optimisation routines to
*   use.

* Arguments:
*     REDRAW = LOGICAL (Given and returned)
*        If to redraw line profile
*     INSTANT = LOGICAL (Given and returned)
*        Perform fits immediately
*     DECCNTR(*) = INTEGER ARRAY (Given and returned)
*        Fit coding
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*     WAVDIM = INTEGER (Given)
*        Number of channels in data
*     GPSCAL = LOGICAL (Given and returned)
*        If to scale previous results when using them for multiple
*        guessing
*     PRVFIT = LOGICAL (Given and returned)
*        If to get previous results from previous block
*     USEPEAK = LOGICAL (Given and returned)
*        If to get centre from peak (rather than centriod), this is for
*        singles.
*     BIMTST = LOGICAL (Given and returned)
*        If to test for bimodality
*     TYAIC = INTEGER (Given and returned)
*        Type of AIC testing
*     CURMCMP = INTEGER (Given and returned)
*        Current maximum number of components
*     PRVPOS = INTEGER (Given and returned)
*        Where to use previous fits from
*     MGAUSS = INTEGER (Given)
*        Maximum number of components allowed by results structure
*     LINE_COUNT = INTEGER (Given)
*        Number of lines identified
*     ERRPRE = LOGICAL (Given)
*        If error array present
*     INHERIT = INTEGER (Given and returned)
*        Where to inherit fits from (take guesses from previous fits)
*
*  Global variables:
*     DEFAULT_MODEL(MAX_DECODE_CONTROL) = INTEGER ARRAY (Given and Returned)
*        Decoded control  (include file status_inc)

* Authors:
*   TNW: T.N.Wilkins, Cambridge until 9/92, then Durham
*   AJH: A.J. Holloway Manchester 97+

* History:
*   TNW: current version 18-APR-1991
*   TNW: Altered 17,18-SEP-1991 for new FIT_LINE and more easily
*        followed code!
*   TNW: October 1991 More options
*   TNW: November 1991 More changes!
*   TNW: 4-5th February 1992 set_fit and fits_menu combined into
*        set_fit_menu.
*   TNW: 27-28th February 1992 Made into smaller menus
*   TNW: 10th June 1992 Bug fixes, re output text
*   TNW: 31st July 1992 Bug fixes, re absorption/emission
*   TNW: 14th May 1993 Bug fix for when no fit defined
*   TNW: 10th Dec 1993, Reduced maximum number of continuation lines
*        below Sun default
*   AJH: 1st Nov 1997, Remove fitting options not supported under PDA.
*   ACD: 28/9/00 Remove local unused variables.
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'fit_coding_inc'
      integer wavdim,tyaic,curmcmp,prvpos,mgauss,line_count,inherit
      logical gpscal,prvfit,usepeak,bimtst,errpre
      integer ib,ib2
      logical redraw
      logical instant
      integer status
      logical loop
      integer NDICT
* remove blank line
*      parameter (NDICT = 22)

      parameter (NDICT = 19)
      integer nwelm,slot,ptr0
      integer kp1,MAX_KP1
      parameter (MAX_KP1 = 20)
      double precision coeffs(MAX_KP1)
      character*79 dict(NDICT)
      integer NBASMN
      parameter (NBASMN = 5)
      character*49 basmen(NBASMN)
      integer NGESMN
      parameter (NGESMN = 7)
      character*54 gesmen(NGESMN)
      integer NTESMN
      parameter (NTESMN = 6)
      character*44 tesmen(NTESMN)
      character*49 chars
      integer i,moditm,typitm
      logical useerr,bounds
      integer dumi,nnums,ivalue,chr_len
      real dumr
      character dumc
      logical valid
      integer OPT_GUESS, OPT_BASE, OPT_BIMODAL, OPT_EXIT,
     :     OPT_INSTANT, OPT_ABS, OPT_TESTS, OPT_OPT, OPT_NO,
     :     OPT_TRANSFER, OPT_GAUSSIAN, OPT_SKEW, OPT_CAUCHY,
     :     OPT_LORENTZIAN, OPT_TSEPARATION, OPT_SINGLE,
     :     OPT_DOUBLE, OPT_MANUAL, OPT_AUTO, OPT_PCYGN
*      parameter (OPT_GUESS = 1,  OPT_BASE = 2, OPT_BIMODAL = 3,
*     :     OPT_INSTANT = 4, OPT_ABS = 5, OPT_TESTS = 6, OPT_OPT = 7,
*     :     OPT_NO = 8,
*     :     OPT_GAUSSIAN = 9, OPT_SKEW = 10, OPT_CAUCHY = 11,
*     :     OPT_LORENTZIAN = 12, OPT_SINGLE = 13, OPT_DOUBLE = 17,
*     :     OPT_TSEPARATION = 14, OPT_MANUAL = 18,
*     :     OPT_AUTO = 19, OPT_PCYGN = 20, OPT_TRANSFER = 21,
*     :     OPT_EXIT = NDICT)
      parameter (OPT_GUESS = 1,  OPT_BASE = 2, OPT_BIMODAL = 3,
     :     OPT_INSTANT = 4, OPT_ABS = 5, OPT_TESTS = 6, OPT_OPT = 97,
     :     OPT_NO = 7,
     :     OPT_GAUSSIAN = 8, OPT_SKEW = 99, OPT_CAUCHY = 9,
     :     OPT_LORENTZIAN = 10, OPT_SINGLE = 11, OPT_DOUBLE = 15,
     :     OPT_TSEPARATION = 12, OPT_MANUAL = 16,
     :     OPT_AUTO = 17, OPT_PCYGN = 918, OPT_TRANSFER = 18,
     :     OPT_EXIT = NDICT)


      integer OPT_MAXCEN, OPT_PREV, OPT_PRESENT, OPT_NEXT, OPT_SCRATCH,
     :     OPT_RESCALE
      parameter (OPT_MAXCEN = 1, OPT_PREV = 2, OPT_PRESENT = 3,
     :     OPT_NEXT = 4, OPT_SCRATCH = 5, OPT_RESCALE = 6)
      integer OPT_MAXCMP, OPT_NOAIC, OPT_POSTAIC, OPT_PREAIC, OPT_TOLS
      parameter (OPT_MAXCMP = 1, OPT_NOAIC = 2, OPT_POSTAIC = 3,
     :     OPT_PREAIC = 4, OPT_TOLS = 5)
      real value
      integer ilen,pos1,pos2
      character*10 em_abs(0:1)
      integer ncmps(0:5)
C      character*2 rout(3)
      integer menlen(NDICT)
      logical ifpcyg

* Menu

      data dict/
     :     'GUESS : Alter guessing','BASE : Alter base model'
     :     ,'BIMODAL    :       Test for bimodality before fitting'
     :     ,'INSTANT    : Define fits only'
     :     ,'ABSORPTION : Fit ABSORPTION lines'
     :     ,'TESTS      : Alter tests for auto'
*     :     ,'ROUTINES   : Select fitting routines ......... Sing - '
* remove blank line
*     :     ,'                                                      '
     :     ,'NOFIT      : Don''t fit this position'
     :     ,'GAUSSIAN   : Fit Gaussian(s)'
*     :     ,'SKEW       : Fit a single Skew Gaussian'
*     :     ,'                                       '
     :     ,'CAUCHY     : Fit a single Cauchy function'
     :     ,'LORENTZIAN : Fit Lorentzian(s)','SINGLE : Fit single(s)'
     :     ,'TSEP       : Fit two Gaussians with fixed separation'
     :     ,'TWIDTH     : Fit two Gaussians with fixed width ratio'
     :     ,'THEIGHT    : Fit two Gaussians with fixed height ratio'
     :     ,'DOUBLE : Fit double(s)','MANUAL : Perform manual fitting'
     :     ,'AUTO       : Automatic multiple Gaussian/Lorentzian'
*     :     ,'PCYGNI     : P Cygni profile'
*     :     ,'                            '
     :     ,'TRANSFER %F : Start from fit to another line'
     :     ,'EXIT       : Exit menu and perform any fitting'/

* remove blank line
*      data menlen/28,30,54,30,34,36,54,40,29,40,42,31,27,53,54,55,27,
*     :     36,54,29,44,47/

*      data menlen/28,30,54,30,34,36,40,29,42,31,27,53,54,55,27,
*     :     36,54,44,47/

      data menlen/28,30,54,30,34,34,37,29,42,31,27,53,54,55,27,
     :     36,52,44,47/


*  Base menu

      data basmen/'CONSTANT : Constant base',
     :     'SPLINE  : Cubic spline base',
     :     'CHEBYSHEV %f(order) : Fit Chebyshev polynomial',
     :     'FITCONT : Use Chebyshev coefficients from FITCONT',
     :     'QUIT : Don''t change'/

*  Guess menu

      data gesmen/
     :     'MAXIMUM : Use MAXIMUM  as guess for centre (singles)',
     :     'PREVIOUS: Start from fit to previous block (multiples)',
     :     'PRESENT : Start from fit to present block (multiples)',
     :     'NEXT : Start from fit to next block (multiples)',
     :     'SCRATCH : Guess from scratch',
     :     'RESCALE :       Rescale fits (multiples)',
     :     'QUIT : Don''t change'/

*  Tests menu

      data tesmen/
     :     'MAXCMP %F : Set maximum number of components',
     :     'NOAIC : Don''t use AIC',
     :     'POSTAIC : Apply AIC after fitting',
     :     'PREAIC : Apply AIC before fitting',
     :     'TOLS : Alter tolerance values',
     :     'QUIT : Don''t change'/

* Tolerance names

      data em_abs/'ABSORPTION','EMISSION'/
      data ncmps/0,1,2,2,2,2/
C      data rout/'N1','N2','LM'/
* ----------------------------------------------------------------------

* Get workspace for character string to output current settings

      ifpcyg = deccntr(FIT_GUES).eq.GUES_PCYG
      loop = status.eq.SAI__OK

* Loop around until the user is satisfied

      do while(loop)

*  Clear any previous info on end of menu items

        do i = 1, NDICT
          dict(i)(menlen(i)+1:) = ' '
        end do

*  Test not in transfer mode-in which case coding is completely different!

        if(deccntr(MAX_DECODE_CONTROL).ge.0) then

*     Set menu to reflect current state of flags

* ....base

          call chr_fill(' ',chars)
          chars = basmen(deccntr(BACK_MODEL))
          ilen = index(chars,'%')
          pos1 = index(chars,':')
          if(ilen.ne.0) then
            ilen = min(ilen,pos1)
          else
            ilen = pos1
          end if
          ilen = chr_len(chars(:ilen-1))
          if(deccntr(BACK_MODEL).eq.CHEBYSHEV) then
            ilen = ilen + 1
            call chr_putc('(order = ',chars,ilen)
            call chr_puti(deccntr(BACK_ORDER),chars,ilen)
            call chr_putc(')',chars,ilen)
          end if
          pos1 = menlen(OPT_BASE) + 1

          pos2 = 72 - ilen
          call chr_fill('.',dict(OPT_BASE)(pos1:pos2))
          pos2 = pos2 + 1
          call chr_putc(chars(:ilen),dict(OPT_BASE),pos2)

* ....tests

          call chr_fill(' ',chars)
          ilen = 0
          if(tyaic.eq.PREAIC) then
            call chr_appnd('AIC before fits',chars,ilen)
          else if(tyaic.eq.POSTAIC) then
            call chr_appnd('AIC after fits',chars,ilen)
          else
            call chr_appnd('AIC not used',chars,ilen)
          end if
          call chr_putc(' (max cmps ',chars,ilen)
          call chr_puti(curmcmp,chars,ilen)
          call chr_putc(')',chars,ilen)
          pos1 = menlen(OPT_TESTS) + 1
          pos2 =  72 - ilen
          call chr_fill('.',dict(OPT_TESTS)(pos1:pos2))
          pos2 = pos2 + 1
          call chr_putc(chars(:ilen),dict(OPT_TESTS),pos2)


*  Absorption or emission?

          dict(OPT_ABS)(:10) = em_abs(deccntr(FIT_ABS))
          dict(OPT_ABS)(18:27) = em_abs(deccntr(FIT_ABS))

*  Guessing

          ilen = 0
          call chr_putc('Multiples - ',chars,ilen)
          if(prvfit) then
            pos1 = index(gesmen(OPT_PRESENT + prvpos),':')-1
            call chr_appnd(gesmen(OPT_PRESENT + prvpos)(:pos1),
     :           chars,ilen)
          else
            call chr_putc('SCRATCH',chars,ilen)
          end if
          call chr_putc(', Singles - ',chars,ilen)
          if(usepeak) then
            call chr_putc('MAXIMUM',chars,ilen)
          else
            call chr_putc('CENTROID',chars,ilen)
          end if
          pos1 = menlen(OPT_GUESS) + 1

          pos2 = 72 - ilen
          call chr_fill('.',dict(OPT_GUESS)(pos1:pos2))
          pos2 = pos2 + 1
          call chr_putc(chars(:ilen),dict(OPT_GUESS),pos2)

* Current model/type

          moditm = deccntr(FIT_MODEL)

          if(moditm.eq.LORENTZ_MODEL) then
             moditm = moditm - 2
          else
             if(moditm.eq.CAUCHY_MODEL) moditm = moditm - 1
          end if

          if(moditm.ge.0) moditm = moditm - 1 + OPT_GAUSSIAN

          typitm = deccntr(FIT_TYPE)
          if(typitm.gt.0) typitm = typitm - 1 + OPT_SINGLE
          if((deccntr(FIT_TYPE).eq.MULTIPLE).and.
     :         (deccntr(FIT_MAN).eq.MAN_NOALTER)) then
            typitm = OPT_AUTO
          end if
          if(ifpcyg) typitm = OPT_PCYGN

*  Handle case of no fit defined

          if(typitm.eq.0) then
             call chr_fill('.',dict(OPT_NO)(menlen(OPT_NO)+1:67))
             dict(OPT_NO)(69:75) = '*******'
          else
             call chr_fill('.',dict(typitm)(menlen(typitm)+1:67))
             call chr_fill('.',dict(moditm)(menlen(moditm)+1:67))
             dict(moditm)(69:75) = '*******'
             dict(typitm)(69:75) = '*******'
          end if

*          dict(OPT_OPT)(menlen(OPT_OPT):) = rout(opt_routines(1))//
*     :         ', Dble - '//rout(opt_routines(2))//', Mult - '//
*     :         rout(opt_routines(3))

*          dict(OPT_OPT)(menlen(OPT_OPT):) = '  , Dble -   , Mult -   '
* remove blank line
*          dict(OPT_OPT)(menlen(OPT_OPT):) = '                        '


       else

          ilen = menlen(OPT_TRANSFER)
          call chr_puti(' = ',dict(OPT_TRANSFER),ilen)
          call chr_puti(-deccntr(MAX_DECODE_CONTROL),dict(OPT_TRANSFER)
     :         ,ilen)

       end if

*  Display bimodality test before fitting?

        if(bimtst) then
          dict(OPT_BIMODAL)(14:20) = 'Don''t t'
        else
          dict(OPT_BIMODAL)(14:20) = '      T'
        end if

*  Perform fits now or just define them?

        if(instant) then
          dict(OPT_INSTANT)(14:) = 'Define fits only'
        else
          dict(OPT_INSTANT)(14:) = 'Perform fits immediately'
        end if

*    Get answer

        call qmenu('Fit Menu',dict,NDICT,OPT_EXIT,value,dumc,ib,nnums,
     :       status)

*    Act on response. First re-load default model if we're switching
*    from transfer mode

        if((ib.gt.OPT_NO).and.(ib.le.OPT_PCYGN).and.
     :       (deccntr(MAX_DECODE_CONTROL).lt.0)) then
          do i = 1, MAX_DECODE_CONTROL
            deccntr(i) = default_model(i)
          end do
        end if

        if((ib.eq.OPT_EXIT).or.(status.ne.SAI__OK)) then
          loop = .false.

*  Guessing options

        else if(ib.eq.OPT_GUESS) then
          if(usepeak) then
            gesmen(OPT_MAXCEN)(:8) = 'CENTROID'
          else
            gesmen(OPT_MAXCEN)(:8) = 'MAXIMUM'
          end if
          gesmen(OPT_MAXCEN)(15:22) = gesmen(OPT_MAXCEN)(:8)
          if(gpscal) then
            gesmen(OPT_RESCALE)(11:17) = 'Don''t r'
          else
            gesmen(OPT_RESCALE)(11:17) = '      R'
          end if
          call qmenu('Guess Menu',gesmen,NGESMN,NGESMN,dumr,dumc,ib2,
     :         dumi,status)
          if(ib2.eq.OPT_MAXCEN) then
            usepeak = .not.usepeak
          else if((ib2.ge.OPT_PREV).and.(ib2.le.OPT_NEXT)) then
            prvfit = .true.
            prvpos = ib2 - OPT_PRESENT
          else if(ib2.eq.OPT_SCRATCH) then
            prvfit = .false.
          else if(ib2.eq.OPT_RESCALE) then
            gpscal = .not.gpscal
          end if

*  Items not directly fitting

        else if(ib.eq.OPT_BIMODAL) then
          bimtst = .not.bimtst
        else if(ib.eq.OPT_INSTANT) then
          instant = .not.instant

*  If absorption or emission

        else if(ib.eq.OPT_ABS) then
          deccntr(FIT_ABS) = mod(deccntr(FIT_ABS)+1,2)
        else if(ib.eq.OPT_TESTS) then

*      Tests

          call qmenu('Tests menu',tesmen,NTESMN,NTESMN,value,dumc,ib2,
     :         nnums,status)
          if(ib2.eq.OPT_MAXCMP) then
            ivalue = nint(value)
            if(valid(1,mgauss,ivalue,'Number of components'))
     :           then
              curmcmp = ivalue
            end if
          else if((ib2.eq.OPT_NOAIC).or.(ib2.eq.OPT_POSTAIC).or.
     :           (ib2.eq.OPT_PREAIC)) then
            tyaic = ib2 - OPT_NOAIC
          else if(ib2.eq.OPT_TOLS) then
            call set_guess_tol(status)
          end if
        else if(ib.eq.OPT_OPT) then

*     Optimisation routines to use

          call set_routines(1,deccntr,status)
        else if(ib.eq.OPT_BASE) then

*     Base model

          call qmenu('Base menu',basmen,NBASMN,NBASMN,value,dumc,ib2,
     :         nnums,status)
          if(ib2.ne.NBASMN) deccntr(BACK_MODEL) = ib2
          if(deccntr(BACK_MODEL).EQ.CHEBYSHEV) then
            if(nnums.eq.0) then

*         check initialisation

              kp1 = deccntr(BACK_ORDER) + 1

*       Get order for Chebyshev fitting

              nwelm = max_kp1*max_kp1 + wavdim*6 + max_kp1*3
              call dsa_get_work_array(nwelm,'double',ptr0,slot,status)
              call cheby_base(max_kp1,coeffs,kp1,.true.,
     :                        %VAL(CNF_PVAL(ptr0)),status)
              call dsa_free_workspace(slot,status)
              deccntr(BACK_ORDER) = kp1 - 1
              redraw = .true.
            else if(valid(1,19,nint(value),'Order')) then
              deccntr(BACK_ORDER) = nint(value)
            end if
          end if
        else if(ib.eq.OPT_TRANSFER) then
          ivalue = nint(value)
          if(valid(1,line_count,ivalue,'Line')) then
            call zero_int(deccntr,MAX_DECODE_CONTROL)
            deccntr(MAX_DECODE_CONTROL) = -nint(value)
          end if

*   Various fit models, some only available as singles at present.

        else if(ib.eq.OPT_NO) then
          deccntr(FIT_MODEL) = 0
          deccntr(FIT_TYPE) = 0
        else if(ib.eq.OPT_GAUSSIAN) then
          deccntr(FIT_MODEL) = GAUSSIAN_MODEL
        else if(ib.eq.OPT_SKEW) then
          deccntr(FIT_MODEL) = SKEW_MODEL
          deccntr(FIT_TYPE) = SINGLE
        else if(ib.eq.OPT_CAUCHY) then
          deccntr(FIT_MODEL) = CAUCHY_MODEL
          deccntr(FIT_TYPE) = SINGLE
        else if(ib.eq.OPT_LORENTZIAN) then
          deccntr(FIT_MODEL) = LORENTZ_MODEL

*   Various types of fits (single, double, etc.):

        else if(ib.eq.OPT_SINGLE) then
          deccntr(FIT_TYPE) = SINGLE
        else if((ib.ge.OPT_TSEPARATION).and.(ib.le.OPT_DOUBLE)) then

*    Doubles

          deccntr(FIT_GUES) = BIMODF
          if(ib.eq.OPT_DOUBLE) then
            deccntr(FIT_TYPE) = DOUBLE_U
          else
            deccntr(FIT_MODEL) = GAUSSIAN_MODEL
            deccntr(FIT_TYPE) = DOUBLE_FS + ib - OPT_TSEPARATION
          end if
        else if(ib.eq.OPT_MANUAL) then
          deccntr(FIT_TYPE) = MULTIPLE
          deccntr(FIT_MAN) =  MAN_ALTER
        else if(ib.eq.OPT_AUTO) then
          deccntr(FIT_TYPE) = MULTIPLE
          deccntr(FIT_MAN) =  MAN_NOALTER
        else if(ib.eq.OPT_PCYGN) then
          deccntr(FIT_TYPE) = DOUBLE_U
          ifpcyg = .true.
        end if
        if(ifpcyg.and.(deccntr(FIT_TYPE).ne.DOUBLE_U)) ifpcyg = .false.
      end do

*   Extract relevant information from ARC_DIMS common block, and make sure that
*   the values in DECCNTR are vaguely sensible

      deccntr(FIT_WEIGH) = 0
      if(errpre) then
        call par_rdkey('weights',.true.,useerr)
        if(useerr) deccntr(FIT_WEIGH) = 1
      end if
      deccntr(BACK_WEIGH) = deccntr(FIT_WEIGH)

* Make sure guessing matches model

      if(ifpcyg) then
        deccntr(FIT_GUES) =  GUES_PCYG
      else if(prvfit.and.(deccntr(FIT_TYPE).eq.MULTIPLE)) then
        deccntr(FIT_GUES) = 5 + prvpos
      else if((usepeak.and.(deccntr(FIT_TYPE).eq.SINGLE)).or.
     :       (deccntr(FIT_TYPE).eq.MULTIPLE)) then
        deccntr(FIT_GUES) = PEAK
      else if(inherit.eq.1) then
        deccntr(FIT_GUES) = 5
      else if(inherit.eq.-1) then
        deccntr(FIT_GUES) = 4
      else if(deccntr(FIT_TYPE).eq.1) then
        deccntr(FIT_GUES) = CENTROID
      end if

* Bounded fits and AIC (basically for batch use)-multiples

      if(deccntr(FIT_TYPE).eq.MULTIPLE) then
        deccntr(FIT_OPT) = opt_routines(3)

        if(deccntr(FIT_MAN).eq.MAN_NOALTER) then

*   See if to constrain fits during optimisation for automatic multiple
*   fitting (applicable to batch mode).

          call par_rdkey('bounds',.false.,bounds)

          if(bounds) deccntr(FIT_CONSTR) = 1

* Also see if to use Akaike's information criterion to control fitting
* 0 - don't use
* 1 - use after fitting
* 2 - use after guessing, but before fitting

          deccntr(FIT_STST) = tyaic
        end if
        deccntr(FIT_NCMP) =  curmcmp
      else

        deccntr(FIT_NCMP) = ncmps(deccntr(FIT_TYPE))

        if(deccntr(FIT_TYPE).eq.SINGLE) then
          deccntr(FIT_OPT) = opt_routines(1)
        else if(deccntr(FIT_TYPE).gt.DOUBLE_FH) then
          deccntr(FIT_OPT) = opt_routines(2)
        else

*       Tied doubles, only available for E04GBF.

          deccntr(FIT_OPT) = 1
        end if
        deccntr(FIT_MAN) = MAN_NOALTER
      end if
      if(deccntr(MAX_DECODE_CONTROL).gt.0) then
        do i = 1, MAX_DECODE_CONTROL
          default_model(i) = deccntr(i)
        end do
      end if
      end
