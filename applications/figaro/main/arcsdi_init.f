      subroutine arcsdi_init(nxpdim,old,status)
*+
* Name:
*    ARCSDI_INIT

* Invocation:
*    CALL ARCSDI_INIT(NXPDIM,OLD,STATUS)

* Purpose:
*   To perform initialisation for ARCSDI and COMB.
* Description:
*   The common blocks are initialised, the defaults read in, the input data
*   file is opened and the mode is obtained from the user.
* Arguments:
*      NXPDIM = INTEGER (Given)
*        Number of dimension to set nxp equal to
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      OLD = LOGICAL (Returned)
*        If in OLD mode
* Subroutines/functions referenced:
*      ARCSDI_OPTS      : Get fitting options
*      ARC_INIT         : Initialise common blocks
*      TWO_OPEN         : Open data file
*      PAR_RDKEY        : Get value of keyword parameter
*      PAR_WRUSER       : Write string to terminal/log file

* History:
*   T.N.Wilkins, Cambridge, 20-SEP-1989
*   T.N.Wilkins, Cambridge, 15-NOV-1989 arcsdi_opts combined into this
*   routine.
*-
      implicit none
      include 'SAE_PAR'
      integer nxpdim
      logical old
      integer rdmenu
      integer mode
      integer status
      include 'arc_dims'
      integer pstat,NDICT
      parameter (NDICT = 4)
      character*6 dict_arc(NDICT)
      character*32 menlis(NDICT)
      include 'opt_cmn'
      data dict_arc/'NEW','REPEAT','CLONE','OLD'/
      data menlis/
     :     ': Set up a new analysis',
     :     ': Iterate on previous results',
     :     ': Clone previous results',
     :     ': Correct using previous results'/
*

      call arc_init(.true.)
      call read_defaults(status)
      call par_rdkey('prfits',.false.,prfits)

* get the input spectrum ,map its .X and .Z.data structures

      call two_open('image',2,status)

      if(spdim1.le.1) then
        call par_wruser('This program is for 2-d data!',pstat)
        status = SAI__ERROR
      end if

* set up the allowed dimensions of the results cube

      mxpars = 13
      nyp = 20
      if(nxpdim.eq.1) then
        nxp = wavdim
      else
        nxp = spdim1
      end if
*
* offer the user the fitting options
*
      mode = rdmenu(dict_arc,4,menlis,1,'arc_opts',status)

      setup = mode.gt.1
      refine = mode.eq.2
      clone = mode.eq.3
      old = mode.eq.4
      end
