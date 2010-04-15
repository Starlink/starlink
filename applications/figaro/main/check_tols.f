      subroutine check_tols(line,istore,jstore,nreject,if_arc,wavelength
     :     ,results,fitsta,resvar)
*+
* Name:
*    CHECK_TOLS

* Invocation:
*    CALL CHECK_TOLS(LINE,ISTORE,JSTORE,NREJECT,F_ARC,WAVELENGTH,RESULTS
*            ,FITSTA,RESVAR)

* Purpose:
*   Check the tolerances on the current fit parameters

* Description:
*   Check the tolerances on the current fit parameters
*
* Arguments:
*    LINE = INTEGER (Given)
*        Current line
*    ISTORE = INTEGER (Given)
*        Current X-sect
*    JSTORE = INTEGER (Given)
*        Current position, spatial dimension 2
*    IF_ARC = LOGICAL (Given)
*        If an arc
*    WAVELENGTH(*) = REAL ARRAY (Given)
*        Rest wavelength of line
*    RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results block
*    RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results block variance
*    FITSTA(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Fit status
*    NREJECT = INTEGER (Given and returned)
*        Number rejected
* Global variables:
*    NXP = INTEGER (Given)
*        1st dimension of results block
*    NYP = INTEGER (Given)
*        2nd dimension of results block
*    MXPARS = INTEGER (Given)
*        3rd dimension of results block
*    SPDIM2 = INTEGER (Given)
*        4th dimension of results block
*    TOLERANCE(MAXTOL=13) = REAL ARRAY (Given)
*        Tolerances (values to use)
*    REJECT(SZREJT=8) = LOGICAL ARRAY (Given)
*        Rejection criteria to use

* Authors:
*  TNW: T.N.Wilkins Cambridge until 9/92, then Durham
*   ACD: A C Davenhall, Starlink, Edinburgh

* History:
*  TNW: 31/7/89 Introduced use of GET_PARNUM.
*  TNW: 24,28-MAY-1991, new fit model arrays
*  TNW: 15/3/94 LOGFAIL incorporated into REJECT
*  ACD: 28/9/00 Remove local unused variables.
*-
      implicit none
      include 'arc_dims'
      integer line
      logical if_arc
      real wavelength(*)
* local

* 1 component parameters

      real sg_parms(7)

* 1 component errors

      real sg_error(7)
      integer nreject
      integer istore,jstore
      integer i,j
      integer NFAILS
      parameter (NFAILS = 8)
      logical fails(NFAILS)
      integer tol_stat
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(ncntrl,nyp,nxp,spdim2)
      integer nrej,ppos
      logical anytrue
      integer SUCCESS, TOLFAIL, MINNAG, TOLFAILN
      parameter (SUCCESS = 1, TOLFAIL = 5, MINNAG = 2, TOLFAILN = 6)
      integer get_parnum,status
      character*48 chars
      include 'status_inc'
      include 'fit_coding_inc'
      character*1 number(9)
      data number/'1','2','3','4','5','6','7','8','9'/
* ----------------------------------------------------------------------
      nrej=maxrej
      tol_stat=0

* get the fit status code

      call decode_status(ncntrl,fitsta(1,line,istore,jstore),deccntr)

* interpet code

      if( (deccntr(FIT_STAT).eq.SUCCESS) .or.
     :    (deccntr(FIT_STAT).eq.MINNAG) .or.
     :    (deccntr(FIT_STAT).eq.TOLFAIL) .or.
     :    (deccntr(FIT_STAT).eq.TOLFAILN)) then

        if(deccntr(FIT_NCMP).eq.0) then

*   Then set to no fit-this will allow refitting without any problems

          call zero_int(fitsta(1,line,istore,jstore),ncntrl)
          goto 2

* get the skew and cauchy parameters if those
* models have been used

*   skew

        else if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
          ppos = get_parnum('Skew_1')
          sg_parms(5) = results(ppos,line,istore,jstore)
          sg_error(5) = sqrt(resvar(ppos,line,istore,jstore))

*   Cauchy

        else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
          ppos = get_parnum('Cauchy_1')
          sg_parms(5) = results(ppos,line,istore,jstore)
          sg_error(5) = sqrt(resvar(ppos,line,istore,jstore))
        end if


        ppos = get_parnum('Base')
        sg_parms(4) = results(ppos,line,istore,jstore)
        sg_error(4) = sqrt(resvar(ppos,line,istore,jstore))
        do j = 1, NFAILS
          fails(j) = .false.
        end do

* loop over nparms

        do i = 1,deccntr(FIT_NCMP)

*   get the centre,fwhm,height,base

          ppos = get_parnum('Width_'//number(i))
          sg_parms(1) = results(ppos,line,istore,jstore)
          sg_error(1) = sqrt(resvar(ppos,line,istore,jstore))

          ppos = get_parnum('Height_'//number(i))
          sg_parms(2) = results(ppos,line,istore,jstore)
          sg_error(2) = sqrt(resvar(ppos,line,istore,jstore))

          ppos = get_parnum('Centre_'//number(i))
          sg_parms(3) = results(ppos,line,istore,jstore)
          sg_error(3) = sqrt(resvar(ppos,line,istore,jstore))

*       If we are not dealing with arc lines, then the wavelength
*       required is the difference from the rest wavelength of the
*       line

          if(.not.if_arc) then
            sg_parms(3)=sg_parms(3)-wavelength(line)
          end if

* check if tolerance checking in force

          if (anytrue(reject,nrej)) then
            call check_params(sg_parms,sg_error,tol_stat,fails,
     :           tolerance,reject,deccntr(FIT_MODEL))

*       may still not be right

            if(tol_stat.ne.0) go to 1

*     ....... check anytrue

          end if
        end do

 1      continue

* check if tolerances have been failed

        if(anytrue(reject,nrej).and.anytrue(fails,NFAILS)) then
          if(reject(LOGFAIL)) then
            write(chars,
     : '(''Rejected fit!!  Line='',i3,3x,''Position = '',i4,'','',i4)')
     :        line,istore,jstore
            call par_wruser(chars,status)
            call decodetolfail(fails,sg_parms,sg_error)
          end if
          nreject = nreject + 1

* encode status word in RESULTS

          deccntr(FIT_STAT) = deccntr(FIT_STAT) + 4
          call encode_contrl(deccntr,ncntrl,
     :                  fitsta(1,line,istore,jstore))
        else if((deccntr(FIT_STAT).eq.5).or.(deccntr(FIT_STAT).eq.6))
     :         then
          deccntr(FIT_STAT) = deccntr(FIT_STAT) - 4
          call encode_contrl(deccntr,ncntrl,
     :                  fitsta(1,line,istore,jstore))
          if(reject(LOGFAIL)) then
            write(chars,
     : '('' Line returned:- line='',i3,2x,''Position = '',i4,'','',i4)')
     :        line,istore,jstore
            call par_wruser(chars,status)
          end if
        end if
      end if
   2  continue
      end
