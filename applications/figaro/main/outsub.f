      subroutine outsub(fitsta,results,resvar,iy,xsect,line,deccntr,lu
     :                  ,nagerr)
*+
* Name:
*    OUTSUB

* Invocation:
*    CALL OUTSUB(FITSTA,RESULTS,RESVAR,IY,XSECT,LINE,DECCNTR,LU
*                       ,NAGERR)

* Purpose:
*  Output results of fit

* Description:
*  To display the results of a fit. These are extracted from the
*  results "cube", and either sent to a file, or output using
*  PAR_WRUSER. This is dealt with by WFT.
*
* Arguments:
*      FITSTA(3,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Results fit status array
*      RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results "cube"
*      RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results "cube" variance
*      IY = INTEGER (Given)
*        Position in 2nd spatial direction
*      XSECT = INTEGER (Given)
*        Cross-section of fit
*      LINE = INTEGER (Given)
*        Line
*      LU = INTEGER (Given)
*        Logical unit to write results to
*                         (if less than 0 written to terminal)
*      NAGERR = LOGICAL (Given)
*        If to display fits with Nag error
*      DECCNTR(*) = INTEGER ARRAY (Given and returned)
*        Profile model of fit
*  Common blocks referenced:
*      arc_dims
*  Subroutines/functions referenced:
*      GET_PARNUM = INTEGER (Workspace)
*        Convert parameter name to array position
*
*      CHR_FILL         : Fill string with one character
*      CHR_PUTI         : Format integer into string
*      CHR_PUTC         : Format string into string
*      CHR_PUTR         : Format real number into string
*      WFT              : Write string to file or terminal
*
* Author:
*   T.N.Wilkins, Cambridge, 19-JUL-1989
* History:
*   T.N.Wilkins, Cambridge, 28-JUL-1989 More lines moved to here.
*   T.N.Wilkins, Cambridge, 1-AUG-1989 Use of WFT
*   T.N.Wilkins, Cambridge, 14-DEC-1989 Divided into output_fit and outsub
*         "          "      11-SEP-1990 Format to output fit model
*                                       changed
*         "          "      1 to 9-JUL-1991 Changes for new results format
*         "          "      12-JUN-1992 Create header using chr routines
*         "          "      31-JUL-1992 chars filled with blanks properly
*-
      implicit none
      integer status
      include 'arc_dims'
      include 'status_inc'
      include 'fit_coding_inc'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      integer iy
      integer xsect
      integer line
      integer lu
      logical nagerr
      integer get_parnum
      integer ppos
      integer len1,len2
      character*80 chars,echars
      integer gauss
      logical ifbase
      integer cstat
      character*1 number(9)
      data number/'1','2','3','4','5','6','7','8','9'/

* Get and decode fitsta, with meaningful output

* Output position and id of line

      call decode_status(ncntrl,fitsta(1,line,xsect,iy),deccntr)

* Indicate fit model, success, etc.

      call describe_fit(deccntr,chars)
      call wft(chars,lu,status)

* Output fit results

      cstat = deccntr(FIT_STAT)

*     If the fit failed or there are no components to output, return

      if((.not.((cstat.eq.1).or.((cstat.eq.2).and.nagerr))).or.
     :     ((deccntr(FIT_TYPE).eq.MULTIPLE).and.
     :        (deccntr(FIT_NCMP).le.0))) then
        return
      end if

*  Write header

      ifbase = deccntr(BACK_MODEL).gt.0
      len1 = 0
      call chr_fill(' ',chars)

*   if more than 1 component, then we'll number them

      if(deccntr(FIT_NCMP).gt.1) call chr_putc('Component',chars,len1)

      len1 = 12
      call chr_putc('Centre          Fwhm          Height',chars,len1)

*  Do we have a base?

      if(ifbase) then
        len1 = len1 + 8
        call chr_putc('Base',chars,len1)
      endif

*   skew

      if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
        len1 = len1 + 8
        call chr_putc('Skew',chars,len1)

*   cauchy

      else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
        len1 = len1 + 8
        call chr_putc('Cauchy',chars,len1)
      end if

      call wft(chars,lu,status)

*  Write fit results

      do gauss = 1, deccntr(FIT_NCMP)

        call chr_fill(' ',chars)
        call chr_fill(' ',echars)

*   double or multiple gaussians

        if(deccntr(FIT_NCMP).gt.1) then
          len1 = 3
          call chr_puti(gauss,chars,len1)
        end if

        len1 = 12
        ppos = get_parnum('Centre_'//number(gauss))
        call chr_putr(results(ppos,line,xsect,iy),chars,len1)
        len2 = 12
        call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,len2)

        len1 = 26
        ppos = get_parnum('Width_'//number(gauss))
        call chr_putr(results(ppos,line,xsect,iy),chars,len1)
        len2 = 26
        call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,len2)

        len1 = 40
        ppos = get_parnum('Height_'//number(gauss))
        call chr_putr(results(ppos,line,xsect,iy),chars,len1)
        len2 = 40
        call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,len2)

*    Deal with base, skew, Cauchy as required

        if((gauss.eq.1).and.ifbase) then
          len1 = 54
          ppos = get_parnum('Base')
          call chr_putr(results(ppos,line,xsect,iy),chars,len1)
          len2 = 54
          call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,len2)
          if(deccntr(FIT_MODEL).gt.GAUSSIAN_MODEL) then
            len1 = 66
            len2 = 66

*       skew

            if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
              ppos = get_parnum('Skew_1')
              call chr_putr(results(ppos,line,xsect,iy),chars,len1)
              call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,
     :                  len2)

*       Cauchy

            else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
              ppos = get_parnum('Cauchy_1')
              call chr_putr(results(ppos,line,xsect,iy),chars,len1)
              call chr_putr(sqrt(resvar(ppos,line,xsect,iy)),echars,
     :                  len2)
            end if
          end if
        end if

*   Output results and errors

        call wft(chars(:len1),lu,status)
        call wft(echars(:len2),lu,status)

      end do

      end
