      subroutine output_fit(fitsta,results,resvar,ix,iy,line,deccntr,
     :            lu,nagerr,shfail,linnam,waves,hex)
*+
* Name:
*    OUTPUT_FIT

* Invocation:
*    CALL OUTPUT_FIT(FITSTA,RESULTS,RESVAR,IX,IY,LINE,DECCNTR,
*                 LU,NAGERR,SHFAIL,LINNAM,WAVES,HEX)

* Purpose:
*  Output results of fit

* Description:
*    To display the results of a fit. These are extracted from the
*    results "cube", and either sent to a file, or output using
*    PAR_WRUSER. This is dealt with by WFT.
*
* Arguments:
*      FITSTA(MXPARS,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status array
*      RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results "cube"
*      RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results "cube"
*      IX = INTEGER (Given)
*        Cross-section of fit
*      LINE = INTEGER (Given)
*        Line
*      LU = INTEGER (Given)
*        Logical unit to write results to
*                         (if less than 0 written to terminal)
*      NAGERR = LOGICAL (Given)
*        If to display fits with Nag error
*      SHFAIL = LOGICAL (Given)
*        If to show presence of failed fits
*      LINNAM(NYP) = CHARACTER*10 ARRAY (Given)
*        NameS of lines
*      WAVES(NYP) = REAL ARRAY (Given)
*        Wavelength of line
*      HEX = LOGICAL (Given)
*        If hex data
*      DECCNTR(*) = INTEGER ARRAY (Returned)
*        Profile model of fit
*
* Global variables:
*      MXPARS = INTEGER (Given)
*        1st dimension of results
*      NYP = INTEGER (Given)
*
*      NXP = INTEGER (Given)
*
*      SPDIM2 = INTEGER (Given)
*
*    Subroutines/functions referenced:
*      GET_PARNUM = INTEGER FUNCTION
*        Convert parameter name to array position
*
*      CHR_FILL         : Fill string with one character
*      CHR_PUTI         : Format integer into string
*      CHR_PUTR         : Format real number into string
*      CHR_LEN = INTEGER (Workspace)
*        Get non-blank length of character string
*      WFT              : Write string to file or terminal
*
* Author:
*   T.N.Wilkins, Cambridge, 19-JUL-1989
*   A.C. Davenhall, Edinburgh, Starlink
*
* History:
*   T.N.Wilkins, Cambridge, 28-JUL-1989 More lines moved to here.
*   T.N.Wilkins, Cambridge, 1-AUG-1989 Use of WFT
*   T.N.Wilkins, Cambridge, 14-DEC-1989 Divided into output_fit and
*                           outsub
*       "            "      JUL-1991 Altered for new results structure
*       "            "      3-MAR-1992 Don't show deleted fits.
*   A.C. Davenhall, 06-SEP-00: Regularised the use of pointer d_vptr
*         to the Starlink style.
*-
      implicit none
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(ncntrl,nyp,nxp,spdim2)
      character*10 linnam(nyp)
      real waves(nyp)
      integer ix,iy
      integer line
      integer lu
      logical nagerr,shfail,hex
      integer get_parnum
      integer ppos
      character*80 chars
      integer cstat
      integer len1
      integer frstxs,lstxs
      integer status
      integer chr_len
      include 'status_inc'
      real value,value2,px,py,gen_elemf

* Get and decode fitsta, with meaningful output

      call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)

      cstat = deccntr(FIT_STAT)
      if((deccntr(FIT_MODEL).ne.0).and.((shfail.and.(cstat.gt.2)).or.
     :  ((cstat.eq.1).or.((cstat.eq.2).and.nagerr)))) then
        ppos = get_parnum('Space1_pos')
        value = results(ppos,line,ix,iy)
        value2 = sqrt(resvar(ppos,line,ix,iy))
        frstxs = nint(value - value2 + 0.5)
        lstxs = nint(value + value2 - 0.5)
        if(spdim2.eq.1) then
          call chr_fill(' ',chars)
          len1 = 0
          if(chr_len(linnam(line)).gt.0) then
            call chr_putc(linnam(line),chars,len1)
            call chr_putc(' (',chars,len1)
            call chr_putr(waves(line),chars,len1)
            call chr_putc(') ',chars,len1)
          end if
          call chr_putc('Position:',chars,len1)
          call encode_range(' ',' ',frstxs,lstxs,chars,len1)
          if(lu.ge.0) write(lu,'(/)')
          call wft(chars,lu,status)
        else
          px = gen_elemf(%VAL(CNF_PVAL(xptr)),ix)
          if(hex) px = px + gen_elemf(%VAL(CNF_PVAL(xdptr)),iy)
          py = gen_elemf(%VAL(CNF_PVAL(yptr)),iy)
          call morepts(results,%VAL(CNF_PVAL(d_vptr)),ix,iy,line,lu)
          write(chars,'(''Spatial position : '',f8.3,'','',f8.3)')px,py
          call wft(chars,lu,status)
        endif

* Output fit results

        call outsub(fitsta,results,resvar,iy,ix,line,deccntr,lu,nagerr)
      end if
      end
