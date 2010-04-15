      subroutine fit_glbase(xsect,nwindow,sdata,sdata1,deccntr,ileft,m,
     :     vbase,ifseek,adata,ptr0,status)
*+
* Name:
*    FIT_GLBASE

* Invocation:
*    CALL FIT_GLBASE(XSECT,NWINDOW,SDATA,SDATA1,DECCNTR,
*                     ILEFT,M,VBASE,IFSEEK,ADATA,PTR0,STATUS)

* Purpose:
*   Fit global base

* Description:
*   To get the values of the Chebyshev base for addition to fit total
*   work arrays.
*
* Arguments:
*    XSECT = INTEGER (Given)
*        Starting cross-section of fit
*    NWINDOW = INTEGER (Given)
*        Number of cross-section in fit
*    SDATA(WAVDIM) = REAL ARRAY (Given)
*        Wavelengths array (used for limits only)
*    WAVDIM = INTEGER (Given)
*        Number of channels in data
*    SDATA1(M) = REAL ARRAY (Given)
*        Wavelengths array (used to give output). Only used
*                   for the Chebyshev models.
*    ILEFT = INTEGER (Given)
*        Left channel boundary of line
*    M = INTEGER (Given)
*        Number of channels in data
*    VBASE = INTEGER (Given)
*        Pointer to array element of dynamic_mem starting
*                   real array to write base to (should be M*4 bytes)
*    IFSEEK = LOGICAL (Given)
*        If to seek for order (as opposed to taking current
*                    one)-needed for deccntr(back_model)=3
*    ADATA(M) = DOUBLE PRECISION ARRAY (Given)
*        Double precision version of SDATA (within line range
*                   only). Only used for Spline model.
*    PTR0 = INTEGER (Given)
*        Pointer to workspace:
*                       DECCNTR(BACK_MODEL)  ELEMENTS (double precision)
*                        2          WAVDIM*2+M
*                        3          WAVDIM*6+460
*    DECCNTR(BACK_MODEL) = INTEGER ARRAY (Given and returned)
*        Model for back The folowing are used by this
*                   routine = INTEGER (Given and returned)
*
*                        2 - Cubic spline interpolation
*                        3 - Chebyshev polynomial fit, carried out now
*                        4 - Chebyshev polynomial fit, coefficients
*                            stored by FITCONT
*    STATUS = INTEGER (Returned)
*        Error status (0=ok)
* Subroutines/functions referenced:
*    E_CPOLYRL = INTEGER (Returned)
*        Evaluate Chebyshev polynomial
*    ZERO_REAL              : Zero real array
*
*    DSA_SPECIFIC_STRUCTURE : Find name of application-specific
*                             structure
*    DTA_RDVARD             : Read double precision data from file
*    TNW_DTAERR             : Output error message for DTA
*    CHR_LEN = INTEGER (Returned)
*        Get logical length of character string
*
*    CHR_PUTC               : Format character string into
*                             character string
*    CHR_PUTI               : Format integer into character string

* History:
*   T.N.Wilkins Manchester 27/10/88
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'status_inc'
      include 'arc_dims'
      integer vbase,xsect,kp1,k,len1,len2,j
      real sdata(wavdim)
      integer nwindow,ileft,m,xsend,chr_len,pstat
      real sdata1(m)
      double precision adata(m)
      logical ifseek
      character*72 chars
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1 = 10)
      integer xin,yin,dbase,nbad,cnv_fmtcnv,ptr0
      integer e_cpolyrl,fstat
      double precision chbcfs(MAX_KPLUS1)

      call zero_real(%VAL(CNF_PVAL(vbase)),m)
      if(deccntr(back_model).eq.4) then

*  Previously stored fit

        call dsa_specific_structure('data','continuum','r',chars,status)
        xsend = min((xsect+nwindow-1),spdim1)
        len1 = chr_len(chars)
        call chr_putc('.coeff[1,',chars,len1)
        do k = xsect,xsend

*   Read coefficients from file

          len2 = len1
          call chr_puti(k,chars,len2)
          call chr_putc(']',chars,len2)
          call dta_rdvard(chars,20,chbcfs,status)
          if(status.ne.SAI__OK) then
            call tnw_dtaerr(status,'reading',chars)
            goto 500
          end if

*   Decide on order

          kp1 = 0
          do j=1,20
            if(abs(chbcfs(j)).gt.1.0d-6) kp1=j
          end do

*   Evaluate polynomials

          fstat = e_cpolyrl(sdata(1),sdata(wavdim),sdata1(ileft),
     :                      chbcfs,kp1,m,.true.,%VAL(CNF_PVAL(vbase)))
        end do
      else if(deccntr(back_model).eq.3) then

*    We'll fit a polynomial now

*    Take base, and fit a Chebyshev polynomial to the area outside the
*    tram lines

        kp1 = deccntr(back_order) + 1
        if(kp1.gt.MAX_KPLUS1) then
          call par_wruser('Error, order too high, setting to maximum',
     :         pstat)
          kp1 = MAX_KPLUS1
        end if

        call cheby_base(MAX_KPLUS1,chbcfs,kp1,ifseek,
     :                  %VAL(CNF_PVAL(ptr0)),status)

*   Evaluate polynomials

        fstat = e_cpolyrl(sdata(1),sdata(wavdim),sdata1(ileft),
     :                    chbcfs,kp1,m,.false.,%VAL(CNF_PVAL(vbase)))
        deccntr(back_order) = kp1 - 1
      else if (deccntr(back_model).eq.2) then

* Cubic spline model. This needs the array pointed to by D_XPTR in case
* this is not the same as SDATA.

        xin = ptr0
        yin = xin + wavdim*val__nbd
        dbase = yin + wavdim*val__nbd
        call spline_base(wavdim,sdata,%VAL(CNF_PVAL(d_vsptr)),
     :                   line_count,%VAL(CNF_PVAL(d_tlptr)),
     :                   %VAL(CNF_PVAL(d_trptr)),m,adata,
     :                   %VAL(CNF_PVAL(dbase)),status,
     :                   %VAL(CNF_PVAL(xin)),%VAL(CNF_PVAL(yin)),
     :                   %VAL(CNF_PVAL(d_xptr)))
        status = cnv_fmtcnv('double','real',%VAL(CNF_PVAL(dbase)),
     :                       %VAL(CNF_PVAL(vbase)),m,nbad)
      end if
 500  continue
      end
