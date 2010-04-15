      real function get_wave(cent,nid,dline,w,dwave,a)
*+
* Name:
*    GET_WAVE

* Invocation:
*   (REAL) = GET_WAVE(CENT,NID,DLINE,W,DWAVE,A)

* Purpose:
*   Estimate line wavelength

* Description:
*  To get an estimate for the wavelength of an unidentified line, from
* lines which have been identified, if possible.

* Arguments:
*    DWAVE(NID) = DOUBLE PRECISION ARRAY (Given)
*        Wavelengths of lines already identified
*    NID = INTEGER (Given)
*        Number of lines already identified
*    CENT(NID) = DOUBLE PRECISION ARRAY (Given)
*        Centres of lines already identified
*    DLINE = DOUBLE PRECISION (Given)
*        Centre of unidentified line
*    GET_WAVE = REAL (Returned)
*        Estimated wavelength of line (using spline
*                    interpolation). Returned as 0 if an error occurs.
*    W(NID) = DOUBLE PRECISION ARRAY (Workspace)
*    DWAVE(NID) = DOUBLE PRECISION ARRAY (Workspace)
*    A(20,20) = DOUBLE PRECISION ARRAY (Workspace)
*        Stores cheby coeffs

* History:
*   T.N.Wilkins 5-Feb-1986 Manchester
*   Revised to use INTRPL 10-Feb-1987
*   Revised to print out both results 12-Feb-1987
*   Anthony Holloway 1-9-1998 Manchester
*     Changed calls to support PDA version of fit_cpoly.f and generation
*     of Taylor-series polynomial.
*   ACD: 28/9/00 Remove character strings continued across continuation
*     lines and remove local unused variables.
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer nid
      double precision cent(nid)
      double precision dline

*  Local

      integer status
      integer IFAIL
      integer fit_cpoly,kp1
      integer MAX_KPLUS1
      parameter (MAX_KPLUS1 = 20)
      double precision w(nid),dwave(nid)
      double precision dwave1

* residual sum of squares

      double precision ss(MAX_KPLUS1)
      double precision a(3*nid+3*MAX_KPLUS1)
C      double precision fit_coeffs(MAX_KPLUS1)
      double precision xc(11)
      double precision gen_epolyd,lim1,lim2
      integer i,order,len1
      character*40 chars
      integer workptr,slot

      if(nid.gt.1) then
         lim1 = VAL__MAXR
         lim2 = VAL__MINR
         do i = 1, nid
            lim1 = min(lim1,cent(i))
            lim2 = max(lim2,cent(i))
            w(i)     = 1.0d0
         end do
         call darsort(cent,dwave,nid)

* Fit a Chebyshev polynomial to the wavelengths and centres

         status = SAI__OK
         call dsa_get_work_array(nid*3+2*MAX_KPLUS1,'double',workptr,
     :                           slot,status)
         if(status.ne.SAI__OK) then
            get_wave = VAL__BADR
            return
         end if
         if(fit_cpoly(w,nid,cent,dwave,a,ss,kp1,MAX_KPLUS1,
     :                %VAL(CNF_PVAL(workptr))).eq.0) then


* Copy chebyshev polynomial coefficients into 1d array

            kp1 = min(kp1,4)

* Not needed with PDA version
*            do i=1,kp1
*               fit_coeffs(i)=a(kp1,i)
*            end do

            do i =1,11
               xc(i)=0.0d0
            end do

* Convert to "normal" polynomial coefficients

*NAG vers
*            call gen_chb2no(kp1-1,lim1,lim2,fit_coeffs,xc)
*            call gen_revr8(xc,kp1,1,.true.,xc)

            IFAIL = 0
            CALL PDA_DPCOEF( kp1-1, 0D0, xc, a, IFAIL)
            IF (IFAIL .NE. 0) THEN
               CALL PAR_WRUSER('Error during conversion of fit '/
     :           /'co-effs to Taylor series co-effs in get_wave.f',
     :           status)
            end if

            dwave1 = gen_epolyd(dline,xc,kp1)
            order = kp1 - 1
            if((dline.lt.lim1).or.(dline.gt.lim2)) then
               call par_wruser('Warning, line outside Cheby limits'
     :              ,status)
            end if
            len1 = 0
            call chr_putc('Fit (order=',chars,len1)
            call chr_puti(order,chars,len1)
            call chr_putc(') : ',chars,len1)
            call chr_putr(real(dwave1),chars,len1)
            call par_wruser(chars(:len1),status)
         end if
         call dsa_free_workspace(slot,status)

* Use cubic spline interpolation to estimate wavelength

         status = SAI__OK
         call intrpl(nid,cent,dwave,1,dline,dwave1,status)
         if(status.eq.SAI__OK) then
            get_wave = real(dwave1)
            len1 = 0
            call chr_putc('Interpolated : ',chars,len1)
            call chr_putr(get_wave,chars,len1)
            call par_wruser(chars(:len1),status)
         else
            get_wave = 0.0
         end if
      else
         get_wave = 0.0
      end if
      end
