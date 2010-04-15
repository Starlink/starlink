      subroutine mg_good(m,n,fc,b,d,n1,g1,var,hesd,hesl,lh,fiterr,
     :     opt_lu,keep_itt,istate,nfree)
*+
* Name:
*    MG_GOOD

* Invocation:
*    CALL MG_GOOD(M,N,FC,B,D,N1,G1,VAR,HESD,HESL,LH,FITERR,
*          OPT_LU,KEEP_ITT,ISTATE,NFREE)

* Purpose:
*  Goodness of fit analysis

* Description:
*  Goodness of fit analysis

* Arguments:
*    M = INTEGER (Given)
*        Number of data points
*    N = INTEGER (Given)
*        number of free parameters
*    FC = DOUBLE PRECISION (Given)
*
*    NFREE = INTEGER (Given)
*        Number of free parameters
*    N1 = INTEGER (Given)
*        NFREE+1
*    HESD(N) = DOUBLE PRECISION ARRAY (Given)
*        diagonal elements of Hessian
*    HESL(LH) = DOUBLE PRECISION ARRAY (Given)
*        lower triangle of Hessian
*    LH = INTEGER (Given)
*        dimension of hesl
*    OPT_LU = INTEGER (Given)
*        Iteration file unit
*    KEEP_ITT = LOGICAL (Given)
*        If to keep iteration files
*    G1(N1,NFREE) = DOUBLE PRECISION ARRAY (Returned)
*        Covariance matrix g1(i,j), for i>j??
*    VAR(N) = DOUBLE PRECISION ARRAY (Returned)
*        Variances
*    FITERR(N) = REAL ARRAY (Returned)
*        Errors
*    D(NFREE) = DOUBLE PRECISION ARRAY (Workspace)
*
*    B(N1,NFREE) = DOUBLE PRECISION ARRAY (Workspace)
*

* Authors:
*  TNW: T.N.Wilkins, Durham

* History:
*  TNW: 7/7/93 Made to work if some parameters are not free.
*  JWP: Jan 1997 Removed NAG and used PDA
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer lh
      integer n1
      integer m
      integer opt_lu
      integer n,nfree,ipvt
      double precision det(2)
      double precision hesd(n)
      real fiterr(n)
      double precision hesl(lh)
      double precision d(n),b(n1,n),var(n),g1(n1,n),fc
      logical keep_itt
      integer SOFT_FAIL
      parameter (SOFT_FAIL = 1)
      integer istate(n)
      integer ON_BOUND
      parameter (ON_BOUND = 0)
      integer NO_FREE_PARAMETERS
      integer INVERSE_OK
      parameter (INVERSE_OK = 0)
      parameter (NO_FREE_PARAMETERS = 0)
      double precision factor
      character*30 chars
      integer status,i,ii,k,ifail
      integer idf


* determine the number of degrees of freedom

      idf = m-nfree

* only do the calulation if there are free parameters

      if( nfree .GT. NO_FREE_PARAMETERS) then

* zero out the Hessian store

         call zero_dble(g1,n1*n)

* fill in the diagonal.  hesd should only use the 1st nfree
* positions,

         do i = 1, nfree
            g1(i,i) = hesd(i)
         end do

* handle case of more than 1 free paramater. If there's only 1 free
* parameter then this will have already been handled by the loop above.

         if( nfree .gt. 1) then

* fill in the upper triangle

            ii = 0
            do i = 2, nfree
               do k = 1, (i-1)
                  ii = ii + 1
                  g1(k,i) = hesl(ii)
               end do
            end do
         end if

* invert matrix

         ifail=SOFT_FAIL

*         replace NAG call here
*         call f01abf(g1,n1,nfree,b,n1,d,ifail)
*         get extra workspace for PDA calls
          CALL PSX_CALLOC(n, '_INTEGER', ipvt, status )

          IF( STATUS .EQ. SAI__OK ) THEN
              CALL  PDA_DGEFA(g1,n1,nfree,%VAL(CNF_PVAL( ipvt )),ifail)
              IF( ifail .EQ. 0 ) THEN
                  CALL  PDA_DGEDI(g1,n1,nfree,%VAL( CNF_PVAL( ipvt ) ),
     :                            det,d,01)
              ENDIF
          ENDIF

          CALL PSX_FREE( ipvt, STATUS )

         if(ifail.ne. INVERSE_OK) then
            call opt_wruser('Error working out errors',status)
            write(chars,3) ifail
            call opt_wruser(chars,status)
         end if
         factor = 2.0d0 * fc / dble(idf)

* now sort out which parameters are free and copy over the
*correct variances for them. Start by assuming that all
*parameters are bound. If we have NFree parameters
* then only the 1st Nfree elements of B are valid.
* Step through the N elements of Istate and if we found
* one that is free look up the element of B which corresponds
* to that parameter in XC.


         do  i = 1, n
            var(i) = 0.0
            if(istate(i).gt.ON_BOUND) then
               var(i) = b(istate(i),istate(i))*factor
            endif
         end do
         if(keep_itt) then
            write(opt_lu,3) ifail
 3          format(1x,'Error number of inverse',i4)
            do  i = 1, n
               write(opt_lu,4) i,var(i)
            end do
 4          format(/,2x,'VAR(',i3,') = ',1pg17.8)
         end if

* Note that the first argument of this is really fitpar, but since we're
* not dealing with a skew model it doesn't get used.

         call rescale_errs(fiterr,var,n,fiterr,1)

* So we can recognise bad errors, set them to zero.

         if(ifail.ne. INVERSE_OK) then
            call zero_real(fiterr,n)
         end if
      else
         call zero_real(fiterr,n)
      end if
      end
