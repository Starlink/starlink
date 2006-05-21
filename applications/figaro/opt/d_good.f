      subroutine d_good(ajc,ajtjc,m,f,n,b,d,var)
*+
* Name:
*    D_GOOD

* Invocation:
*    CALL D_GOOD(AJC,AJTJC,M,F,N,B,D,VAR)
*
* Purpose:
*      goodness of fit analysis for least squares

* Description:
*      goodness of fit analysis for least squares
*
* Arguments:
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Given)
*
*    M = INTEGER (Given)
*       Number of data points
*    F = DOUBLE PRECISION (Given)
*
*    N = INTEGER (Given)
*        Number of fit parameters
*    KEEP_ITT = LOGICAL (Given)
*
*    VAR(N) = DOUBLE PRECISION ARRAY (Returned)
*        Errors on parameters (expressed as variances)
*    AJTJC(N+1,N) = DOUBLE PRECISION ARRAY (Returned)
*        Hessian
*    B(N,N) = DOUBLE PRECISION ARRAY (Workspace)
*
*    D(N) = DOUBLE PRECISION ARRAY (Workspace)
*
*
*   Tidied TNW 7/11/88 Manchester
*   Workspace requirements reduced, TNW 11/10/91 Cambridge
*   Further changes, including calculation of ajtjc, TNW 13-17/10/91
*   Modified to eliminate NAG and use PDA Jan 97 JWP
*
      implicit none
      include 'opt_cmn'
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      DOUBLE PRECISION d_probst
*-
      integer n,m
      double precision ajc(m,n),ajtjc(n+1,n)
      double precision b(n,n),d(n),f
      double precision var(n)

* local
*
      integer status,ipvt
      integer idf,i,j,k,ifail
      double precision BETA99,factor,pt99,b99
      double precision sum, det(2)

* 99% confidence

      parameter (BETA99 = 0.995)

*
* AJTJC : the hesian matrix nb since symmetric do l .le. i
*
      call zero_dble(ajtjc,n*(n+1))
      do i = 1, n
        do j = 1, i
          sum = 0.0d0
          do k = 1, m
            sum = sum+ajc(k,i)*ajc(k,j)
          end do
          ajtjc(j,i) = sum
        end do
      end do
*
* inversion
*

* set IFAIL
* AJH changed
      ifail = 0

* invert matrix

*     replace NAG call here
*     call f01abf(ajtjc,n+1,n,b,n,d,ifail)
*     get extra workspace for PDA calls
      CALL PSX_CALLOC(n, '_INTEGER', ipvt, status )

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL  PDA_DGEFA(ajtjc,n+1,n,%VAL( CNF_PVAL( ipvt ) ),ifail)
         IF( ifail .EQ. 0 ) THEN
            CALL  PDA_DGEDI(ajtjc,n+1,n,%VAL( CNF_PVAL( ipvt ) ),det,d,
     :                      01)
         ENDIF
      ENDIF

      CALL PSX_FREE( ipvt, status )

      if(keep_itt) then
        write(opt_lu,1) ifail
 1      format(' Error number for inverse = ',i4)
      end if
*
* Error return
*
      if(ifail.ne.0) then
        call opt_wruser('*** WARNING HESSIAN NOT POS DEF ***',status)
      else

*   degrees of freedom

        idf = m - n

*   reduced chi-square

        factor = f/(dble(idf))
*
* evaluate var
*
        do i = 1, n
          var(i) = b(i,i)*factor
        end do
        if(keep_itt) then
*
* calculate t confidence limits. At the moment nothing is done with these
* unless an iteration file is been written, but they should really be used.
*
          b99   = BETA99
          ifail = 0
* d_probst is a new Student T tail function added by JWP
* replacing call to GO1CAF
          pt99  = d_probst(b99,idf,ifail)
*
* fill in 99% confidence limits
*
          do i = 1, n
            t99(i) = var(i)*pt99
          end do
          write(opt_lu,2)
 2        format(/,' Errors (variance) and T confidence limits')
          do i = 1, n
            write(opt_lu,3) i,var(i),i,t99(i)
 3          format(/,2x,'VAR(',i2,') = ',e16.8,'T99(',i2,') = ',e16.8)
          end do
        end if
      end if
      end
