      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
*+
* Name:
*    QRSOLV

* Invocation:
*    CALL QRSOLV(N,R,LDR,IPVT,DIAG,QTB,X,SDIAG,WA)

* Purpose:
*   Least squares

* Description:
*     given an m by n matrix a, an n by n diagonal matrix d,
*     and an m-vector b, the problem is to determine an x which
*     solves the system
*
*           a*x = b ,     d*x = 0 ,
*
*     in the least squares sense.
*
*     this subroutine completes the solution of the problem
*     if it is provided with the necessary information from the
*     qr factorization, with column pivoting, of a. that is, if
*     a*p = q*r, where p is a permutation matrix, q has orthogonal
*     columns, and r is an upper triangular matrix with diagonal
*     elements of nonincreasing magnitude, then qrsolv expects
*     the full upper triangle of r, the permutation matrix p,
*     and the first n components of (q transpose)*b. the system
*     a*x = b, d*x = 0, is then equivalent to
*
*                  t       t
*           r*z = q *b ,  p *d*p*z = 0 ,
*
*     where x = p*z. if this system does not have full rank,
*     then a least squares solution is obtained. on output qrsolv
*     also provides an upper triangular matrix s such that
*
*            t   t               t
*           p *(a *a + d*d)*p = s *s .
*
*     s is computed within qrsolv and may be of separate interest.
*
* Arguments:
*    N = INTEGER (Given)
*     is a positive input variable set to the order of r.
*    R(N,N) = DOUBLE PRECISION ARRAY (Given and returned)
*     on input the full upper triangle
*      must contain the full upper triangle of the matrix r.
*      on output the full upper triangle is unaltered, and the
*      strict lower triangle contains the strict upper triangle
*      (transposed) of the upper triangular matrix s.
*    LDR = INTEGER (Given)
*     is a positive input variable not less than n
*      which specifies the leading dimension of the array r.
*    IPVT(N) = INTEGER ARRAY (Given)
*     is an input array which defines the
*      permutation matrix p such that a*p = q*r. column j of p
*      is column ipvt(j) of the identity matrix.
*    DIAG(N) = DOUBLE PRECISION ARRAY (Given)
*     is an input array which must contain the
*      diagonal elements of the matrix d.
*    QTB(N) = DOUBLE PRECISION ARRAY (Given)
*     is an input array which must contain the first
*      n elements of the vector (q transpose)*b.
*    X(N) = DOUBLE PRECISION ARRAY (Returned)
*     is an output array which contains the least
*      squares solution of the system a*x = b, d*x = 0.
*    SDIAG(N) = DOUBLE PRECISION ARRAY (Returned)
*     is an output array which contains the
*      diagonal elements of the upper triangular matrix s.
*    WA(N) = DOUBLE PRECISION ARRAY (Workspace)
*
* subprograms called:
*
*       fortran-supplied ... dabs,dsqrt
* History:
*     argonne national laboratory. minpack project. march 1980.
*     burton s. garbow, kenneth e. hillstrom, jorge j. more
*-
      integer n,ldr
      integer ipvt(n)
      double precision r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n)
      integer i,j,jp1,k,kp1,l,nsing
      double precision cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero
      data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/
*
*     copy r and (q transpose)*b to preserve input and initialize s.
*     in particular, save the diagonal elements of r in x.
*
      do 20 j = 1, n
        do 10 i = j, n
          r(i,j) = r(j,i)
 10     continue
        x(j) = r(j,j)
        wa(j) = qtb(j)
 20   continue
*
*     eliminate the diagonal matrix d using a givens rotation.
*
      do 100 j = 1, n
*
*        prepare the row of d to be eliminated, locating the
*        diagonal element using p from the qr factorization.
*
        l = ipvt(j)
        if (diag(l) .eq. zero) go to 90
        do 30 k = j, n
          sdiag(k) = zero
 30     continue
        sdiag(j) = diag(l)
*
*        the transformations to eliminate the row of d
*        modify only a single element of (q transpose)*b
*        beyond the first n, which is initially zero.
*
        qtbpj = zero
        do 80 k = j, n
*
*           determine a givens rotation which eliminates the
*           appropriate element in the current row of d.
*
          if (sdiag(k) .eq. zero) go to 70
          if (abs(r(k,k)) .ge. abs(sdiag(k))) then
            tan = sdiag(k)/r(k,k)
            cos = p5/sqrt(p25+p25*tan**2)
            sin = cos*tan
          else
            cotan = r(k,k)/sdiag(k)
            sin = p5/sqrt(p25+p25*cotan**2)
            cos = sin*cotan
          endif
*
*           compute the modified diagonal element of r and
*           the modified element of ((q transpose)*b,0).
*
          r(k,k) = cos*r(k,k) + sin*sdiag(k)
          temp = cos*wa(k) + sin*qtbpj
          qtbpj = -sin*wa(k) + cos*qtbpj
          wa(k) = temp
*
*           accumulate the tranformation in the row of s.
*
          kp1 = k + 1
          do 60 i = kp1, n
            temp = cos*r(i,k) + sin*sdiag(i)
            sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
            r(i,k) = temp
 60       continue
 70       continue
 80     continue
 90     continue
*
*        store the diagonal element of s and restore
*        the corresponding diagonal element of r.
*
        sdiag(j) = r(j,j)
        r(j,j) = x(j)
 100  continue
*
*     solve the triangular system for z. if the system is
*     singular, then obtain a least squares solution.
*
      nsing = n
      do 110 j = 1, n
        if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
        if (nsing .lt. n) wa(j) = zero
 110  continue
      do 140 k = 1, nsing
        j = nsing - k + 1
        sum = zero
        jp1 = j + 1
        do 120 i = jp1, nsing
          sum = sum + r(i,j)*wa(i)
 120    continue
        wa(j) = (wa(j) - sum)/sdiag(j)
 140  continue
*
*     permute the components of z back to components of x.
*
      do 160 j = 1, n
        l = ipvt(j)
        x(l) = wa(j)
 160  continue
*
*     last card of subroutine qrsolv.
*
      end
