      subroutine pda_surfit(iopt,m,x,y,z,w,xb,xe,yb,ye,
     :                      kx,ky,s,nxest,nyest,
     :                      nmax,eps,nx,tx,ny,ty,c,fp,
     :                      wrk1,lwrk1,wrk2,lwrk2,iwrk,kwrk,
     :                      ier,status)

c given the set of data points (x(i),y(i),z(i)) and the set of positive
c numbers w(i),i=1,...,m, subroutine surfit determines a smooth bivar-
c iate spline approximation s(x,y) of degrees kx and ky on the rect-
c angle xb <= x <= xe, yb <= y <= ye.
c if iopt = -1 surfit calculates the weighted least-squares spline
c according to a given set of knots.
c if iopt >= 0 the total numbers nx and ny of these knots and their
c position tx(j),j=1,...,nx and ty(j),j=1,...,ny are chosen automatic-
c ally by the routine. the smoothness of s(x,y) is then achieved by
c minimalizing the discontinuity jumps in the derivatives of s(x,y)
c across the boundaries of the subpanels (tx(i),tx(i+1))*(ty(j),ty(j+1).
c the amounth of smoothness is determined by the condition that f(p) =
c sum ((w(i)*(z(i)-s(x(i),y(i))))**2) be <= s, with s a given non-neg-
c ative constant, called the smoothing factor.
c the fit is given in the b-spline representation (b-spline coefficients
c c((ny-ky-1)*(i-1)+j),i=1,...,nx-kx-1;j=1,...,ny-ky-1) and can be eval-
c uated by means of subroutine bispev.
c
c calling sequence:
c     call pda_surfit(iopt,m,x,y,z,w,xb,xe,yb,ye,kx,ky,s,nxest,nyest,
c    *  nmax,eps,nx,tx,ny,ty,c,fp,wrk1,lwrk1,wrk2,lwrk2,iwrk,kwrk,ier,status)
c
c parameters:
c  iopt  : integer flag. on entry iopt must specify whether a weighted
c          least-squares spline (iopt=-1) or a smoothing spline (iopt=0
c          or 1) must be determined.
c          if iopt=0 the routine will start with an initial set of knots
c          tx(i)=xb,tx(i+kx+1)=xe,i=1,...,kx+1;ty(i)=yb,ty(i+ky+1)=ye,i=
c          1,...,ky+1. if iopt=1 the routine will continue with the set
c          of knots found at the last call of the routine.
c          attention: a call with iopt=1 must always be immediately pre-
c                     ceded by another call with iopt=1 or iopt=0.
c          unchanged on exit.
c  m     : integer. on entry m must specify the number of data points.
c          m >= (kx+1)*(ky+1). unchanged on exit.
c  x     : real array of dimension at least (m).
c  y     : real array of dimension at least (m).
c  z     : real array of dimension at least (m).
c          before entry, x(i),y(i),z(i) must be set to the co-ordinates
c          of the i-th data point, for i=1,...,m. the order of the data
c          points is immaterial. unchanged on exit.
c  w     : real array of dimension at least (m). before entry, w(i) must
c          be set to the i-th value in the set of weights. the w(i) must
c          be strictly positive. unchanged on exit.
c  xb,xe : real values. on entry xb,xe,yb and ye must specify the bound-
c  yb,ye   aries of the rectangular approximation domain.
c          xb<=x(i)<=xe,yb<=y(i)<=ye,i=1,...,m. unchanged on exit.
c  kx,ky : integer values. on entry kx and ky must specify the degrees
c          of the spline. 1<=kx,ky<=5. it is recommended to use bicubic
c          (kx=ky=3) splines. unchanged on exit.
c  s     : real. on entry (in case iopt>=0) s must specify the smoothing
c          factor. s >=0. unchanged on exit.
c          for advice on the choice of s see further comments
c  nxest : integer. unchanged on exit.
c  nyest : integer. unchanged on exit.
c          on entry, nxest and nyest must specify an upper bound for the
c          number of knots required in the x- and y-directions respect.
c          these numbers will also determine the storage space needed by
c          the routine. nxest >= 2*(kx+1), nyest >= 2*(ky+1).
c          in most practical situation nxest = kx+1+sqrt(m/2), nyest =
c          ky+1+sqrt(m/2) will be sufficient. see also further comments.
c  nmax  : integer. on entry nmax must specify the actual dimension of
c          the arrays tx and ty. nmax >= nxest, nmax >=nyest.
c          unchanged on exit.
c  eps   : real.
c          on entry, eps must specify a threshold for determining the
c          effective rank of an over-determined linear system of equat-
c          ions. 0 < eps < 1.  if the number of decimal digits in the
c          computer representation of a real number is q, then 10**(-q)
c          is a suitable value for eps in most practical applications.
c          unchanged on exit.
c  nx    : integer.
c          unless ier=10 (in case iopt >=0), nx will contain the total
c          number of knots with respect to the x-variable, of the spline
c          approximation returned. if the computation mode iopt=1 is
c          used, the value of nx should be left unchanged between sub-
c          sequent calls.
c          in case iopt=-1, the value of nx should be specified on entry
c  tx    : real array of dimension nmax.
c          on succesful exit, this array will contain the knots of the
c          spline with respect to the x-variable, i.e. the position of
c          the interior knots tx(kx+2),...,tx(nx-kx-1) as well as the
c          position of the additional knots tx(1)=...=tx(kx+1)=xb and
c          tx(nx-kx)=...=tx(nx)=xe needed for the b-spline representat.
c          if the computation mode iopt=1 is used, the values of tx(1),
c          ...,tx(nx) should be left unchanged between subsequent calls.
c          if the computation mode iopt=-1 is used, the values tx(kx+2),
c          ...tx(nx-kx-1) must be supplied by the user, before entry.
c          see also the restrictions (ier=10).
c  ny    : integer.
c          unless ier=10 (in case iopt >=0), ny will contain the total
c          number of knots with respect to the y-variable, of the spline
c          approximation returned. if the computation mode iopt=1 is
c          used, the value of ny should be left unchanged between sub-
c          sequent calls.
c          in case iopt=-1, the value of ny should be specified on entry
c  ty    : real array of dimension nmax.
c          on succesful exit, this array will contain the knots of the
c          spline with respect to the y-variable, i.e. the position of
c          the interior knots ty(ky+2),...,ty(ny-ky-1) as well as the
c          position of the additional knots ty(1)=...=ty(ky+1)=yb and
c          ty(ny-ky)=...=ty(ny)=ye needed for the b-spline representat.
c          if the computation mode iopt=1 is used, the values of ty(1),
c          ...,ty(ny) should be left unchanged between subsequent calls.
c          if the computation mode iopt=-1 is used, the values ty(ky+2),
c          ...ty(ny-ky-1) must be supplied by the user, before entry.
c          see also the restrictions (ier=10).
c  c     : real array of dimension at least (nxest-kx-1)*(nyest-ky-1).
c          on succesful exit, c contains the coefficients of the spline
c          approximation s(x,y)
c  fp    : real. unless ier=10, fp contains the weighted sum of
c          squared residuals of the spline approximation returned.
c  wrk1  : real array of dimension (lwrk1). used as workspace.
c          if the computation mode iopt=1 is used the value of wrk1(1)
c          should be left unchanged between subsequent calls.
c          on exit wrk1(2),wrk1(3),...,wrk1(1+(nx-kx-1)*(ny-ky-1)) will
c          contain the values d(i)/max(d(i)),i=1,...,(nx-kx-1)*(ny-ky-1)
c          with d(i) the i-th diagonal element of the reduced triangular
c          matrix for calculating the b-spline coefficients. it includes
c          those elements whose square is less than eps,which are treat-
c          ed as 0 in the case of presumed rank deficiency (ier<-2).
c  lwrk1 : integer. on entry lwrk1 must specify the actual dimension of
c          the array wrk1 as declared in the calling (sub)program.
c          lwrk1 must not be too small. let
c            u = nxest-kx-1, v = nyest-ky-1, km = max(kx,ky)+1,
c            ne = max(nxest,nyest), bx = kx*v+ky+1, by = ky*u+kx+1,
c            if(bx.le.by) b1 = bx, b2 = b1+v-ky
c            if(bx.gt.by) b1 = by, b2 = b1+u-kx  then
c          lwrk1 >= u*v*(2+b1+b2)+2*(u+v+km*(m+ne)+ne-kx-ky)+b2+1
c  wrk2  : real array of dimension (lwrk2). used as workspace, but
c          only in the case a rank deficient system is encountered.
c  lwrk2 : integer. on entry lwrk2 must specify the actual dimension of
c          the array wrk2 as declared in the calling (sub)program.
c          lwrk2 > 0 . a save upper boundfor lwrk2 = u*v*(b2+1)+b2
c          where u,v and b2 are as above. if there are enough data
c          points, scattered uniformly over the approximation domain
c          and if the smoothing factor s is not too small, there is a
c          good chance that this extra workspace is not needed. a lot
c          of memory might therefore be saved by setting lwrk2=1.
c          (see also ier > 10)
c  iwrk  : integer array of dimension (kwrk). used as workspace.
c  kwrk  : integer. on entry kwrk must specify the actual dimension of
c          the array iwrk as declared in the calling (sub)program.
c          kwrk >= m+(nxest-2*kx-1)*(nyest-2*ky-1).
c  ier   : integer. unless the routine detects an error, ier contains a
c          non-positive value on exit, i.e.
c   ier=0  : normal return. the spline returned has a residual sum of
c            squares fp such that abs(fp-s)/s <= tol with tol a relat-
c            ive tolerance set to 0.001 by the program.
c   ier=-1 : normal return. the spline returned is an interpolating
c            spline (fp=0).
c   ier=-2 : normal return. the spline returned is the weighted least-
c            squares polynomial of degrees kx and ky. in this extreme
c            case fp gives the upper bound for the smoothing factor s.
c   ier<-2 : warning. the coefficients of the spline returned have been
c            computed as the minimal norm least-squares solution of a
c            (numerically) rank deficient system. (-ier) gives the rank.
c            especially if the rank deficiency which can be computed as
c            (nx-kx-1)*(ny-ky-1)+ier, is large the results may be inac-
c            curate. they could also seriously depend on the value of
c            eps.
c   ier=1  : error. the required storage space exceeds the available
c            storage space, as specified by the parameters nxest and
c            nyest.
c            probably causes : nxest or nyest too small. if these param-
c            eters are already large, it may also indicate that s is
c            too small
c            the approximation returned is the weighted least-squares
c            spline according to the current set of knots.
c            the parameter fp gives the corresponding weighted sum of
c            squared residuals (fp>s).
c   ier=2  : error. a theoretically impossible result was found during
c            the iteration proces for finding a smoothing spline with
c            fp = s. probably causes : s too small or badly chosen eps.
c            there is an approximation returned but the corresponding
c            weighted sum of squared residuals does not satisfy the
c            condition abs(fp-s)/s < tol.
c   ier=3  : error. the maximal number of iterations maxit (set to 20
c            by the program) allowed for finding a smoothing spline
c            with fp=s has been reached. probably causes : s too small
c            there is an approximation returned but the corresponding
c            weighted sum of squared residuals does not satisfy the
c            condition abs(fp-s)/s < tol.
c   ier=4  : error. no more knots can be added because the number of
c            b-spline coefficients (nx-kx-1)*(ny-ky-1) already exceeds
c            the number of data points m.
c            probably causes : either s or m too small.
c            the approximation returned is the weighted least-squares
c            spline according to the current set of knots.
c            the parameter fp gives the corresponding weighted sum of
c            squared residuals (fp>s).
c   ier=5  : error. no more knots can be added because the additional
c            knot would (quasi) coincide with an old one.
c            probably causes : s too small or too large a weight to an
c            inaccurate data point.
c            the approximation returned is the weighted least-squares
c            spline according to the current set of knots.
c            the parameter fp gives the corresponding weighted sum of
c            squared residuals (fp>s).
c   ier=10 : error. on entry, the input data are controlled on validity
c            the following restrictions must be satisfied.
c            -1<=iopt<=1, 1<=kx,ky<=5, m>=(kx+1)*(ky+1), nxest>=2*kx+2,
c            nyest>=2*ky+2, 0<eps<1, nmax>=nxest, nmax>=nyest,
c            xb<=x(i)<=xe, yb<=y(i)<=ye, w(i)>0, i=1,...,m
c            lwrk1 >= u*v*(2+b1+b2)+2*(u+v+km*(m+ne)+ne-kx-ky)+b2+1
c            kwrk >= m+(nxest-2*kx-1)*(nyest-2*ky-1)
c            if iopt=-1: 2*kx+2<=nx<=nxest
c                        xb<tx(kx+2)<tx(kx+3)<...<tx(nx-kx-1)<xe
c                        2*ky+2<=ny<=nyest
c                        yb<ty(ky+2)<ty(ky+3)<...<ty(ny-ky-1)<ye
c            if iopt>=0: s>=0
c            if one of these conditions is found to be violated,control
c            is immediately repassed to the calling program. in that
c            case there is no approximation returned.
c   ier>10 : error. lwrk2 is too small, i.e. there is not enough work-
c            space for computing the minimal least-squares solution of
c            a rank deficient system of linear equations. ier gives the
c            requested value for lwrk2. there is no approximation re-
c            turned but, having saved the information contained in nx,
c            ny,tx,ty,wrk1, and having adjusted the value of lwrk2 and
c            the dimension of the array wrk2 accordingly, the user can
c            continue at the point the program was left, by calling
c            surfit with iopt=1.
c   status   Starlink error status. 
c
c further comments:
c  by means of the parameter s, the user can control the tradeoff
c   between closeness of fit and smoothness of fit of the approximation.
c   if s is too large, the spline will be too smooth and signal will be
c   lost ; if s is too small the spline will pick up too much noise. in
c   the extreme cases the program will return an interpolating spline if
c   s=0 and the weighted least-squares polynomial (degrees kx,ky)if s is
c   very large. between these extremes, a properly chosen s will result
c   in a good compromise between closeness of fit and smoothness of fit.
c   to decide whether an approximation, corresponding to a certain s is
c   satisfactory the user is highly recommended to inspect the fits
c   graphically.
c   recommended values for s depend on the weights w(i). if these are
c   taken as 1/d(i) with d(i) an estimate of the standard deviation of
c   z(i), a good s-value should be found in the range (m-sqrt(2*m),m+
c   sqrt(2*m)). if nothing is known about the statistical error in z(i)
c   each w(i) can be set equal to one and s determined by trial and
c   error, taking account of the comments above. the best is then to
c   start with a very large value of s ( to determine the least-squares
c   polynomial and the corresponding upper bound fp0 for s) and then to
c   progressively decrease the value of s ( say by a factor 10 in the
c   beginning, i.e. s=fp0/10, fp0/100,...and more carefully as the
c   approximation shows more detail) to obtain closer fits.
c   to choose s very small is strongly discouraged. this considerably
c   increases computation time and memory requirements. it may also
c   cause rank-deficiency (ier<-2) and endager numerical stability.
c   to economize the search for a good s-value the program provides with
c   different modes of computation. at the first call of the routine, or
c   whenever he wants to restart with the initial set of knots the user
c   must set iopt=0.
c   if iopt=1 the program will continue with the set of knots found at
c   the last call of the routine. this will save a lot of computation
c   time if surfit is called repeatedly for different values of s.
c   the number of knots of the spline returned and their location will
c   depend on the value of s and on the complexity of the shape of the
c   function underlying the data. if the computation mode iopt=1
c   is used, the knots returned may also depend on the s-values at
c   previous calls (if these were smaller). therefore, if after a number
c   of trials with different s-values and iopt=1, the user can finally
c   accept a fit as satisfactory, it may be worthwhile for him to call
c   surfit once more with the selected value for s but now with iopt=0.
c   indeed, surfit may then return an approximation of the same quality
c   of fit but with fewer knots and therefore better if data reduction
c   is also an important objective for the user.
c   the number of knots may also depend on the upper bounds nxest and
c   nyest. indeed, if at a certain stage in surfit the number of knots
c   in one direction (say nx) has reached the value of its upper bound
c   (nxest), then from that moment on all subsequent knots are added
c   in the other (y) direction. this may indicate that the value of
c   nxest is too small. on the other hand, it gives the user the option
c   of limiting the number of knots the routine locates in any direction
c   for example, by setting nxest=2*kx+2 (the lowest allowable value for
c   nxest), the user can indicate that he wants an approximation which
c   is a simple polynomial of degree kx in the variable x.
c
c  other subroutines required:
c    fpback,fpbspl,fpsurf,fpdisc,fpgivs,fprank,fprati,fprota,fporde
c
c  references:
c   dierckx p. : an algorithm for surface fitting with spline functions
c                ima j. numer. anal. 1 (1981) 267-283.
c   dierckx p. : an algorithm for surface fitting with spline functions
c                report tw50, dept. computer science,k.u.leuven, 1980.
c   dierckx p. : curve and surface fitting with splines, monographs on
c                numerical analysis, oxford university press, 1993.
c
c  author:
c    p.dierckx
c    dept. computer science, k.u. leuven
c    celestijnenlaan 200a, b-3001 heverlee, belgium.
c    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
c
c  creation date : may 1979
c  latest update : march 1987
c
c  ..

c  Starlink Global Constants.
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
 
c  Starlink status.     
      INTEGER STATUS                  ! Global status

c  ..scalar arguments..
      real xb,xe,yb,ye,s,eps,fp
      integer iopt,m,kx,ky,nxest,nyest,nmax,nx,ny,lwrk1,lwrk2,kwrk,ier
c  ..array arguments..
      real x(m),y(m),z(m),w(m),tx(nmax),ty(nmax),
     * c((nxest-kx-1)*(nyest-ky-1)),wrk1(lwrk1),wrk2(lwrk2)
      integer iwrk(kwrk)
c  ..local scalars..
      real tol
      integer i,ib1,ib3,jb1,ki,kmax,km1,km2,kn,kwest,kx1,ky1,la,lbx,
     * lby,lco,lf,lff,lfp,lh,lq,lsx,lsy,lwest,maxit,ncest,nest,nek,
     * nminx,nminy,nmx,nmy,nreg,nrint,nxk,nyk
c  ..function references..
      integer max0
c  ..subroutine references..
c    fpsurf
c  ..

c   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

c  we set up the parameters tol and maxit.
      maxit = 20
      tol = 0.1e-02

c  before starting computations a data check is made. if the input data
c  are invalid,control is immediately repassed to the calling program.
      ier = 10
      if(eps.le.0. .or. eps.ge.1.) go to 70
      if(kx.le.0 .or. kx.gt.5) go to 70
      kx1 = kx+1
      if(ky.le.0 .or. ky.gt.5) go to 70
      ky1 = ky+1
      kmax = max0(kx,ky)
      km1 = kmax+1
      km2 = km1+1
      if(iopt.lt.(-1) .or. iopt.gt.1) go to 70
      if(m.lt.(kx1*ky1)) go to 70
      nminx = 2*kx1
      if(nxest.lt.nminx .or. nxest.gt.nmax) go to 70
      nminy = 2*ky1
      if(nyest.lt.nminy .or. nyest.gt.nmax) go to 70
      nest = max0(nxest,nyest)
      nxk = nxest-kx1
      nyk = nyest-ky1
      ncest = nxk*nyk
      nmx = nxest-nminx+1
      nmy = nyest-nminy+1
      nrint = nmx+nmy
      nreg = nmx*nmy
      ib1 = kx*nyk+ky1
      jb1 = ky*nxk+kx1
      ib3 = kx1*nyk+1
      if(ib1.le.jb1) go to 10
      ib1 = jb1
      ib3 = ky1*nxk+1
  10  lwest = ncest*(2+ib1+ib3)+2*(nrint+nest*km2+m*km1)+ib3
      kwest = m+nreg
      if(lwrk1.lt.lwest .or. kwrk.lt.kwest) go to 70
      if(xb.ge.xe .or. yb.ge.ye) go to 70
      do 20 i=1,m
        if(w(i).le.0.) go to 70
        if(x(i).lt.xb .or. x(i).gt.xe) go to 70
        if(y(i).lt.yb .or. y(i).gt.ye) go to 70
  20  continue
      if(iopt.ge.0) go to 50
      if(nx.lt.nminx .or. nx.gt.nxest) go to 70
      nxk = nx-kx1
      tx(kx1) = xb
      tx(nxk+1) = xe
      do 30 i=kx1,nxk
        if(tx(i+1).le.tx(i)) go to 70
  30  continue
      if(ny.lt.nminy .or. ny.gt.nyest) go to 70
      nyk = ny-ky1
      ty(ky1) = yb
      ty(nyk+1) = ye
      do 40 i=ky1,nyk
        if(ty(i+1).le.ty(i)) go to 70
  40  continue
      go to 60
  50  if(s.lt.0.) go to 70
  60  ier = 0
c  we partition the working space and determine the spline approximation
      kn = 1
      ki = kn+m
      lq = 2
      la = lq+ncest*ib3
      lf = la+ncest*ib1
      lff = lf+ncest
      lfp = lff+ncest
      lco = lfp+nrint
      lh = lco+nrint
      lbx = lh+ib3
      nek = nest*km2
      lby = lbx+nek
      lsx = lby+nek
      lsy = lsx+m*km1
      call pda_fpsurf(iopt,m,x,y,z,w,xb,xe,yb,ye,kx,ky,s,nxest,nyest,
     * eps,tol,maxit,nest,km1,km2,ib1,ib3,ncest,nrint,nreg,nx,tx,
     * ny,ty,c,fp,wrk1(1),wrk1(lfp),wrk1(lco),wrk1(lf),wrk1(lff),
     * wrk1(la),wrk1(lq),wrk1(lbx),wrk1(lby),wrk1(lsx),wrk1(lsy),
     * wrk1(lh),iwrk(ki),iwrk(kn),wrk2,lwrk2,ier)  

*   Check the status.
 70   IF ((IER.LT.-2).OR.(IER.GT.0)) STATUS=SAI__ERROR

      END


      subroutine pda_fpback(a,z,n,k,c,nest)
c  subroutine fpback calculates the solution of the system of
c  equations a*c = z with a a n x n upper triangular matrix
c  of bandwidth k.
c  ..
c  ..scalar arguments..
      integer n,k,nest
c  ..array arguments..
      real a(nest,k),z(n),c(n)
c  ..local scalars..
      real store
      integer i,i1,j,k1,l,m
c  ..
      k1 = k-1
      c(n) = z(n)/a(n,1)
      i = n-1
      if(i.eq.0) go to 30
      do 20 j=2,n
        store = z(i)
        i1 = k1
        if(j.le.k1) i1 = j-1
        m = i
        do 10 l=1,i1
          m = m+1
          store = store-c(m)*a(i,l+1)
  10    continue
        c(i) = store/a(i,1)
        i = i-1
  20  continue
  30  return
      end


      subroutine pda_fpdisc(t,n,k2,b,nest)
c  subroutine fpdisc calculates the discontinuity jumps of the kth
c  derivative of the b-splines of degree k at the knots t(k+2)..t(n-k-1)
c  ..scalar arguments..
      integer n,k2,nest
c  ..array arguments..
      real t(n),b(nest,k2)
c  ..local scalars..
      real an,fac,prod
      integer i,ik,j,jk,k,k1,l,lj,lk,lmk,lp,nk1,nrint
c  ..local array..
      real h(12)
c  ..
      k1 = k2-1
      k = k1-1
      nk1 = n-k1
      nrint = nk1-k
      an = nrint
      fac = an/(t(nk1+1)-t(k1))
      do 40 l=k2,nk1
        lmk = l-k1
        do 10 j=1,k1
          ik = j+k1
          lj = l+j
          lk = lj-k2
          h(j) = t(l)-t(lk)
          h(ik) = t(l)-t(lj)
  10    continue
        lp = lmk
        do 30 j=1,k2
          jk = j
          prod = h(j)
          do 20 i=1,k
            jk = jk+1
            prod = prod*h(jk)*fac
  20      continue
          lk = lp+k1
          b(lmk,j) = (t(lk)-t(lp))/prod
          lp = lp+1
  30    continue
  40  continue
      return
      end


      subroutine pda_fpgivs(piv,ww,cos,sin)
c  subroutine fpgivs calculates the parameters of a givens
c  transformation .
c  ..
c  ..scalar arguments..
      real piv,ww,cos,sin
c  ..local scalars..
      real dd,one,store
c  ..function references..
      real abs,sqrt
c  ..
      one = 0.1e+01
      store = abs(piv)
      if(store.ge.ww) dd = store*sqrt(one+(ww/piv)**2)
      if(store.lt.ww) dd = ww*sqrt(one+(piv/ww)**2)
      cos = ww/dd
      sin = piv/dd
      ww = dd
      return
      end


      subroutine pda_fporde(x,y,m,kx,ky,
     :                      tx,nx,ty,ny,nummer,index,nreg)
c  subroutine fporde sorts the data points (x(i),y(i)),i=1,2,...,m
c  according to the panel tx(l)<=x<tx(l+1),ty(k)<=y<ty(k+1), they belong
c  to. for each panel a stack is constructed  containing the numbers
c  of data points lying inside; index(j),j=1,2,...,nreg points to the
c  first data point in the jth panel while nummer(i),i=1,2,...,m gives
c  the number of the next data point in the panel.
c  ..
c  ..scalar arguments..
      integer m,kx,ky,nx,ny,nreg
c  ..array arguments..
      real x(m),y(m),tx(nx),ty(ny)
      integer nummer(m),index(nreg)
c  ..local scalars..
      real xi,yi
      integer i,im,k,kx1,ky1,k1,l,l1,nk1x,nk1y,num,nyy
c  ..
      kx1 = kx+1
      ky1 = ky+1
      nk1x = nx-kx1
      nk1y = ny-ky1
      nyy = nk1y-ky
      do 10 i=1,nreg
        index(i) = 0
  10  continue
      do 60 im=1,m
        xi = x(im)
        yi = y(im)
        l = kx1
        l1 = l+1
  20    if(xi.lt.tx(l1) .or. l.eq.nk1x) go to 30
        l = l1
        l1 = l+1
        go to 20
  30    k = ky1
        k1 = k+1
  40    if(yi.lt.ty(k1) .or. k.eq.nk1y) go to 50
        k = k1
        k1 = k+1
        go to 40
  50    num = (l-kx1)*nyy+k-ky
        nummer(im) = index(num)
        index(num) = im
  60  continue
      return
      end


      subroutine pda_fprank(a,f,n,m,na,tol,c,sq,rank,aa,ff,h)
c  subroutine fprank finds the minimum norm solution of a least-
c  squares problem in case of rank deficiency.
c
c  input parameters:
c    a : array, which contains the non-zero elements of the observation
c        matrix after triangularization by givens transformations.
c    f : array, which contains the transformed right hand side.
c    n : integer,wich contains the dimension of a.
c    m : integer, which denotes the bandwidth of a.
c  tol : real value, giving a threshold to determine the rank of a.
c
c  output parameters:
c    c : array, which contains the minimum norm solution.
c   sq : real value, giving the contribution of reducing the rank
c        to the sum of squared residuals.
c rank : integer, which contains the rank of matrix a.
c
c  ..scalar arguments..
      integer n,m,na,rank
      real tol,sq
c  ..array arguments..
      real a(na,m),f(n),c(n),aa(n,m),ff(n),h(m)
c  ..local scalars..
      integer i,ii,ij,i1,i2,j,jj,j1,j2,j3,k,kk,m1,nl
      real cos,fac,piv,sin,yi
      double precision store,stor1,stor2,stor3
c  ..function references..
      integer min0
c  ..subroutine references..
c    fpgivs,fprota
c  ..
      m1 = m-1
c  the rank deficiency nl is considered to be the number of sufficient
c  small diagonal elements of a.
      nl = 0
      sq = 0.
      do 90 i=1,n
        if(a(i,1).gt.tol) go to 90
c  if a sufficient small diagonal element is found, we put it to
c  zero. the remainder of the row corresponding to that zero diagonal
c  element is then rotated into triangle by givens rotations .
c  the rank deficiency is increased by one.
        nl = nl+1
        if(i.eq.n) go to 90
        yi = f(i)
        do 10 j=1,m1
          h(j) = a(i,j+1)
  10    continue
        h(m) = 0.
        i1 = i+1
        do 60 ii=i1,n
          i2 = min0(n-ii,m1)
          piv = h(1)
          if(piv.eq.0.) go to 30
          call pda_fpgivs(piv,a(ii,1),cos,sin)
          call pda_fprota(cos,sin,yi,f(ii))
          if(i2.eq.0) go to 70
          do 20 j=1,i2
            j1 = j+1
            call pda_fprota(cos,sin,h(j1),a(ii,j1))
            h(j) = h(j1)
  20      continue
          go to 50
  30      if(i2.eq.0) go to 70
          do 40 j=1,i2
            h(j) = h(j+1)
  40      continue
  50      h(i2+1) = 0.
  60    continue
c  add to the sum of squared residuals the contribution of deleting
c  the row with small diagonal element.
  70    sq = sq+yi**2
  90  continue
c  rank denotes the rank of a.
      rank = n-nl
c  let b denote the (rank*n) upper trapezoidal matrix which can be
c  obtained from the (n*n) upper triangular matrix a by deleting
c  the rows and interchanging the columns corresponding to a zero
c  diagonal element. if this matrix is factorized using givens
c  transformations as  b = (r) (u)  where
c    r is a (rank*rank) upper triangular matrix,
c    u is a (rank*n) orthonormal matrix
c  then the minimal least-squares solution c is given by c = b' v,
c  where v is the solution of the system  (r) (r)' v = g  and
c  g denotes the vector obtained from the old right hand side f, by
c  removing the elements corresponding to a zero diagonal element of a.
c  initialization.
      do 100 i=1,rank
        do 100 j=1,m
          aa(i,j) = 0.
 100  continue
c  form in aa the upper triangular matrix obtained from a by
c  removing rows and columns with zero diagonal elements. form in ff
c  the new right hand side by removing the elements of the old right
c  hand side corresponding to a deleted row.
      ii = 0
      do 120 i=1,n
        if(a(i,1).le.tol) go to 120
        ii = ii+1
        ff(ii) = f(i)
        aa(ii,1) = a(i,1)
        jj = ii
        kk = 1
        j = i
        j1 = min0(j-1,m1)
        if(j1.eq.0) go to 120
        do 110 k=1,j1
          j = j-1
          if(a(j,1).le.tol) go to 110
          kk = kk+1
          jj = jj-1
          aa(jj,kk) = a(j,k+1)
 110    continue
 120  continue
c  form successively in h the columns of a with a zero diagonal element.
      ii = 0
      do 200 i=1,n
        ii = ii+1
        if(a(i,1).gt.tol) go to 200
        ii = ii-1
        if(ii.eq.0) go to 200
        jj = 1
        j = i
        j1 = min0(j-1,m1)
        do 130 k=1,j1
          j = j-1
          if(a(j,1).le.tol) go to 130
          h(jj) = a(j,k+1)
          jj = jj+1
 130    continue
        do 140 kk=jj,m
          h(kk) = 0.
 140    continue
c  rotate this column into aa by givens transformations.
        jj = ii
        do 190 i1=1,ii
          j1 = min0(jj-1,m1)
          piv = h(1)
          if(piv.ne.0.) go to 160
          if(j1.eq.0) go to 200
          do 150 j2=1,j1
            j3 = j2+1
            h(j2) = h(j3)
 150      continue
          go to 180
 160      call pda_fpgivs(piv,aa(jj,1),cos,sin)
          if(j1.eq.0) go to 200
          kk = jj
          do 170 j2=1,j1
            j3 = j2+1
            kk = kk-1
            call pda_fprota(cos,sin,h(j3),aa(kk,j3))
            h(j2) = h(j3)
 170      continue
 180      jj = jj-1
          h(j3) = 0.
 190    continue
 200  continue
c  solve the system (aa) (f1) = ff
      ff(rank) = ff(rank)/aa(rank,1)
      i = rank-1
      if(i.eq.0) go to 230
      do 220 j=2,rank
        store = ff(i)
        i1 = min0(j-1,m1)
        k = i
        do 210 ii=1,i1
          k = k+1
          stor1 = ff(k)
          stor2 = aa(i,ii+1)
          store = store-stor1*stor2
 210    continue
        stor1 = aa(i,1)
        ff(i) = store/stor1
        i = i-1
 220  continue
c  solve the system  (aa)' (f2) = f1
 230  ff(1) = ff(1)/aa(1,1)
      if(rank.eq.1) go to 260
      do 250 j=2,rank
        store = ff(j)
        i1 = min0(j-1,m1)
        k = j
        do 240 ii=1,i1
          k = k-1
          stor1 = ff(k)
          stor2 = aa(k,ii+1)
          store = store-stor1*stor2
 240    continue
        stor1 = aa(j,1)
        ff(j) = store/stor1
 250  continue
c  premultiply f2 by the transpoze of a.
 260  k = 0
      do 280 i=1,n
        store = 0.
        if(a(i,1).gt.tol) k = k+1
        j1 = min0(i,m)
        kk = k
        ij = i+1
        do 270 j=1,j1
          ij = ij-1
          if(a(ij,1).le.tol) go to 270
          stor1 = a(ij,j)
          stor2 = ff(kk)
          store = store+stor1*stor2
          kk = kk-1
 270    continue
        c(i) = store
 280  continue
c  add to the sum of squared residuals the contribution of putting
c  to zero the small diagonal elements of matrix (a).
      stor3 = 0.
      do 310 i=1,n
        if(a(i,1).gt.tol) go to 310
        store = f(i)
        i1 = min0(n-i,m1)
        if(i1.eq.0) go to 300
        do 290 j=1,i1
          ij = i+j
          stor1 = c(ij)
          stor2 = a(i,j+1)
          store = store-stor1*stor2
 290    continue
 300    fac = a(i,1)*c(i)
        stor1 = a(i,1)
        stor2 = c(i)
        stor1 = stor1*stor2
        stor3 = stor3+stor1*(stor1-store-store)
 310  continue
      fac = stor3
      sq = sq+fac
      return
      end

      real function pda_fprati(p1,f1,p2,f2,p3,f3)
c  given three points (p1,f1),(p2,f2) and (p3,f3), function fprati
c  gives the value of p such that the rational interpolating function
c  of the form r(p) = (u*p+v)/(p+w) equals zero at p.
c  ..
c  ..scalar arguments..
      real p1,f1,p2,f2,p3,f3
c  ..local scalars..
      real h1,h2,h3,p
c  ..
      if(p3.gt.0.) go to 10
c  value of p in case p3 = infinity.
      p = (p1*(f1-f3)*f2-p2*(f2-f3)*f1)/((f1-f2)*f3)
      go to 20
c  value of p in case p3 ^= infinity.
  10  h1 = f1*(f2-f3)
      h2 = f2*(f3-f1)
      h3 = f3*(f1-f2)
      p = -(p1*p2*h3+p2*p3*h1+p3*p1*h2)/(p1*h1+p2*h2+p3*h3)
c  adjust the value of p1,f1,p3 and f3 such that f1 > 0 and f3 < 0.
  20  if(f2.lt.0.) go to 30
      p1 = p2
      f1 = f2
      go to 40
  30  p3 = p2
      f3 = f2
  40  pda_fprati = p
      return
      end


      subroutine pda_fprota(cos,sin,a,b)
c  subroutine fprota applies a givens rotation to a and b.
c  ..
c  ..scalar arguments..
      real cos,sin,a,b
c ..local scalars..
      real stor1,stor2
c  ..
      stor1 = a
      stor2 = b
      b = cos*stor2+sin*stor1
      a = cos*stor1-sin*stor2
      return
      end


      subroutine pda_fpsurf(iopt,m,x,y,z,w,xb,xe,yb,ye,kxx,kyy,s,nxest,
     * nyest,eta,tol,maxit,nmax,km1,km2,ib1,ib3,nc,intest,nrest,
     * nx0,tx,ny0,ty,c,fp,fp0,fpint,coord,f,ff,a,q,bx,by,spx,spy,h,
     * index,nummer,wrk,lwrk,ier)
c  ..
c  ..scalar arguments..
      real xb,xe,yb,ye,s,eta,tol,fp,fp0
      integer iopt,m,kxx,kyy,nxest,nyest,maxit,nmax,km1,km2,ib1,ib3,
     * nc,intest,nrest,nx0,ny0,lwrk,ier
c  ..array arguments..
      real x(m),y(m),z(m),w(m),tx(nmax),ty(nmax),c(nc),fpint(intest),
     * coord(intest),f(nc),ff(nc),a(nc,ib1),q(nc,ib3),bx(nmax,km2),
     * by(nmax,km2),spx(m,km1),spy(m,km1),h(ib3),wrk(lwrk)
      integer index(nrest),nummer(m)
c  ..local scalars..
      real acc,arg,cos,dmax,fac1,fac2,fpmax,fpms,f1,f2,f3,hxi,p,pinv,
     * piv,p1,p2,p3,sigma,sin,sq,store,wi,x0,x1,y0,y1,zi,eps,
     * rn,one,con1,con9,con4,half,ten
      integer i,iband,iband1,iband3,iband4,ibb,ichang,ich1,ich3,ii,
     * in,irot,iter,i1,i2,i3,j,jrot,jxy,j1,kx,kx1,kx2,ky,ky1,ky2,l,
     * la,lf,lh,lwest,lx,ly,l1,l2,n,ncof,nk1x,nk1y,nminx,nminy,nreg,
     * nrint,num,num1,nx,nxe,nxx,ny,nye,nyy,n1,rank
c  ..local arrays..
      real hx(6),hy(6)
c  ..function references..
      real abs,pda_fprati,sqrt
      integer min0
c  ..subroutine references..
c    fpback,fpbspl,fpgivs,fpdisc,fporde,fprank,fprota
c  ..
c  set constants
      one = 0.1e+01
      con1 = 0.1e0
      con9 = 0.9e0
      con4 = 0.4e-01
      half = 0.5e0
      ten = 0.1e+02
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c part 1: determination of the number of knots and their position.     c
c ****************************************************************     c
c given a set of knots we compute the least-squares spline sinf(x,y),  c
c and the corresponding weighted sum of squared residuals fp=f(p=inf). c
c if iopt=-1  sinf(x,y) is the requested approximation.                c
c if iopt=0 or iopt=1 we check whether we can accept the knots:        c
c   if fp <=s we will continue with the current set of knots.          c
c   if fp > s we will increase the number of knots and compute the     c
c      corresponding least-squares spline until finally  fp<=s.        c
c the initial choice of knots depends on the value of s and iopt.      c
c   if iopt=0 we first compute the least-squares polynomial of degree  c
c     kx in x and ky in y; nx=nminx=2*kx+2 and ny=nminy=2*ky+2.        c
c     fp0=f(0) denotes the corresponding weighted sum of squared       c
c     residuals                                                        c
c   if iopt=1 we start with the knots found at the last call pda_of the    c
c     routine, except for the case that s>=fp0; then we can compute    c
c     the least-squares polynomial directly.                           c
c eventually the independent variables x and y (and the corresponding  c
c parameters) will be switched if this can reduce the bandwidth of the c
c system to be solved.                                                 c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  ichang denotes whether(1) or not(-1) the directions have been inter-
c  changed.
      ichang = -1
      x0 = xb
      x1 = xe
      y0 = yb
      y1 = ye
      kx = kxx
      ky = kyy
      kx1 = kx+1
      ky1 = ky+1
      nxe = nxest
      nye = nyest
      eps = sqrt(eta)
      if(iopt.lt.0) go to 20
c  calculation of acc, the absolute tolerance for the root of f(p)=s.
      acc = tol*s
      if(iopt.eq.0) go to 10
      if(fp0.gt.s) go to 20
c  initialization for the least-squares polynomial.
  10  nminx = 2*kx1
      nminy = 2*ky1
      nx = nminx
      ny = nminy
      ier = -2
      go to 30
  20  nx = nx0
      ny = ny0
c  main loop for the different sets of knots. m is a save upper bound
c  for the number of trials.
  30  do 420 iter=1,m
c  find the position of the additional knots which are needed for the
c  b-spline representation of s(x,y).
        l = nx
        do 40 i=1,kx1
          tx(i) = x0
          tx(l) = x1
          l = l-1
  40    continue
        l = ny
        do 50 i=1,ky1
          ty(i) = y0
          ty(l) = y1
          l = l-1
  50    continue
c  find nrint, the total number of knot intervals and nreg, the number
c  of panels in which the approximation domain is subdivided by the
c  intersection of knots.
        nxx = nx-2*kx1+1
        nyy = ny-2*ky1+1
        nrint = nxx+nyy
        nreg = nxx*nyy
c  find the bandwidth of the observation matrix a.
c  if necessary, interchange the variables x and y, in order to obtain
c  a minimal bandwidth.
        iband1 = kx*(ny-ky1)+ky
        l = ky*(nx-kx1)+kx
        if(iband1.le.l) go to 130
        iband1 = l
        ichang = -ichang
        do 60 i=1,m
          store = x(i)
          x(i) = y(i)
          y(i) = store
  60    continue
        store = x0
        x0 = y0
        y0 = store
        store = x1
        x1 = y1
        y1 = store
        n = min0(nx,ny)
        do 70 i=1,n
          store = tx(i)
          tx(i) = ty(i)
          ty(i) = store
  70    continue
        n1 = n+1
        if(nx-ny) 80,120,100
  80    do 90 i=n1,ny
          tx(i) = ty(i)
  90    continue
        go to 120
 100    do 110 i=n1,nx
          ty(i) = tx(i)
 110    continue
 120    l = nx
        nx = ny
        ny = l
        l = nxe
        nxe = nye
        nye = l
        l = nxx
        nxx = nyy
        nyy = l
        l = kx
        kx = ky
        ky = l
        kx1 = kx+1
        ky1 = ky+1
 130    iband = iband1+1
c  arrange the data points according to the panel they belong to.
        call pda_fporde(x,y,m,kx,ky,tx,nx,ty,ny,nummer,index,nreg)
c  find ncof, the number of b-spline coefficients.
        nk1x = nx-kx1
        nk1y = ny-ky1
        ncof = nk1x*nk1y
c  initialize the observation matrix a.
        do 140 i=1,ncof
          f(i) = 0.
          do 140 j=1,iband
            a(i,j) = 0.
 140    continue
c  initialize the sum of squared residuals.
        fp = 0.
c  fetch the data points in the new order. main loop for the
c  different panels.
        do 250 num=1,nreg
c  fix certain constants for the current panel; jrot records the column
c  number of the first non-zero element in a row of the observation
c  matrix according to a data point of the panel.
          num1 = num-1
          lx = num1/nyy
          l1 = lx+kx1
          ly = num1-lx*nyy
          l2 = ly+ky1
          jrot = lx*nk1y+ly
c  test whether there are still data points in the panel.
          in = index(num)
 150      if(in.eq.0) go to 250
c  fetch a new data point.
          wi = w(in)
          zi = z(in)*wi
c  evaluate for the x-direction, the (kx+1) non-zero b-splines at x(in).
          call pda_fpbspl(tx,nx,kx,x(in),l1,hx)
c  evaluate for the y-direction, the (ky+1) non-zero b-splines at y(in).
          call pda_fpbspl(ty,ny,ky,y(in),l2,hy)
c  store the value of these b-splines in spx and spy respectively.
          do 160 i=1,kx1
            spx(in,i) = hx(i)
 160      continue
          do 170 i=1,ky1
            spy(in,i) = hy(i)
 170      continue
c  initialize the new row of observation matrix.
          do 180 i=1,iband
            h(i) = 0.
 180      continue
c  calculate the non-zero elements of the new row by making the cross
c  products of the non-zero b-splines in x- and y-direction.
          i1 = 0
          do 200 i=1,kx1
            hxi = hx(i)
            j1 = i1
            do 190 j=1,ky1
              j1 = j1+1
              h(j1) = hxi*hy(j)*wi
 190        continue
            i1 = i1+nk1y
 200      continue
c  rotate the row into triangle by givens transformations .
          irot = jrot
          do 220 i=1,iband
            irot = irot+1
            piv = h(i)
            if(piv.eq.0.) go to 220
c  calculate the parameters of the givens transformation.
            call pda_fpgivs(piv,a(irot,1),cos,sin)
c  apply that transformation to the right hand side.
            call pda_fprota(cos,sin,zi,f(irot))
            if(i.eq.iband) go to 230
c  apply that transformation to the left hand side.
            i2 = 1
            i3 = i+1
            do 210 j=i3,iband
              i2 = i2+1
              call pda_fprota(cos,sin,h(j),a(irot,i2))
 210        continue
 220      continue
c  add the contribution of the row to the sum of squares of residual
c  right hand sides.
 230      fp = fp+zi**2
c  find the number of the next data point in the panel.
 240      in = nummer(in)
          go to 150
 250    continue
c  find dmax, the maximum value for the diagonal elements in the reduced
c  triangle.
        dmax = 0.
        do 260 i=1,ncof
          if(a(i,1).le.dmax) go to 260
          dmax = a(i,1)
 260    continue
c  check whether the observation matrix is rank deficient.
        sigma = eps*dmax
        do 270 i=1,ncof
          if(a(i,1).le.sigma) go to 280
 270    continue
c  backward substitution in case of full rank.
        call pda_fpback(a,f,ncof,iband,c,nc)
        rank = ncof
        do 275 i=1,ncof
          q(i,1) = a(i,1)/dmax
 275    continue
        go to 300
c  in case of rank deficiency, find the minimum norm solution.
c  check whether there is sufficient working space
 280    lwest = ncof*iband+ncof+iband
        if(lwrk.lt.lwest) go to 780
        do 290 i=1,ncof
          ff(i) = f(i)
          do 290 j=1,iband
            q(i,j) = a(i,j)
 290    continue
        lf =1
        lh = lf+ncof
        la = lh+iband
        call pda_fprank(q,ff,ncof,iband,nc,sigma,c,sq,rank,wrk(la),
     *    wrk(lf),wrk(lh))
        do 295 i=1,ncof
          q(i,1) = q(i,1)/dmax
 295    continue
c  add to the sum of squared residuals, the contribution of reducing
c  the rank.
        fp = fp+sq
 300    if(ier.eq.(-2)) fp0 = fp
c  test whether the least-squares spline is an acceptable solution.
        if(iopt.lt.0) go to 820
        fpms = fp-s
        if(abs(fpms).le.acc) if(fp) 815,815,820
c  test whether we can accept the choice of knots.
        if(fpms.lt.0.) go to 430
c  test whether we cannot further increase the number of knots.
        if(ncof.gt.m) go to 790
        ier = 0
c  search where to add a new knot.
c  find for each interval the sum of squared residuals fpint for the
c  data points having the coordinate belonging to that knot interval.
c  calculate also coord which is the same sum, weighted by the position
c  of the data points considered.
 310    do 320 i=1,nrint
          fpint(i) = 0.
          coord(i) = 0.
 320    continue
        do 360 num=1,nreg
          num1 = num-1
          lx = num1/nyy
          l1 = lx+1
          ly = num1-lx*nyy
          l2 = ly+1+nxx
          jrot = lx*nk1y+ly
          in = index(num)
 330      if(in.eq.0) go to 360
          store = 0.
          i1 = jrot
          do 350 i=1,kx1
            hxi = spx(in,i)
            j1 = i1
            do 340 j=1,ky1
              j1 = j1+1
              store = store+hxi*spy(in,j)*c(j1)
 340        continue
            i1 = i1+nk1y
 350      continue
          store = (w(in)*(z(in)-store))**2
          fpint(l1) = fpint(l1)+store
          coord(l1) = coord(l1)+store*x(in)
          fpint(l2) = fpint(l2)+store
          coord(l2) = coord(l2)+store*y(in)
          in = nummer(in)
          go to 330
 360    continue
c  find the interval for which fpint is maximal on the condition that
c  there still can be added a knot.
 370    l = 0
        fpmax = 0.
        l1 = 1
        l2 = nrint
        if(nx.eq.nxe) l1 = nxx+1
        if(ny.eq.nye) l2 = nxx
        if(l1.gt.l2) go to 810
        do 380 i=l1,l2
          if(fpmax.ge.fpint(i)) go to 380
          l = i
          fpmax = fpint(i)
 380    continue
c  test whether we cannot further increase the number of knots.
        if(l.eq.0) go to 785
c  calculate the position of the new knot.
        arg = coord(l)/fpint(l)
c  test in what direction the new knot is going to be added.
        if(l.gt.nxx) go to 400
c  addition in the x-direction.
        jxy = l+kx1
        fpint(l) = 0.
        fac1 = tx(jxy)-arg
        fac2 = arg-tx(jxy-1)
        if(fac1.gt.(ten*fac2) .or. fac2.gt.(ten*fac1)) go to 370
        j = nx
        do 390 i=jxy,nx
          tx(j+1) = tx(j)
          j = j-1
 390    continue
        tx(jxy) = arg
        nx = nx+1
        go to 420
c  addition in the y-direction.
 400    jxy = l+ky1-nxx
        fpint(l) = 0.
        fac1 = ty(jxy)-arg
        fac2 = arg-ty(jxy-1)
        if(fac1.gt.(ten*fac2) .or. fac2.gt.(ten*fac1)) go to 370
        j = ny
        do 410 i=jxy,ny
          ty(j+1) = ty(j)
          j = j-1
 410    continue
        ty(jxy) = arg
        ny = ny+1
c  restart the computations with the new set of knots.
 420  continue
c  test whether the least-squares polynomial is a solution of our
c  approximation problem.
 430  if(ier.eq.(-2)) go to 830
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c part 2: determination of the smoothing spline sp(x,y)                c
c *****************************************************                c
c we have determined the number of knots and their position. we now    c
c compute the b-spline coefficients of the smoothing spline sp(x,y).   c
c the observation matrix a is extended by the rows of a matrix,        c
c expressing that sp(x,y) must be a polynomial of degree kx in x and   c
c ky in y. the corresponding weights of these additional rows are set  c
c to 1./p.  iteratively we than have to determine the value of p       c
c such that f(p)=sum((w(i)*(z(i)-sp(x(i),y(i))))**2) be = s.           c
c we already know that the least-squares polynomial corresponds to     c
c p=0  and that the least-squares spline corresponds to p=infinity.    c
c the iteration process which is proposed here makes use of rational   c
c interpolation. since f(p) is a convex and strictly decreasing        c
c function of p, it can be approximated by a rational function r(p)=   c
c (u*p+v)/(p+w). three values of p(p1,p2,p3) with corresponding values c
c of f(p) (f1=f(p1)-s,f2=f(p2)-s,f3=f(p3)-s) are used to calculate the c
c new value of p such that r(p)=s. convergence is guaranteed by taking c
c f1 > 0 and f3 < 0.                                                   c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      kx2 = kx1+1
c  test whether there are interior knots in the x-direction.
      if(nk1x.eq.kx1) go to 440
c  evaluate the discotinuity jumps of the kx-th order derivative of
c  the b-splines at the knots tx(l),l=kx+2,...,nx-kx-1.
      call pda_fpdisc(tx,nx,kx2,bx,nmax)
 440  ky2 = ky1 + 1
c  test whether there are interior knots in the y-direction.
      if(nk1y.eq.ky1) go to 450
c  evaluate the discontinuity jumps of the ky-th order derivative of
c  the b-splines at the knots ty(l),l=ky+2,...,ny-ky-1.
      call pda_fpdisc(ty,ny,ky2,by,nmax)
c  initial value for p.
 450  p1 = 0.
      f1 = fp0-s
      p3 = -one
      f3 = fpms
      p = 0.
      do 460 i=1,ncof
        p = p+a(i,1)
 460  continue
      rn = ncof
      p = rn/p
c  find the bandwidth of the extended observation matrix.
      iband3 = kx1*nk1y
      iband4 = iband3 +1
      ich1 = 0
      ich3 = 0
c  iteration process to find the root of f(p)=s.
      do 770 iter=1,maxit
        pinv = one/p
c  store the triangularized observation matrix into q.
        do 480 i=1,ncof
          ff(i) = f(i)
          do 470 j=1,iband
            q(i,j) = a(i,j)
 470      continue
          ibb = iband+1
          do 480 j=ibb,iband4
            q(i,j) = 0.
 480    continue
        if(nk1y.eq.ky1) go to 560
c  extend the observation matrix with the rows of a matrix, expressing
c  that for x=cst. sp(x,y) must be a polynomial in y of degree ky.
        do 550 i=ky2,nk1y
          ii = i-ky1
          do 550 j=1,nk1x
c  initialize the new row.
            do 490 l=1,iband
              h(l) = 0.
 490        continue
c  fill in the non-zero elements of the row. jrot records the column
c  number of the first non-zero element in the row.
            do 500 l=1,ky2
              h(l) = by(ii,l)*pinv
 500        continue
            zi = 0.
            jrot = (j-1)*nk1y+ii
c  rotate the new row into triangle by givens transformations without
c  square roots.
            do 540 irot=jrot,ncof
              piv = h(1)
              i2 = min0(iband1,ncof-irot)
              if(piv.eq.0.) if(i2) 550,550,520
c  calculate the parameters of the givens transformation.
              call pda_fpgivs(piv,q(irot,1),cos,sin)
c  apply that givens transformation to the right hand side.
              call pda_fprota(cos,sin,zi,ff(irot))
              if(i2.eq.0) go to 550
c  apply that givens transformation to the left hand side.
              do 510 l=1,i2
                l1 = l+1
                call pda_fprota(cos,sin,h(l1),q(irot,l1))
 510          continue
 520          do 530 l=1,i2
                h(l) = h(l+1)
 530          continue
              h(i2+1) = 0.
 540        continue
 550    continue
 560    if(nk1x.eq.kx1) go to 640
c  extend the observation matrix with the rows of a matrix expressing
c  that for y=cst. sp(x,y) must be a polynomial in x of degree kx.
        do 630 i=kx2,nk1x
          ii = i-kx1
          do 630 j=1,nk1y
c  initialize the new row
            do 570 l=1,iband4
              h(l) = 0.
 570        continue
c  fill in the non-zero elements of the row. jrot records the column
c  number of the first non-zero element in the row.
            j1 = 1
            do 580 l=1,kx2
              h(j1) = bx(ii,l)*pinv
              j1 = j1+nk1y
 580        continue
            zi = 0.
            jrot = (i-kx2)*nk1y+j
c  rotate the new row into triangle by givens transformations .
            do 620 irot=jrot,ncof
              piv = h(1)
              i2 = min0(iband3,ncof-irot)
              if(piv.eq.0.) if(i2) 630,630,600
c  calculate the parameters of the givens transformation.
              call pda_fpgivs(piv,q(irot,1),cos,sin)
c  apply that givens transformation to the right hand side.
              call pda_fprota(cos,sin,zi,ff(irot))
              if(i2.eq.0) go to 630
c  apply that givens transformation to the left hand side.
              do 590 l=1,i2
                l1 = l+1
                call pda_fprota(cos,sin,h(l1),q(irot,l1))
 590          continue
 600          do 610 l=1,i2
                h(l) = h(l+1)
 610          continue
              h(i2+1) = 0.
 620        continue
 630    continue
c  find dmax, the maximum value for the diagonal elements in the
c  reduced triangle.
 640    dmax = 0.
        do 650 i=1,ncof
          if(q(i,1).le.dmax) go to 650
          dmax = q(i,1)
 650    continue
c  check whether the matrix is rank deficient.
        sigma = eps*dmax
        do 660 i=1,ncof
          if(q(i,1).le.sigma) go to 670
 660    continue
c  backward substitution in case of full rank.
        call pda_fpback(q,ff,ncof,iband4,c,nc)
        rank = ncof
        go to 675
c  in case of rank deficiency, find the minimum norm solution.
 670    lwest = ncof*iband4+ncof+iband4
        if(lwrk.lt.lwest) go to 780
        lf = 1
        lh = lf+ncof
        la = lh+iband4
        call pda_fprank(q,ff,ncof,iband4,nc,sigma,c,sq,rank,wrk(la),
     *   wrk(lf),wrk(lh))
 675    do 680 i=1,ncof
          q(i,1) = q(i,1)/dmax
 680    continue
c  compute f(p).
        fp = 0.
        do 720 num = 1,nreg
          num1 = num-1
          lx = num1/nyy
          ly = num1-lx*nyy
          jrot = lx*nk1y+ly
          in = index(num)
 690      if(in.eq.0) go to 720
          store = 0.
          i1 = jrot
          do 710 i=1,kx1
            hxi = spx(in,i)
            j1 = i1
            do 700 j=1,ky1
              j1 = j1+1
              store = store+hxi*spy(in,j)*c(j1)
 700        continue
            i1 = i1+nk1y
 710      continue
          fp = fp+(w(in)*(z(in)-store))**2
          in = nummer(in)
          go to 690
 720    continue
c  test whether the approximation sp(x,y) is an acceptable solution.
        fpms = fp-s
        if(abs(fpms).le.acc) go to 820
c  test whether the maximum allowable number of iterations has been
c  reached.
        if(iter.eq.maxit) go to 795
c  carry out one more step of the iteration process.
        p2 = p
        f2 = fpms
        if(ich3.ne.0) go to 740
        if((f2-f3).gt.acc) go to 730
c  our initial choice of p is too large.
        p3 = p2
        f3 = f2
        p = p*con4
        if(p.le.p1) p = p1*con9 + p2*con1
        go to 770
 730    if(f2.lt.0.) ich3 = 1
 740    if(ich1.ne.0) go to 760
        if((f1-f2).gt.acc) go to 750
c  our initial choice of p is too small
        p1 = p2
        f1 = f2
        p = p/con4
        if(p3.lt.0.) go to 770
        if(p.ge.p3) p = p2*con1 + p3*con9
        go to 770
 750    if(f2.gt.0.) ich1 = 1
c  test whether the iteration process proceeds as theoretically
c  expected.
 760    if(f2.ge.f1 .or. f2.le.f3) go to 800
c  find the new value of p.
        p = pda_fprati(p1,f1,p2,f2,p3,f3)
 770  continue
c  error codes and messages.
 780  ier = lwest
      go to 830
 785  ier = 5
      go to 830
 790  ier = 4
      go to 830
 795  ier = 3
      go to 830
 800  ier = 2
      go to 830
 810  ier = 1
      go to 830
 815  ier = -1
      fp = 0.
 820  if(ncof.ne.rank) ier = -rank
c  test whether x and y are in the original order.
 830  if(ichang.lt.0) go to 930
c  if not, interchange x and y once more.
      l1 = 1
      do 840 i=1,nk1x
        l2 = i
        do 840 j=1,nk1y
          f(l2) = c(l1)
          l1 = l1+1
          l2 = l2+nk1x
 840  continue
      do 850 i=1,ncof
        c(i) = f(i)
 850  continue
      do 860 i=1,m
        store = x(i)
        x(i) = y(i)
        y(i) = store
 860  continue
      n = min0(nx,ny)
      do 870 i=1,n
        store = tx(i)
        tx(i) = ty(i)
        ty(i) = store
 870  continue
      n1 = n+1
      if(nx-ny) 880,920,900
 880  do 890 i=n1,ny
        tx(i) = ty(i)
 890  continue
      go to 920
 900  do 910 i=n1,nx
        ty(i) = tx(i)
 910  continue
 920  l = nx
      nx = ny
      ny = l
 930  if(iopt.lt.0) go to 940
      nx0 = nx
      ny0 = ny
 940  return
      end



      subroutine pda_bispev(tx,nx,ty,ny,c,kx,ky,
     :                      x,mx,y,my,z,wrk,lwrk,
     :                      iwrk,kwrk,ier,status)

c  subroutine bispev evaluates on a grid (x(i),y(j)),i=1,...,mx; j=1,...
c  ,my a bivariate spline s(x,y) of degrees kx and ky, given in the
c  b-spline representation.
c
c  calling sequence:
c     call pda_bispev(tx,nx,ty,ny,c,kx,ky,x,mx,y,my,z,wrk,lwrk,
c    * iwrk,kwrk,ier,status)
c
c  input parameters:
c   tx    : real array, length nx, which contains the position of the
c           knots in the x-direction.
c   nx    : integer, giving the total number of knots in the x-direction
c   ty    : real array, length ny, which contains the position of the
c           knots in the y-direction.
c   ny    : integer, giving the total number of knots in the y-direction
c   c     : real array, length (nx-kx-1)*(ny-ky-1), which contains the
c           b-spline coefficients.
c   kx,ky : integer values, giving the degrees of the spline.
c   x     : real array of dimension (mx).
c           before entry x(i) must be set to the x co-ordinate of the
c           i-th grid point along the x-axis.
c           tx(kx+1)<=x(i-1)<=x(i)<=tx(nx-kx), i=2,...,mx.
c   mx    : on entry mx must specify the number of grid points along
c           the x-axis. mx >=1.
c   y     : real array of dimension (my).
c           before entry y(j) must be set to the y co-ordinate of the
c           j-th grid point along the y-axis.
c           ty(ky+1)<=y(j-1)<=y(j)<=ty(ny-ky), j=2,...,my.
c   my    : on entry my must specify the number of grid points along
c           the y-axis. my >=1.
c   wrk   : real array of dimension lwrk. used as workspace.
c   lwrk  : integer, specifying the dimension of wrk.
c           lwrk >= mx*(kx+1)+my*(ky+1)
c   iwrk  : integer array of dimension kwrk. used as workspace.
c   kwrk  : integer, specifying the dimension of iwrk. kwrk >= mx+my.
c
c  output parameters:
c   z     : real array of dimension (mx*my).
c           on succesful exit z(my*(i-1)+j) contains the value of s(x,y)
c           at the point (x(i),y(j)),i=1,...,mx;j=1,...,my.
c   ier   : integer error flag
c   ier=0 : normal return
c   ier=10: invalid input data (see restrictions)
c   status: Integer. Starlink status report

c  restrictions:
c   mx >=1, my >=1, lwrk>=mx*(kx+1)+my*(ky+1), kwrk>=mx+my
c   tx(kx+1) <= x(i-1) <= x(i) <= tx(nx-kx), i=2,...,mx
c   ty(ky+1) <= y(j-1) <= y(j) <= ty(ny-ky), j=2,...,my
c
c  other subroutines required:
c    fpbisp,fpbspl
c
c  references :
c    de boor c : on calculating with b-splines, j. approximation theory
c                6 (1972) 50-62.
c    cox m.g.  : the numerical evaluation of b-splines, j. inst. maths
c                applics 10 (1972) 134-149.
c    dierckx p. : curve and surface fitting with splines, monographs on
c                 numerical analysis, oxford university press, 1993.
c
c  author :
c    p.dierckx
c    dept. computer science, k.u.leuven
c    celestijnenlaan 200a, b-3001 heverlee, belgium.
c    e-mail : Paul.Dierckx@cs.kuleuven.ac.be
c
c  latest update : march 1987
c

C  Starlink Global Constants.
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
         
C  Starlink Status.     
      INTEGER STATUS                  ! Global status
 
c  ..scalar arguments..
      integer nx,ny,kx,ky,mx,my,lwrk,kwrk,ier
c  ..array arguments..
      integer iwrk(kwrk)
      real tx(nx),ty(ny),c((nx-kx-1)*(ny-ky-1)),x(mx),y(my),z(mx*my),
     * wrk(lwrk)
c  ..local scalars..
      integer i,iw,lwest
c  ..

c   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

c  before starting computations a data check is made. if the input data
c  are invalid control is immediately repassed to the calling program.
      ier = 10
      lwest = (kx+1)*mx+(ky+1)*my
      if(lwrk.lt.lwest) go to 100
      if(kwrk.lt.(mx+my)) go to 100
      if(mx-1) 100,30,10
  10  do 20 i=2,mx
        if(x(i).lt.x(i-1)) go to 100
  20  continue
  30  if(my-1) 100,60,40
  40  do 50 i=2,my
        if(y(i).lt.y(i-1)) go to 100
  50  continue
  60  ier = 0
      iw = mx*(kx+1)+1
      call pda_fpbisp(tx,nx,ty,ny,c,kx,ky,x,mx,y,my,z,wrk(1),wrk(iw),
     * iwrk(1),iwrk(mx+1))

c   Check the status.
 100  IF(IER.EQ.0) THEN
         STATUS=SAI__OK
      ELSE
         STATUS=SAI__ERROR
      END IF

      end


      subroutine pda_fpbisp(tx,nx,ty,ny,c,kx,ky,
     :                      x,mx,y,my,z,wx,wy,lx,ly)
c  ..scalar arguments..
      integer nx,ny,kx,ky,mx,my
c  ..array arguments..
      integer lx(mx),ly(my)
      real tx(nx),ty(ny),c((nx-kx-1)*(ny-ky-1)),x(mx),y(my),z(mx*my),
     * wx(mx,kx+1),wy(my,ky+1)
c  ..local scalars..
      integer kx1,ky1,l,l1,l2,m,nkx1,nky1
      real arg,sp,tb,te
c  ..local arrays..
      real h(6)
c  ..subroutine references..
c    fpbspl
c  ..
      kx1 = kx+1
      nkx1 = nx-kx1
      tb = tx(kx1)
      te = tx(nkx1+1)
      l = kx1
      l1 = l+1
      do 40 i=1,mx
        arg = x(i)
        if(arg.lt.tb) arg = tb
        if(arg.gt.te) arg = te
  10    if(arg.lt.tx(l1) .or. l.eq.nkx1) go to 20
        l = l1
        l1 = l+1
        go to 10
  20    call pda_fpbspl(tx,nx,kx,arg,l,h)
        lx(i) = l-kx1
        do 30 j=1,kx1
          wx(i,j) = h(j)
  30    continue
  40  continue
      ky1 = ky+1
      nky1 = ny-ky1
      tb = ty(ky1)
      te = ty(nky1+1)
      l = ky1
      l1 = l+1
      do 80 i=1,my
        arg = y(i)
        if(arg.lt.tb) arg = tb
        if(arg.gt.te) arg = te
  50    if(arg.lt.ty(l1) .or. l.eq.nky1) go to 60
        l = l1
        l1 = l+1
        go to 50
  60    call pda_fpbspl(ty,ny,ky,arg,l,h)
        ly(i) = l-ky1
        do 70 j=1,ky1
          wy(i,j) = h(j)
  70    continue
  80  continue
      m = 0
      do 130 i=1,mx
        l = lx(i)*nky1
        do 90 i1=1,kx1
          h(i1) = wx(i,i1)
  90    continue
        do 120 j=1,my
          l1 = l+ly(j)
          sp = 0.
          do 110 i1=1,kx1
            l2 = l1
            do 100 j1=1,ky1
              l2 = l2+1
              sp = sp+c(l2)*h(i1)*wy(j,j1)
 100        continue
            l1 = l1+nky1
 110      continue
          m = m+1
          z(m) = sp
 120    continue
 130  continue
      return
      end


      subroutine pda_fpbspl(t,n,k,x,l,h)
c  subroutine fpbspl evaluates the (k+1) non-zero b-splines of
c  degree k at t(l) <= x < t(l+1) using the stable recurrence
c  relation of de boor and cox.
c  ..
c  ..scalar arguments..
      real x
      integer n,k,l
c  ..array arguments..
      real t(n),h(6)
c  ..local scalars..
      real f,one
      integer i,j,li,lj
c  ..local arrays..
      real hh(5)
c  ..
      one = 0.1e+01
      h(1) = one
      do 20 j=1,k
        do 10 i=1,j
          hh(i) = h(i)
  10    continue
        h(1) = 0.
        do 20 i=1,j
          li = l+i
          lj = li-j
          f = hh(i)/(t(li)-t(lj))
          h(i) = h(i)+f*(t(li)-x)
          h(i+1) = f*(x-t(lj))
  20  continue
      return
      end
