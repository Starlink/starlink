
* See gau2_pro for discussion

      subroutine gau2_calc (which, n, p, l, x, nf, xv, yv,
     :     gau2par, a, da)

*+
*   Description:
*     This is the central routine, which calculates the array of
*     function and jacobian values.  It obtains arrays A(i,j) and
*     DA(i,j), where
*       A(i,g) is the height of gaussian number g at the position
*         corresponding to datapoint i;
*       DA(i,k=5(g-1)+n) is the derivative of gaussian number g, with
*         respect to parameter x_n (n=1..5), evaluated at the position
*         corresponding to datapoint i.
*
*   Arguments:
*     which = integer (given)
*       if which >= 0, calculate A; which <= 0, calculate DA; so do both if
*       which==0
*     n = integer (given)
*       number of data points
*     p = integer (given)
*       number of non-linear parameters
*     l = integer (given)
*       number of linear parameters (equal to the number of gaussians,
*       plus one if the background is being fitted)
*     x = doubleprecision(p) (given)
*       the current vector of parameters
*     nf = integer (given)
*       invocation number.  This is set by the routine gau2_drnsg.  It
*       is _not_ used by this routine, but it can be a useful debugging
*       aid, if problems appear in future.
*     xv = integer(n) (given)
*       xv(i) is the x-coordinate of the datapoint i
*     yv = integer(n) (given)
*       yv(i) is the y-coordinate of the datapoint i
*     gau2par = integer(gau2len) (given and modified)
*       Array of parameters and feedback.  See the include file gau_par
*       for the indexes
*     a = doubleprecision(n,ngaussians) (returned)
*       the matrix A (see above).  ngaussians=l, or ngaussians=l-1 if
*       the background is being fitted
*     da = doubleprecision(n,5*ngaussians) (returned)
*       the matrix DA (see above).
*
*   Notes:
*
*     The code in here is derived from the following Maple program:
*
*        # read into maple with   > read `gaussian.ms`:
*
*        gi:=amp*exp(-(aparam^2/sa^2+bparam^2/sb^2)/2):
*        aparam:=cos(theta)*(xc-x0)+sin(theta)*(yc-y0):
*        bparam:=-sin(theta)*(xc-x0)+cos(theta)*(yc-y0):
*
*        xa := array(1..5):
*        ca := array(1..1):
*
*        x0 := xa[1]:
*        y0 := xa[2]:
*        sa := xa[3]:
*        sb := xa[4]:
*        theta := xa[5]:
*        amp := ca[1]:
*
*        # aadf are the variables we'll differentiate with respect to
*        aadf:=[x0,y0,sa,sb,theta]:
*        # array of differentials.
*        dyda:=array(1..6):
*        for i to 5 do
*        	dyda[i]:=diff(gi,aadf[i])
*        od:
*        dyda[6] := gi:
*
*        fortran (dyda, filename=`gaussianab.f`, optimized);
*
*     This calculates the five partial differentials of the gaussian `gi'
*     with respect to its five parameters.
*-

*   type declarations
      implicit none
      include 'GAU_PAR'

*   arguments
      integer n, p, l, gau2par(gau2len), nf
      integer which
      doubleprecision x(p), a(n,l+1), da(n,p)
      integer xv(n), yv(n)

*   local variables
*   pixi is index 1..n
      integer pixi
*   Renamed parameters: ngaussians is the number of gaussians.
      integer ngaussians
*   fitbkgd is true if we're fitting the background
      logical fitbkgd
*   gn is gaussian number 1..ngaussians.  ofs is offset into x array
      integer gn,ofs
*   temporary variables
      doubleprecision t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t14,t15,
     :     t16,t17,t21,t23,t26

      fitbkgd = (gau2par(gau2bg) .ne. 0)

      if (gau2par(gau2debug) .gt. 0)
     :     write (*, '("calcada: nf=",i2,"   which=",sp,i2)') nf, which

      if (fitbkgd) then
         ngaussians = l-1
      else
         ngaussians = l
      endif

*   loop over gaussians
      do 20, gn=1,ngaussians
         ofs = (gn-1)*5

*      loop over pixel numbers, increasing y fastest
         do 10, pixi=1,n
*         Parameters in x are x(ofs+1)=x0, x(ofs+2)=y0,
*         x(ofs+3)=sa, x(ofs+4)=sb, x(ofs+5)=theta
*         partial derivatives in da are da(pixi,ofs+1)=dA(pixi)/dx0, etc
*         This is all made a bit simpler by each of the terms A_ij
*         depending on a separate set of parameters
*         x_{5(i-1)+1} to x_{5(i-1)+5}.

            t1 = cos(x(ofs+5))
            t2 = xv(pixi)-x(ofs+1)
            t3 = t1*t2
            t4 = sin(x(ofs+5))
            t5 = yv(pixi)-x(ofs+2)
            t6 = t4*t5
            t7 = t3+t6
            t8 = x(ofs+3)**2
            t9 = 1/t8
            t14 = -t4*t2+t1*t5
            t15 = x(ofs+4)**2
            t16 = 1/t15
            t21 = t7**2
            t23 = t14**2
            t26 = exp(-t21*t9/2-t23*t16/2)

            if (which .ge. 0) then
               a(pixi,gn)     = t26
            endif

            if (which .le. 0) then
*            This array is filled in in column order - the wrong way
*            round.  I did some quick tests with a C analogue of this,
*            though, and it actually made very little difference.
               t10 = t7*t9
               t17 = t14*t16
               da(pixi,ofs+1) = (t10*t1-t17*t4)*t26
               da(pixi,ofs+2) = (t10*t4+t17*t1)*t26
               da(pixi,ofs+3) = t21/t8/x(ofs+3)*t26
               da(pixi,ofs+4) = t23/t15/x(ofs+4)*t26
               da(pixi,ofs+5) = (-t10*t14-t17*(-t3-t6))*t26
            endif

 10      continue
 20   continue

      if (fitbkgd .and. (which .ge. 0)) then
*      fitting background - add extra all-1 column to a(*,l)
         do 130, pixi=1,n
            a(pixi,l) = 1
 130     continue
      endif

      end
