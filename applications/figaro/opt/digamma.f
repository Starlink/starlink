      real function digamma(x,ifault)
*+
* Name:
*    DIGAMMA

* Invocation:
*   (REAL) = DIGAMMA(X,IFAULT)

* Description:
*   A fortran routine to compute
*     d/dz[ log {GAMMA(z)}] = GAMMA'(z)/GAMMA(z)
*   for real positive values of z.
*    BASED on algorithm  AS103 from Applied Statistics
* Arguments:
*   input
*     X           point at which function rquired
*   Output
*     IFAULT      Is a fault indicator with the following code
*                  IFAULT  = 0     X is possitive  - OK
*                  IFAULT  = 1     X is negative in which case
*                                    we return a function value = 0
*-
      implicit none
      real x
      integer ifault
* local
      real y
      real r
* symbolic constants

      integer ERROR
      integer OK
      real S,C,S3,S4,S5,D1
      parameter( S = 1.0e-5)
      parameter( C = 8.5e0)
      parameter( S3 = 8.333333333e-2)
      parameter( S4 = 8.333333333e-3)
      parameter( S5 = 3.968253968e-3)
      parameter( D1 = -0.5772156649)
      parameter (ERROR = 1)
      parameter (OK   = 0)

      DIGAMMA = 0.0
      y       = x
      ifault  = ERROR
*
* check for negative argument
*

      if ( y .gt. DIGAMMA) then
        ifault = OK
*
* use approximation if argument  .LE. S
*
        if( y .LE. S ) then

          DIGAMMA = D1 - 1.0/y

        else

* reduce to DIGAMMA( X + N ), (X +  N ) .GE. C

          do while ( y .LT. C)
            DIGAMMA = DIGAMMA  - 1.0/y
            y  = y  + 1.0
          end do

* use stirling when argument  .GE. C

          r    =  1.0/y
          DIGAMMA = DIGAMMA  + log(y) - 0.5 * r
          r    =  r * r
          DIGAMMA = DIGAMMA  - r * ( S3 -r * ( S4 - r * S5))
        end if
      end if
      end
