c
      subroutine pda_der(x,dx,d2x,d3x,d4x,d5x)
c
c      Appl. Statist. algorithm as 128.1 (1978), vol 27.
c      Davis C.S. and Stephens M.A.
c
c      Computes derivatives for the david-johnson approximation to the
c      variances and covariances of normal order statistics.
c
c      arguments : x - real number at which derivative is calculated.
c                 dx - first derivative of normal probability integral
c                      evaluated at x.
c                  :                 :               :
c                d5x - fifth derivative of normal probability integral
c                      evaluated at x.
c
      implicit none
      double precision x,dx,d2x,d3x,d4x,d5x
c
c      local real variables
c
      double precision forty6,one,onept5,rad2pi,seven,six,
     1       term,twent4,two,twopi,x2
c
c     initialise constants
c
      data one /1.0d0/, onept5 /1.5d0/, two /2.0d0/,
     1     six /6.0d0/,seven /7.0d0/, twent4 /24.0d0/,
     2     forty6 /46.0d0/, rad2pi /2.506628274631d0/,
     3     twopi/6.2831853071796d0/
      x2=x*x
      dx=rad2pi*exp(x2/two)
      d2x=twopi*x*exp(x2)
      d3x=twopi*rad2pi*(two*x2+one)*exp(onept5*x2)
      term=twopi*twopi*exp(two*x2)
      d4x=term*x*(six*x2+seven)
      d5x=term*dx*(x2*(twent4*x2+forty6)+seven)
      return
      end
