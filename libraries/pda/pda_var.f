c
      double precision function pda_var(dxr,d2xr,d3xr,d4xr,d5xr,pr,qr)
c
c      Appl. Statist. algorithm 128.2 (1978), vol 27
c
c      Computes David-Johnson approximation for the variance of
c      the rth largest order statistic from the normal dist. for
c      a sample size n.
c
c      arguments : dxr - first derivative of normal probability integral
c                        evaluated at xr.
c                   :                   :                 :
c                 d5xr - fifth derivative of normal probability integral
c                        evaluated at xr.
c                   pr - expected value of rth largest order statistic
c                        from uniform dist. ( = r/(n+1) r=1,...,n).
c                   qr - 1-pr
c                        n.b. xr is the inverse normal probability integral
c                        of pr.
c
      implicit none
      double precision dxr,d2xr,d3xr,d4xr,d5xr,pr,qr
c
c      local real variables
c
      double precision d2xr2,dxr2,fiveth,fourth,half,onept5,prqr,
     1       qrmpr,rn2,rn22,rn23,two,three
c
      common /cons/ rn2,rn22,rn23
c
c      initialise constants
c
      data fourth /0.25d0/, half /0.5d0/, onept5 /1.5d0/,
     1     fiveth /1.6666666667d0/, two /2.0d0/ , three /3.0d0/
      dxr2=dxr*dxr
      prqr=pr*qr
      pda_var=prqr*dxr2/rn2
c
c     to order (n+2)**(-2)
c
      qrmpr=qr-pr
      d2xr2=d2xr*d2xr
      pda_var = pda_var+prqr/rn22*(two*qrmpr*dxr*d2xr+prqr*
     1    (dxr*d3xr+half*d2xr2))
c
c     to order (n+2)**(-3)
c
      pda_var = pda_var+prqr/rn23*(-two*qrmpr*dxr*d2xr+
     1    (qrmpr*qrmpr-prqr)*(two*dxr*d3xr+onept5*d2xr2)
     2    +prqr*qrmpr*(fiveth*dxr*d4xr+three*d2xr*d3xr)
     3    +fourth*prqr*prqr*(dxr*d5xr+two*d2xr*d4xr+
     4    fiveth*d3xr*d3xr))
      return
      end
