c
      double precision function pda_cov(dxr,d2xr,d3xr,d4xr,d5xr
     1                     ,pr,qr,dxs,d2xs,d3xs,d4xs,d5xs,ps)
c
c      Appl. statist. algorithm as 128.3 (1978), vol. 27
c      Davis C.S. and Stephens M.A.
c
c      Computes David-Johnson approximation for covariance between rth
c      and sth order statistics from the normal dist. for a sample size n.
c
c      arguments : dxr - first derivative of normal probability integral
c                        evaluated at xr.
c                   :                     :               :
c                 d5xr - fifth derivative of normal probability integral
c                        evaluated at xr.
c                   pr - expected value of rth order statistic from
c                        uniform dist. ( = r/(n+1) r=1,...n ).
c                   qr - 1-pr.
c                  dxs - first derivative of normal probability integral
c                        evaluated at xs.
c                   :                     :             :
c                 d5xs - fifth derivative of normal probability integral
c                        evaluated at xs.
c                   ps - expected value of sth order statistic from
c                        uniform distribution. ( = s/(n+1) s=1,...,n).
c                        n.b. xr is the inverse normal probability
c                        integral of pr etc.
c
      implicit none
      double precision dxr,d2xr,d3xr,d4xr,d5xr,pr,qr,dxs,d2xs,
     1         d3xs,d4xs,d5xs,ps
c
c      local real variables
c
      double precision eigth,five6,fourth,half,one,onept5,pr2,
     1       prqr,prqs,ps2,psqr,psqs,qr2,qrmpr,qs,qs2,qsmps,
     2       rn2,rn22,rn23,term1,term2,term3,term4,term5,
     3       three,twelth,two
c
      common /cons/ rn2,rn22,rn23
c
c      initialise constants
c
      data twelth /0.0833333333333d0/, eigth /0.125d0/,
     1     fourth /0.25d0/, half /0.5d0/,
     2     five6 /0.833333333333d0/, one /1.0d0/,
     3     onept5 /1.5d0/, two /2.0d0/, three /3.0d0/
c
      qs=one-ps
      prqs=pr*qs
      pda_cov=prqs*dxr*dxs/rn2
c
c     to order (n+2)**(-2)
c
      qrmpr=qr-pr
      qsmps=qs-ps
      prqr=pr*qr
      psqs=ps*qs
      pda_cov = pda_cov+prqs/rn22*(qrmpr*d2xr*dxs+
     1    qsmps*dxr*d2xs+half*prqr*d3xr*dxs+
     2    half*psqs*dxr*d3xs+half*prqs*d2xr*d2xs)
c
c     to order (n+2)**(-3)
c
      pr2=pr*pr
      qr2=qr*qr
      ps2=ps*ps
      qs2=qs*qs
      psqr=ps*qr
      term1=-d2xr*dxs*qrmpr-qsmps*dxr*d2xs+
     1      (qrmpr*qrmpr-prqr)*d3xr*dxs
      term2=(qsmps*qsmps-psqs)*dxr*d3xs+(onept5*qrmpr*
     1       qsmps+half*psqr-two*prqs)*d2xr*d2xs
      term3=five6*(prqr*qrmpr*d4xr*dxs+psqs*qsmps*dxr*
     1      d4xs)+(prqs*qrmpr+half*prqr*qsmps)*d3xr*d2xs
      term4=(prqs*qsmps+half*psqs*qrmpr)*d2xr*d3xs+
     1      eigth*(pr2*qr2*d5xr*dxs+ps2*qs2*dxr*d5xs)
      term5=fourth*(pr2*qr*qs*d4xr*d2xs+pr*ps*qs2*
     1      d2xr*d4xs)+twelth*(two*pr2*qs2+three*pr*qr*
     2      ps*qs)*d3xr*d3xs
      pda_cov = pda_cov+prqs/rn23*(term1+term2+term3+term4+term5)
      return
      end
