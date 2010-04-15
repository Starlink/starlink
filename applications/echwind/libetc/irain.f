*+  IRAIN  -  Set up a rainbow-like colour index range

      subroutine irain(ns,ne,igrey)
c
      integer ib, idel, ig, igrey, iir, io, ir, iuv, iy
      integer ns, ne
      real col(3), be, bs, ge, gs, re, rs
c
      col(1)=0.7
      col(2)=0.7
      col(3)=0.7
      call PGSCR(igrey, col(1), col(2), col(3))
c
      idel=(ne-ns)/6

      iir=ns
      ir=iir+2.0*idel
      io=iir+3.5*idel
      iy=iir+4.2*idel
      ig=iir+4.6*idel
      ib=iir+5.0*idel
      iuv=ne
c
      rs=0.5
      re=1.0
      gs=0.0
      ge=0.0
      bs=0.0
      be=0.0
      call ishower(iir,ir,rs,re,gs,ge,bs,be)
c
      rs=re
      re=1.0
      gs=ge
      ge=0.5
      bs=be
      be=0.0
      call ishower(ir,io,rs,re,gs,ge,bs,be)
c
      rs=re
      re=1.0
      gs=ge
      ge=1.0
      bs=be
      be=0.0
      call ishower(io,iy,rs,re,gs,ge,bs,be)
c
      rs=re
      re=0.0
      gs=ge
      ge=1.0
      bs=be
      be=0.0
      call ishower(iy,ig,rs,re,gs,ge,bs,be)
      rs=re
c
      re=0.0
      gs=ge
      ge=0.0
      bs=be
      be=1.0
      call ishower(ig,ib,rs,re,gs,ge,bs,be)
c
      rs=re
      re=0.0
      gs=ge
      ge=0.0
      bs=be
      be=0.5
      call ishower(ib,iuv,rs,re,gs,ge,bs,be)
c
      return
      end

