      subroutine nhist (im, nlin, nchars)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer im
      integer nlin
      integer nchars
      integer sp
      integer rp
      integer hist
      save
         call smark (sp)
         call salloc (hist, 511 , 2)
         call amovc (memc((((im+200 +223)-1)*2+1)) , memc(hist), 511 )
         nlin = 0
         rp=hist
110      if (.not.(memc(rp) .ne. 0)) goto 112
            if (.not.( memc(rp) .eq. 10 )) goto 120
               nlin = nlin + 1 
120         continue
111         rp=rp+1
            goto 110
112      continue
         nchars = rp - hist
         call sfree(sp)
100      return
      end
