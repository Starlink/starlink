      subroutine gethis (im, hlin, hislin)
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
      integer hlin
      integer sp
      integer rp
      integer hist
      integer start
      integer endc
      integer n
      integer len
      integer*2 str(70+1)
       character*(70) hislin
      save
         call smark (sp)
         call salloc (hist, 511 , 2)
         rp=hist
110      if (.not.(memc(rp) .ne. 0)) goto 112
            memc(rp) = 32
111         rp=rp+1
            goto 110
112      continue
         call amovc (memc((((im+200 +223)-1)*2+1)) , memc(hist), 511 )
         start = hist
         n = 0
         rp=hist
120      if (.not.(memc(rp) .ne. 0)) goto 122
            if (.not.( memc(rp) .eq. 10 )) goto 130
               n = n + 1
               if (.not.(n .eq. (hlin-1) )) goto 140
                  start = rp+1
140            continue
               if (.not.(n .eq. hlin )) goto 150
                  endc = rp
150            continue
130         continue
121         rp=rp+1
            goto 120
122      continue
         len = endc - start
         call amovc (memc(start), str, len)
         call f77pak(str, hislin, len)
         call sfree(sp)
100      return
      end
