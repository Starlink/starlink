      subroutine getlin (im, cardno, card)
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
      integer cardno
      integer sp
      integer cp
      integer rp
      integer ua
      integer start
      integer endc
      integer n
      integer len
	character*(*) card
      save
         call smark (sp)
         call salloc (cp, 161 , 2)
         rp=cp
110      if (.not.(memc(rp) .ne. 0)) goto 112
            memc(rp) = 32
111         rp=rp+1
            goto 110
112      continue
         ua = ((((im+713 )-1)*2+1))
         start = ua
         n = 0
         rp=ua
120      if (.not.(memc(rp) .ne. 0)) goto 122
            if (.not.(memc(rp) .eq. 10)) goto 130
               n=n+1
               if (.not.(n .eq. (cardno-1) )) goto 140
                  start = rp+1
140            continue
               if (.not.(n .eq. cardno )) goto 150
                  endc = rp
150            continue
130         continue
121         rp=rp+1
            goto 120
122      continue
         len = endc - start
         call amovc (memc(start), memc(cp), len)
         call f77pak ( memc(cp), card, 161 )
         call sfree(cp)
         call sfree (sp)
100      return
      end
