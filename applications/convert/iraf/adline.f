      subroutine adline (im, card)
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
      integer sp
      integer cp
      integer rp
      integer ua
      integer op
      integer maxlea
	character*(*) card
      save
         call smark (sp)
         call salloc (cp, 161 , 2)
         call f77upk (card, memc(cp), 161 )
         maxlea = (200 + memi(im+6) - 713 + 1) * 2
         ua = ((((im+713 )-1)*2+1))
         rp=ua
110      if (.not.(memc(rp) .ne. 0)) goto 112
111         rp=rp+1
            goto 110
112      continue
         if (.not.(rp - ua + 80 + 1 .ge. maxlea)) goto 120
            call sysers (836, card)
120      continue
         if (.not.(rp .gt. ua .and. memc(rp-1) .ne. 10)) goto 130
            memc(rp) = 10
            rp = rp + 1
130      continue
         do 140 op = rp, rp + 80
            memc(op) = 32
140      continue
141      continue
         call amovc (memc(cp), memc(rp), 80)
         memc(rp+80) = 10
         memc(rp+80+1) = 0
         memi(im+2) = 1
         call sfree (sp)
100      return
      end
c     maxlea  max_lenuserarea
c     sysers  syserrs
