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
         op = rp
140      if (.not.(op .le. rp + 80)) goto 142
            if (.not.(memc(cp) .eq. 0 .or. memc(cp) .eq. 10)) goto 150
               goto 142
150         continue
            memc(op) = memc(cp)
            cp = cp + 1
141         op = op + 1
            goto 140
142      continue
160      if (.not.(op .le. rp+80)) goto 162
            memc(op) = 32
161         op = op + 1
            goto 160
162      continue
         memc(op) = 10
         memc(op+1) = 0
         memi(im+2) = 1
         call sfree (sp)
100      return
      end
c     maxlea  max_lenuserarea
c     sysers  syserrs
