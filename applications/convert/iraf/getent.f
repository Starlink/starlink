      subroutine getent (buffer, lineno, line)
      integer lineno
      integer*2 buffer(1+1)
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
      integer rp
      integer start
      integer endc
      integer n
      integer len
      character*(*)     line
      save
         start = 1
         endc = 0
         n = 0
         rp=1
110      if (.not.((buffer(rp) .ne. 0) .and. (n .ne. lineno))) goto 112
            if (.not.(buffer(rp) .eq. 10)) goto 120
               n = n + 1
               if (.not.(n .eq. (lineno-1) )) goto 130
                  start = rp+1
130            continue
               if (.not.(n .eq. lineno )) goto 140
                  endc = rp
140            continue
120         continue
111         rp=rp+1
            goto 110
112      continue
         if (.not.( endc .eq. 0 )) goto 150
            endc = rp
150      continue
         len = endc - start
         call f77pak ( buffer(start), line, len)
100      return
      end
