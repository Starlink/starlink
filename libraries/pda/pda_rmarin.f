
      subroutine PDA_RMARIN(IJ,KL,STATUS)
C  This subroutine and the next function generate random numbers. See
C  the comments for PDA_SA for more information. The only changes from the
C  orginal code is that (1) the test to make sure that PDA_RMARIN runs first
C  was taken out since PDA_SA assures that this is done (this test didn't
C  compile under IBM's VS Fortran) and (2) typing ivec as integer was
C  taken out since ivec isn't used. With these exceptions, all following
C  lines are original.

C  Modification:
C     No longer stop when given arguments out of range, but return
C     a status equal to 1. Also no longer print the message.  (HME)

C This is the initialization routine for the random number generator
C     PDA_RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
      real U(97), C, CD, CM
      integer I97, J97
      integer STATUS
      common /pda_raset1/ U, C, CD, CM, I97, J97
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or.
     *    KL .lt. 0  .or.  KL .gt. 30081 ) then
C          print '(A)', ' The first random number seed must have a value
C     *between 0 and 31328'
C          print '(A)',' The second seed must have a value between 0 and
C     *30081'
C            stop
         STATUS = 1
         return
      endif
      i = mod(IJ/177, 177) + 2
      j = mod(IJ    , 177) + 2
      k = mod(KL/169, 178) + 1
      l = mod(KL,     169)
      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      return
      end
