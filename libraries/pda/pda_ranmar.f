
      function pda_ranmar()
      real U(97), C, CD, CM
      integer I97, J97
      common /pda_raset1/ U, C, CD, CM, I97, J97
         uni = U(I97) - U(J97)
         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         PDA_RANMAR = uni
      return
      END
