      SUBROUTINE TR32 (X,Y,MX,MY)
      SAVE
C
      COMMON /ISOSR1/ ISLBT      ,U          ,V          ,W
C
C A.S.F. FOR SCALING
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
      XX = X
      YY = Y
      IF (ISLBT)  10, 20, 30
   10 CALL TRN32I (SU(U),SV(XX-1.),SW(YY-1.),XT,YT,DUM,2)
      GO TO  40
   20 CALL TRN32I (SU(XX-1.),SV(V),SW(YY-1.),XT,YT,DUM,2)
      GO TO  40
   30 CALL TRN32I (SU(XX-1.),SV(YY-1.),SW(W),XT,YT,DUM,2)
   40 MX = XT
      MY = YT
      RETURN
      END
