**==case.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996
c
c==============================================================================
c
      CHARACTER*(*) FUNCTION CASE(STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
c
c For UNIX, leave the cases of the characters alone!
c
      CASE = STRING
      RETURN
      END
