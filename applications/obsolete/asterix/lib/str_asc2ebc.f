*+  STR_ASC2EBC - Translate an ascii string to EBCDIC string
      SUBROUTINE STR_ASC2EBC( ASC, EBC )
*
*    Description :
*
*     Translates each character of an ASCII collating sequence TO
*     an EBCDIC string.
*
*    Method :
*
*     Simple look-up table.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     26 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*(*)             ASC
*
*    Export :
*
      CHARACTER*(*)             EBC
*
*    Function declarations :
*
      INTEGER                   CHR_SIZE
*
*    Local variables :
*
      INTEGER                   LEN, ICH, I
      INTEGER                   LO, HI
*
*    Local data :
*
      INTEGER                   TRANS1(0:15,0:7)
      INTEGER                   TRANS2(0:15,8:15)
      DATA  TRANS1/
     :  '00'X,'10'X,'40'X,'F0'X,'7C'X,'D7'X,'79'X,'97'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '01'X,'11'X,'4F'X,'F1'X,'C1'X,'D8'X,'81'X,'98'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '02'X,'12'X,'7F'X,'F2'X,'C2'X,'D9'X,'82'X,'99'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '03'X,'13'X,'7B'X,'F3'X,'C3'X,'E2'X,'83'X,'A2'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '37'X,'3C'X,'5B'X,'F4'X,'C4'X,'E3'X,'84'X,'A3'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '2D'X,'3D'X,'6C'X,'F5'X,'C5'X,'E4'X,'85'X,'A4'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '2E'X,'32'X,'50'X,'F6'X,'C6'X,'E5'X,'86'X,'A5'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '2F'X,'26'X,'7D'X,'F7'X,'C7'X,'E6'X,'87'X,'A6'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X/
      DATA  TRANS2/
     :  '16'X,'18'X,'4D'X,'F8'X,'C8'X,'E7'X,'88'X,'A7'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '05'X,'19'X,'5D'X,'F9'X,'C9'X,'E8'X,'89'X,'A8'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '25'X,'3F'X,'5C'X,'7A'X,'D1'X,'E9'X,'91'X,'A9'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '0B'X,'27'X,'4E'X,'5E'X,'D2'X,'4A'X,'92'X,'C0'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '0C'X,'1C'X,'6B'X,'4C'X,'D3'X,'E0'X,'93'X,'6A'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '0D'X,'1D'X,'60'X,'7E'X,'D4'X,'5A'X,'94'X,'D0'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '0E'X,'1E'X,'4B'X,'6E'X,'D5'X,'5F'X,'95'X,'A1'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'3F'X,
     :  '0F'X,'1F'X,'61'X,'6F'X,'D6'X,'6D'X,'96'X,'07'X,'3F'X,'3F'X,
     :    '3F'X,'3F'X,'3F'X,'3F'X,'3F'X,'FF'X/
*-

*    Translate only up to minimum of two lengths
      LEN = MIN( CHR_SIZE(EBC), CHR_SIZE(ASC) )

*    Do translation
      DO I = 1, LEN
        ICH = ICHAR(ASC(I:I))
        IF ( ICH .LT. 0 ) ICH = 256 + ICH
        HI = ICH / 16
        LO = MOD(ICH,16)
        IF ( LO .GT. 7 ) THEN
          EBC(I:I) = CHAR(TRANS2(HI,LO))
        ELSE
          EBC(I:I) = CHAR(TRANS1(HI,LO))
        END IF
      END DO

      END
