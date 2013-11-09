*+  STR_EBC2ASC - Translate an EBCDIC string to an ascii one
      SUBROUTINE STR_EBC2ASC( EBC, ASC )
*
*    Description :
*
*     Translates each character of an EBCDIC string to its equivalent
*     in the ASCII collating sequence.
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
      CHARACTER*(*)             EBC
*
*    Export :
*
      CHARACTER*(*)             ASC
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
     :  '00'X,'10'X,'5C'X,'5C'X,'20'X,'26'X,'2D'X,'5C'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'7B'X,'7D'X,'5C'X,'30'X,
     :  '01'X,'11'X,'5C'X,'5C'X,'5C'X,'5C'X,'2F'X,'5C'X,'61'X,'6A'X,
     :    '7E'X,'5C'X,'41'X,'4A'X,'5C'X,'31'X,
     :  '02'X,'12'X,'5C'X,'16'X,'5C'X,'5C'X,'5C'X,'5C'X,'62'X,'6B'X,
     :    '73'X,'5C'X,'42'X,'4B'X,'53'X,'32'X,
     :  '03'X,'13'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'63'X,'6C'X,
     :    '74'X,'5C'X,'43'X,'4C'X,'54'X,'33'X,
     :  '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'64'X,'6D'X,
     :    '75'X,'5C'X,'44'X,'4D'X,'55'X,'34'X,
     :  '09'X,'5C'X,'0A'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'65'X,'6E'X,
     :    '76'X,'5C'X,'45'X,'4E'X,'56'X,'35'X,
     :  '5C'X,'08'X,'17'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'66'X,'6F'X,
     :    '77'X,'5C'X,'46'X,'4F'X,'57'X,'36'X,
     :  '7F'X,'5C'X,'1B'X,'04'X,'5C'X,'5C'X,'5C'X,'5C'X,'67'X,'70'X,
     :    '78'X,'5C'X,'47'X,'50'X,'58'X,'37'X/
      DATA  TRANS2/
     :  '5C'X,'18'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'68'X,'71'X,
     :    '79'X,'5C'X,'48'X,'51'X,'59'X,'38'X,
     :  '5C'X,'19'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'60'X,'69'X,'72'X,
     :    '7A'X,'5C'X,'49'X,'52'X,'5A'X,'39'X,
     :  '5C'X,'5C'X,'5C'X,'5C'X,'5B'X,'5D'X,'7C'X,'3A'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,
     :  '0B'X,'5C'X,'5C'X,'5C'X,'2E'X,'24'X,'2C'X,'23'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,
     :  '0C'X,'1C'X,'5C'X,'14'X,'3C'X,'2A'X,'25'X,'40'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,
     :  '0D'X,'1D'X,'05'X,'15'X,'28'X,'29'X,'5F'X,'27'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,
     :  '0E'X,'1E'X,'06'X,'5C'X,'2B'X,'3B'X,'3E'X,'3D'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'5C'X,
     :  '0F'X,'1F'X,'07'X,'1A'X,'21'X,'5E'X,'3F'X,'22'X,'5C'X,'5C'X,
     :    '5C'X,'5C'X,'5C'X,'5C'X,'5C'X,'FF'X/
*-

*    Translate only up to minimum of two lengths
      LEN = MIN( CHR_SIZE(EBC), CHR_SIZE(ASC) )

*    Do translation
      DO I = 1, LEN
        ICH = ICHAR(EBC(I:I))
        IF ( ICH .LT. 0 ) ICH = 256 + ICH
        HI = ICH / 16
        LO = MOD(ICH,16)
        IF ( LO .GT. 7 ) THEN
          ASC(I:I) = CHAR(TRANS2(HI,LO))
        ELSE
          ASC(I:I) = CHAR(TRANS1(HI,LO))
        END IF
      END DO

      END
