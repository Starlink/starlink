*+  HDX_TYPINT - Is TYPE an integer type?
      LOGICAL FUNCTION HDX_TYPINT( TYPE )
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Sep 91 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(*)         TYPE                ! HDS type to test
*-

*    Do this test in likely order if incidence in datasets because most
*    compilers will short cut the evaluation on the first successful match
      HDX_TYPINT = ( ( TYPE .EQ. '_INTEGER' ) .OR.
     :               ( TYPE .EQ. '_UBYTE' ) .OR.
     :               ( TYPE .EQ. '_BYTE' ) .OR.
     :               ( TYPE .EQ. '_UWORD' ) .OR.
     :               ( TYPE .EQ. '_WORD' ) )

      END
