*+   CHI_ABLOCK - Block data for CHI parser system
      BLOCK DATA CHI_ABLOCK
*    Description :
*     Initialises common blocks for the CHI parser
*    Invocation
*     None
*    Parameters :
*     None
*    Method :
*     Use data statement to set common block values
*    Authors :
*     Alan Wood (STADAT::ARW)  Jon Fairclough (RAL::IPMAF)
*    History :
*     4-Feb-1992:Original
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'                 ! SAI Symbolic Constants
      include 'CHI_PAR'
      include 'CHIPAR_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      include 'CHIWRK_CMN'
*    Local constants
      integer chi__mxitmexd
      parameter (chi__mxitmexd=chi__mxitm*chi__mxexd)
      integer chi__mxrdsexd
      parameter (chi__mxrdsexd=chi__mxrds*chi__mxexd)
      integer chi__mxcnsexd
      parameter (chi__mxcnsexd=chi__mxcns*chi__mxexd)
*
*   adcelm_cmn_1
*
      data Etype/chi__mxelm*0/
*
*   adcelm_cmn_1
*
      data Enitems /0/
      data Ename/chi__mxelm*' '/
      data Eformt/chi__mxelm*' '/
      data Eunit/chi__mxelm*' '/
*      data Enfrmt/chi__mxelm*' '/
      data Enull/chi__mxelm*' '/
      data Ecomnt/chi__mxelm*' '/
*
*   adcelm_cmn_2
*
*
*   adcexp_cmn
*
*      data Xfree/chi__mxexd*.true./
      data Xsize/0/
      data Xlist/chi__mxitm*0/
      data Xqual/chi__mxitm*0/
      data Xrdsize/0/
      data Xrdlist/chi__mxrds*0/
      data Xlval/chi__mxcns*.false./
      data Xival/chi__mxcns*0/
      data Xrval/chi__mxcns*0.0/
      data Xdval/chi__mxcns*0.0d0/
      data Xname/' '/
      data Xunit/' '/
      data Xcomnt/' '/
      data Xstring/' '/
*
*    adcwrk_cmn
*
      data Wlist/chi__mxitm*0/
      data Wqual/chi__mxitm*0/
      data Worigin/chi__mxitm*0/
      data Wlval/chi__mxcns*.false./
      data Wival/chi__mxcns*0/
      data Wrval/chi__mxcns*0.0/
      data Wdval/chi__mxcns*0.d0/
      data Wstring/' '/
*
*    adcfun_cmn
*
      data ftions( 1)/'SQRT'/
      data fsizes( 1)/4/
      data ftargs( 1)/1/
      data fatype( 1,1)/CHI__C/
*      data fatype( 1,1)/CHI__D/
      data ftcons( 1)/'NOT_C'/
      data fcomnt( 1)/'NONE'/
      data funits( 1)/'NONE'/
      data fdtres( 1)/CHI__D/

      data ftions( 2)/'LOG10'/
      data fsizes( 2)/5/
      data ftargs( 2)/1/
      data fatype( 2,1)/CHI__D/
      data ftcons( 2)/'NOT_C'/
      data fcomnt( 2)/'NONE'/
      data funits( 2)/'NONE'/
      data fdtres( 2)/CHI__D/

      data ftions( 3)/'LOG'/
      data fsizes( 3)/3/
      data ftargs( 3)/1/
      data fatype( 3,1 )/CHI__D/
      data ftcons( 3)/'NOT_C'/
      data fcomnt( 3)/'NONE'/
      data funits( 3)/'NONE'/
      data fdtres( 3)/CHI__D/

      data ftions( 4)/'EXP'/
      data fsizes( 4)/3/
      data ftargs( 4)/1/
      data fatype( 4,1 )/CHI__D/
      data ftcons( 4)/'NOT_C'/
      data fcomnt( 4)/'NONE'/
      data funits( 4)/'NONE'/
      data fdtres( 4)/CHI__D/

      data ftions( 5)/'SIN'/
      data fsizes( 5)/3/
      data ftargs( 5)/1/
      data fatype( 5,1 )/CHI__D/
      data ftcons( 5)/'NOT_C'/
      data fcomnt( 5)/'NONE'/
      data funits( 5)/'NONE'/
      data fdtres( 5)/CHI__D/

      data ftions( 6)/'COS'/
      data fsizes( 6)/3/
      data ftargs( 6)/1/
      data fatype( 6,1 )/CHI__D/
      data ftcons( 6)/'NOT_C'/
      data fcomnt( 6)/'NONE'/
      data funits( 6)/'NONE'/
      data fdtres( 6)/CHI__D/

      data ftions( 7)/'TAN'/
      data fsizes( 7)/3/
      data ftargs( 7)/1/
      data fatype( 7,1 )/CHI__D/
      data ftcons( 7)/'NOT_C'/
      data fcomnt( 7)/'NONE'/
      data funits( 7)/'NONE'/
      data fdtres( 7)/CHI__D/

      data ftions( 8)/'ASIN'/
      data fsizes( 8)/4/
      data ftargs( 8)/1/
      data fatype( 8,1 )/CHI__D/
      data ftcons( 8)/'NOT_C'/
      data fcomnt( 8)/'NONE'/
      data funits( 8)/'NONE'/
      data fdtres( 8)/CHI__D/

      data ftions( 9)/'ACOS'/
      data fsizes( 9)/4/
      data ftargs( 9)/1/
      data fatype( 9,1 )/CHI__D/
      data ftcons( 9)/'NOT_C'/
      data fcomnt( 9)/'NONE'/
      data funits( 9)/'NONE'/
      data fdtres( 9)/CHI__D/

      data ftions(10)/'ATAN'/
      data fsizes(10)/4/
      data ftargs(10)/1/
      data fatype(10,1)/CHI__D/
      data ftcons(10)/'NOT_C'/
      data fcomnt(10)/'NONE'/
      data funits(10)/'NONE'/
      data fdtres(10)/CHI__D/

      data ftions(11)/'SINH'/
      data fsizes(11)/4/
      data ftargs(11)/1/
      data fatype(11,1)/CHI__D/
      data ftcons(11)/'NOT_C'/
      data fcomnt(11)/'NONE'/
      data funits(11)/'NONE'/
      data fdtres(11)/CHI__D/

      data ftions(12)/'COSH'/
      data fsizes(12)/4/
      data ftargs(12)/1/
      data fatype(12,1)/CHI__D/
      data ftcons(12)/'NOT_C'/
      data fcomnt(12)/'NONE'/
      data funits(12)/'NONE'/
      data fdtres(12)/CHI__D/

      data ftions(13)/'TANH'/
      data fsizes(13)/4/
      data ftargs(13)/1/
      data fatype(13,1)/CHI__D/
      data ftcons(13)/'NOT_C'/
      data fcomnt(13)/'NONE'/
      data funits(13)/'NONE'/
      data fdtres(13)/CHI__D/

      data ftions(14)/'ABS'/
      data fsizes(14)/3/
      data ftargs(14)/1/
      data fatype(14,1)/CHI__D/
      data ftcons(14)/'NOT_C'/
      data fcomnt(14)/'NONE'/
      data funits(14)/'NONE'/
      data fdtres(14)/CHI__D/

      data ftions(15)/'STRING'/
      data fsizes(15)/6/
      data ftargs(15)/2/
      data fatype(15,1)/CHI__C/
      data fatype(15,2)/CHI__C/
      data ftcons(15)/'C_ONLY'/
      data fcomnt(15)/'NONE'/
      data funits(15)/'NONE'/
      data fdtres(15)/CHI__L/

*      data ftions(16)/'GLONG_EG50'/    ! GLONG = EG50X(RA, DEC)
*      data fsizes(16)/10/
*      data ftargs(16)/2/
*      data fatype(16,1)/CHI__D/
*      data fatype(16,2)/CHI__D/
*      data ftcons(16)/'B1950'/
*      data fcomnt(16)/'DEGREE'/
*      data funits(16)/'RADIAN'/
*      data fdtres(16)/CHI__D/
*
*      data ftions(17)/'GLAT_EG50'/    ! GLAT  = EG50Y(RA, DEC)
*      data fsizes(17)/9/
*      data ftargs(17)/2/
*      data fdtype(17,1)/CHI__D/
*      data ftcons(17)/'B1950'/
*      data fcomnt(17)/'DEGREE'/
*      data funits(17)/'RADIAN'/
*      data fdtres(17)/CHI__P/
*
*      data ftions(18)/'RA_GE50'/    ! RA    = GE50X(GLONG, GLAT)
*      data fsizes(18)/7/
*      data ftargs(18)/2/
*      data fdtype(18)/CHI__D/
*      data ftcons(18)/'B1950'/
*      data fcomnt(18)/'HHMMSS.S'/
*      data funits(18)/'RADIAN'/
*      data fdtres(18)/CHI__O/
*
*      data ftions(19)/'DEC_GE50'/    ! DEC   = GE50Y(GLONG, GLAT)
*      data fsizes(19)/8/
*      data ftargs(19)/2/
*      data fdtype(19)/CHI__D/
*      data ftcons(19)/'B1950'/
*      data fcomnt(19)/'SDDMMSS'/
*      data funits(19)/'RADIAN'/
*      data fdtres(19)/CHI__Q/
*
*      data ftions(20)/'RA_FK4PRE'/  ! RA2   = FK4PREX(EP1, EP2, RA1, DEC1)
*      data fsizes(20)/9/
*      data ftargs(20)/4/
*      data fdtype(20)/CHI__D/
*      data ftcons(20)/'B1950'/
*      data fcomnt(20)/'HHMMSS.S'/
*      data funits(20)/'RADIAN'/
*      data fdtres(20)/CHI__O/
*
*      data ftions(21)/'DEC_FK4PRE'/  ! DEC2  = FK4PREY(EP1, EP2, RA1, DEC1)
*      data fsizes(21)/10/
*      data ftargs(21)/4/
*      data fdtype(21)/CHI__D/
*      data ftcons(21)/'B1950'/
*      data fcomnt(21)/'SDDMMSS'/
*      data funits(21)/'RADIAN'/
*      data fdtres(21)/CHI__Q/
*
*      data ftions(22)/'RA_FK5PRE'/  ! RA2   = FK5PREX(EP1, EP2, RA1, DEC1)
*      data fsizes(22)/9/
*      data ftargs(22)/4/
*      data fdtype(22)/CHI__D/
*      data ftcons(22)/'J2000'/
*      data fcomnt(22)/'HHMMSS.S'/
*      data funits(22)/'RADIAN'/
*      data fdtres(22)/CHI__O/
*
*      data ftions(23)/'DEC_FK5PRE'/  ! DEC2  = FK5PREY(EP1, EP2, RA1, DEC1)
*      data fsizes(23)/10/
*      data ftargs(23)/4/
*      data fdtype(23)/CHI__D/
*      data ftcons(23)/'J2000'/
*      data fcomnt(23)/'SDDMMSS'/
*      data funits(23)/'RADIAN'/
*      data fdtres(23)/CHI__Q/
*
*      data ftions(24)/'GLONG_EQGAL'/   ! GLONG  = EQGALX(RA, DEC)
*      data fsizes(24)/11/
*      data ftargs(24)/2/
*      data fdtype(24)/CHI__D/
*      data ftcons(24)/'J2000'/
*      data fcomnt(24)/'DEGREE'/
*      data funits(24)/'RADIAN'/
*      data fdtres(24)/CHI__P/
*
*      data ftions(25)/'GLAT_EQGAL'/   ! GLAT   = EQGALY(RA, DEC)
*      data fsizes(25)/10/
*      data ftargs(25)/2/
*      data fdtype(25)/CHI__D/
*      data ftcons(25)/'J2000'/
*      data fcomnt(25)/'DEGREE'/
*      data funits(25)/'RADIAN'/
*      data fdtres(25)/CHI__P/
*
*      data ftions(26)/'RA_GALEQ'/   ! RA     = GALEQX(GLONG, GLAT)
*      data fsizes(26)/8/
*      data ftargs(26)/2/
*      data fdtype(26)/CHI__D/
*      data ftcons(26)/'J2000'/
*      data fcomnt(26)/'HHMMSS.S'/
*      data funits(26)/'RADIAN'/
*      data fdtres(26)/CHI__O/
*
*      data ftions(27)/'DEC_GALEQ'/   ! DEC    = GALEQY(GLONG, GLAT)
*      data fsizes(27)/9/
*      data ftargs(27)/2/
*      data fdtype(27)/CHI__D/
*      data ftcons(27)/'J2000'/
*      data fcomnt(27)/'SDDMMSS'/
*      data funits(27)/'RADIAN'/
*      data fdtres(27)/CHI__Q/
*
*      data ftions(28)/'RA_FK425'/   ! RA  = FK425X(RA, DEC, PMR, PMD, PX, RV)
*      data fsizes(28)/8/
*      data ftargs(28)/6/
*      data fdtype(28)/CHI__D/
*      data ftcons(28)/'B1950'/
*      data fcomnt(28)/'HHMMSS.S'/
*      data funits(28)/'RADIAN'/
*      data fdtres(28)/CHI__O/
*
*      data ftions(29)/'DEC_FK425'/   ! DEC = FK425Y(RA, DEC, PMR, PMD, PX, RV)
*      data fsizes(29)/9/
*      data ftargs(29)/6/
*      data fdtype(29)/CHI__D/
*      data ftcons(29)/'B1950'/
*      data fcomnt(29)/'SDDMMSS'/
*      data funits(29)/'RADIAN'/
*      data fdtres(29)/CHI__Q/
*
*      data ftions(30)/'RA_FK524'/   ! RA  = FK524X(RA, DEC, PMR, PMD, PX, RV)
*      data fsizes(30)/8/
*      data ftargs(30)/6/
*      data fdtype(30)/CHI__D/
*      data ftcons(30)/'J2000'/
*      data fcomnt(30)/'HHMMSS.S'/
*      data funits(30)/'RADIAN'/
*      data fdtres(30)/CHI__O/
*
*      data ftions(31)/'DEC_FK524'/   ! DEC = FK524Y(RA, DEC, PMR, PMD, PX, RV)
*      data fsizes(31)/9/
*      data ftargs(31)/6/
*      data fdtype(31)/CHI__D/
*      data ftcons(31)/'J2000'/
*      data fcomnt(31)/'SDDMMSS'/
*      data funits(31)/'RADIAN'/
*      data fdtres(31)/CHI__Q/
*
*      data ftions(32)/'ELONG_EQECL'/   ! ELONG = EQECLX(RA, DEC, EPOCH)
*      data fsizes(32)/11/
*      data ftargs(32)/3/
*      data fdtype(32)/CHI__D/
*      data ftcons(32)/'J2000'/
*      data fcomnt(32)/'DEGREE'/
*      data funits(32)/'RADIAN'/
*      data fdtres(32)/CHI__P/
*
*      data ftions(33)/'ELAT_EQECL'/   ! ELAT = EQECLY(RA, DEC, EPOCH)
*      data fsizes(33)/10/
*      data ftargs(33)/3/
*      data fdtype(33)/CHI__D/
*      data ftcons(33)/'J2000'/
*      data fcomnt(33)/'DEGREE'/
*      data funits(33)/'RADIAN'/
*      data fdtres(33)/CHI__P/
*
*      data ftions(34)/'RA_ECLEQ'/   ! RA   = ECLEQX(ELONG, ELAT, EPOCH)
*      data fsizes(34)/8/
*      data ftargs(34)/3/
*      data fdtype(34)/CHI__D/
*      data ftcons(34)/'J2000'/
*      data fcomnt(34)/'HHMMSS.S'/
*      data funits(34)/'RADIAN'/
*      data fdtres(34)/CHI__O/
*
*      data ftions(35)/'DEC_ECLEQ'/   ! DEC  = ECLEQY(ELONG, ELAT, EPOCH)
*      data fsizes(35)/9/
*      data ftargs(35)/3/
*      data fdtype(35)/CHI__D/
*      data ftcons(35)/'J2000'/
*      data fcomnt(35)/'SDDMMSS'/
*      data funits(35)/'RADIAN'/
*      data fdtres(35)/CHI__Q/
*
*      data ftions(36)/'RA_PM'/      ! RA2  = PMX(RA1, DEC1, PMR, PMD, PX, RV,
*      data fsizes(36)/5/            !            EP1, EP2)
*      data ftargs(36)/8/
*      data fdtype(36)/CHI__D/
*      data ftcons(36)/'NONE'/
*      data fcomnt(36)/'HHMMSS.SSS'/
*      data funits(36)/'RADIAN'/
*      data fdtres(36)/CHI__O/
*
*      data ftions(37)/'DEC_PM'/      ! DEC2 = PMY(RA1, DEC1, PMR, PMD, PX, RV,
*      data fsizes(37)/6/             !            EP1, EP2)
*      data ftargs(37)/8/
*      data fdtype(37)/CHI__D/
*      data ftcons(37)/'NONE'/
*      data fcomnt(37)/'SDDMMSS.SS'/
*      data funits(37)/'RADIAN'/
*      data fdtres(37)/CHI__Q/
*
      data ftions(38)/'DIFF'/        ! Absolute difference ABS(a-b)
      data fsizes(38)/4/
      data ftargs(38)/2/
      data fatype(38,1)/CHI__D/
      data fatype(38,2)/CHI__D/
      data ftcons(38)/'NOT_C'/
      data fcomnt(38)/'NONE'/
      data funits(38)/'NONE'/
      data fdtres(38)/CHI__D/

      data ftions(39)/'GREAT_CIRCLE'/ ! Great circle distance in radians
      data fsizes(39)/12/             ! D = GREAT_CIRCLE(LONG1, LAT1
      data ftargs(39)/4/              !                  LONG2, LAT2)
      data fatype(39,1)/CHI__D/
      data fatype(39,2)/CHI__D/
      data fatype(39,3)/CHI__D/
      data fatype(39,4)/CHI__D/
      data ftcons(39)/'NOT_C'/
      data fcomnt(39)/'ARCSEC'/
      data funits(39)/'RADIAN'/
      data fdtres(39)/CHI__D/

*      data ftions(40)/'SGLONG_GALSUP'/! SGLONG =
*      data fsizes(40)/13/             ! SGLONG_GALSUP(GLONG, GLAT)
*      data ftargs(40)/2/
*      data fdtype(40)/CHI__D/
*      data ftcons(40)/'NONE'/
*      data fcomnt(40)/'DEGREE'/
*      data funits(40)/'RADIAN'/
*      data fdtres(40)/CHI__P/
*
*      data ftions(41)/'SGLAT_GALSUP'/ ! SGLAT =
*      data fsizes(41)/12/             ! SGLAT_GALSUP(GLONG, GLAT)
*      data ftargs(41)/2/
*      data fdtype(41)/CHI__D/
*      data ftcons(41)/'NONE'/
*      data fcomnt(41)/'DEGREE'/
*      data funits(41)/'RADIAN'/
*      data fdtres(41)/CHI__P/

*      data ftions(42)/'GLONG_SUPGAL'/   ! GLONG =
*      data fsizes(42)/12/               ! GLONG_SUPGAL(SGLONG, SGLAT)
*      data ftargs(42)/2/
*      data fdtype(42)/CHI__D/
*      data ftcons(42)/'NONE'/
*      data fcomnt(42)/'DEGREE'/
*      data funits(42)/'RADIAN'/
*      data fdtres(42)/CHI__P/

*      data ftions(43)/'GLAT_SUPGAL'/   ! GLAT =
*      data fsizes(43)/11/              ! GLAT_SUPGAL(SGLONG, SGLAT)
*      data ftargs(43)/2/
*      data fdtype(43)/CHI__D/
*      data ftcons(43)/'NONE'/
*      data fcomnt(43)/'DEGREE'/
*      data funits(43)/'RADIAN'/
*      data fdtres(43)/CHI__P/

      data ftions(44)/'IRAS_TEMP23'/
      data fsizes(44)/11/
      data ftargs(44)/2/
      data fatype(44,1)/CHI__D/
      data fatype(44,2)/CHI__D/
      data ftcons(44)/'NOT_C'/
      data fcomnt(44)/'NONE'/
      data funits(44)/'deg K'/
      data fdtres(44)/CHI__I/

      data ftions(45)/'IRAS_TEMP12'/
      data fsizes(45)/11/
      data ftargs(45)/2/
      data fatype(45,1)/CHI__D/
      data fatype(45,2)/CHI__D/
      data ftcons(45)/'NOT_C'/
      data fcomnt(45)/'NONE'/
      data funits(45)/'deg K'/
      data fdtres(45)/CHI__I/

      data ftions(46)/'IRAS_TEMP34'/
      data fsizes(46)/11/
      data ftargs(46)/2/
      data fatype(46,1)/CHI__D/
      data fatype(46,2)/CHI__D/
      data ftcons(46)/'NOT_C'/
      data fcomnt(46)/'NONE'/
      data funits(46)/'deg K'/
      data fdtres(46)/CHI__I/

      data ftions(47)/'INT'/
      data fsizes(47)/3/
      data ftargs(47)/1/
      data fatype(47,1)/CHI__D/
      data ftcons(47)/'NOT_C'/
      data fcomnt(47)/'NONE'/
      data funits(47)/'NONE'/
      data fdtres(47)/CHI__I/

      data ftions(48)/'NINT'/
      data fsizes(48)/4/
      data ftargs(48)/1/
      data fatype(48,1)/CHI__D/
      data ftcons(48)/'NOT_C'/
      data fcomnt(48)/'NONE'/
      data funits(48)/'NONE'/
      data fdtres(48)/CHI__I/

      data ftions(49)/'MIN'/
      data fsizes(49)/3/
      data ftargs(49)/-2/
      data fatype(49,1)/CHI__D/
      data ftcons(49)/'NOT_C'/
      data fcomnt(49)/'NONE'/
      data funits(49)/'NONE'/
      data fdtres(49)/CHI__D/

      data ftions(50)/'MAX'/
      data fsizes(50)/3/
      data ftargs(50)/-2/
      data fatype(50,1)/CHI__D/
      data ftcons(50)/'NOT_C'/
      data fcomnt(50)/'NONE'/
      data funits(50)/'NONE'/
      data fdtres(50)/CHI__D/

      data ftions(51)/'UCASE'/
      data fsizes(51)/5/
      data ftargs(51)/1/
      data fatype(51,1)/CHI__C/
      data ftcons(51)/'C_ONLY'/
      data fcomnt(51)/'NONE'/
      data funits(51)/'NONE'/
      data fdtres(51)/CHI__C/

      data ftions(52)/'LCASE'/
      data fsizes(52)/5/
      data ftargs(52)/1/
      data fatype(52,1)/CHI__C/
      data ftcons(52)/'C_ONLY'/
      data fcomnt(52)/'NONE'/
      data funits(52)/'NONE'/
      data fdtres(52)/CHI__C/


*      data ftions(53)/'CSI1EXP'/
*      data fsizes(53)/7/
*      data ftargs(53)/1/
*      data fatype(53)/CHI__C/
*      data ftcons(53)/'C_ONLY'/
*      data fcomnt(53)/'NONE'/
*      data funits(53)/'NONE'/
*      data fdtres(53)/CHI__C/


*      data ftions(54)/'CSI2EXP'/
*      data fsizes(54)/7/
*      data ftargs(54)/1/
*      data fdtype(54)/CHI__C/
*      data ftcons(54)/'C_ONLY'/
*      data fcomnt(54)/'NONE'/
*      data funits(54)/'NONE'/
*      data fdtres(54)/CHI__C/


*      data ftions(55)/'CSI3EXP'/
*      data fsizes(55)/7/
*      data ftargs(55)/1/
*      data fdtype(55)/CHI__C/
*      data ftcons(55)/'C_ONLY'/
*      data fcomnt(55)/'NONE'/
*      data funits(55)/'NONE'/
*      data fdtres(55)/CHI__C/


*      data ftions(56)/'CSI4EXP'/
*      data fsizes(56)/7/
*      data ftargs(56)/1/
*      data fdtype(56)/CHI__C/
*      data ftcons(56)/'C_ONLY'/
*      data fcomnt(56)/'NONE'/
*      data funits(56)/'NONE'/
*      data fdtres(56)/CHI__C/


*      data ftions(57)/'CSI5EXP'/
*      data fsizes(57)/7/
*      data ftargs(57)/1/
*      data fdtype(57)/CHI__C/
*      data ftcons(57)/'C_ONLY'/
*      data fcomnt(57)/'NONE'/
*      data funits(57)/'NONE'/
*      data fdtres(57)/CHI__C/


*      data ftions(58)/'CSI6EXP'/
*      data fsizes(58)/7/
*      data ftargs(58)/1/
*      data fdtype(58)/CHI__C/
*      data ftcons(58)/'C_ONLY'/
*      data fcomnt(58)/'NONE'/
*      data funits(58)/'NONE'/
*      data fdtres(58)/CHI__C/


*      data ftions(59)/'CSI7EXP'/
*      data fsizes(59)/7/
*      data ftargs(59)/1/
*      data fdtype(59)/CHI__C/
*      data ftcons(59)/'C_ONLY'/
*      data fcomnt(59)/'NONE'/
*      data funits(59)/'NONE'/
*      data fdtres(59)/CHI__C/


*      data ftions(60)/'CSI8EXP'/
*      data fsizes(60)/7/
*      data ftargs(60)/1/
*      data fdtype(60)/CHI__C/
*      data ftcons(60)/'C_ONLY'/
*      data fcomnt(60)/'NONE'/
*      data funits(60)/'NONE'/
*      data fdtres(60)/CHI__C/


*      data ftions(61)/'CSI9EXP'/
*      data fsizes(61)/7/
*      data ftargs(61)/1/
*      data fdtype(61)/CHI__C/
*      data ftcons(61)/'C_ONLY'/
*      data fcomnt(61)/'NONE'/
*      data funits(61)/'NONE'/
*      data fdtres(61)/CHI__C/


*      data ftions(62)/'CSI10EXP'/
*      data fsizes(62)/8/
*      data ftargs(62)/1/
*      data fdtype(62)/CHI__C/
*      data ftcons(62)/'C_ONLY'/
*      data fcomnt(62)/'NONE'/
*      data funits(62)/'NONE'/
*      data fdtres(62)/CHI__C/

      data ftflag/chi__mxfun*.false./
      data ftsave/chi__mxfun*0/
*
*    chirel_cmn
*
      data  Rstart/chi__mxrel*0/
      data  Rsplit/chi__mxrel*0/
      data  Rend/chi__mxrel*0/
      data  Rcons/chi__mxrel*0/
      data  Rtype/chi__mxrel*0/
      data  Rshape/chi__mxrel*0/
      data  Rnext_free/0/

      end
