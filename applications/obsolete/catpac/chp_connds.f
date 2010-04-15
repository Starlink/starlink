      SUBROUTINE
     : CHP_CONNDS( INVALUE, INFORMAT, OUTVALUE, STATUS )
*+
*  Name:
*     CHP_CONNDS

*  Purpose:
*     Convert non standard format
*     value into a double precision standard value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_CONNDS( INVALUE, INFORMAT, OUTVALUE, STATUS )
*
*  Description:
*     Convert non standard format
*     value into a double precision standard value.

*  Arguments:
*     INVALUE = CHARACTER * ( CHI__SZFFMT ) (Given)
*        Input value.
*     INFORMAT = CHARACTER * ( CHP__SZFFMT ) (Given)
*        Input non standard format.
*     OUTVALUE = DOUBLE PRECISION (Returned)
*        Output value.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants.
      INCLUDE 'CHP_ERR'   ! Standard CHP errors.

*  Arguments Given:
      CHARACTER * ( * ) INVALUE
      CHARACTER * ( * ) INFORMAT

*  Arguments Returned:
      DOUBLE PRECISION OUTVALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      logical chr_simlr


*  Local Variables:
      character * ( 1 ) CLOCTYPES( CHP__NUMCOLS )
      character*(1) csign
      integer cd
      integer cc
      character*(chp__szcname) catfnames(chp__numcols)
      character*(chp__szcfmt) fmat
      character*(chp__szcfmt) sexfmat
      logical sexflag
      character*(12) cval
      integer ideg
      integer iabsdeg
      integer iamin
      integer iasec
      integer iramin
      integer imin
      integer ihr
      integer isec
      integer jsec
      integer w
      real rmin
      real rsec
      real rasec
      real ramin
      integer jstat
      integer fldcount
      integer len
      double precision dsec
      double precision dasec
      double precision dval

*  Local Data:
*
*  PI, 2PI, PI/2
      REAL*8 DPI
      PARAMETER (DPI=3.141592653589793238462643D0)
      REAL*8 D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
      REAL*8 DPIBY2
      PARAMETER (DPIBY2 = DPI/2)
*
*  RADIANS to DEGREES
      REAL*8 DR2D
      PARAMETER (DR2D=57.29577951308232087679815D0)
*
*  RADIANS to SECONDS
      REAL*8 DR2S
      PARAMETER (DR2S=0.1375098708313975703D+05)
*
*  RADIANS to HOURS
      REAL*8 DR2H
      PARAMETER (DR2H=DR2S/3600)
*
*  RADIANS to ARC SECONDS
      REAL*8 DR2AS
      PARAMETER (DR2AS=0.2062648062470963560D+06)
*
*  DEGREES to RADIANS
      REAL*8 DD2R
      PARAMETER (DD2R=1.745329251994329576923691D-02)
*
*  SECONDS to RADIANS
      REAL*8 DS2R
      PARAMETER (DS2R=0.7272205216643039849D-04)
*
*  HOURS TO RADIANS
      REAL*8 DH2R
      PARAMETER (DH2R=DS2R*3600)
*
*  MINUTES TO RADIANS
      REAL*8 DM2R
      PARAMETER (DM2R=DS2R*60)
*
*  ARC SECONDS to RADIANS
      REAL*8 DAS2R
      PARAMETER (DAS2R=0.4848136811095359949D-05)
*
*  ARC MINUTES TO RADIANS
      REAL*8 DAM2R
      PARAMETER (DAM2R=DAS2R*60)


*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      cval = invalue
      if (chr_simlr(informat(:10), 'HH MM SS.S') .or.
     :  chr_simlr(informat(:10), 'HH:MM:SS.S')) then
        read (cval, '(I2,1X,I2,1X,F4.1)', err=1) ihr, imin, rsec
        dsec = rsec
        call sla_dtf2r(ihr, imin, dsec, dval, jstat)
*
      elseif (chr_simlr(informat(:8), 'HH MM SS') .or.
     :          chr_simlr(informat(:8), 'HH:MM:SS')) then
        read (cval, '(I2,1X,I2,1X,I2)', err=1) ihr, imin, isec
        dsec = isec
        call sla_dtf2r(ihr, imin, dsec, dval, jstat)
*
      elseif (chr_simlr(informat(:5), 'HH MM') .or.
     :          chr_simlr(informat(:5), 'HH:MM')) then
        read (cval, '(I2,1X,I2)', err=1) ihr, imin
        dsec = 0
        call sla_dtf2r(ihr, imin, dsec, dval, jstat)
*
      elseif (chr_simlr(informat(:6), 'DEGREE')) then
        w = len(cval)
        read(cval, 10, err=1) dval
   10   format(BN, D<w>.0)
        dval = dval * DD2R
*
      elseif (chr_simlr(informat(:11), 'SDD MM SS.S')
     :   .or. chr_simlr(informat(:11), 'SDD:MM:SS.S')) then
        read (cval, '(I3, 1X, I2, 1X, F4.2)', err=1)
     -  ideg, iamin, dasec
        iabsdeg = abs(ideg)
        call sla_daf2r(iabsdeg, iamin, dasec, dval, jstat)
        read (cval, '(A1)', err=1) csign
        if (csign .EQ. '-') dval = -dval
*
      elseif (chr_simlr(informat(:9), 'SDD MM SS') .or.
     :          chr_simlr(informat(:9), 'SDD:MM:SS')) then
        read (cval, '(I3, 1X, I2, 1X, I2)', err=1)
     -  ideg, iamin,iasec
        dasec = iasec
        iabsdeg = abs(ideg)
        call sla_daf2r(iabsdeg, iamin, dasec, dval, jstat)
        read (cval, '(A1)', err=1) csign
        if (csign .EQ. '-') dval = -dval
*
      elseif (chr_simlr(informat(:6), 'SDD MM') .or.
     :          chr_simlr(informat(:6), 'SDD:MM')) then
        read (cval, '(I3, 1X, I2)', err=1)
     -  ideg, iamin
        dasec = 0.0
        iabsdeg = abs(ideg)
        call sla_daf2r(iabsdeg, iamin, dasec, dval, jstat)
        read (cval, '(A1)', err=1) csign
        if (csign .EQ. '-') dval = -dval
*
      endif
*
      outvalue = dval
*
      return
   1  continue
        status = CHP__IVLDCFMT
      end
