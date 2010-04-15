      SUBROUTINE
     : CHP_CONDSN( INVALUE, OUTFORMAT, OUTVALUE, STATUS )
*+
*  Name:
*     CHP_CONDSN

*  Purpose:
*     Convert a double precision value into a non standard format
*     value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_CONDSN( INVALUE, OUTFORMAT, OUTVALUE, STATUS )
*
*  Description:
*     Convert a double precision standard format value into a non standard
*     format
*     value.

*  Arguments:
*     INVALUE = DOUBLE PRECISION (Given)
*        Input value.
*     OUTFORMAT = CHARACTER * ( CHP__SZFFMT ) (Given)
*        Output non standard format.
*     OUTVALUE = CHARACTER * ( CHI__SZFFMT ) (Returned)
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
      INCLUDE 'CHP_ERR'   ! Standard CHP errors.

*  Arguments Given:
      DOUBLE PRECISION INVALUE
      CHARACTER * ( * ) OUTFORMAT

*  Arguments Returned:
      CHARACTER * ( * ) OUTVALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      logical chr_simlr


*  Local Variables:
      integer idmsf(4)
      integer ihmsf(4)
      character*1 sign
      double precision rvald
      character*(1) csign
      integer counter
      integer count
      real rval
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
*   Apply the conversion.
*
      dval = invalue
      if (chr_simlr(outformat(:4), 'HOUR')) then
         rval = dval * DR2H
         write(cval, '(F11.7)', err=1) rval
*
      elseif (chr_simlr(outformat(:6), 'MINUTE')) then
         rval = dval * DR2H * 60
         write(cval, '(F11.5)', err=1) rval
*
      elseif (chr_simlr(outformat(:6), 'SECOND')) then
         rval = dval * DR2H * 3600
         write(cval, '(F11.4)', err=1) rval
*
      elseif (chr_simlr(outformat(:12), 'HH MM SS.SSS')) then
         call sla_dr2tf(3, dval, sign, ihmsf)
         rsec = ihmsf(3) + real(ihmsf(4))/1000
         write (cval, '(I2,1X,I2,1X,F6.3)', err=1)
     -     ihmsf(1), ihmsf(2) , rsec
*
      elseif (chr_simlr(outformat(:11), 'HH MM SS.SS')) then
         call sla_dr2tf(2, dval, sign, ihmsf)
         rsec = ihmsf(3) + real(ihmsf(4))/100
         write (cval, '(I2,1X,I2,1X,F5.2)', err=1)
     -     ihmsf(1), ihmsf(2) , rsec
*
      elseif ((chr_simlr(outformat(:10), 'HH MM SS.S')) .or.
     -          (chr_simlr(outformat(:10), 'HH:MM:SS.S'))) then
         call sla_dr2tf(1, dval, sign, ihmsf)
         rsec = ihmsf(3) + real(ihmsf(4))/10
         write (cval, '(I2,1X,I2,1X,F4.1)', err=1)
     -           ihmsf(1), ihmsf(2) , rsec
*
      elseif (chr_simlr(outformat(:10), 'HHMMSS.SSS')) then
        call sla_dr2tf(3, dval, sign, ihmsf)
        rsec = ihmsf(3) + real(ihmsf(4))/1000
        write (cval, '(I2,I2,F6.3)', err=1)
     -        ihmsf(1), ihmsf(2) , rsec
*
      elseif (chr_simlr(outformat(:9), 'HHMMSS.SS')) then
        call sla_dr2tf(2, dval, sign, ihmsf)
        rsec = ihmsf(3) + real(ihmsf(4))/100
        write (cval, '(I2,I2,F5.2)', err=1)
     -        ihmsf(1), ihmsf(2) , rsec
*
      elseif (chr_simlr(outformat(:9), 'HH MM SSS')) then
        call sla_dr2tf(1, dval, sign, ihmsf)
        jsec = ihmsf(3)*10 + ihmsf(4)
        write (cval, '(I2,1X,I2,1X,I3)', err=1)
     -        ihmsf(1), ihmsf(2), jsec
*
      elseif (chr_simlr(outformat(:8), 'HHMMSS.S')) then
        call sla_dr2tf(1, dval, sign, ihmsf)
        rsec = ihmsf(3) + real(ihmsf(4))/10
        write (cval, '(2I2,F4.1)', err=1)
     -       ihmsf(1), ihmsf(2) , rsec
*
      elseif ((chr_simlr(outformat(:8), 'HH MM SS')) .or.
     -        (chr_simlr(outformat(:8), 'HH:MM:SS'))) then
        call sla_dr2tf(0, dval, sign, ihmsf)
        write (cval, '(I2,1X,I2,1X,I2)', err=1)
     -        ihmsf(1), ihmsf(2), ihmsf(3)
*
      elseif (chr_simlr(outformat(:7), 'HH MM.M')) then
        call sla_dr2tf(1, dval, sign, ihmsf)
        rmin = ihmsf(2) + real(ihmsf(3)+real(ihmsf(4))/10)/60
        write (cval, '(I2,F4.1)', err=1)
     -           ihmsf(1), rmin
*
      elseif (chr_simlr(outformat(:7), 'HHMMSSS')) then
        call sla_dr2tf(1, dval, sign, ihmsf)
        jsec = ihmsf(3)*10 + ihmsf(4)
        write (cval, '(2I2,I3)', err=1)
     -        ihmsf(1), ihmsf(2), jsec
*
      elseif (chr_simlr(outformat(:7), 'HHMM.MM')) then
        call sla_dr2tf(1, dval, sign, ihmsf)
        rmin = ihmsf(2) + real(ihmsf(3)+real(ihmsf(4))/10)/60
        write (cval, '(I2,F5.2)', err=1)
     -        ihmsf(1), rmin
*
      elseif (chr_simlr(outformat(:6), 'HHMMSS')) then
        call sla_dr2tf(0, dval, sign, ihmsf)
        write (cval, '(3I2)', err=1)
     -      ihmsf(1), ihmsf(2), ihmsf(3)
*
      elseif (chr_simlr(outformat(:4), 'HHMM')) then
        call sla_dr2tf(0, dval, sign, ihmsf)
        write (cval, '(2I2)', err=1)
     -       ihmsf(1), nint(ihmsf(2)+ihmsf(3)/60.0)
*
      elseif ((chr_simlr(outformat(:5), 'HH MM')) .or.
     -      (chr_simlr(outformat(:5), 'HH:MM'))) then
        call sla_dr2tf(0, dval, sign, ihmsf)
        write (cval, '(I2,1X,I2)', err=1)
     -        ihmsf(1), nint(ihmsf(2)+ihmsf(3)/60.0)
*
      elseif (chr_simlr(outformat(:6), 'DEGREE')) then
        rvald = dval * DR2D
        write (cval, '(F13.7)', err=1) rvald
*
      elseif (chr_simlr(outformat(:6), 'ARCMIN')) then
        rval = dval * DR2D * 60
        write (cval, '(F11.5)', err=1) rval
*
      elseif (chr_simlr(outformat(:6), 'ARCSEC')) then
        rval = dval * DR2D * 3600
        write (cval, '(1PG10.3)', err=1) rval
*
      elseif (chr_simlr(outformat(:14), 'S DD MM SS.SSS')) then
        call sla_dr2af(3, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/1000.0
        write (cval, '(A1,1X,I2,1X,I2,1X,F6.3)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif (chr_simlr(outformat(:13), 'SDD MM SS.SSS')) then
        call sla_dr2af(3, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/1000.0
        write (cval, '(A1,I2,1X,I2,1X,F6.3)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif (chr_simlr(outformat(:13), 'S DD MM SS.SS')) then
        call sla_dr2af(2, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/100.0
        write (cval, '(A1,1X,I2,1X,I2,1X,F5.2)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif ((chr_simlr(outformat(:11), 'SDD MM SS.S')) .or.
     -           (chr_simlr(outformat(:11), 'SDD:MM:SS.S'))) then
        call sla_dr2af(2, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/100.0
        write (cval, '(A1,I2,1X,I2,1X,F4.1)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif (chr_simlr(outformat(:11), 'SDDMMSS.SSS')) then
        call sla_dr2af(3, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/1000.0
        write (cval, '(A1,I2,I2,F6.3)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif (chr_simlr(outformat(:10), 'SDDMMSS.SS')) then
        call sla_dr2af(2, dval, sign, idmsf)
        rasec = idmsf(3) + idmsf(4)/100.0
        write (cval, '(A1,I2,I2,F5.2)', err=1)
     -            sign, idmsf(1), idmsf(2), rasec
*
      elseif ((chr_simlr(outformat(:9), 'SDD MM SS')) .or.
     -            (chr_simlr(outformat(:9), 'SDD:MM:SS'))) then
        call sla_dr2af(1, dval, sign, idmsf)
        write (cval, '(A1,I2,1X,I2,1X,I2)', err=1)
     -            sign, idmsf(1), idmsf(2), idmsf(3)
*
      elseif (chr_simlr(outformat(:8), 'SDD MM.M')) then
        call sla_dr2af(0, dval, sign, idmsf)
        ramin = idmsf(2) + real(idmsf(3))/60
        if (ramin .gt. 59.9) then
          ramin = 0.0
          idmsf(1) =idmsf(1) + 1
        endif
        write (cval, '(A1,I2,1X,F4.1)', err=1)
     -           sign, idmsf(1), ramin
*
      elseif ((chr_simlr(outformat(:6), 'SDD MM')) .or.
     -            (chr_simlr(outformat(:6), 'SDD:MM'))) then
        call sla_dr2af(0, dval, sign, idmsf)
        ramin = idmsf(2) + real(idmsf(3))/60
        iamin = nint(ramin)
        if (iamin .gt. 59) then
          iamin = iamin - 60
          idmsf(1) =idmsf(1) + 1
        endif
        write (cval, '(A1,I2,1X,I2)', err=1)
     -           sign, idmsf(1), iamin
*
      elseif (chr_simlr(outformat(:7), 'SDDMMSS')) then
        call sla_dr2af(0, dval, sign, idmsf)
        write (cval, '(A1,3I2)', err=1)
     -            sign, idmsf(1), idmsf(2), idmsf(3)
*
      elseif (chr_simlr(outformat(:6), 'SDDMMT')) then
        call sla_dr2af(0, dval, sign, idmsf)
        ramin = idmsf(2) + real(idmsf(3))/60
        if (ramin .gt. 59.9) then
           ramin = 0.0
           idmsf(1) =idmsf(1) + 1
        endif
        write (cval, '(A1,I2,I3)', err=1)
     -           sign, idmsf(1), int(ramin*10.0)
*
      elseif (chr_simlr(outformat(:7), 'SDDMM.M')) then
        call sla_dr2af(0, dval, sign, idmsf)
        ramin = idmsf(2) + real(idmsf(3))/60
        if (ramin .gt. 59.9) then
          ramin = 0.0
          idmsf(1) =idmsf(1) + 1
        endif
        write (cval, '(A1,I2,F4.1)', err=1)
     -           sign, idmsf(1), ramin
*
      endif
      outvalue = cval
      return
   1  continue
        status = CHP__IVLDCFMT
      end
