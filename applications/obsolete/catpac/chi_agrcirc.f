*+  ADC_GRCIRC - Evaluate Great Circle distance
      double precision function chi_agrcirc(ilong1, ilat1, ilong2,
     :      ilat2)
*    Description :
*     Evaluate the great circle distance betwenn two points in a
*     spherical coordinate system
*    Invocation
*     ADC_GRCIRC(ILONG1, ILAT1, ILONG2, ILAT2)
*    Parameters :
*     ILONG1=DOUBLE(INPUT)
*           Longitude of position 1 in radians
*     ILAT1=DOUBLE(INPUT)
*           Latitude of position 1 in radians
*     ILONG2=DOUBLE(INPUT)
*           Longitude of position 2 in radians
*     ILAT2=DOUBLE(INPUT)
*           Latitude of position 2 in radians
*     ADC_GRCIRC=DOUBLE(OUTPUT)
*           Great circle distance in radians between P1 and P2
*    Method :
*     Use the cosine rule
*    Authors :
*     Jon Fairclough & Brian Stewart(RAL::IPMAF)
*    History :
*     22-Apr-1985: Original
*     890815     : Check that values are less than pi (STADAT::DATAMAN)
*     19-Jun-1990: Fix 24 hour boundary and neg dec   (STADAT::ARW)
*    Type Definitions :
      implicit none
*    Global constants :
      include 'CHIPAR2_PAR'                ! SLA contants
*    Import
      real*8 ilong1
      real*8 ilat1
      real*8 ilong2
      real*8 ilat2
*    Local variables :
      real*8 tlong1
      real*8 tlat1
      real*8 tlong2
      real*8 tlat2
      real*8 a                           ! Corner angles
      real*8 ab, ac                      ! Side angles
      real*8 cbc                         ! Cosines of the side angles
      real*8 dsec
      integer hms, dec
      integer ihr, imin
      integer ideg
      integer istat
*-
*    Begin
*
      tlong1 = ilong1
      tlat1  = ilat1
      tlong2 = ilong2
      tlat2  = ilat2
******************************************************
*    Covert to radians if needed - recognise by long OR lat > pi or pi/2

      if ( tlong1 .gt. D2PI .or. abs(tlat1) .gt. DPIBY2) then
         hms = nint( tlong1)
         dec = nint( tlat1 )
         ihr = hms/10000
         imin= (hms - 10000*ihr)/100
         dsec= (tlong1 - 10000*ihr - 100*imin)
         call sla_dtf2r( ihr, imin, dsec, tlong1, istat)
         ideg = abs(dec)/10000
         imin= (abs(dec) - 10000*ideg)/100
         dsec= (abs(dec) - 10000*ideg - 100*imin)
         call sla_daf2r( ideg, imin, dsec, tlat1, istat)
         tlat1= tlat1*sign(1.0, float(dec) )
      endif
      if ( tlong2 .gt. D2PI .or. abs(tlat2) .gt. DPIBY2) then
         hms = nint( tlong2)
         dec = nint( tlat2 )
         ihr = hms/10000
         imin= (hms - 10000*ihr)/100
         dsec= (tlong2 - 10000*ihr - 100*imin)
         call sla_dtf2r( ihr, imin, dsec, tlong2, istat)
         ideg = abs(dec)/10000
         imin= (abs(dec) - 10000*ideg)/100
         dsec= (abs(dec) - 10000*ideg - 100*imin)
         call sla_daf2r( ideg, imin, dsec, tlat2, istat)
         tlat2= tlat2*sign(1.0, float(dec) )
      endif
****************************************************************
      ab = dpiby2 - tlat1
      ac = dpiby2 - tlat2
*
*    Compute the included angle
*
*    If the included angle is greater than PI or less than -PI then subtract
*    or add 2PI to compensate for points on either side of the Date line.
*
      a  = tlong2 - tlong1
      if (a .lt. -DPI) a = a + D2PI
      if (a .gt. DPI) a = a - D2PI
*
*    Cosine rule with ACOS protection  : ABS(CBC) .le. 1
*
      cbc = cos(ac)*cos(ab) + sin(ac)*sin(ab)*cos(a)
      if (cbc .gt. 1) then
         cbc = 1
      endif
      if (cbc .lt. -1) then
         cbc = -1
      endif
*
      chi_agrcirc = acos (cbc)
*
      if (chi_agrcirc .lt. 0) then
         chi_agrcirc = abs(chi_agrcirc)
      endif
*
      end
