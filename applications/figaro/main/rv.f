      integer function rv(vhel,vlsr,vgal,vlgrp,right_ac,dec,observatory
     :     ,year,month,day,hours,equinox,julian_epoch,inlat,inlong,
     :     if_obs_pos,dsign)
*+
* Name:
*    RV

* Invocation:
*   (INTEGER) = RV(VHEL,VLSR,VGAL,VLGRP,RIGHT_AC,DEC,OBSERVATORY
*            ,YEAR,MONTH,DAY,HOURS,EQUINOX,JULIAN_EPOCH,INLAT,INLONG,
*            IF_OBS_POS,DSIGN)
* Purpose:
*   Get radial velocity correction

* Description:
*   Get radial velocity correction
*
* Arguments:-
*
*      HOURS           r Time of observation in hours
*      JULIAN_EPOCH    l If Julian epoch
*      YEAR            i Year
*      MONTH           i Month
*      DAY             i Day (n.b. if this is the day of the year,
*                        month should be 1, e.g. 200 is ok, for month=1)
*      EQUINOX         i Epoch
*      RIGHT_AC(3)     r Right ascension
*      DEC(3)          r Declination
*      IF_OBS_POS      l If observatory position given
*       If above true then:-
*      INLAT(3)        r Latitude of observatory
*      INLONG(3)       r Longitude of observatory
*       Otherwise:-
*      OBSERVATORY c*(*) Name of observatory (or "HELP")
*        (i.e. only inlat and inlong OR OBSERVATORY needed
*                                      ==
*      DSIGN           i Sign of DEC (+1 or -1)
*   Correction to subtract from observed velocity to get...
*      VLSR            r Local standard of rest velocity
*      VGAL            r Galactic velocity
*      VLGRP           r Local group velocity
*      VHEL            r Heliocentric velocity
*      RV              i Error status:
*                              0  ok
*                              1  Error with observatory position
*                              2  Error with date
*                              3  Error with equinox
*                              4  Object below horizon
*
*  Observatory Position Record:
*     This is either the station identifier for use by the
*     sla_OBS routine, or a longitude and latitude.  The
*     longitude and latitude are each three numeric fields,
*     degrees, arcminutes, arcseconds;  West and North are
*     positive.
*
*  Date & Number of days record:
*     The date is three numeric fields, year, month, day,
*     and the number of days is a single numeric field.
*     THE DATE IS THE UT DATE, NOT THE LOCAL DATE, in the
*     conventional (Gregorian) calendar.
*
*  Star Position Record:
*     The star position is supplied as a mean RA,Dec in either
*     the old (FK4) or new (FK5) system.  There are seven numeric
*     fields;  fields 1-3 are the RA hours, minutes, seconds,
*     fields 4-6 are the Dec degrees, arcminutes, arcseconds,
*     and field 7 is the equinox, optionally preceded by B
*     or J.
*
*  P.T.Wallace   Starlink   25 April 1986
*
*  Adapted for LONGSLIT (made into a function)   T.N.Wilkins 1986
*  Change to use sla_dtt rather than sla_dta TNW 21/11/89 CAVAD
*  More tidying, TNW 22/5/90.
*  Minor change TNW 21/3/91
*  Made to work properly for observatory given as longitude/latitude,
*  also tidied up at bit.      TNW 29/4/91
*  AUKM value improved (taken from later version of rv), TNW 29/11/93
*  Improved error trapping, TNW 1/12/93
*  Bug fix as regard layout of IF blocks, TNW 2/2/94
*  Use sla_rvlsrk, 5/5/94
*-
      implicit none

* input

      real hours
      character*(*) observatory
      logical julian_epoch
      integer year
      integer month
      integer day
      integer equinox,dsign
      real right_ac(3)
      real dec(3)
      real inlat(3),inlong(3)
      logical if_obs_pos

* output

      real vlsr
      real vgal
      real vlgrp
      real vhel

* Local

      real values(6)
      integer next_char

*  Functions

      real sla_rvlsrk
      real sla_rvgalc
      real sla_rvlg
      real sla_rverot
      double precision sla_dtt
      double precision sla_epj
      double precision sla_gmst
      double precision sla_dvdv

*  Radians to degrees

      double precision R2D
      parameter (R2D = 57.29577951308232087679815d0)

*  AU to km

      double precision AUKM
      parameter (AUKM = 149.597870d6)

*  Variables

      character ident*10,name*40

      integer j,j1,n,i,iwd,iwm,ipd,ipm
      integer ird,irm,idd,idm

      real r2s,d2s

      double precision w,p,h,ws,ps,djm0,deltat
      double precision rs,ds,r,d,e,r2,d2,rw,dw,v2(3),ra,da
      double precision vclsr,vcgalc,vclg,tdb,ut,st,ha,cosz
      double precision zd,dvb(3),dpb(3),dvh(3),dph(3),vcrot,vcorb
      integer status
*
*  Observatory record
*  ------------------

*  Help
*  Check for termination or supplementary help request

      if(if_obs_pos) then
         do i = 1,3
            values(i) = inlong(i)
            values(i+3) = inlat(i)
         end do
         name = '?'
         iwd = nint(values(1))
         iwm = nint(values(2))
         ws  = dble(values(3))
         call sla_daf2r(abs(iwd),iwm,ws,w,j)
         ipd = nint(values(4))
         ipm = nint(values(5))
         ps  = dble(values(6))
         call sla_daf2r(abs(ipd),ipm,ps,p,j1)
         if ((j1.ne.0).or.(j.ne.0)) then
            rv = 1
            call par_wruser('Error converting to radians',status)
            return
         end if

*     If values were -ve, make sure ok

         if(values(1).lt.0.0) then
            w = -w
         end if
         if(values(4).lt.0.0) then
            p = -p
         end if
      else if(observatory(:4).eq.'HELP') then
         call par_wruser(
     :        'Enter either the observatory longitude (West +ve)',status
     :        )
         call par_wruser('and latitude,',status)
         call par_wruser('or an observatory identifier, for example:'
     :        ,status)
         call par_wruser('      -149 03 57.9  -31 16 37',status)
         call par_wruser(' or:',status)
         call par_wruser( '      AAT',status)

*    Supplementary help - table of observatory IDs

         ident = ' '
         n = 1
         call sla_obs(n,ident,name,w,p,h)
         do while (name.ne.'?')
            call par_wruser(' '//ident//' '//name,status)
            n = n+1
            call sla_obs(n,ident,name,w,p,h)
         end do
         rv = 0
         return

*    Interpret input

      else

*    Look for observatory ID, perhaps preceded by spaces

         ident = ' '
         call chr_ldblk(observatory)
         call chr_move(observatory,ident)

*      Request observatory parameters

         call sla_obs(0,ident,name,w,p,h)
         if (name.eq.'?') then
            name = '(anon)'

*       Valid observatory ID not found;  decode long & lat.

            next_char = 1
            call sla_dafin(observatory,next_char,w,j)
            call sla_dafin(observatory,next_char,p,j)
            if (j.ne.0) then
               rv = 1
               return
            end if
         end if
      end if
*
*    Date record
*    -----------

*    Prompt and input

*    Check for termination or help request


*    Decode date,days

      call sla_caldj(year,month,day,djm0,j)
      if (j.ne.0.and.j.ne.3) then
         rv = 2
         return
      end if

*    UT1 (neglecting UT1-UTC)

      ut = djm0+dble(hours)/24d0

*    Local ST (radians)

      st = sla_gmst(ut)-w

*    Correction from UTC to TDT (days)

      deltat = sla_dtt(djm0)/86400d0

*    TDB

      tdb = ut+deltat
*
*    Star record
*    -----------
*    Interpret input

*    Get RA,Dec,Equinox

      ird = nint(right_ac(1))
      irm = nint(right_ac(2))
      rs = right_ac(3)
      idd = nint(dec(1))
      idm = nint(dec(2))
      ds = dec(3)
      call sla_dtf2r(ird,irm,rs,r,j)
      call sla_daf2r(abs(idd),idm,ds,d,j)
      e = equinox
      if (j.ne.0.or.e.lt.1900d0.or.e.gt.2100d0) then
         rv = 3
         return
      end if

*    Accept new data

      if(right_ac(1).lt.0.0) r = -r
      if(dsign.eq.-1) d = -d

*    Mean J2000 place

      r2 = r
      d2 = d
      if(julian_epoch) then
         call sla_preces('FK5',e,2000d0,r2,d2)
      else
         call sla_preces('FK4',e,1950d0,r2,d2)
         call sla_fk45z(r2,d2,sla_epj(tdb),rw,dw)
         r2 = rw
         d2 = dw
      end if
      call sla_dcs2c(r2,d2,v2)

*    Geocentric apparent place

      call sla_map(r2,d2,0d0,0d0,0d0,0d0,2000d0,tdb,ra,da)

*    Slowly changing velocity components

      r2s = real(r2)
      d2s = real(d2)

*    Sun with respect to LSR

      vclsr = dble(sla_rvlsrk(r2s,d2s))

*    LSR with respect to galaxy

      vcgalc = dble(sla_rvgalc(r2s,d2s))

*    Sun with respect to local group

      vclg = dble(sla_rvlg(r2s,d2s))

*      ---------------------------------------
*      RADIAL COMPONENT OF OBSERVERS VELOCITY'
*      ---------------------------------------
*       Units: km/s
*
*       Sign convention: +ve means the observer is moving
*      AWAY FROM the position. (i.e. To correct an observed radial
*      velocity to one of the given rest standards, SUBTRACT the
*      appropriate tabulated value.)
*      The solar velocity defining the LSR
*     (Local Standard of Rest) is
*     20 km/s towards  18 00 00.0 +30 00 00  1900  (AQ3).
*     The LSR velocity with respect to the galactic centre is'
*     250 km/s towards  L2=90, B2=0  (AQ3).
*     The solar velocity with respect to
*     the mean motion of the local group is
*     300 km/s towards  L2=90, B2 = 0  (IAU 1976).
*

*    ZD (degrees)

      ha = st-ra
      cosz = sin(p)*sin(da)+cos(p)*cos(da)*cos(ha)
      zd = R2D*(atan2(sqrt(1d0-min(1d0,cosz*cosz)),cosz))

*    Earth/Sun velocity and position

      call sla_evp(tdb,2000d0,dvb,dpb,dvh,dph)

*    Velocity components

*    Earth rotation

      vcrot = dble(sla_rverot(real(p),real(ra),real(da),real(st)))

*    Earth orbit

      vcorb = -sla_dvdv(v2,dvh)*AUKM

*    Totals

      vhel  =  real(vcrot+vcorb)
      vlsr  = vhel+real(vclsr)
      vgal  = vlsr+real(vcgalc)
      vlgrp = vhel+real(vclg)
      if (zd.ge.90d0) then
         rv = 4
      else
         rv = 0
      end if
      end
