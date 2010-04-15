      subroutine rv_correct(filnam,vcorr,ians)
*+
* Name:
*    RV_CORRECT

* Invocation:
*    CALL RV_CORRECT(FILNAM,VCORR,IANS)

* Purpose:
*   Get radial velocity correction
* Description:
*   To obtain a value to be subtracted from the radial velocities
*   obtained from the fits, to allow for earth motion etc.

* Arguments:
*    FILNAM (c* = INTEGER (Given)
*        Logical name for file eg. 'image', must NOT have
*                   any trailing blanks
*    VCORR = REAL (Returned)
*        Correction as above
*    IANS = INTEGER (Returned)
*        Which velocity used:-
*                 1 - VHEL
*                 2 - VLSR
*                 3 - VGAL
*                 4 - VGROUP (local group)
*                 5 - Other
*
*    RDINF searches both the .FITS and .OBS structures for the values
*    requested, and will make an attempt to cope with different data
*    types.  RV evaluates the corrections based upon the input data.

* Authors:
*  TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*                   Durham
*  JWP: J.W.Palmer Manchester, from September 1995
*  ACD: A.C. Davenhall, Starlink, Edinburgh.
*  MJC: Malcolm J Currie (Starlink)

* History:
*  TNW: Original version
*  TNW: to have FILNAM parameter
*  TNW: 30-OCT-1989 to use QMENU
*  TNW: 11-MAY-1990 To look for UTSTART
*  TNW: 2-JAN-1991 to use GEN_TIME rather than LIB$DATE_TIME.
*  TNW: 6-FEB-1991 Bug fixed regarding maximum allowed value for year.
*  TNW: 28-FEB-1991 Error reporting improved
*  TNW: 26-JAN-2-FEB-1994 Start to allow user to set FITS keyword
*       mapping
*  TNW: 8-FEB-1994 If observatory not read correctly, only prompt for
*       that
*  JWP: 5-OCT-1995 Bug fixed regarding picking up correct values for
*       RA and DEC from the FITS header
*  ACD: 5-SEP-2000 Made reading year from character string more robust.
*  ACD: 28/9/00 Remove local unused variables.
*  MJC: 2005 February 22:  Removed NSMALL definition for f90 compiler.
*
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'arc_dims'
      real vcorr
      integer status
      integer rv
      real vhel,vlsr,vgal,vlgrp,ra(3),dec(3)
      real d,h
      integer year,month,day,dsign
      real hours
      logical qstat,par_qnum
      integer equinox
      logical julian_epoch,jset
      character*(*) filnam
      character*50 observatory
      real value(3)
      character*72 string
      integer rvstatus
      integer ians
      logical if_obs_pos
      real lat(3),long(3)
      integer len1,lday,ldate
      real default,curyear
      integer dumi,nfnd,nfnd2,iloop,i,iloop2
      real dumr,C
      parameter (C = 2.997925e5)
      character dumc
      integer lstat
      integer NDICT
      parameter (NDICT = 6)
      character*38 dict(NDICT)
      data dict/
     :     'HEL   : Suns ref frame',
     :     'LSR   : Local standard of rest',
     :     'GAL   : Galaxy ref frame',
     :     'GROUP : Local galactic group ref frame',
     :     'OTHER : Other - enter from terminal',
     :     'RE-ENTER : Re-enter values'/
      data dec,ra,day,month/0.0,0.0,0.0,0.0,0.0,0.0,1,1/

* Set default year to current

      call gen_time(0,string(21:29),lday,string(1:20),ldate,
     :     string(30:41),len1)
      read(string(ldate-3:ldate),'(i4)', iostat=lstat) year
      if (lstat .ne. 0) then
         year = 2000
         call par_wruser('Failed to obtain year; assuming 2000',
     :     status)
      end if
      curyear = real(year)
      jset = .false.

* Try, if possible, to get values of variables from the data file.
* If this fails, prompt for them. Only the .OBS and .FITS structures
* are searched, and only variables likely to be in a suitable form
* are used.

 1    continue

* Dec
* JWP code to set up correct values for dec() (Oct 1995)
      dsign = 0
      status = SAI__OK
      call rdinf(filnam,'DEC',1,dec(1),nfnd,status)
      if(status.ne.SAI__OK) then
         status=0
         call rdinf(filnam,'MEANDEC',1,dec(1),nfnd,status)
      endif
      if(status.eq.SAI__OK) then
         if(dec(1).lt.0.0) then
            dsign = -1
         else
            dsign = 1
         endif
         d = dsign*dec(1)
         dec(1) = AINT(d)
         dec(2) = AINT((d-dec(1))*60.0)
         dec(3) = (((d-dec(1))*60.0)-dec(2))*60.0
         dec(1) = dsign*dec(1)
      else
         status = SAI__OK
         call par_wruser(
     :        'Declination deg,mins,secs (1 field may be negative)'
     :        ,status)
         default=dec(1)
         qstat = par_qnum('Enter declination degrees',-90.0,+90.0
     :        ,default,.true.,'Degrees',dec(1))
         default=dec(2)
         qstat = par_qnum('Enter declination minutes',-60.0,+60.0,
     :        default,.true.,'minutes',dec(2))
         default=dec(3)
         qstat = par_qnum('Enter declination seconds',-60.0,+60.0,
     :        default,.true.,'seconds',dec(3))
      endif


*  Try to get sign on assumption that first non-zero element will have
*  sign required

      if(dsign.eq.0) then
         if(dec(1).gt.VAL__SMLR) then
            dsign = 1
         else if(abs(dec(1)).gt.VAL__SMLR) then
            dsign = -1
            dec(1) = abs(dec(1))
         else if(dec(2).gt.VAL__SMLR) then
            dsign = 1
         else if(abs(dec(2)).gt.VAL__SMLR) then
            dsign = -1
            dec(2) = abs(dec(2))
         else if(dec(3).gt.VAL__SMLR) then
            dsign = 1
         else if(abs(dec(3)).gt.VAL__SMLR) then
            dsign = -1
            dec(3) = abs(dec(3))
         end if
      end if
      call chr_fill(' ',string)
      len1=1
      call chr_putc('DEC',string,len1)
      len1 = len1+1
      call chr_putr(dec(1),string,len1)
      len1 = len1+1
      call chr_putr(dec(2),string,len1)
      len1 = len1+1
      call chr_putr(dec(3),string,len1)
      call par_wruser(string,status)

* RA
* JWP code to set up correct values for ra() (Oct 1995)
      status = SAI__OK
      call rdinf(filnam,'RA',3,ra,nfnd,status)
      if(status.ne.SAI__OK) then
         status = SAI__OK
         call rdinf(filnam,'MEANRA',1,ra(1),nfnd,status)
      endif
      if(status.eq.SAI__OK) then
* (RA seems to be in degrees)
         ra(1)=ra(1)/15.0
         h = ra(1)
         ra(1) = AINT(h)
         ra(2) = AINT((h-ra(1))*60.0)
         ra(3) = (((h-ra(1))*60.0)-ra(2))*60.0
      else
         status = SAI__OK
         call par_wruser('RA hours,mins,secs',status)
         default=ra(1)
         qstat = par_qnum('Enter RA hours',0.0,24.0,default,
     :        .true.,'Hours',ra(1))
         default=ra(2)
         qstat = par_qnum('Enter RA minutes',0.0,60.0,default,
     :        .true.,'minutes',ra(2))
         default=ra(3)
         qstat = par_qnum('Enter RA seconds',0.0,60.0,default,
     :        .true.,'seconds',ra(3))
      end if
      call chr_fill(' ',string)
      len1=1
      call chr_putc('RA',string,len1)
      len1 = len1+1
      call chr_putr(ra(1),string,len1)
      len1 = len1+1
      call chr_putr(ra(2),string,len1)
      len1 = len1+1
      call chr_putr(ra(3),string,len1)
      call par_wruser(string,status)

* Equinox

      do iloop = 1, NFITSVAR
         if(fits_unames(iloop,FITS_EQUINOX).ne.' ') then
            status = SAI__OK
            call rdinf(filnam,fits_unames(iloop,FITS_EQUINOX),1,value
     :           ,nfnd,status)
            if(status.eq.SAI__OK) then
               goto 4
            else if(status.eq.1) then
               call rdcinf(filnam,fits_unames(iloop,FITS_EQUINOX),string
     :              ,status)
               if(status.eq.SAI__OK) then
                  if(string(1:1).eq.'B') then
                     jset = .true.
                     julian_epoch = .false.
                     call chr_ctor(string(2:),value(1),status)
                     if(status.eq.SAI__OK) goto 4
                  else if(string(1:1).eq.'J') then
                     jset = .true.
                     julian_epoch = .true.
                     call chr_ctor(string(2:),value(1),status)
                     if(status.eq.SAI__OK) goto 4
                  end if
               end if
            end if
         end if
      enddo
      status = SAI__OK
      qstat = par_qnum('Year of equinox?',1900.0,3000.0,1950.0,.true.
     :     ,'Years',value(1))
 4    continue
      equinox=nint(value(1))
      call chr_fill(' ',string)
      len1 = 1
      call chr_putc('Equinox:- ',string,len1)
      call chr_puti(equinox,string,len1)
      call par_wruser(string(:len1),status)

      if(.not.jset) julian_epoch = (equinox.ge.1984)

* Date

      status=3
      call rdinf(filnam,'UTDATE',3,value,nfnd,status)
      if(status.eq.SAI__OK) then

* Allow for either order of date (likely to have been a character string
* )

         year = nint(max(value(1),value(3)))
         day = nint(min(value(1),value(3)))
         month = nint(value(2))
      else
         status = SAI__OK
         call rdinf(filnam,'YEAR',1,value(1),nfnd,status)
         if(status.ne.SAI__OK) then
            status = SAI__OK
            default=real(year)
            qstat = par_qnum('Year of observation?',1800.0,curyear
     :           ,default,.true.,'Years',value(1))
         end if
         year=nint(value(1))
         call rdinf(filnam,'DAY',1,value(1),nfnd,status)
         if(status.eq.SAI__OK) then
            day=nint(value(1))
         end if
         default=real(day)
         qstat = par_qnum('Day of observation?',1.0,366.0,default,.true.
     :        ,'Days',value(1))
         day=nint(value(1))
         if(day.lt.32) then
            default = real(month)
            qstat = par_qnum('Month of observation?',1.0,12.0,default,
     :           .true.,'Months',value(1))
            month = nint(value(1))
         end if
      end if
      call chr_fill(' ',string)
      len1 = 1
      call chr_putc('Date:- ',string,len1)
      call chr_puti(day,string,len1)
      call chr_putc('/',string,len1)
      call chr_puti(month,string,len1)
      call chr_putc('/',string,len1)
      call chr_puti(year,string,len1)
      call par_wruser(string(:len1),status)

* UT

      do iloop = 1, NFITSVAR
         if(fits_unames(iloop,FITS_UT).ne.' ') then
            status = SAI__OK
            call rdinf(filnam,fits_unames(iloop,FITS_UT),3,value
     :           ,nfnd,status)
            if(status.eq.SAI__OK) goto 5
         end if
      enddo
      status = SAI__OK
      call par_wruser('UT hours,mins,secs',status)
      qstat = par_qnum('Enter UT hours',0.0,24.0,0.0,.true.,'Hours',
     :     value(1))
      qstat = par_qnum('Enter UT minutes',0.0,60.0,0.0,.true.,'minutes'
     :     ,value(2))
      qstat = par_qnum('Enter UT seconds',0.0,60.0,0.0,.true.,'seconds'
     :     ,value(3))
 5    continue
      call chr_fill(' ',string)
      len1=1
      call chr_putc('UT',string,len1)
      len1 = len1+1
      call chr_putr(value(1),string,len1)
      len1 = len1+1
      call chr_putr(value(2),string,len1)
      len1 = len1+1
      call chr_putr(value(3),string,len1)
      call par_wruser(string,status)
      hours=value(1)+(((value(3)/60.0)+value(2))/60.0)

* Observatory name or position

      status = SAI__OK
      do iloop = 1, NFITSVAR
         if(fits_unames(iloop,FITS_LONG_OBS).ne.' ') then
            call rdinf(filnam,fits_unames(iloop,FITS_LONG_OBS),3,long
     :           ,nfnd,status)
            if(status.eq.SAI__OK) then
               do i = 1, 3
                  long(i) = long(i)*fits_corrtn(iloop,FITS_LONG_OBS)
               enddo
               do iloop2 = 1, NFITSVAR
                  if(fits_unames(iloop2,FITS_LAT_OBS).ne.' ') then
                     call rdinf(filnam,fits_unames(iloop2,FITS_LAT_OBS)
     :                    ,3,lat,nfnd2,status)
                     if(status.eq.SAI__OK) then
                        do i = 1, 3
                           lat(i) = lat(i)*fits_corrtn(iloop2
     :                          ,FITS_LAT_OBS)
                        enddo
                        call chr_fill(' ',string)
                        len1=1
                        call chr_putc('Observatory position',string,len1
     :                       )
                        len1 = len1+1
                        call chr_putr(abs(long(1)),string,len1)
                        if(nfnd.gt.1) then
                           len1 = len1+1
                           call chr_putr(long(2),string,len1)
                           len1 = len1+1
                           call chr_putr(long(3),string,len1)
                        endif
                        if(long(1).gt.0.0) then
                           call chr_putc(' W,',string,len1)
                        else
                           call chr_putc(' E,',string,len1)
                        endif
                        len1 = len1+1
                        call chr_putr(abs(lat(1)),string,len1)
                        if(nfnd2.gt.1) then
                           len1 = len1+1
                           call chr_putr(lat(2),string,len1)
                           len1 = len1+1
                           call chr_putr(lat(3),string,len1)
                        endif
                        if(lat(1).gt.0.0) then
                           call chr_putc(' N',string,len1)
                        else
                           call chr_putc(' S',string,len1)
                        endif
                        call par_wruser(string,status)
                        if_obs_pos=.true.
                        goto 3
                     endif
                  endif
               enddo
            endif
         endif
      enddo
      if_obs_pos=.false.
      do iloop = 1, NFITSVAR
         if(fits_unames(iloop,FITS_TELESCOP).ne.' ') then
            call rdcinf(filnam,fits_unames(iloop,FITS_TELESCOP)
     :           ,observatory,status)
            if(status.eq.SAI__OK) then
               len1 = 0
               call chr_putc(' Observatory :',string,len1)
               call chr_appnd(observatory,string,len1)
               call par_wruser(string(:len1),status)
               goto 3
            endif
         endif
      enddo
 2    continue
      call par_qstr('Observatory (or "HELP")?',' ',.false.,.true.
     :     ,observatory)
 3    continue
      status = SAI__OK

* Evaluate the radial velocities, unless OBSERVATORY is set to "HELP",
* in which case give help information.

      rvstatus = rv(vhel,vlsr,vgal,vlgrp,ra,dec,observatory,year,month
     :     ,day,hours,equinox,julian_epoch,lat,long,if_obs_pos,dsign)
      if(observatory.eq.'HELP') then
         status=1
         goto 2
      end if
      if(rvstatus.eq.1) then
         call par_wruser(
     :        'Error reading observatory (must be SLALIB-compatable)'
     :        ,status)
         go to 2
      else if(rvstatus.eq.2) then
         call par_wruser('Error with date',status)
      else if(rvstatus.eq.3) then
         call par_wruser('Error with equinox',status)
      else if(rvstatus.eq.4) then
         call par_wruser('Object below horizon, or very low!!',status)
      end if

* Write the various velocities to the terminal.
* Ask user which velocity to use, and set VCORR to that

      write(string,'(1x,1p,''V_HEL='',e12.5,'' V_LSR='',e12.5)')vhel
     :     ,vlsr
      call par_wruser(string,status)
      write(string,'(1x,1p,''V_GAL='',e12.5,'' V_LGROUP='',e12.5)')
     :     vgal,vlgrp
      call par_wruser(string,status)
      call qmenu('Velocity Correction',dict,NDICT,2,dumr,dumc,ians,dumi,
     :     status)
      if(ians.eq.1) then
         vcorr = vhel
      else if(ians.eq.2) then
         vcorr = vlsr
      else if(ians.eq.3) then
         vcorr = vgal
      else if(ians.eq.4) then
         vcorr = vlgrp
      else if(ians.eq.5) then
         qstat = par_qnum('Enter value',-C,C,0.0,.true.,'km/s',vcorr)
      else if(ians.eq.6) then
         goto 1
      end if
      end
