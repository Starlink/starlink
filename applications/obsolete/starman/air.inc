CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIR.INC
C
C   alan penny                 ral             1990=-02-05


      real      pi              ! Pi
      parameter ( pi=3.1415926536)
   
      real     EXPTIM 		! Exposure time

      integer   LOCFILT(2)      !Filter descriptor location start/end
      real      EXTVAL(10)      !Possible extinction values
      integer   NUMFILT		!Number of filter names
      integer   COORDTYPE        !Ra, Dec input format (1=degrees,2=radians,3=string)

      double precision DLAT	!Latitude of site in radians

      character*20 FNAME(10)    !Filter names
      character*79 FILTNAME     !Name of descriptor with filter info
      character*79 EXPNAME      !Name of descriptor having exposure time
      character*79 AIRNAME      !Name of descriptor having airmass
      character*79 RANAME       !Name of descriptor having RA
      character*79 DECNAME      !Name of descriptor having Dec
      character*79 SIDTNAME     !Name of descriptor having Sid Time

      logical  DOHAIR        ! Get header airmass 
      logical  DOHFILT       ! Apply header airmass corection
      logical  DOHTIME       ! Apply header exposure time corection
      logical  DOKTIME       ! Apply keyboard exposure time corection


      common / aira /  exptim,  numfilt,  locfilt,  extval, coordtype

      common / airb /  dohair,  dohtime,  dohfilt, doktime

      common / airc /  fname,  filtname,  expname,  airname, raname,
     +                 decname, sidtname

      common / aird /  dlat
