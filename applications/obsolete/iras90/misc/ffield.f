
c   this is the first version of FFIELD (find field)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      PROGRAM FFIELD
      real      epoch, myeqnx, olon, olat
      character*8    tapes(10), unlstd
      character coord*4, posn*40, mycs*4, cra*15, cdec*15
      character plates*10
      character*18  clon,clat
      character*4 ecs
      character*6 rom
      character*1 ans,best
      integer   iptype
      integer*2 online(162)
c
      double precision        yzpxls(2,8), deams(10),
     +             pcras(10), pcdecs(10)
      integer      indx, pns(10)
c
c
c    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      common /com1/vepc, radpc, decdpc
      real         vepc(3,1716)
      real       radpc(1716), decdpc(1716)
c    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      data  myeqnx /1950./, mycs   /'equd'/
      data  unlstd /'unlstd'/, ecs /'ecl'/
      data  epoch  /1950./
      data  plates /'cmdd'/
      data online / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
     + 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     + 30, 31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 47, 48,
     + 49, 50, 51, 52, 53, 54, 55, 68, 69, 70, 71, 72, 73, 74, 75,
     + 76, 77, 78, 79, 80, 98, 99, 100, 101, 102, 103, 104, 105,
     + 106, 107, 133, 134, 135, 136, 137, 138, 293, 294, 295, 296,
     + 297, 298, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333,
     +352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364,
     +365, 366, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386,
     +387, 388, 389, 390, 391, 396, 397, 398, 399, 400, 401, 402, 403,
     +404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416,
     +417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429,
     +430/

      len = 0
c
c---------------------------------------------
      print*,'-----------------------------------------------'
      print*,'This is the Find Field Program (ffield)'
      print*,'It will tell you the best ISSA field to use'
      print*,'for a specified position'
      print*,'-----------------------------------------------'
  720 print*,' Specify the coordinate system you wish to use:'
      print*,'                1 - Equatorial Degrees'
      print*,'                2 - Sexigesimal '
      print*,'                3 - Ecliptic Degrees'
      print*,'                4 - Galactic Degrees'
      write(*,899)
  899 format(' Enter your selection (from 1 to 4)=========>',$)
      read(*,301) ans
  301 format(a1)
      if (ans .EQ. '1' .OR. ans .EQ. '2' .OR. ans .EQ. '3'
     +    .OR. ans .EQ. '4') then
           read(ans,'(i1)') isys
      else
	   print*,'---ERROR--- Not a valid selection'
	   go to 720
      endif
      if (isys .EQ. 1 .OR. isys .EQ. 2) then
  352    write(*,901)
  901    format(' Specify the equinox of your position ======>',$)
	 read(*,302,err=803)  epoch
  302    format(f10.0)
	 go to 804
803      print*,' ERROR in epoch specification'
         go to 352
804      continue
	 if (epoch .EQ. 0.0) epoch = 1950.
	 if (epoch .GT. 2500. .OR. epoch .LT. 1500.) then
	    print*,'---ERROR--- range:(1500 < e < 2500)'
	    go to 352
         endif
      endif
 816  continue
      if (isys .EQ. 2) then
	 write(*,910)
  910    format(' Enter RA as nnHnnMnn.nS====================>',$)
	 read(*,303) clon
	 write(*,911)
  911    format(' Enter DEC as +-nnDnnMnnS===================>',$)
	 read(*,303) clat
      endif
      if (isys .NE. 2) then
	write(*,902)
  902   format(' Enter longitude in degrees ================>',$)
	read(*,303) clon
  303   format(a18)
	write(*,903)
  903   format(' Enter latitude  in degrees ================>',$)
	read(*,303) clat
        ilen = INDEX(clon,' ')
        jlen = INDEX(clon,'.')
        if(jlen .EQ. 0) clon(ilen:ilen) = '.'
        ilen = INDEX(clat,' ')
        jlen = INDEX(clat,'.')
        if(jlen .EQ. 0) clat(ilen:ilen) = '.'
      endif
c
c     convert position fm input chars & coord to ra  dec float
c
c
      do 543 j=1,18
	if (clon(j:j) .NE. ' ') len =j
  543 continue
      lonl = len
      do 544 j=1,18
	if (clat(j:j) .NE. ' ') len =j
  544 continue
      latl = len

      posn = clon(1:lonl) // ';' // clat(1:latl)
      if (isys .EQ. 1) coord = 'equd'
      if (isys .EQ. 2) coord = 'equs'
      if (isys .EQ. 3) coord = 'ecl '
      if (isys .EQ. 4) coord = 'gal '
      call gic(coord,epoch,posn,mycs,myeqnx,olon,olat,cra,cdec,ierr)
      print*,'======================================================='
      print*,'Input Coordinates:'
      write(*,907) clon,clat,coord,epoch
  907 format(3x,a15,2x,a15,'  (',a4,f8.2,')')
      write(*,905) cra,cdec
  905 format(2x,a15,2x,a15,'   (equatorial 1950.00)')
      write(*,906) olon,olat
  906 format(f10.3,6x,f10.3,'           (equatorial 1950.00)')

c--------------------------------------------
c-----get the ecliptic longitude and latitude
c--------------------------------------------
      call gic(coord,epoch,posn,ecs,myeqnx,elon,elat,cra,cdec,ierr)
c
      if (ierr.eq.0) goto 1100
      write (*,51)
      goto 1500
1100  continue
      iptype=2
      call sbstpn (olon,olat, iptype, ipcnt,indx,pns,tapes,
     +      yzpxls,deams,pcras,pcdecs)
      print*,' '
      print*,' '
      print*,'  B'
      print*,'  e        Nearest                 1991 ISSA'
      print*,'  s Field   Edge    Field Center     CDROM'
      print*,'  t Number (arcmin)  RA     DEC    (HCON Data)'
      print*,'======================================================='
c     print all plates
c---------------------------------
      do 500 il=1,ipcnt
      rom = 'n.a. '
      if (il .EQ. indx) best = '>'
      if (il .NE. indx) best = ' '
c-----loop over all posible plates
      do 499 ill = 1,162
        if (pns(il) .EQ. online(ill)) then
         if (pns(il) .GE.   1 .AND. pns(il) .LE.  71) rom='CDROM2'
         if (pns(il) .GE.  72 .AND. pns(il) .LE. 363) rom='CDROM3'
         if (pns(il) .GE. 364 .AND. pns(il) .LE. 430) rom='CDROM4'
	  go to 489
        endif
499   continue
489   continue
      write (*,502) best,pns(il),deams(il),
     +              pcras(il),pcdecs(il),rom
502   format (2x,a1,i5,f8.1,f8.1,f9.1,5x,a6)
500   continue
822   continue
      write(*,823)
  823 format(' Enter another position? (y/n)==============>',$)
      read(*,821) ans
  821 format(a1)
      if (ans .EQ. 'y') go to 816
      if (ans .EQ. 'n') go to 1500
      go to 822
c
c
51    format (' ERROR in coordinate conversion - please check input ')
476   format (i6,4x,f6.1,',',f6.1,f10.1)
c
1490  continue
c           ....  final  exit ....
1500  continue
      stop
      end
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      subroutine sbstpn
     +    (rad,decd,iptype,ipcnt,ipndx,pltnos,tapnos,pxlnos,
     +     deams,pcras, pcdecs)
c           in  in   in    out     out      out   out    out
c           out   out    out
c
      integer    iptype, ipcnt, ipndx,  pltnos(10)
      integer    iy1max, iz1max, ipnmax
c----note ... below was single precision
      double precision pxlnos(2,10), deams(10), pcras(10), pcdecs(10)
cts      real       pxlnos(2,10), deams(10), pcras(10), pcdecs(10)
      real       dpr, dry1, drz1, sy1, sz1
      real       te2g(3,3)
      real       sxe, sye, sze, dot
      real       lon0, lat0, line, sample, dl, ds, theta
cts      real*8     tapnos(10),unlstd
      character*8     tapnos(10),unlstd
c    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      common /com1/vepc, radpc, decdpc
      real         vepc(3,1716)
      real       radpc(1716), decdpc(1716)
c    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      data  dpr /57.29577951/, dry1 /.25/,  drz1  /.25/,
     *      iy1max /1584/,  iz1max /1584/ ,
     *      myeqnx /1950./, mycs   /'equd'/,
     *      unlstd /'unlstd'/
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     initialize
c      write (6,20)  rad,decd,iptype
  20  format (' sbstpn : rad, decd,iptype:',2f8.2,i4)
100   continue
      demax=0.
      pn=0
      rapn=0
      decpn=0
      ipcnt=0
      ipndx=1
c     crdd plate init
      ipnmax=1716
c     set circle search radius around the selected position
c     max dist any point in the plate can be from the plate center is
c     =(plate diag)/2. set theta at least that big.(crdd=4.74;cmdd=8.84)
c     any point farther than theta from a plate center cannot be in the
      theta=5.0
      if (iptype.eq.1) goto 102
c     cmdd plate init
      ipnmax=430
      theta=9.1
102   continue
c     convert point position to radians
      rar=rad/dpr
      decr=decd/dpr
      call rd2ve (rar, decr, sxe, sye, sze)
c       write (6,46) rar,decr,sxe,sye,sze
c46    format ('  point rar,decr,sxe,sye,sze: ',5f12.7)
c
c     conpute all plate centers and store in common (vectors&ra,dec)
c     generate plate center positions (and vectors)
      call genpc(iptype)
c
c
      do 500 ipn=1,ipnmax
c     is the source within theta dist of this plate's center?                01091069
c
c     dot the plate center against the source
      dot=sxe*vepc(1,ipn) + sye*vepc(2,ipn) + sze*vepc(3,ipn)
      if (abs(acos(dot)).gt.theta/dpr) goto 500
c
c     is the source inside the plate?
c
c     select the desired plate scheme   transformation
      if (iptype.eq.1) goto 70
c
c     sky to plate transformation for cmdd plates follows
      rapd=radpc(ipn)
      decpd=decdpc(ipn)
      lon0=radpc(ipn)/dpr
      lat0=decdpc(ipn)/dpr
      call s2pcmdd (rar, decr, lon0, lat0, line, sample)
c
c     now insure the source is inside the plate
      if (.not.((line  .ge.-249.).and.(line.  le.250.).and.
     *          (sample.ge.-249.).and.(sample.le.250.))) goto 500
c     find distance to nearest edge along line axis and sample axis
      dl=250.0-abs(line)
      ds=250.0-abs(sample)
c     find distance to nearest edge
      de=dl
      if (ds.lt.dl) de=ds
c     convert dist to nearest edge to arcmins
      deam=de*1.5
c     equate sy1 and sz1 to allow use of previous crdd plate code
      sy1=line
      sz1=sample
      dry1=1.5
      goto 200
c
c     sky to plate transformation for crdd plates follows
c
70    continue
c     first compute te2g transformation for the plate
      rapd=radpc(ipn)
      decpd=decdpc(ipn)
      twspd=180.
      call cte2g(rapd,decpd,twspd,te2g)
c
c     transform source to a plate pixel position
c                  r*4 r*4  i*4    i*4   r4   r4   r*8  r4  r4
      call skytog (sy1,sz1,iy1max,iz1max,dry1,drz1,te2g,rad,decd)
c
c     now insure the source is inside the plate
      if (.not.((sy1.ge.0.5).and.(sy1.le.1584.5).and.
     *(sz1.ge.0.5).and.(sz1.le.1584.5))) goto 500
c
c     find dist to nearest edge along y axis(dy), then along z axis(dz)
      dy=sy1
      if (sy1.gt.iy1max/2) dy=iy1max-sy1
      dz=sz1
      if (sz1.gt.iz1max/2) dz=iz1max-sz1
c
c     find the distance(de) to the nearest edge in any direction
      de=dy
      if (dz.lt.dy) de=dz
c
      deam=de*dry1
c
c     save the candidate plate #s ,tape #'s & pxls in arrays
c
200   continue
c
      ipcnt=ipcnt+1
      pltnos(ipcnt)=ipn
      pxlnos(1,ipcnt)=sy1
      pxlnos(2,ipcnt)=sz1
      deams(ipcnt)=deam
      pcras(ipcnt)=rapd
      pcdecs(ipcnt)=decpd
      if (iptype.eq.1) call platap(ipn,tapnos(ipcnt))
      if (deam.gt.demax) then
        demax=deam
        ipndx=ipcnt
      endif
500   continue
c
503   return
      end
c xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c
       subroutine rd2ve (rar,decr,xe,ye,ze)
c
c convert alf,del to to vector in the e system
      real       rar, decr
      real       xe, ye, ze,
     1           cosa, cosd, sina, sind, dpdel, dpalf
       dpdel=decr
       dpalf=rar
       cosd=cos(dpdel)
       cosa=cos(dpalf)
       sina=sin(dpalf)
       sind=sin(dpdel)
       xe=sind
       ye=-cosd*sina
       ze=cosd*cosa
       return
       end
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c *********************************************************************
c
      subroutine genpc(ptype)
c
c
c     generate plate  center vectors in equatorial
c        store vectors in common vepc array
c *********************************************************************
      real       xe, ye, ze, rpd
      real       decd, decr,  rar, latdlt
      integer    ibc,ibw,ibcsf,ibwsf, ptype, ilcmax
      dimension  ibc(37), ibw(37),ibcsf(19), ibwsf(19)
c    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      common /com1/vepc,radpc,decdpc
      real       vepc(3,1716)
      real       radpc(1716), decdpc(1716)
c
      data       rpmin     /.0043633231249/
      data         rpd     / 0.01745329253d0/
      data         dpr     / 57.29578/
c following array is bin size in minutes at different lat bands
      data ibw /1440,144,90,66,52,44,38,33,30,28,26,24,23,22,21,20,
     x    20,20,20,
     x       20,20,
     x    20,21,22,23,24,26,28,30,33,38,44,52,66,90,144,1440/
      data ibc /   1, 10,16,22,28,33,38,44,48,52,56,60,63,66,69,72,
     x    72,72,72,
     x       72,72,
     x    72,69,66,63,60,56,52,48,44,38,33,28,22,16,10,1/
c sky flux arrays follow
      data ibwsf /1440,180,104,76,60,52,46,42,40,40,40,42,46,52,60,76,
     x    104,180,1440/
      data ibcsf /   1,  8, 14,19,24,28,32,35,36,36,36,35,32,28,24,19,
     x     14,  8,   1/
c    init for crdd plates
       ipnmax=1716
       ilcmax=37
       latdlt=5.0
c      crdd plates or cmdd?
       if (ptype.eq.1) goto 30
c    init for cmdd plates
       ipnmax=430
       ilcmax=19
       latdlt=10.
       do 15 it=1,ilcmax
       ibw(it)=ibwsf(it)
15     ibc(it)=ibcsf(it)
30     continue
       decd=-90.
       ipn=1
c latitude loop
       do 70 ilc=1,ilcmax
c longitude loop for this lat
       ram=0.
       bw=ibw(ilc)
       inb=ibc(ilc)
       do 67 j=1,inb
       rar=ram*rpmin
       decr=decd*rpd
c      save center co-ords
       radpc(ipn)=rar*dpr
       decdpc(ipn)=decd
c      convert ra,dec to vectors
       call rd2ve( rar,decr,xe,ye,ze)
c      write (6,66) ipn,rar,decr,xe,ye,ze
c66    format (i6, 5f12.7)
       vepc(1,ipn)=xe
       vepc(2,ipn)=ye
       vepc(3,ipn)=ze
       ipn=ipn+1
       ram=ram+bw
       if (ipn.gt.ipnmax) goto 60
67     continue
c above is 360 deg longitude loop end range
       decd=decd+latdlt
70     continue
60     return
       end
c xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c      s2pcmdd: sky to plate transformation for cmdd plates
       subroutine  s2pcmdd (lon,lat,lon0,lat0,line,sample)
c                            in  in  in   in   out   out
c                            r   r   r    r    r     r
c
c          lon,lat       lon,lat of desired position in radians
c          lon0, lat0    lon,lat of center of plate in radians
c          line, sample  pixel numbers of input position
c
      real  lat, lon, lat0, lon0, line, sample, r2d, a, ff
c
      r2d=45./atan(1.)
      a=cos(lat)*cos(lon0-lon)
      ff=sin(lat0)*sin(lat)+a*cos(lat0)
      if(ff.eq.0.) ff=1.e-34
      f=40.*r2d/ff
      sample=f*cos(lat)*sin(lon0-lon)
      line=-f*(cos(lat0)*sin(lat)-a*sin(lat0))
      return
      end
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
      subroutine cte2g(alpg,delg,twsg,te2g)
c
c    compute eme50 to grid transformation
c
c     te2g   =  transformation from eme to grid coordinates
c     alpg   =  right ascension of grid center (deg, eme50)
c     delg   =  declination     of grid center (deg, eme50)
c     twsg   =  twist angle about  grid center (deg, eme50)
c
      real   te2g(3,3)
      data rtd/57.29577951/
c
      salp = sin( alpg/rtd )
      calp = cos( alpg/rtd )
      sdel = sin( delg/rtd )
      cdel = cos( delg/rtd )
      stws = sin( twsg/rtd )
      ctws = cos( twsg/rtd )
      te2g(1,1) = +sdel
      te2g(1,2) = -salp*cdel
      te2g(1,3) = +calp*cdel
      te2g(2,1) = -cdel*ctws
      te2g(2,2) = -calp*stws -salp*sdel*ctws
      te2g(2,3) = -salp*stws +calp*sdel*ctws
      te2g(3,1) = +cdel*stws
      te2g(3,2) = -calp*ctws +salp*sdel*stws
      te2g(3,3) = -salp*ctws -calp*sdel*stws
c
      return
      end
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c         below sub fm elk
c
c
      subroutine skytog(py,pz,ny,nz,dry,drz,te2g,alp,del)
c
c      skytog - convert ra & dec (alp,del) to pixel (py,pz)
c
      integer   ny,nz,nyc,nzc
      real      alp,del,py,pz
      real      te2g(3,3),ve(3),vt(3)
      data rtd/57.29577951/
c                           compute eme vector
c
      nyc = ny/2 +1
      nzc = nz/2 +1
      sdel = sin( del/rtd )
      cdel = cos( del/rtd )
      salp = sin( alp/rtd )
      calp = cos( alp/rtd )
      ve(1) = +sdel
      ve(2) = -salp*cdel
      ve(3) = +calp*cdel
c                           compute grid vector and py,pz
      vt(1) = te2g(1,1)*ve(1)+te2g(1,2)*ve(2)+te2g(1,3)*ve(3)
      vt(2) = te2g(2,1)*ve(1)+te2g(2,2)*ve(2)+te2g(2,3)*ve(3)
      vt(3) = te2g(3,1)*ve(1)+te2g(3,2)*ve(2)+te2g(3,3)*ve(3)
      py  = 60.*rtd*vt(2)/dry + nyc
      pz  = 60.*rtd*vt(3)/drz + nzc
c
      return
      end
c
      subroutine platap(plate, tape)
c
c//////////////////////////////////////////////////////////////////////
c                                                                     /
c     look up the proper tape name for the given plate number         /
c                                                                     /
c//////////////////////////////////////////////////////////////////////
c
      character*(*) tape
      integer       plate
c
      integer       tapno, plt(600), plt1(100), plt2(100), plt3(100)
      integer              plt4(100), plt5(100), plt6(100)
c
      equivalence   (plt(  1), plt1(1))
      equivalence   (plt(101), plt2(1))
      equivalence   (plt(201), plt3(1))
      equivalence   (plt(301), plt4(1))
      equivalence   (plt(401), plt5(1))
      equivalence   (plt(501), plt6(1))
c
      data plt1/
     +     1,    2,    3,    4,    5,    6,    7,    9,   10,   11,
     +    13,   15,   16,   17,   19,   21,   23,   24,   26,   28,
     +    30,   31,   32,   33,   34,   35,   37,   39,   41,   43,
     +    45,   47,   50,   52,   54,   55,   56,   57,   58,   59,
     +    60,   62,   64,   67,   69,   71,   74,   77,   79,   82,
     +    84,   85,   86,   87,   88,   89,   90,   92,   94,   97,
     +    99,  101,  104,  107,  110,  113,  116,  118,  120,  121,
     +   122,  124,  126,  128,  130,  133,  136,  138,  141,  144,
     +   147,  150,  154,  156,  158,  160,  161,  163,  165,  167,
     +   169,  172,  175,  178,  180,  183,  186,  189,  192,  197/
      data plt2/
     +   199,  201,  203,  205,  207,  209,  211,  214,  217,  220,
     +   223,  226,  229,  232,  235,  238,  242,  246,  249,  251,
     +   253,  255,  257,  259,  262,  266,  270,  273,  276,  279,
     +   283,  286,  289,  294,  299,  302,  305,  307,  309,  311,
     +   314,  318,  323,  326,  329,  332,  336,  340,  343,  346,
     +   352,  356,  359,  362,  364,  366,  368,  371,  375,  380,
     +   384,  387,  390,  393,  397,  400,  404,  409,  414,  417,
     +   420,  423,  425,  428,  431,  434,  440,  445,  449,  452,
     +   455,  459,  463,  467,  473,  478,  481,  484,  487,  490,
     +   493,  496,  500,  505,  510,  514,  517,  520,  524,  528/
      data plt3/
     +   532,  538,  543,  547,  550,  553,  556,  559,  562,  565,
     +   570,  575,  580,  583,  586,  589,  593,  597,  601,  607,
     +   612,  616,  619,  622,  625,  628,  631,  634,  639,  645,
     +   650,  654,  657,  660,  664,  667,  671,  675,  682,  687,
     +   690,  693,  696,  699,  702,  705,  708,  714,  720,  724,
     +   727,  730,  733,  737,  741,  745,  752,  758,  762,  765,
     +   768,  771,  774,  778,  782,  789,  794,  798,  801,  804,
     +   808,  811,  815,  821,  827,  832,  835,  838,  842,  846,
     +   850,  854,  861,  866,  870,  873,  876,  880,  883,  887,
     +   893,  899,  904,  907,  910,  914,  918,  922,  926,  933/
      data plt4/
     +   938,  942,  945,  948,  951,  954,  957,  961,  968,  974,
     +   978,  981,  985,  989,  993,  997, 1003, 1009, 1013, 1016,
     +  1019, 1022, 1025, 1028, 1031, 1037, 1043, 1048, 1051, 1054,
     +  1058, 1062, 1066, 1070, 1076, 1082, 1086, 1089, 1091, 1094,
     +  1097, 1100, 1103, 1109, 1115, 1119, 1122, 1125, 1129, 1133,
     +  1137, 1141, 1147, 1152, 1155, 1158, 1160, 1162, 1165, 1168,
     +  1171, 1177, 1183, 1187, 1190, 1193, 1197, 1201, 1205, 1209,
     +  1215, 1220, 1223, 1226, 1228, 1230, 1233, 1236, 1241, 1246,
     +  1251, 1255, 1258, 1262, 1266, 1270, 1274, 1280, 1285, 1288,
     +  1291, 1293, 1295, 1298, 1301, 1307, 1312, 1316, 1319, 1322/
      data plt5/
     +  1326, 1330, 1334, 1338, 1344, 1347, 1350, 1352, 1354, 1356,
     +  1359, 1362, 1368, 1372, 1375, 1378, 1381, 1385, 1389, 1392,
     +  1395, 1400, 1404, 1406, 1408, 1410, 1411, 1413, 1415, 1418,
     +  1423, 1427, 1430, 1433, 1436, 1440, 1443, 1446, 1449, 1454,
     +  1457, 1459, 1461, 1463, 1464, 1466, 1468, 1472, 1477, 1480,
     +  1483, 1486, 1489, 1492, 1495, 1498, 1501, 1505, 1507, 1509,
     +  1511, 1513, 1514, 1516, 1518, 1521, 1524, 1527, 1530, 1533,
     +  1536, 1539, 1542, 1545, 1548, 1551, 1553, 1555, 1557, 1558,
     +  1559, 1560, 1562, 1564, 1566, 1568, 1571, 1574, 1576, 1579,
     +  1582, 1585, 1588, 1591, 1593, 1594, 1595, 1596, 1597, 1598/
      data plt6/
     +  1599, 1600, 1602, 1604, 1606, 1609, 1612, 1614, 1617, 1620,
     +  1623, 1625, 1627, 1629, 1630, 1631, 1632, 1633, 1634, 1636,
     +  1638, 1640, 1643, 1645, 1648, 1651, 1654, 1656, 1658, 1660,
     +  1661, 1662, 1663, 1664, 1666, 1668, 1670, 1672, 1675, 1678,
     +  1680, 1682, 1683, 1684, 1685, 1687, 1689, 1691, 1693, 1695,
     +  1697, 1699, 1701, 1702, 1703, 1705, 1707, 1709, 1711, 1713,
     +  1714, 1715, 1716, 37*9999/
      data ntap /563/
c
      do 10 itap = 1,ntap
          if(plt(itap) .gt. plate) go to 11
          tapno = itap
  10  continue
  11  continue
c
      write(tape,15) tapno
  15  format('cpt',i3.3)
c
      return
      end
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c-gic
c
       subroutine gic(cs,epoch,posn,mycs,myepoc,olon,olat,cra,cdec,
     +                err)
c
c  subroutine to do general coordinate conversions and precessions
c
c
       character cs*4,lon*40,lat*40,mycs*4,cra*(*),cdec*(*),pos*40,
     +           posn*(*)
       integer err
       logical okay
       real myepoc
       double precision depoch,dlon,dlat,toyear,ra,dec
       err = 0
       do 50 i = 1,40
          lon(i:i) = ' '
 50       lat(i:i) = ' '
       cra = ' '
       cdec = ' '
       olon = 0.
       olat = 0.
       ncs = 0
       if (cs .eq. 'equs' .or. cs .eq. 'EQUS') ncs = 1
       if (cs(:3) .eq. 'gal' .or. cs(:3) .eq. 'GAL') ncs = 2
       if (cs(:3) .eq. 'ecl' .or. cs(:3) .eq. 'ECL') ncs = 3
       if (cs .eq. 'equd' .or. cs .eq. 'EQUD') ncs = 4
       if (ncs .eq. 0) then
          err = 1
          return
       endif
       nmycs = 0
       if (mycs .eq. 'equs' .or. mycs .eq. 'EQUS') nmycs = 1
       if (mycs(:3) .eq. 'gal' .or. mycs(:3) .eq. 'GAL') nmycs = 2
       if (mycs(:3) .eq. 'ecl' .or. mycs(:3) .eq. 'ECL') nmycs = 3
       if (mycs .eq. 'equd' .or. mycs .eq. 'EQUD') nmycs = 4
       if (nmycs .eq. 0) then
          err = 2
          return
       endif
c   error checking, of sorts, and splitting the coords into lon and lat
c   err = 1    illegal coord system on input
c   err = 2       "      "     "     " output
c   err = 3    illegal char in pos
       ilen = len(posn)
       do 100 i = 1,ilen
          pos(i:i) = posn(i:i)
          okay = .true.
c   hms is only allowed for 'equs'
          if (ncs.ne.1 .and.
     +        (pos(i:i).eq.'H' .or. pos(i:i).eq.'h' .or.
     +         pos(i:i).eq.'M' .or. pos(i:i).eq.'m' .or.
     +         pos(i:i).eq.'S' .or. pos(i:i).eq.'s')) then
              err = 3
              return
          endif
c   however, d is allowed for all; just needs to be changed to blank
c   for everything but 'equs'
          if (ncs.ne.1 .and.
     +        (pos(i:i).eq.'D' .or. pos(i:i).eq.'d')) pos(i:i) = ' '
c   now check for any other undesireables
          if (pos(i:i).ne.' ' .and.
     +        pos(i:i).ne.'H' .and. pos(i:i).ne.'h' .and.
     +        pos(i:i).ne.'M' .and. pos(i:i).ne.'m' .and.
     +        pos(i:i).ne.'S' .and. pos(i:i).ne.'s' .and.
     +        pos(i:i).ne.'D' .and. pos(i:i).ne.'d' .and.
     +        pos(i:i).ne.'+' .and. pos(i:i).ne.'-' .and.
     +        pos(i:i).ne.'.' .and. pos(i:i).ne.';')
     +        okay = .false.
          if (.not. okay .and.
     +        (ichar(pos(i:i)).lt.48 .or.
     +         ichar(pos(i:i)).gt.57)) then
              err = 3
              return
          endif
 100   continue
c   separate the string into lon/lat upon discovery of the semi-colon,
c   if any is to be found
       do 110 i = 1,ilen
          if (pos(i:i) .eq. ';') then
             lon = pos(:i-1)
             lat = pos(i+1:)
             goto 150
          endif
 110   continue
       err = 3
       return
c  convert input position ra and dec (if appropriate) to degrees
c  or just read in the decimal degrees
 150   if (ncs .ne. 1) then
          read(lon,1) olon
 1        format(f10.0)
          read(lat,1) olat
       else
          call sex2dd(lon,lat,olon,olat)
       endif
       if (olon.lt.0. .or. olon.gt.360.) then
          err = 4
          return
       endif
       if (olat.lt.-90. .or. olat.gt.90.) then
          err = 5
          return
       endif
       if (ncs .eq. 4) ncs = 1
       if (nmycs .eq. 4) nmycs = 1
c  precess coords if not in program epoch (convert coords to equatorial since
c  that is the system the precession routine works in)
       if (epoch .ne. myepoc) then
          if (ncs.eq.2 .or. ncs.eq.3)
     +       call jconvc(ncs,olon,olat,1,olon,olat)
          depoch = epoch
          dlon = olon
          dlat = olat
          toyear = myepoc
          call jprech(depoch,dlon,dlat,toyear,ra,dec)
          olon = ra
          olat = dec
          call dd2sex(olon,olat,cra,cdec)
	  return
       endif
       if (ncs .ne. nmycs) call jconvc(ncs,olon,olat,nmycs,olon,olat)
       if (nmycs .eq. 1) call dd2sex(olon,olat,cra,cdec)
       return
       end
c
      subroutine SEX2DD(crain, cdecin, ra, dec)
c
c//////////////////////////////////////////////////////////////////////
c                                                                     /
c     Converts Coordinates from Sexigesimal to Decimal Degrees        /
c                                                                     /
c//////////////////////////////////////////////////////////////////////
c
      character*(*) crain, cdecin
      character*80  subst(10), cra, cdec
      character*40  tmph, tmpd, tmpm, tmps
      character*1   chr
c
      integer       ideg, TRUE, FALSE
c
      real          hr, deg, min, sec
c
      data TRUE, FALSE /1, 0/
c
      cra = crain
      cdec = cdecin
c
c
c     RA ----------------------------------
c
      ideg = FALSE
      hr  = 0.0
      min = 0.0
      sec = 0.0
      tmph = '0'
      tmpm = '0'
      tmps = '0'
c
c
c     Determine the Sign on the RA
c
      isign = 1
      leng = len(cra)
      do 92 i = 1,leng
          if(cra(i:i) .EQ. '-') then
              isign = -1
              cra(i:i) = ' '
          endif
c
          if(cra(i:i) .EQ. '+') then
              cra(i:i) = ' '
          endif
  92  continue
c
c
c     Find the positions of the 'h' (or 'd'), 'm', and 's'
c
      nh = 0
      nm = 0
      ns = 0
      leng = len(cra)
      do 10 ich = 1,leng
          chr = cra(ich:ich)
          if(chr.EQ.'h' .OR. chr.EQ.'H') then
              nh = ich
              nm = leng + 1
              ns = leng + 1
          endif
          if(chr.EQ.'d' .OR. chr.EQ.'D') then
              ideg = TRUE
              nh = ich
              nm = leng + 1
              ns = leng + 1
          endif
          if(chr.EQ.'m' .OR. chr.EQ.'M') nm = ich
          if(chr.EQ.'s' .OR. chr.EQ.'S') ns = ich
  10  continue
c
c
c     Extract the Integer Hours, Minutes, and Seconds
c
      if(nh   .GT.  1) tmph = cra(   1:nh-1)
      if(nm-1 .GT. nh) tmpm = cra(nh+1:nm-1)
      if(ns-1 .GT. nm) tmps = cra(nm+1:ns-1)
c
c
c     If There are No 'hms', Find the Substrings
c
      if(nh.EQ.0 .AND. nm.EQ.0 .AND. ns.EQ.0) then
          call SUBSTR(cra, subst, nsub)
c
          if(nsub.GE.3) then
              tmph = subst(1)
              tmpm = subst(2)
              tmps = subst(3)
          endif
c
          if(nsub.EQ.2) then
              tmpm = subst(1)
              tmps = subst(2)
          endif
c
          if(nsub.EQ.1) then
              tmps = subst(1)
          endif
      endif
      if(tmph(1:2) .EQ. '  ') tmpd = '0.'
      if(tmpm(1:2) .EQ. '  ') tmpm = '0.'
      if(tmps(1:2) .EQ. '  ') tmps = '0.'
c
c
c     If the Number is Too Big, Assume +hhmmss.s
c
      read(tmps,'(f20.0)') sec
      tsec = abs(sec)
      xyz = tsec/1000.
c
      if(tsec.GT.60.) then
          hr = aint(tsec/10000.)
          min = aint((tsec - hr*10000.) / 100.)
          sec = tsec - hr*10000. - min*100.
      else
c-------
      ilen = INDEX(tmph,' ')
      jlen = INDEX(tmph,'.')
      if(jlen .EQ. 0) tmph(ilen:ilen) = '.'
      ilen = INDEX(tmpm,' ')
      jlen = INDEX(tmpm,'.')
      if(jlen .EQ. 0) tmpm(ilen:ilen) = '.'
      ilen = INDEX(tmps,' ')
      jlen = INDEX(tmps,'.')
      if(jlen .EQ. 0) tmps(ilen:ilen) = '.'
          read(tmph, '(f20.0)') hr
          read(tmpm, '(f20.0)') min
          read(tmps, '(f20.0)') sec
      endif
c
c
c     Generate the RA in Decimal Degrees
c
      scale = 15.
      if(ideg .EQ. TRUE) scale = 1.
      ra = isign * scale * (hr + (min/60.) + (sec/3600.))
c
c
c     DEC -------------------------------------
c
      deg = 0.0
      min = 0.0
      sec = 0.0
      tmpd = '0'
      tmpm = '0'
      tmps = '0'
c
c
c     Determine the Sign on the Dec
c
      isign = 1
      leng = len(cdec)
      do 91 i = 1,leng
          if(cdec(i:i) .EQ. '-') then
              isign = -1
              cdec(i:i) = ' '
	      cdec(1:leng) = cdec(i+1:leng)
          endif
c
          if(cdec(i:i) .EQ. '+') then
              cdec(i:i) = ' '
	      cdec(1:leng) = cdec(i+1:leng)
          endif
  91  continue
c
c
c     Find the position of the 'd', 'm', and 's'
c
      nd = 0
      nm = 0
      ns = 0
      leng = len(cdec)
      lend = INDEX(cdec,'.')
      do 20 ich = 1,leng
          chr = cdec(ich:ich)
          if(chr.EQ.'d' .OR. chr.EQ.'D') then
              nd = ich
              nm = leng + 1
              ns = leng + 1
          endif
          if(chr.EQ.'m' .OR. chr.EQ.'M') nm = ich
          if(chr.EQ.'s' .OR. chr.EQ.'S') ns = ich
  20  continue
c
      if(nm .LT. nd) nm = nd
      if(ns .LT. nd) ns = nd
      if(ns .LT. nm) ns = nm
c
c
c     Extract the Integer Degrees, Minutes, and Seconds
c
      if(nd   .GT.  1) tmpd = cdec(   1:nd-1)
      if(nm-1 .GT. nd) tmpm = cdec(nd+1:nm-1)
      if(ns-1 .GT. nm) tmps = cdec(nm+1:ns-1)
c
c
c     If There are No 'dms', Find the Substrings
c
      if(nd.EQ.0 .AND. nm.EQ.0 .AND. ns.EQ.0) then
          call SUBSTR(cdec, subst, nsub)
c
          if(nsub.GE.3) then
              tmpd = subst(1)
              tmpm = subst(2)
              tmps = subst(3)
          endif
c
          if(nsub.EQ.2) then
              tmpm = subst(1)
              tmps = subst(2)
          endif
c
          if(nsub.EQ.1) then
              tmps = subst(1)
          endif
      endif
      if(tmpd(1:2) .EQ. '  ') tmpd = '0.'
      if(tmpm(1:2) .EQ. '  ') tmpm = '0.'
      if(tmps(1:2) .EQ. '  ') tmps = '0.'
c
c
c     If There the Number is Too Big, Assume +ddmmss.s
c
      ilen = INDEX(tmps,' ')
      jlen = INDEX(tmps,'.')
      if(jlen .EQ. 0) tmps(ilen:ilen) = '.'
      read(tmps,'(f20.0)') sec
      tsec = abs(sec)
c
      if(tsec.GT.60.) then
          deg = aint(tsec/10000.)
          min = aint((tsec - deg*10000.) / 100.)
          sec = tsec - deg*10000. - min*100.
      else
      ilen = INDEX(tmpd,' ')
      jlen = INDEX(tmpd,'.')
      if(jlen .EQ. 0) tmpd(ilen:ilen) = '.'
      ilen = INDEX(tmpm,' ')
      jlen = INDEX(tmpm,'.')
      if(jlen .EQ. 0) tmpm(ilen:ilen) = '.'
      ilen = INDEX(tmps,' ')
      jlen = INDEX(tmps,'.')
      if(jlen .EQ. 0) tmps(ilen:ilen) = '.'
          read(tmpd, '(f20.0)') deg
          read(tmpm, '(f20.0)') min
          read(tmps, '(f20.0)') sec
      endif
c
c
c     Generate the DEC in Decimal Degrees
c
      deg = abs(deg)
      min = abs(min)
      sec = abs(sec)
      dec = isign * (deg + (min/60.) + (sec/3600.))
c
      return
      end
c----
c
      subroutine DD2SEX(ra, dec, cra, cdec)
c
c//////////////////////////////////////////////////////////////////////
c                                                                     /
c     determines the sexigesimal equivalent of the input ra and dec   /
c                                                                     /
c//////////////////////////////////////////////////////////////////////
c
      real          ra, dec, res
      character*(*) cra, cdec
c
      character*1   sign
c
c     RA
c
      if(ra .LT. 0) ra = ra + 360.
      res=abs(ra)
      res=res/15.0
c
      irah=int(res)
      res=res-float(irah)
      res=res*60.0
      iram=int(res)
      res=res-float(iram)
      res=res*60.0
      fras=res
c
      ids = int(fras * 10. + 0.5)
      iras = ids / 10
      irads = ids - iras * 10
c
      if(iras .EQ. 60) then
          iram=iram+1
          iras=0
      endif
c
      if(iram .EQ. 60) then
          irad=irad+1
          iram=0
      endif
c
      if(iram .EQ. -1) then
          irad=irad-1
          iram=0
      endif
c
c
c     Dec
c
      sign='+'
      if(dec.lt.0.0) sign='-'
      res=abs(dec)
      idecd=int(res)
      res=res-float(idecd)
      res=res*60.0
      idecm=int(res)
      res=res-float(idecm)
      res=res*60.0
      idecs=int(res+.4999990)
c
      if(idecs .EQ. 60) then
          idecm=idecm+1
          idecs=0
      endif
c
      if(idecm .EQ. 60) then
          idecd=idecd+1
          idecm=0
      endif
c
      if(idecs .EQ. -1) then
          idecm=idecm-1
          idecs=0
      endif
c
      if(idecm .EQ. -1) then
          idecd=idecd-1
          idecm=0
      endif
c
      write(cra,15) irah, iram, iras, irads
  15  format(1x,i2.2,'h',1x,i2.2,'m',1x,i2.2,'.',i1,'s')
c
      write(cdec,25) sign, idecd, idecm, idecs
  25  format(a1,i2.2,'d',1x,i2.2,'m',1x,i2.2,'s')
c
      return
      end
c \iras\jprech.for  version of jprece that call hgtprc (herget precession)
c    = :user.iras649.ipacx.jprech_f     dec87
c
      subroutine jprech(epoch1,radin,decdin,epoch2,radou,decdou)
c all arguments double precision
c radin,decdin,radou,decdou decimal degrees.
c epoch1,epoch2 year A.D.  (e.g. 1900., 1950.)
c
      double precision  radin,decdin,radou,decdou
      double precision ra1,dec1,epoch1,epoch2,ra2,dec2,dtor
c
      data dtor/0.17453292519943d-01/
c
      ra1 = radin * dtor
      dec1 = decdin * dtor
c
c hgtprc in \iras\hgtprc.for
c
      call hgtprc(ra1,dec1,epoch1,epoch2,ra2,dec2)
c
      radou = ra2/dtor
      decdou = dec2/dtor
      if(radou .lt. 0.d0) radou = radou + 360.d0
      if(radou .ge. 360.d0) radou = radou - 360.d0
      return
      end

      subroutine jconvc(jsysin,x,y,msys,xnew,ynew)
c x,y = input lon and lat in degrees; xnew,ynew=output lon and lat in degrees.
c jsysin = input coordinate system#;  msys = output system#.
c sys#1 = equ, sys#2 = gal, sys#3 = ecl
      real x,y,xnew,ynew
      integer jsysin,msys
      if(jsysin .eq. msys) go to 10
      if(jsysin-2) 1,2,3
    1  if(msys .eq. 2) call equgal(x,y,xnew,ynew)
       if(msys .eq. 3) call equecl(x,y,xnew,ynew)
       go to 20
    2  if(msys .eq. 1) call galequ(x,y,xnew,ynew)
       if(msys .eq. 3) call galecl(x,y,xnew,ynew)
       go to 20
    3  if(msys .eq. 1) call eclequ(x,y,xnew,ynew)
       if(msys .eq. 2) call eclgal(x,y,xnew,ynew)
       go to 20
c
   10 xnew = x
      ynew = y
   20 return
      end

      subroutine eclequ(xlam,beta,rad,decd)
c 16jun86 jdb  cdc double precision version of eclequ.
c   probably will not be needed. in file declequ_f.
c 22mar85 for hepvax (changed darsin to dasin)
c jdb  22jan81,28jan81,29jan81  08apr81 to speed routine up a bit.
c ecliptic to 1950. equatorial coordinate subroutine
c
c  epsilon = e = 23deg26'44.84"     equations from j.fowler
c
c inputs: xlam=lambda=ecliptic longitude in degrees (single prec.)
c                     (0 - 360 deg)
c
c         beta= ecliptic latitude in degrees (single prec.)
c                      (-90. to +90. deg)
c  xl = sin(beta)              xe = xl*cos(e) - yl*sin(e)
c  yl = -cos(beta)*sin(lambda) ye = xl*sin(e) + yl*cos(e)
c  zl = cos(beta)*cos(lambda)  ze = zl
c
c  outputs: rad ra in degrees; decd dec in degrees. (single prec.)
c
cc    real*8 darsin (required statement for series/1 only)
      double precision dtor,rtod,cosb,cosl,sinb,sinl,xl,yl,zl,
     1 xe,ye,ze,cose,sine,xlamr,betar,rar,decr
c
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      data cose/0.9174369452102114d0/,sine/0.3978812028273710d0/
c
      xlamr = dble(xlam)*dtor
      betar = dble(beta)*dtor
c
      cosb = dcos(betar)
      cosl = dcos(xlamr)
      sinb = dsin(betar)
      sinl = dsin(xlamr)
c
      xl = sinb
      yl = -(cosb*sinl)
      zl = cosb*cosl
c
      xe = cose*xl - sine*yl
      ye = sine*xl + cose*yl
      ze = zl
c
      if(dabs(xe) .gt. 1.d0) xe = dsign(1.0d0,xe)
cc    decr = darsin(xe)
      decr =  dasin(xe)
c     if(dabs(ze) .gt. 1.d0) ze = dsign(1.0d0,ze) do not need these
c     if(dabs(ye) .gt. 1.d0) ye = dsign(1.0d0,ye) ..2 statements.
      rar = datan2(-ye,ze)
c
      rad = rar * rtod
      if(rad .lt. 0.) rad = 360. + rad
      decd = decr * rtod
      return
c
      end

      subroutine eclgal(xlam,beta,xlg,xbg)
c 16jun86 jdb.  cdc/ve double precision version of eclgal
c   probably will not be needed on cdc. stored in declgal_f.
c
c 22mar85 for hepvax (changed darsin to dasin)
c jdb  04jun81  ecliptic to galactic coordinate conversion
c inputs: xlam is ecl.long in degrees;beta is ecl.lat in degrees.
c outputs:xlg is gal.long in degrees;xbg is gal.lat in degrees.
c   all subroutine arguments single precision.
c
c note: statements w/ cc**** are kept here for comparison w/ the
c  individual subroutines that this one is based on.
c
c 1st section based on ecliptic to equatorial coordinate subroutine
c    eclequ.  2nd based on equ to gal subroutine galcor.
c
c  epsilon = e = 23deg26'44.84"  equations for ecl. conv. from j.fowler
c
c inputs: xlam=lambda=ecliptic longitude in degrees (single prec.)
c                     (0 - 360 deg)
c
c         beta= ecliptic latitude in degrees (single prec.)
c                      (-90. to +90. deg)
c  xl = sin(beta)              xe = xl*cos(e) - yl*sin(e)
c  yl = -cos(beta)*sin(lambda) ye = xl*sin(e) + yl*cos(e)
c  zl = cos(beta)*cos(lambda)  ze = zl
c
compute equatorial from ecliptic  (section 1)
c
cc    real*8 darsin (required statement for series/1 only)
      double precision dtor,rtod,cosb,cosl,sinb,sinl,xl,yl,zl,
     1 xe,ye,ze,cose,sine,xlamr,betar,rar,decr
      double precision cosa,sina,cosd,x,y,z,eq2,eq3,xbr,xlr
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      data cose/0.9174369452102114d0/,sine/0.3978812028273710d0/
c
c
      xlamr = dble(xlam)*dtor
      betar = dble(beta)*dtor
c
      cosb = dcos(betar)
      cosl = dcos(xlamr)
      sinb = dsin(betar)
      sinl = dsin(xlamr)
c
      xl = sinb
      yl = -(cosb*sinl)
      zl = cosb*cosl
c
      xe = cose*xl - sine*yl
      ye = sine*xl + cose*yl
      ze = zl
c
      if(dabs(xe) .gt. 1.d0) xe = dsign(1.0d0,xe)
cc    decr = darsin(xe)
      decr =  dasin(xe)
c     if(dabs(ze) .gt. 1.d0) ze = dsign(1.0d0,ze)
c     if(dabs(ye) .gt. 1.d0) ye = dsign(1.0d0,ye)
c     if(dabs(ze) .gt. 1.d0) write(10,2001) ze,ye
c     if(dabs(ye) .gt. 1.d0) write(10,2001) ze,ye
c2001 format(1x,'ze,ye=',2f12.5,' abs val > 1.')
      rar = datan2(-ye,ze)
c
cc****rad = rar * rtod
cc****if(rad .lt. 0.) rad = 360. + rad
c
cc****decd = decr * rtod
c
c compute galactic  from equatorial  (section 2)
c        equations for gal. conversion from t. soifer.
c
c inputs: rar ra in radians, decr is dec in radians. double precision.
c b = galactic latitude (-90. to + 90. deg);l=galactic longitude(0-360)
c  x = cos(alpha)*cos(delta)
c  y = sin(alpha)*cos(delta)
c  z = sin(delta)
c  eq1 = sin(b) = -.8676*x - .1884*y + .4602*z
c  eq2 = cos(b)*cos(l) = -.0669*x - .8728*y - .4835*z
c  eq3 = cos(b)*sin(l) = +.4927*x - .4503*y + .7446*z
c    b = arsin(sin(b))
c  cos(b) = sqrt(1.-sin(b)*sin(b))
c  cos(l) = eq2/cos(b)
c  sin(l) = eq3/cos(b)
c  l = atan2(sin(l)/cos(l): if(l.lt.0) l = l+2*pi if radians.
c
c note: variable names cosb,sinb,cosl,sinl reused in this section,but
c  with different meanings than previous section.
c
      cosa = dcos(rar)
      sina = dsin(rar)
c     note: xe is dsin(decr) already
cc****z = dsin(decr)
      cosd = dcos(decr)
      z    = xe
      x = cosa*cosd
      y = sina*cosd
      sinb = -.86760082d0*x - .18837460d0*y + .46019979d0*z
      if(dabs(sinb).lt.1.d0) go to 2
      sinb = dsign(1.d0,sinb)
cc    xbr = darsin(sinb)
      xbr =  dasin(sinb)
      xlr = 0.d0
      go to 3
    2 eq2  = -.06698874d0*x - .87275577d0*y - .48353892d0*z
      eq3  = +.49272847d0*x - .45034696d0*y + .74458464d0*z
cc    xbr = darsin(sinb)
      xbr =  dasin(sinb)
      cosb = dsqrt(1.d0-sinb*sinb)
      cosl = eq2/cosb
      sinl = eq3/cosb
      if(dabs(cosl).gt.1.d0) cosl = dsign(1.d0,cosl)
      if(dabs(sinl).gt.1.d0) sinl = dsign(1.d0,sinl)
      xlr = datan2(sinl,cosl)
    3 xlg = xlr * rtod
      if(xlg .lt. 0.) xlg = 360. + xlg
      xbg = xbr * rtod
      return
c
      end

      subroutine equecl(rad,decd,xlam,beta)
c  1950. equatorial to ecliptic conversion coordinate subroutine
c
c  epsilon = e = 23deg26'44.84"     equations from j.fowler
c
c inputs: rad ra in degrees; decd dec in degrees. (single prec.)
c
c returned: xlam=lambda=ecliptic longitude in degrees (single prec.)
c                     (0 - 360 deg)
c
c         beta= ecliptic latitude in degrees (single prec.)
c                      (-90. to +90. deg)
c  xe = sin(dec)              xl = xe*cos(e) - ye*sin(e)
c  ye = -cos(dec)*sin(ra)     yl = xe*sin(e) + ye*cos(e)
c  ze = cos(dec)*cos(ra)      zl = ze
c
cc    real*8 darsin  (needed for series/1 only)
      double precision dtor,rtod,cosd,cosr,sind,sinr,xl,yl,zl,
     1 xe,ye,ze,cose,sine,xlamr,betar,rar,decr
c
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      data cose/0.9174369452102114d0/,sine/0.3978812028273710d0/
c
      rar = dble(rad)*dtor
      decr = dble(decd)*dtor
c
      cosd = dcos(decr)
      cosr = dcos(rar)
      sind = dsin(decr)
      sinr = dsin(rar)
c
      xe = sind
      ye = -(cosd*sinr)
      ze = cosd*cosr
c
      xl = cose*xe + sine*ye
      yl = -sine*xe + cose*ye
      zl = ze
c
      if(dabs(xl) .gt. 1.d0) xl = dsign(1.0d0,xl)
cc    betar = darsin(xl)
      betar =  dasin(xl)
c     if(dabs(zl) .gt. 1.d0) zl = dsign(1.0d0,zl) do not need these 2
c     if(dabs(yl) .gt. 1.d0) yl = dsign(1.0d0,yl) .. statements.
      xlamr = datan2(-yl,zl)
c
      xlam = xlamr * rtod
      if(xlam .lt. 0.) xlam = 360. + xlam
      beta = betar * rtod
      return
c
      end

      subroutine equgal(rad,decd,xl,xb)
c 16jun86 jdb. a double precision vers. for cdc/ve (probably will not be
c   necessary). will be stored in dequgal_f. real*8 changed to double pr
c
c 22mar85 for hepvax (darsin changed to dasin;entry galcor removed)
cc    entry galcor(rad,decd,xl,xb)
c 17mar82 updated for more sensible name- equgal, while retaining
c     alias for old name of galcor.
c 02oct80 jdb:subroutine to compute galactic coords from earth
c  equatorial coords.29jan81 for more places in dtor,rtod.22apr81
c inputs: rad ra in degrees, decd is dec in degrees.  single precision.
c b = galactic latitude (-90. to + 90. deg);l=galactic longitude(0-360)
c  x = cos(alpha)*cos(delta)
c  y = sin(alpha)*cos(delta)
c  z = sin(delta)
c  eq1 = sin(b) = -.8676*x - .1884*y + .4602*z
c  eq2 = cos(b)*cos(l) = -.0669*x - .8728*y - .4835*z
c  eq3 = cos(b)*sin(l) = +.4927*x - .4503*y + .7446*z
c    b = arsin(sin(b))
c  cos(b) = sqrt(1.-sin(b)*sin(b))
c  cos(l) = eq2/cos(b)
c  sin(l) = eq3/cos(b)
c  l = atan2(sin(l)/cos(l): if(l.lt.0) l = l+2*pi if radians.
c
cc    real*8 darsin  (required for series/1 only)
      double precision dtor,rtod,rar,decr,cosa,sina,cosd,x,y,z,cosb,
     1 sinb,eq2,eq3,xbr,xlr,cosl,sinl
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      rar  = dble(rad) * dtor
      decr = dble(decd) * dtor
      cosa = dcos(rar)
      sina = dsin(rar)
      cosd = dcos(decr)
      z    = dsin(decr)
      x = cosa*cosd
      y = sina*cosd
      sinb = -.86760082d0*x - .18837460d0*y + .46019979d0*z
      if(dabs(sinb).lt.1.d0) go to 2
      sinb = dsign(1.d0,sinb)
cc    xbr = darsin(sinb)
      xbr =  dasin(sinb)
      xlr = 0.d0
      go to 3
    2 eq2  = -.06698874d0*x - .87275577d0*y - .48353892d0*z
      eq3  = +.49272847d0*x - .45034696d0*y + .74458464d0*z
cc    xbr = darsin(sinb)
      xbr =  dasin(sinb)
      cosb = dsqrt(1.d0-sinb*sinb)
      cosl = eq2/cosb
      sinl = eq3/cosb
      if(dabs(cosl).gt.1.d0) cosl = dsign(1.d0,cosl)
      if(dabs(sinl).gt.1.d0) sinl = dsign(1.d0,sinl)
      xlr = datan2(sinl,cosl)
    3 xl  = xlr * rtod
      if(xl .lt. 0.) xl = 360. + xl
      xb = xbr * rtod
      return
c
      end

      subroutine galecl(xlg,xbg,xlam,beta)
c 16jun86  cdc/ve double precision version stored in dgalecl_f
c  probably no need for this version on cdc.
c
c 22mar85 for hepvax (changed darsin to dasin)
c jdb  04jun81 galactic to ecliptic coordinate conversion.
c inputs: xlg is gal.long in degrees; xbg is gal.lat in degrees.
c outputs:xlam is ecl.long in degrees;beta is ecl.lat in degrees.
c  all subroutine arguments single precision.
c
c note: statements w/cc**** are kept here for comparison w/ the
c  individual subroutines that this one is based on.
c
c 1st section based on galactic to equatorial conversion subroutine
c   equcor.  2nd based on equ. to ecliptic subroutine equecl.
c
c compute equatorial from galcatic (section 1). equations from t.soifer
c
c inputs: xlg l in degrees, xbg b in degrees.(single precision)
c b = galactic latitude (-90. to + 90. deg);l=galactic longitude(0-360)
c  x = cos(b)*cos(l)
c  y = sin(l)*cos(b)
c  z = sin(b)
c
cc    real*8 darsin  (required statement for series/1 only)
      double precision dtor,rtod,xlr,xbr,cosb,cosl,sinl,x,y,z,sind,
     1 cosd,eq2,eq3,cosa,sina
      double precision sinr,cosr,xl,yl,zl,xe,ye,ze,cose,sine,xlamr,
     1 betar
cc****real*8 decr,rar
      equivalence (sinr,sina),(cosr,cosa)
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      data cose/0.9174369452102114d0/,sine/0.3978812028273710d0/
c
      xlr = dble(xlg) * dtor
      xbr = dble(xbg) * dtor
      cosb = dcos(xbr)
      cosl = dcos(xlr)
      sinl = dsin(xlr)
      z    = dsin(xbr)
      x = cosb*cosl
      y = sinl*cosb
      sind = -.48353892d0*x + .74458464d0*y + .46019979d0*z
      if(dabs(sind).lt.1.d0) go to 2
      sind = dsign(1.d0,sind)
      cosd = 0.0d0
      sina = 0.0d0
      cosa = 1.0d0
cc****decr = darsin(sind)
cc****rar = 0.d0
      go to 3
    2 eq2  = -.06698874d0*x + .49272847d0*y - .86760082d0*z
      eq3  = -.87275577d0*x - .45034696d0*y - .18837460d0*z
cc****decr = darsin(sind)
      cosd = dsqrt(1.d0-sind*sind)
      cosa = eq2/cosd
      sina = eq3/cosd
      if(dabs(cosa).gt.1.d0) cosa = dsign(1.d0,cosa)
      if(dabs(sina).gt.1.d0) sina = dsign(1.d0,sina)
cc****rar = datan2(sina,cosa)
    3 continue
cc**3 rad  = rar * rtod
cc****if(rad .lt. 0.) rad = 360. + rad
cc****decd = decr * rtod
c
c
c compute ecliptic from equatorial coordinates (section 2)
c
c  1950. equatorial to ecliptic conversion coordinate subroutine
c
c  epsilon = e = 23deg26'44.84"  equations for ecl.conv.from j.fowler
c
c inputs: rar ra in radians; decr dec in radians. (double prec.)
c
c returned: xlam=lambda=ecliptic longitude in degrees (single prec.)
c                     (0 - 360 deg)
c
c         beta= ecliptic latitude in degrees (single prec.)
c                      (-90. to +90. deg)
c  xe = sin(dec)              xl = xe*cos(e) - ye*sin(e)
c  ye = -cos(dec)*sin(ra)     yl = xe*sin(e) + ye*cos(e)
c  ze = cos(dec)*cos(ra)      zl = ze
c
c
cc****rar = dble(rad)*dtor
cc****decr = dble(decd)*dtor
c
c sind & cosd previously computed; also sinr & cosr which are
c    equivalenced w/ sina & cosa.
cc****cosd = dcos(decr)
cc****cosr = dcos(rar)
cc****sind = dsin(decr)
cc****sinr = dsin(rar)
c
      xe = sind
      ye = -(cosd*sinr)
      ze = cosd*cosr
c
      xl = cose*xe + sine*ye
      yl = -sine*xe + cose*ye
      zl = ze
c
      if(dabs(xl) .gt. 1.d0) xl = dsign(1.0d0,xl)
cc    betar = darsin(xl)
      betar =  dasin(xl)
c     if(dabs(zl) .gt. 1.d0) zl = dsign(1.0d0,zl) do not need these 2
c     if(dabs(yl) .gt. 1.d0) yl = dsign(1.0d0,yl) .. statements.
      xlamr = datan2(-yl,zl)
c
      xlam = xlamr * rtod
      if(xlam .lt. 0.) xlam = 360. + xlam
      beta = betar * rtod
      return
c
      end

      subroutine galequ(xl,xb,rad,decd)
c 16jun86  jdb.  double precision cdc/ve version (probably will not be
c    needed)  stored in dgalequ_f.
c
cccc  entry equcor(xl,xb,rad,decd)   obsolete entry pt equcor.
c 22mar85 for hepvax (changed darsin to dasin;removed entry equcor)
c 17mar82 updated so has more sensible name- galequ, but still
c   retains alias for equcor.
c 02oct80  jdb   29jan81 to input dtor,rtod to more places. 22apr81
c subroutine to compute earth equatorial coords from galactic coords
c inputs: xl l in degrees, xb b in degrees.(single precision)
c b = galactic latitude (-90. to + 90. deg);l=galactic longitude(0-360)
c outputs: rad ra in degrees, decd dec in degrees.(single precision)
c  x = cos(b)*cos(l)
c  y = sin(l)*cos(b)
c  z = sin(b)
c
ccc   real*8 darsin  (required state.for series/1 only)
      double precision dtor,rtod,xlr,xbr,cosb,cosl,sinl,x,y,z,sind,
     1 cosd,decr,rar,eq2,eq3,cosa,sina
      data dtor/0.01745329251994329d0/,rtod/57.29577951308233d0/
      xlr = dble(xl) * dtor
      xbr = dble(xb) * dtor
      cosb = dcos(xbr)
      cosl = dcos(xlr)
      sinl = dsin(xlr)
      z    = dsin(xbr)
      x = cosb*cosl
      y = sinl*cosb
      sind = -.48353892d0*x + .74458464d0*y + .46019979d0*z
      if(dabs(sind).lt.1.d0) go to 2
      sind = dsign(1.d0,sind)
cc    decr = darsin(sind)
      decr =  dasin(sind)
      rar = 0.d0
      go to 3
    2 eq2  = -.06698874d0*x + .49272847d0*y - .86760082d0*z
      eq3  = -.87275577d0*x - .45034696d0*y - .18837460d0*z
cc    decr = darsin(sind)
      decr =  dasin(sind)
      cosd = dsqrt(1.d0-sind*sind)
      cosa = eq2/cosd
      sina = eq3/cosd
      if(dabs(cosa).gt.1.d0) cosa = dsign(1.d0,cosa)
      if(dabs(sina).gt.1.d0) sina = dsign(1.d0,sina)
      rar = datan2(sina,cosa)
    3 rad  = rar * rtod
      if(rad .lt. 0.) rad = 360. + rad
      decd = decr * rtod
      return
c
      end

c \iras\hgtprc.for = :user.iras649.ipacx.hgtprc_f
c herget general precession routine (note: all arguments double precision).
c from tom chester's friend, wayne at nssdc, 16dec87 - method used
c to precess bright star cat positions to 2000. at nssdc.
c
c modifications by jdb:
c changed DARSIN to DASIN in function call for cdc/apc3
c made PI2 double precision ( was double in data statement, but not
c   specified such in specification statement).
c corrected value of PI2 (rather insignificant correction).
c
      SUBROUTINE HGTPRC (RA1,DEC1,EPOCH1,EPOCH2,RA2,DEC2)
C     HERGET PRECESSION, SEE P. 6 OF PUBL. CINCINNATI OBS. NO. 24.
C INPUT=  RA1 AND DEC1 MEAN PLACE, IN RADIANS, FOR EPOCH1, IN YEARS A.D.
C OUTPUT= RA2 AND DEC2 MEAN PLACE, IN RADIANS, FOR EPOCH2, IN YEARS A.D.
      DOUBLE PRECISION CDR,RA1,DEC1,EPOCH1,EPOCH2,RA2,DEC2,X1(3),X2(3),
     1 R(3,3),T,ST,A,B,C,EP1,EP2,CSR
      double precision pi2
      DATA CDR /0.17453292519943D-01/
      DATA EP1,EP2,PI2 /2*0.D0,6.283185307179586D0/
C     COMPUTE INPUT DIRECTION COSINES
      A = DCOS(DEC1)
      X1(1) = A * DCOS(RA1)
      X1(2) = A * DSIN(RA1)
      X1(3) = DSIN(DEC1)
C     SET UP ROTATION MATRIX (R)
      IF (EP1 .EQ. EPOCH1 .AND. EP2 .EQ. EPOCH2) GO TO 10
      CSR = CDR/3600.D0
      T = 0.001D0 * (EPOCH2 - EPOCH1)
      ST = 0.001D0 * (EPOCH1 - 1900.D0)
      A = CSR * T * (23042.53D0 + ST * (139.75D0 + 0.06D0 * ST) + T *
     1   (30.23D0 - 0.27D0 * ST + 18.0D0*T))
      B = CSR * T * T * (79.27D0 + 0.66D0 * ST + 0.32D0 * T) + A
      C = CSR * T * (20046.85D0 - ST * (85.33D0 + 0.37D0 * ST) + T *
     1   (-42.67D0 - 0.37D0 * ST - 41.8D0 * T))
      SINA = DSIN(A)
      SINB = DSIN(B)
      SINC = DSIN(C)
      COSA = DCOS(A)
      COSB = DCOS(B)
      COSC = DCOS(C)
      R(1,1) = COSA*COSB*COSC - SINA*SINB
      R(1,2) = -COSA*SINB - SINA*COSB*COSC
      R(1,3) = -COSB*SINC
      R(2,1) = SINA*COSB + COSA*SINB*COSC
      R(2,2) = COSA*COSB - SINA*SINB*COSC
      R(2,3) = -SINB*SINC
      R(3,1) = COSA*SINC
      R(3,2) = -SINA*SINC
      R(3,3) = COSC
C     PERFORM THE ROTATION TO GET THE DIRECTION COSINES AT EPOCH2
   10 DO 20 I=1,3
      X2(I) = 0.D0
      DO 20 J=1,3
   20 X2(I) = X2(I) + R(I,J)*X1(J)
      RA2 = DATAN2(X2(2),X2(1))
      IF (RA2 .LT. 0) RA2 = PI2 + RA2
      DEC2 = DASIN(X2(3))
      RETURN
      END
c
      subroutine SUBSTR(string, subst, nsub)
c
c/////////////////////////////////////////////////////////////////
c                                                                /
c     SUBSTR finds the substrings in string                      /
c                                                                /
c/////////////////////////////////////////////////////////////////
c
      character*(*) string, subst(*)
      integer       nsub
c
      iend = 0
      leng = len(string)
      nsub = 0
c
c     Find the Beginning of the Next Substring
c
 200  continue
      do 10 i=iend+1,leng
          ibgn = i
          if(string(i:i) .NE. ' ') go to 11
  10  continue
      ibgn = leng + 1
      go to 300
  11  continue
c
c     find the end of the substring
      do 20 i=ibgn+1,leng
          iend = i-1
          if(string(i:i) .EQ. ' ') go to 21
  20  continue
      iend = leng + 1
  21  continue
c
      nsub = nsub + 1
      subst(nsub) = string(ibgn:iend)
c
      go to 200
 300  continue
c
      return
      end


