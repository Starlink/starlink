      character*50 name			!Image name
      character*5 imgchr		!Image descriptor name
      character*6  outtype		!Output image type (real:short)

      character*32 objname		!Object name parameter name
      character*32 expname		!Exposure time parameter name
      character*32 prefname		!Preflash time parameter name

      character*30 titleo		!Output image title

      real zgain			!
      real znoise			!
      real alpha			!
      real abias			!
      real abiasstd			!
      real anorm			!
      real expt				!Exposure time
      real preft			!Preflash time
      logical dobval			! Find bias value in image?
      logical dolims			! Flag values outside limits?
      real    alolim			! Low value limit
      real    ahilim			! High value limit
      logical dodisp			! Display output?
      integer ipwb			! Pointer to bias work area

      integer knorm			! Normalisation type (1=;
					! 2=none;3=4=)
      logical donorm			! Flag to do normalisation
      integer ktype			! Input data type (1=signed 16bit,
					!       2=unsigned 16bit, 
					!       3=badsigned 16bit,4=real)
      integer corrtype			!
      integer lintype			!
      integer ipt			! Work space for real image
      integer iptb			! Work space for bad pixel map
      integer nxw(2)			!
      integer nyw(2)			!
      integer nxbw(2)			!
      integer nybw(2)			!
      integer nxnw(2)			!
      integer nynw(2)			!
      integer nxdw(2)			!
      integer nydw(2)			!
      integer nxb			! Bias area x size
      integer nyb			! Bias area y size

      real bsb				!
      real bzb				!
      integer invalb			!
      integer ipb			!
      integer nsb(2)			!
      logical dobias			!

      real bsd				!
      real bzd				!
      integer invald			!
      integer ipd			!
      integer nsd(2)			!
      logical dodark			!

      real bsfl				!
      real bzfl				!
      integer invalfl			!
      integer ipfl			!
      integer nsfl(2)			!
      logical doflat			!

      real bsfr				!
      real bzfr				!
      integer invalfr			!
      integer ipfr			!
      integer nsfr(2)			!
      logical dofrin			!

      real bsp				!
      real bzp				!
      integer invalp			!
      integer ipp			!
      integer nsp(2)			!
      logical dopref			!
    
      integer nxc			! X size of reference images
      integer nyc			! Y size of reference images
      integer nxof			! Reference image X offset
      integer nyof			! Reference image Y offset
      real bso				!
      real bzo				!
      integer invalo			!
      real    rinvalo			!
      integer ipo			!
      integer nxo			!
      integer nyo			!
      logical dostore			!

      integer ipbpl			! Main bpl pointer
      integer nxbpl			! Main bpl x size
      integer nybpl			! Main bpl y size
      integer ipbpmap			! Main bpmap pointer
      logical dobad			! Use main bpl
      integer ipbbpl			! Bias bpl pointer
      integer nxbbpl			! Bias bpl x size
      integer nybbpl			! Bias bpl y size
      integer ipbbpmap			! Bias bpmap pointer
      logical dobadb			! Use bias bpl

CX      logical un_dum1			!Make logicals up to times 4
CX      logical un_dum2			!Make logicals up to times 4
CX      logical un_dum3			!Make logicals up to times 4



      common /unccdaa/ bsb,    bsp,    bsfr,    bsfl,    bsd,
     +                 bzb,    bzp,    bzfr,    bzfl,    bzd,
     +                 nsb,    nsp,    nsfr,    nsfl,    nsd,
     +                 ipb,    ipp,    ipfr,    ipfl,    ipd,
     +                 invalb, invalp, invalfr, invalfl, invald,
     +                 bso,    bzo,
     +                 nxo,    nxbbpl,   nxbpl,   nxb,
     +                 nyo,    nybbpl,   nybpl,   nyb,
     +                 ipo,    ipbbpmap, ipbpmap, ipwb,
     +                 invalo, rinvalo,  ipbbpl,  ipbpl

      common /unccdab/ nxw,    nyw,    nxbw,     nybw,    nxnw, 
     +                 nynw,   nxdw,   nydw,     nxc,     nyc,  
     +                 ipt,    iptb

      common /unccdac/ abias,  anorm,  expt,     preft,   abiasstd, 
     +                 nxof,   nyof,   alolim,   ahilim,  ktype, 
     +                 alpha,  knorm,  zgain,    znoise,  lintype,
     +                 corrtype

      common /unccdb/ dobias,    dopref,   dofrin,   doflat,  
     +                dodark,    dobad,    dobadb,   dobval,  
     +                dodisp,    dostore,  dolims,   donorm

      common /unccdc/ name,    imgchr, objname,  expname, titleo,
     +                prefname, outtype



