CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DS_GEN.INC
C
C  The largest screen that can be supported is one that is less
C  than 1.5kx1.5k.
C
C  The virtual arrays that are used, are filled up as X likes,
C  that is the start is at [0,0] and at the top left hand of
C  the picture.
C
C  The three coordinate systems used are 'image' 'virtual' and 
C  'screen'. These all start at (1,1) from the bottom left hand.
C
C  Thus in filling the virtual arrays and in any action writing
C  to the screen, coordinates have to be transformed.
C
C     screen x =  array x - 1
C     screen y =  screen size - array y
C


      integer dscomfx		! Displayed X image compression factor to virtual image
      integer dscomfy		! Displayed Y image compression factor to virtual image

      integer dsnxs             ! Displayed image blh pixel X true image pixel number
      integer dsnys             ! Displayed image blh pixel Y true image pixel number
      integer dsnxe             ! Displayed image trh pixel X true image pixel number
      integer dsnye             ! Displayed image trh pixel X true image pixel number

      integer dsixs             ! Displayed image blh pixel X virtual image position
      integer dsiys             ! Displayed image blh pixel Y virtual image position

      integer dssnx		! Screen (and virtual image) X size
      integer dssny		! Screen (and virtual image) Y size

      integer dszm		! Screen zoom factor
      integer dstype		! Display type (2=vws_vaxstation;3=ikon;4=x11_dec)

      real    dsvmin		! Displayed image unscaled display min value
      real    dsvmax		! Displayed image unscaled display max value

      real    dscrsl		! Size of crosses to be painted

      integer dszpx		! X position in virtual image that is at tlh of screen
      integer dszpy		! Y position in virtual image that is at tlh of screen

      integer dswindx		! x min offset of window edge from screen edge (cms)
      integer dswindy		! y min offset of window edge from screen edge (cms)
      integer dswindxm		! max x min offset of window edge from screen edge (cms)

      real    dscurposx		! Cursor X screen position
      real    dscurposy		! Cursor Y screen position
      integer dskvrange		! image contrast scale got? (0=no;1=yes)

      integer dszoommax		!Maximum zoom the display allows
      integer numxbuttons	! Number of buttons on mouse with X windows (2 or 3)


      common /ds_gen/ dscomfx,   dscomfy, dsnxs,    dsnys,     dsnxe,      
     +                dsnye,
     +                dsixs,    dsiys,    dssnx,     dssny,       dszm, 
     +                dstype,  dsvmin,   dsvmax,    dscrsl,      dszpx,
     +                dszpy,  dswindx,  dswindy,  dswindxm,  dscurposx,
     +                dscurposy, dskvrange, dszoommax, numxbuttons

      logical dswrap		! Displayed image wrap display values around min/max (t/f)?
      logical dsscur		! Cursor started?
      logical dsopen		! Display open?
      logical dscurset		! Cursor placed?

      common /ds_genb/ dswrap, dsscur, dsopen, dscurset


