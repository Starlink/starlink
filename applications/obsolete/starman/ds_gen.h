/* DS_GEN.H */


extern struct
{
      F77_INTEGER_TYPE dscomfx;		/* Displayed X image compression factor to virtual image */
      F77_INTEGER_TYPE dscomfy;		/* Displayed Y image compression factor to virtual image */

      F77_INTEGER_TYPE dsnxs;           /* Displayed image blh pixel X true image pixel number */
      F77_INTEGER_TYPE dsnys;           /* Displayed image blh pixel Y true image pixel number */
      F77_INTEGER_TYPE dsnxe;           /* Displayed image trh pixel X true image pixel number */
      F77_INTEGER_TYPE dsnye;           /* Displayed image trh pixel X true image pixel number */

      F77_INTEGER_TYPE dsixs;           /* Displayed image blh pixel X virtual image position */
      F77_INTEGER_TYPE dsiys;           /* Displayed image blh pixel Y virtual image position */

      F77_INTEGER_TYPE dssnx;		/* Screen (and virtual image) X size */
      F77_INTEGER_TYPE dssny;		/* Screen (and virtual image) Y size */

      F77_INTEGER_TYPE dszm;		/* Screen zoom factor */
      F77_INTEGER_TYPE dstype;		/* Display type (2=vws_vaxstation;3=ikon;4=x11_dec) */

      F77_REAL_TYPE dsvmin;		/* Displayed image unscaled display min value */
      F77_REAL_TYPE dsvmax;		/* Displayed image unscaled display max value */
      F77_REAL_TYPE dscrsl;		/* Size of crosses to be painted */
      F77_INTEGER_TYPE dszpx;		/* X position in virtual image that is at tlh of screen */
      F77_INTEGER_TYPE dszpy;		/* Y position in virtual image that is at tlh of screen */

      F77_INTEGER_TYPE dswindx;		/* x min offset of window edge from screen edge (cms) */
      F77_INTEGER_TYPE dswindy;		/* y min offset of window edge from screen edge (cms) */
      F77_INTEGER_TYPE dswindxm;	/* max x min offset of window edge from screen edge (cms) */
      F77_REAL_TYPE dscurposx;		/* Cursor X screen position */
      F77_REAL_TYPE dscurposy;		/* Cursor Y screen position */
      F77_INTEGER_TYPE dskvrange;	/* image contrast scale got? (0=no;1=yes) */
      F77_INTEGER_TYPE dszoommax;	/* Maximum zoom the display allows*/
      F77_INTEGER_TYPE numxbuttons;	/* Number of buttons on mouse with X windows (2 or 3) */
} F77_NAMED_COMMON(ds_gen);


extern struct
{
      F77_LOGICAL_TYPE dswrap;		/* Displayed image wrap display values around min/max (t/f)? */
      F77_LOGICAL_TYPE dsscur;		/* Cursor started? */
      F77_LOGICAL_TYPE dsopen;		/* Display open? */
      F77_LOGICAL_TYPE dscurset;	/* Cursor placed? */
} F77_NAMED_COMMON(ds_genb);
