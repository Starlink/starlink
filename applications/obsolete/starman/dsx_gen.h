/* DSX_GEN.H */

/* The largest screen that can be supported is one that is less
   than 1.5kx1.5k.

   The virtual arrays that are used, are filled up as X likes,
   that is the start is at [0,0] and at the top left hand of
   the picture.

   The three coordinate systems used are 'image' 'virtual' and
   'screen'. These all start at (1,1) from the bottom left hand.

  Thus in filling the virtual arrays and in any action writing
  to the screen, coordinates have to be transformed.

     screen x =  array x - 1
     screen y =  screen size - array y

*/

int       SC_ID;			/* Screen identifier */
Display   *VD_ID; 			/* Display identifier */
Window    WD_ID;			/* Window identifier */
Window    RW_ID;			/* Root window of screen identifier */
GC        GC_ID;			/* Graphic context identifier */
Colormap  CM_ID;			/* Default colour map identifier */
unsigned long int  PC_ID[256];		/* Colour indexes pointers */
XColor    COLOUR[256];			/* Colour index */
Pixmap    PIXMAP;			/* Image display pixmap */


Visual    *D_VISUAL;			/* Visual type */
char      BY_PC_ID[256];		/* Byte equivalents of PC_ID */
int       PID;

Bool      OWNCOL;			/* Use own colours in loading display LUT  */


/* DSX_PANEL.H */


extern Window      WD_P_ID;
extern char        XPM[500*500];	/* Panel area */


/* DS_GEN.H */

int   DSWINDX, DSWINDXM,   DSWINDY, DSZOOMMAX, DSTYPE, DSNXS, DSNYS,  DSKVRANGE;
int   DSNXE,      DSNYE,    DSCOMFX, DSCOMFY,    DSIXS,  DSIYS, DSSNX, DSSNY, NUMXBUTTONS;
int   DSZM,       DSZPX,     DSZPY;
float DSVMIN,    DSVMAX, DSCURPOSX, DSCURPOSY, DSCRSL;
Bool  DSWRAP,    DSSCUR,    DSOPEN,  DSCURSET;

/* DS_PANEL.H */

extern int PNSNX, PNSNY, PNNUM, PNNROW, PNNCOL, PDSTYPE, PDNUML, PNHPOSX, PNHPOSY;
extern int* PNJCON;
extern int* PNNUMS;
extern Bool DOPANEL, PDSOPEN, PDS_DUM1, PDS_DUM2;


/* VIRT.H */

#define MAXVIRT 1536*1536
      char      VT_IM[MAXVIRT];         /* Display virtual image */


/* IMAGE.H */

int NX, NY, INVAL;
float BS, BZ, RINVAL;
POINTER(IPIM);

/*
 LUT.H
					/* No of colours in LUT */
#define NUMDCOL 150
int   NUMDDCOL;				/* No of colours in display for LUT colours */
int   DEPTH;				/* No of memory planes */
extern int	LUT_STORED; 		/* Number of stored LUTs */
extern int	LUT_ENDS;		/* LUT end types (1=black/white;2=wh/bl;3=col/col;4=wrap */
extern int       LUT_NUM;		/* Present LUT number */
extern float     LUT_SC; 		/* LUT scale */
extern float     LUT_ZE;		/* LUT zero */
extern Bool      LUT_FLIPPED;		/* Is LUT flipped? */
