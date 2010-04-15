/* DSX_GEN.H */

extern Display        *VD_ID; 		/* Display identifier */
extern char           BY_PC_ID[256]; 	/* Byte equivalents of PC_ID */
extern Colormap       CM_ID;		/* Panel Default Colour map identifier */
extern unsigned long  PC_ID[256];	/* Panel Colour index identifiers */
extern XColor         COLOUR[256];	 /*Panel Colour index */

extern Bool      OWNCOL;		/* Use own colours in loading display LUT  */


/* DSX_PANEL.H */


int         SC_P_ID;		/* Panel screen identifier */
Window      RW_P_ID;		/* Panel root window of screen identifier */
Window      WD_P_ID;		/* Panel window identifier */
GC          GC_P_ID;		/* Panel graphic context identifier */
XFontStruct *FT_P_ID;		/* Panel font identifier */
Pixmap      P_PIXMAP;		/* Panel image pixmap */
int         P_DEPTH;		/* No of planes of panel screen */
Visual      *P_VISUAL;

int         PID;

Window      WD_PH_ID;		/* Panel Help window identifier */
GC          GC_PH_ID;		/* Panel Help graphic context identifier */
Pixmap      PH_PIXMAP;		/* Panel Help image pixmap */
int         PHNX;
int         PHNY;

char        XPM[500*500];	/* Panel areas */
char        XPD[2000];
char        XPL[2000];
char        GL, GM, GD;
char        XDU[86*21];		/* Panel buttons */
char        XDD[86*21];
char        IR[67*16];
char        IG[67*16];
char        XDH[86*46];


/* DS_GEN.H */

extern int DSWINDX,DSWINDXM,DSWINDY,DSZOOMMAX,DSTYPE,DSNXS,DSNYS,DSKVRANGE;
extern int DSNXE,DSNYE,DSCOMFX,DSCOMFY,DSIXS,DSIYS,DSSNX,DSSNY,NUMXBUTTONS;
extern int DSZM ,DSZPX ,DSZPY;
extern float DSVMIN, DSVMAX, DSCURPOSX, DSCURPOSY, DSCRSL;
extern Bool DSWRAP, DSSCUR, DSOPEN, DSCURSET;

/* DS_PANEL.H */

int PNSNX, PNSNY, PNNUM, PNNROW, PNNCOL, PDSTYPE, PDNUML, PNHPOSX, PNHPOSY;
int* PNJCON;
int* PNNUMS;
Bool DOPANEL, PDSOPEN, PDS_DUM1, PDS_DUM2;


/******************************************************************
 LUT.H

  Color tables copyright Smithsonian Astrophysical Observatory 1989
*/

#define NUMDCOL 150			/* No of colours in LUT */
extern int   NUMDDCOL;			/* No of colours in display for LUT colours */

/* IMAGE.H */

extern int NX, NY, INVAL;
extern float BS, BZ, RINVAL;
extern POINTER(IPIM);

