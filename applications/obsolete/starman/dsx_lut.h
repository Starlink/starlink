/* DSX_LUT.H */

extern Display   *VD_ID; 		/* Display identifier */
extern Colormap  CM_ID;			/* Default colour map identifier */
extern unsigned long PC_ID[256];	/* Colour indexes pointers */
extern XColor    COLOUR[256];		/* Colours */

extern char      BY_PC_ID[256];		/* Byte equivalents of PC_ID */

extern Bool      OWNCOL;		/* Use own colours in loading display LUT  */

/* DSX_PANEL.H */


extern Display     *VD_P_ID;		/* Panel display identifier */
/*extern Colormap    CM_P_ID;		 Panel Default Colour map identifier */
/*extern unsigned long PC_P_ID[256];	 Panel Colour index identifiers */
/*extern XColor      P_COLOUR[256];	 Panel Colour index */

/* DS_GEN.H */

extern int DSWINDX,DSWINDXM,DSWINDY,DSZOOMMAX,DSTYPE,DSNXS,DSNYS,DSKVRANGE;
extern int DSNXE,DSNYE,DSCOMFX,DSCOMFY,DSIXS,DSIYS,DSSNX,DSSNY,NUMXBUTTONS;
extern int DSZM ,DSZPX ,DSZPY;
extern float DSVMIN, DSVMAX, DSCURPOSX, DSCURPOSY, DSCRSL;
extern Bool DSWRAP, DSSCUR, DSOPEN, DSCURSET;

/* DS_PANEL.H */

extern int PNSNX, PNSNY, PNNUM, PNNROW, PNNCOL, PDSTYPE,
           PDNUML, PNHPOSX, PNHPOSY;
extern int* PNJCON;
extern int* PNNUMS;
extern Bool DOPANEL, PDSOPEN;


/* LUT.H */

#define NUMDCOL 150		/* No of colours in LUT */
extern int   NUMDDCOL;		/* No of colours in display for LUT colours */
#define NLENT   25              /* 'Length' of a LUT storage table */
extern int   DEPTH;		/* No of memory planes */
