/* This file contains some general definitions for the Figaro/PGPLOT X Window */
/* display server. */

/* Sam Southard, Jr. */
/* Created: 6-Nov-1990 */
/*  7-Nov-1990	SNS/CIT	Added default values */
/*  8-Nov-1990	SNS/CIT	Added LG_WIN_DEPTH, MAXSTRLEN, MALLOC_ERR, BADCOM,  */
/*			and INCCOM */
/* 16-Nov-1990	SNS/CIT	Added LG_MAX_HEIGHT and LG_MAX_WIDTH. */
/* 10-Dec-1990	SNS/CIT	wininfo struct added.  VMS changes merged in. */
/*			LG_DEPTH changed to LG_MIN_DEPTH.  BM_MIN_DEPTH
/*			added. */
/* 11-Dec-1990	SNS/CIT	winname and iconname added to wininfo structure. */
/*			BM_MIN_WIDTH, BM_MIN_HEIGHT, BM_WIDTH, BM_HEIGHT, */
/*			BM_MAX_WIDTH, BM_MAX_HEIGHT, and BM_COLORS added. */
/*			pix member of wininfo structure now a pointer. */
/* 12-Dec-1990	SNS/CIT	LG_BDWIDTH changed to BORDER_WIDTH.  BLANK_WIDTH
/*			added.  Image, line, maxh, minh, maxw, and minw */
/*			members added to wininfo structure. */
/* 17-Dec-1990	SNS/CIT	colors member added to wininfo structure. */
/* 18-Dec-1990	SNS/CIT	cursx and cursy members added to wininfo structure. */
/* 13-Mar-1991	SNS/CIT	yoff member removed from wininfo structure. */
/* 29-Mar-1991	SNS/CIT	line member removed from wininfo structure. */
/* 10-May-1991	SNS/CIT	FAIL changed to -1. */
/* 26-Jul-1991	SNS/CIT	Added bw member to wininfo structure for B&W */
/* 30-Jul-1991	SNS/CIT	Added BM_MIN_COLORS definition.  Added allcells and */
/*			usecells member to wininfo structure for fastdisp */
/* 31-Jul-1991	SNS/CIT	Added im, imdat, xoff, yoff, imwidth, imheight, */
/*			modlut, mlx, mly, slope, offset, and visual members */
/*			to wininfo structure.  Added BM_MAX_SH_COLS and */
/*			BM_MIN_SH_COLS definitions. */
/*  1-Aug-1991	SNS/CIT	Added showcur, curxsc, curxoff, curysc, curyoff, dsc, */
/*			doff, zim, zimdat, and zfac members to wininfo */
/*			structure. */
/*  2-Aug-1991	SNS/CIT	Max window sized increased to 2048x2048.  xoff & yoff */
/*			member of wininfo are now relative to raw data. */
/* 20-Aug-1991	SNS/CIT	font and icon members added to wininfo structure */
/* 22-Aug-1991	SNS/CIT	LOC_WIDTH and LOC_HEIGHT added. */
/* 27-Aug-1991	SNS/CIT	invert member added to wininfo structure */
/*  3-Sep-1991	SNS/CIT	CM_WIDTH and CM_HEIGHT added. */
/*  4-Oct-1991	SNS/CIT	resource structure added. */
/*  7-Oct-1991	SNS/CIT	KeySym members of resource structure added. */
/*  9-Oct-1991	SNS/CIT	Font member of wininfo structure removed */
/* 14-Oct-1991	SNS/CIT	Allcells and usecells members of wininfo removed */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT Resource structure now includes a PS output file, a */
/*			printer, the number of microseconds to sleep between */
/*			client existance checks, whether or not we should */
/*			force the location window pixels to be square, and */
/*			the number of color cells to copy from the default
/*			color map to any private color map. */
/* 25-Nov-1991	SNS/CIT	Now has separate zoom factors for X & Y and allows */
/*			user specification of the behaviour of line plots. */
/* 27-Nov-1991	SNS/CIT	Now has one more way of controlling line plots. */
/* 31-Jan-1992	SNS/CIT	LEAVE_COLORS added and leavecolors added to the */
/*			resource structure */
/* 18-Feb-1992	SNS/CIT	LG_MIN_COLORS added and lgcolors added to the */
/*			resource structure. */
/* 24-Feb-1992	SNS/CIT	Ro member added to the wininfo structure.  Visual */
/*			type #defines added. */
/*  3-Mar-1992	SNS/CIT	visclass member added to resource structure. */
/*  8-Apr-1992	SNS/CIT	USLEEP_TIME changed to 10000 */
/*  9-Apr-1992	SNS/CIT	No longer uses a minimum or maximum dimensions for */
/*			the windows. */
/* 10-Apr-1992	SNS/CIT	winxoff and winyoff members added to the wininfo */
/*			structure. */
/*  7-May-1992	SNS/CIT	lgcross added to resource structure. */
/* 24-Jun-1992	SNS/CIT	Added space for histogram equalization key and size */
/* 25-Jun-1992	SNS/CIT	plothist added to resource structues. */
/* 26-Jun-1992	SNS/CIT	HALF_TICK added. */
/* 30-Sep-1992	SNS/CIT	LUT wrap resources and RCS id string added. */
/* 14-Oct-1992	SNS/CIT	RCS id string now only added if INC_HEADER_RCS */
/*			#define'd.  Space for MOUSEMODE and DOBOX keys added. */
/*			Now protected from doubel #include'sion */

#ifndef INC_FIGDISP_H
#define INC_FIGDISP_H

#ifndef lint
#ifdef INC_HEADER_RCS
static char figdisp_h_rcsid[]="@(#)$Id$";
#endif
#endif

/* The figdisp structure needs definitions from Xutil.h */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* some return value definitions */

/* The basics: */
#define FAIL	1	/* used if a routine fails for some reason. */
#define SUCCEED	0	/* the opposite of FAIL.  MUST be 0 because 0 is the */
			/* only value which can be uniquely identified in a */
			/* test case (all other return values are failures */
			/* of some sort) */

/* some more advanced: */
#define ALREADY_RUNNING	2	/* Another copy of the display routine is */
				/* already running */
#define MALLOC_ERR	3	/* An error allocating something */
#define BADCOM		4	/* Someone gave us a bad command */
#define INCCOM		5	/* The last command in a command buffer was */
				/* incomplete */

/* some default values */
#define LG_WIDTH	512	/* default width of the line graphics window */
#define LG_HEIGHT	512	/* default height of the line graphics window */
#define LG_MIN_WIDTH	64	/* The minimum line graphics window width */
#define LG_MIN_HEIGHT	64	/* The minimum line graphics window height */
#ifdef VMS
/* Our VMS Machines don't have as much memory */
#define LG_MAX_WIDTH	512	/* The maximum line graphics window width */
#define LG_MAX_HEIGHT	512	/* The maximum line graphics window height */
#else
#define LG_MAX_WIDTH	2048	/* The maximum line graphics window width */
#define LG_MAX_HEIGHT	2048	/* The maximum line graphics window height */
#endif
#define LG_COLORS	16	/* The number of colors for line graphics */
#define LG_MIN_COLORS	2	/* Minimum number of colors for line graphics */
#define LG_MIN_DEPTH	4	/* The minimum depth of the line graphics */
				/* window. Right now should be equal to log */
				/* base 2 of LG_COLORS */
#define BORDER_WIDTH	1	/* width of the window border */
#define BLANK_WIDTH	5	/* width of the blank area between drawing */
				/* surface and window border */
#define BM_MIN_DEPTH	8	/* The minimum depth of bitmap window */
#define BM_WIDTH	512	/* default width of bitmap window */
#define BM_HEIGHT	512	/* default height of bitmap window */
#define BM_MIN_WIDTH	64	/* minimum width of bitmap window */
#define BM_MIN_HEIGHT	64	/* minimum height of bitmap window */
#define BM_MAX_WIDTH	2048	/* maximum width of bitmap window. */
#define BM_MAX_HEIGHT	2048	/* maximum height of bitmap window */
#define BM_COLORS	65536	/* number of colors for bitmaps */
#define BM_MIN_COLORS	170	/* minimum number of colors for bitmaps */
#define BM_MAX_SH_COLS	225	/* maximum number of colors to use when we're */
				/* sharing a color table */
#define BM_MIN_SH_COLS	200	/* minimum number of colors to use when we're */
				/* sharing a color table */
#define LOC_WIDTH	128	/* the default location window width */
#define LOC_HEIGHT	128	/* the default location window height */
#define CM_WIDTH	256	/* default width of the color map window */
#define CM_HEIGHT	32	/* default height of the color map window */
#define	MIN_USLEEP_TIME	10000	/* The minimum number of microseconds to */
				/* wait between existance checks */
#define USLEEP_TIME	10000	/* The default number of microseconds to wait */
				/* between existance checks */
#define SAVE_COLORS	6	/* The default number of colors to copy from */
				/* the default colormap to any private */
				/* color map we use. */
#define LEAVE_COLORS	0	/* The default number of colors to leave */
				/* available in the default color map */
#define HIST_WIDTH	0	/* The default width of the histogram area */
				/* 0 = use entire image */
#define	HIST_HEIGHT	0	/* The default height of the histogram area */
#define HALF_TICK	2	/* Half the size of axis tick, in X11 pixels */
#define INIT_LUT_WRAP	1	/* The default initial LUT wrap factor */

/* some misc. constants */
#define MAXSTRLEN	80	/* The maximum string length */
#define UseDefaultCmap	1	/* Use the default color map */
#define	UseRWVisual	2	/* Use a read/write visual */
#define UseROVisual	3	/* Use a read only visual */

/* These are two phony visual classes.  The assume that the normal X visuals */
/* do not clash.  For Openwindows 2.0, visuals classes are non-negative, so */
/* this is a valid assumption. */
#define AnyVis		-1	/* Use any visual */
#define DefaultVis	-2	/* Use only the default visual */

/* The number of characters in the box window */
#define BOX_COL_CHARS	16

/* The window information structure */
struct wininfo {
	Window	win;	/* The window for display */
	unsigned long *pix; /* The pixel values */
	Pixmap pixmap;	/* The pixmap */
	int	mapped;	/* True if window is mapped */
	unsigned int height;	/* current window height */
	unsigned int width;	/* current window width */
	XTextProperty winname;	/* The window's name */
	XTextProperty iconname;	/* The icon's name */
	XImage *image;	/* An image for the window's data */
	unsigned char *imdat;	/* The window's data */
	int winxoff;	/* The offset from the start of the window to the */
	int winyoff;	/* start of the Ximage structure */
	int colors;	/* the number of colors used by this window */
	int cursx;	/* The X position of the cursor */
	int cursy;	/* the Y position of the cursor */
	int bw;		/* If the display is black & white */
	int ro;		/* If the display is read only */
	int xoff,yoff;	/* offset from image data OF APPROPRIATE ZOOM FACTOR */
			/* into Ximage */
	int imwidth,imheight;	/* width & height of the image */
	int modlut;	/* true is we're modifying the LUTs */
	int mlx,mly;	/* the place where we started modifying the luts */
	double slope;	/* slope of the LUT transfer function */
	double offset;	/* offset of the LUT transfer function */
	int showcur;	/* show cursor position */
	float curxsc;	/* cursor x scale value */
	float curxoff;	/* cursor x offset value */
	float curysc;	/* cursor y scale value */
	float curyoff;	/* cursor y offset value */
	float dsc;	/* data scale value */
	float doff;	/* data offset value */
	int xzoom;	/* X zoom factor */
	int yzoom;	/* Y zoom factor */
	Pixmap icon;	/* this window's icon */
	int invert;	/* true if we want to invert the color maps */
};

/* the geometry structure */
struct geometry {
	int w;	/* the width */
	int h;	/* the height */
	int x;	/* the x position */
	int y;	/* the y position */
};

/* The rawdata union */

union rawdata {
	unsigned short *b16;	/* for 16 bit data */
	unsigned char *b8;	/* for 8 bit data */
};

/* The keysym array definitions */
#define ZOOMIN		0
#define ZOOMNORM	1
#define ZOOMOUT		2
#define	HELP		3
#define CURSOR		4
#define RECENTER	5
#define SHOWLOC		6
#define QUIT		7
#define SHOWCM		8
#define SHOWPAT		9
#define ROW		10
#define IMPS		11
#define	WINPS		12
#define INVERT		13
#define SEEING		14
#define COL		15
#define DECSLIT		16
#define INCSLIT		17
#define RESSLIT		18
#define INHIBIT		19
#define	ZOOMXIN		20
#define	ZOOMXOUT	21
#define ZOOMYIN		22
#define ZOOMYOUT	23
#define HISTOGRAM	24
#define DECLUTWRAP	25
#define INCLUTWRAP	26
#define RESLUTWRAP	27
#define MOUSEMODE	28
#define DOBOX		29

#define NKEYS		30	/* the total number of defined keys */

/* the various resources which affect the display server operation */
struct resource {
	struct geometry bmgeo;	/* the bitmap geometry */
	struct geometry lggeo;	/* the line graphics geometry */
	struct geometry pgeo;	/* the patch geometry */
	struct geometry cgeo;	/* the color map geometry */
	struct geometry lgeo;	/* the location geometry */
	struct geometry histgeo;	/* geometry of histogram area */
	int maxcolors;	/* number of shared colors to use */
	int mincolors;	/* minimum shared colors to use */
	int maxpcolors;	/* maximum number of private colors to use */
	int minpcolors;	/* minimum number of private colors to use */
	int showhelp;	/* true if we should show help */
	KeySym keys[NKEYS];	/* the keys we use */
	XFontStruct *textfont;	/* The font to use for labels inside windows */
	char *psfile;	/* The PostScript output file */
	char *printer;	/* The printer to use */
	int sleeptime;	/* The number of microseconds to sleep between */
			/* existance checks.  See waitevent.c for details. */
	int forcesquare;	/* true if we should force location window */
				/* pixels to be square */
	int savecolors;	/* The number of colors to copy from the default */
			/* colormap to a private color map. */
	int leavecolors;	/* The number of colors to leave available */
				/* in the default colormap. */
	int lefttoright;	/* true if the user always wants a line plot */
				/* to go from left to right, even if he moved */
				/* the mouse from right to left */
	int ascendcoord; /* True if arbitrary line plots should follow */
			/* ascending coordinates (which one determined by the */
			/* slope of the line) */
	int bottotop;	/* True if column plots should go from bottom to top */
	int ascendy;	/* True if column plots should follow ascending Y */
			/* values.  If true, takes precedence over bottotop. */
	int rowltor;	/* True if row plots should go from left to right */
	int ascendx;	/* True if row plots should follow ascending X.  If */
			/* true, takes precedence over rowltor. */
	int id;		/* The figdisp id.  Used to allow multiple figdisps */
			/* on the same screen. */
	int lgcolors;	/* The number of colors for the line graphics screen */
	int visclass;	/* The visual class allowed */
	int lgcross;	/* True if we should use the crosshair cursor for the */
			/* line graphics window. */
	int plothist;	/* True if line plots should be in histogram form */
	int initwrap;	/* The initial LUT wrap factor */
};

#endif /* INC_FIGDISP_H */
