/* The routines in this file manipulate the X11 resource databases */

/* Sam Southard, Jr. */
/* Created: 4-Oct-1991 */
/*  7-Oct-1991	SNS/CIT	Handling of patch, cmap, and location geometries and */
/*			min and max shared and private colors added.  Now */
/*			handles specification of keycodes */
/*  8-Oct-1991	SNS/CIT	Global variables moved into globals.h.  -geometry now */
/*			sets lg.geometry in the pgdisp version. */
/*  9-Oct-1991	SNS/CIT	Now handles font initialization */
/* 22-Nov-1991	SNS/CIT	Now handles specifying that output go to a file */
/*			instead of directly to a printer, printer should be */
/*			used for output, how long to wait between checks to */
/*			make sure the client program is still there, */
/*			forcing the location window pixels to be square, and */
/*			the number of colors to be copied from the default */
/*			colormap to a private color map. */
/* 25-Nov-1991	SNS/CIT	Now handles separate zooming in X & Y and flipping */
/*			points in a line graph so that they always go from */
/*			left to right */
/* 27-Nov-1991	SNS/CIT	Now handles figdisp.line.ascending */
/* 31-Jan-1992	SNS/CIT	Now handles figdisp.leaveColors */
/* 14-Feb-1992	SNS/CIT	Now handles figdisp.id to allow specification of */
/*			multiple figdisps/pgdisps. */
/* 18-Feb-1992	SNS/CIT	Resource lookup now works no matter what the program */
/*			is called.  Help message updated to show new options. */
/*			Now handles figdisp.lineColors */
/*  3-Mar-1992	SNS/CIT	Now handles figdisp.visual. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/*  7-May-1992	SNS/CIT	Now handles figdisp.lg.crosshair. */
/* 24-Jun-1992	SNS/CIT	Now handles .histogram and .histgeometry */
/* 25-Jun-1992	SNS/CIT	Now handles .line.histogram. */
/* 30-Sep-1992	SNS/CIT	LUT wrap resources added.  RCS id string added. */
/* 14-Sep-1992	SNS/CIT	Modifications from ARC/HI merged in. */
/*  4-Nov-1992	SNS/CIT	No longer includes malloc.h */
/* 16-Nov-1992	SNS/CIT	resetLUTWrap added into resource table. */

#ifndef lint
static char rcsid[]="@(#)$Id$";
#endif

#include "figdisp.h"
#include "globals.h"

#include <X11/Xresource.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef VMS
#include <pwd.h>
#endif
#ifdef solaris
#include <sys/systeminfo.h>
#endif

#ifdef VMS
#define KEY_HELP	XK_F1	/* help button */
#define KEY_ZOOM_IN	XK_F2	/* key to zoom in */
#define KEY_ZOOM_0	XK_F3	/* go to normal size image */
#define KEY_ZOOM_OUT	XK_F4	/* key to zoom out */

#define KEY_CMAP        XK_F5   /* key to toggle display of color map window */
#define KEY_CUR_TOG     XK_F6   /* key to toggle cursor output */
#define KEY_RECENTER    XK_F7   /* to to recenter image */
#define KEY_LOC_TOG	XK_F8   /* key to toggle the show location window */

#define KEY_QUIT        XK_F9   /* key to quit */
#define KEY_PAT_TOG     XK_F10  /* key to show/update/hide patch window */
#define KEY_ROW         XK_L1   /* key to produce a row plot */
#define KEY_COL         XK_L2   /* key to produce a column plot */

#define KEY_IMPS        XK_L3   /* key to print the entire image on the */
                                /* PostScript printer PRINTER. */
#define KEY_WINPS       XK_L4   /* key to print the visible portion of the */
                                /* image on the PostScript printer PRINTER. */
#define KEY_INHIBIT     XK_L6   /* inhibit other key interpretations */
#define KEY_INVERT      XK_L8   /* key to invert the color maps */
#define KEY_SEE_TOG     XK_L9   /* key to show/update/hide the seeing window */

#define KEY_SLDEC       XK_comma   /* key to decrease slit width */
#define KEY_SLINC       XK_period  /* key to increase slit width */
#define KEY_SLRES       XK_slash   /* key to reset slit width */

#define KEY_HIST        XK_KP_F1   /* key to use histogram equalization */
#define KEY_ZOOMY_IN    XK_KP_1    /* Zoom in Y only */
#define KEY_ZOOMY_OUT   XK_KP_3    /* Zoom in Y only */
#define KEY_ZOOMX_IN    XK_KP_7    /* Zoom in X only */
#define KEY_ZOOMX_OUT   XK_KP_9    /* Zoom in X only */

#define KEY_LWRAP_INC   XK_greater      /* increase the LUT wrap */
#define KEY_LWRAP_DEC   XK_less /* decrease the LUT wrap */
#define KEY_LWRAP_RES   XK_question     /* reset the LUT wrap */
#define KEY_MOUSEMODE   XK_R2   /* key to change function of mouse buttons */
#define KEY_DOBOX       XK_R3   /* key to print image stats within box */

#else /* VMS */

#define KEY_ZOOM_IN     XK_F2   /* key to zoom in */
#define KEY_ZOOM_0      XK_F3   /* go to normal size image */
#define KEY_ZOOM_OUT    XK_F4   /* key to zoom out */
#define KEY_HELP        XK_Help /* help button */
#define KEY_CUR_TOG     XK_F6   /* key to toggle cursor output */
#define KEY_RECENTER    XK_F7   /* to to recenter image */
#define KEY_LOC_TOG     XK_F8   /* key to toggle the show location window */
#define KEY_QUIT        XK_F9   /* key to quit */
#define KEY_CMAP        XK_F10  /* key to toggle display of color map window */
#define KEY_PAT_TOG     XK_L1   /* key to show/update/hide patch window */
#define KEY_ROW         XK_L2   /* key to produce a row plot */
#define KEY_IMPS        XK_L3   /* key to print the entire image on the */
                                /* PostScript printer PRINTER. */
#define KEY_WINPS       XK_L4   /* key to print the visible portion of the */
                                /* image on the PostScript printer PRINTER. */
#define KEY_INVERT      XK_L8   /* key to invert the color maps */
#define KEY_SEE_TOG     XK_L9   /* key to show/update/hide the seeing window */
#define KEY_COL         XK_L10  /* key to produce a column plot */
#define KEY_SLDEC       XK_comma         /* key to decrease slit width */
#define KEY_SLINC       XK_period      /* key to increase slit width */
#define KEY_SLRES       XK_slash     /* key to reset slit width */
#define KEY_INHIBIT	XK_L6	/* inhibit other key interpretations */
#define KEY_HIST	XK_R1	/* key to use histogram equalization */
#define KEY_MOUSEMODE	XK_R2	/* key to change function of mouse buttons */
#define KEY_DOBOX	XK_R3	/* key to print image stats within box */
#define KEY_ZOOMX_IN	XK_R7	/* Zoom in X only */
#define KEY_ZOOMX_OUT	XK_R9	/* Zoom in X only */
#define KEY_ZOOMY_IN	XK_R13	/* Zoom in X only */
#define KEY_ZOOMY_OUT	XK_R15	/* Zoom in X only */
#define KEY_LWRAP_INC	XK_greater	/* increase the LUT wrap */
#define KEY_LWRAP_DEC	XK_less	/* decrease the LUT wrap */
#define KEY_LWRAP_RES	XK_question	/* reset the LUT wrap */
#endif /* VMS */

#define DEFAULT_FONT	"fixed"

/* The options to look for */
static XrmOptionDescRec fdops[] = {
{"-display",	".display",		XrmoptionSepArg, (char *) NULL},
#ifdef PGDISP
{"-geometry",	".lg.geometry",		XrmoptionSepArg, (char *) NULL},
#else
{"-geometry",	".bm.geometry",		XrmoptionSepArg, (char *) NULL},
#endif
{"-bmGeometry",	".bm.geometry",		XrmoptionSepArg, (char *) NULL},
{"-lgGeometry",	".lg.geometry",		XrmoptionSepArg, (char *) NULL},
{"-patchGeometry",".patchgeometry",	XrmoptionSepArg, (char *) NULL},
{"-cmapGeometry",".cmapgeometry",	XrmoptionSepArg, (char *) NULL},
{"-locationGeometry",".locationgeometry",XrmoptionSepArg, (char *) NULL},
{"-histogramGeometry", ".histgeometry",	XrmoptionSepArg, (char *) NULL},
{"-colors",	".bm.maxcolors",	XrmoptionSepArg, (char *) NULL},
{"-maxColors",	".bm.maxcolors",	XrmoptionSepArg, (char *) NULL},
{"-minColors",	".bm.mincolors",	XrmoptionSepArg, (char *) NULL},
{"-privateColors",".bm*private.maxcolors",XrmoptionSepArg, (char *) NULL},
{"-maxPrivateColors",".bm*private.maxcolors",XrmoptionSepArg, (char *) NULL},
{"-minPrivateColors",".bm*private.mincolors",XrmoptionSepArg, (char *) NULL},
{"-lineColors",	".lineColors",		XrmoptionSepArg, (char *) NULL},
{"-help",	".showhelp",		XrmoptionNoArg,  (char *) "True"},
{"-nohelp",	".showhelp",		XrmoptionNoArg,  (char *) "False"},
{"-font",	".font",		XrmoptionSepArg, (char *) NULL},
{"-fn",		".font",		XrmoptionSepArg, (char *) NULL},
{"-psFile",	".psFile",		XrmoptionSepArg, (char *) NULL},
{"-P",		".printer",		XrmoptionSepArg, (char *) NULL},
{"-printer",	".printer",		XrmoptionSepArg, (char *) NULL},
{"-sleepTime",	".sleepTime",		XrmoptionSepArg, (char *) NULL},
{"-forceSquare", ".forceSquare",	XrmoptionNoArg,  (char *) "True"},
{"-noforceSquare", ".forceSquare",	XrmoptionNoArg,  (char *) "False"},
{"-saveColors",	".saveColors",		XrmoptionSepArg, (char *) NULL},
{"-leaveColors",".leaveColors",		XrmoptionSepArg, (char *) NULL},
{"-leftToRight", ".line.leftToRight",	XrmoptionNoArg,  (char *) "True"},
{"-noleftToRight", ".line.leftToRight",	XrmoptionNoArg,  (char *) "False"},
{"-ascendingCoord", ".line.ascending",	XrmoptionNoArg,  (char *) "True"},
{"-noascendingCoord", ".line.ascending", XrmoptionNoArg, (char *) "False"},
{"-plothist",	".line.histogram",	XrmoptionNoArg,  (char *) "True"},
{"-noplothist",	".line.histogram",	XrmoptionNoArg,  (char *) "False"},
{"-ascendingX",	".row.ascending",	XrmoptionNoArg,  (char *) "True"},
{"-noascendingX", ".row.ascending",	XrmoptionNoArg,  (char *) "False"},
{"-rowLeftToRight", ".row.leftToRight",	XrmoptionNoArg,  (char *) "True"},
{"-norowLeftToRight", ".row.leftToRight", XrmoptionNoArg, (char *) "False"},
{"-ascendingY",	".col.ascending",	XrmoptionNoArg,  (char *) "True"},
{"-noascendingY", ".col.ascending",	XrmoptionNoArg,  (char *) "False"},
{"-bottomToTop", ".col.bottomToTop",	XrmoptionNoArg,  (char *) "True"},
{"-nobottomToTop", ".col.bottomToTop",	XrmoptionNoArg,  (char *) "False"},
{"-id",		".id",			XrmoptionSepArg, (char *) NULL},
{"-visual",	".visual",		XrmoptionSepArg, (char *) NULL},
{"-lgCrosshair", ".lg.crosshair",	XrmoptionNoArg,  (char *) "True"},
{"-nolgCrosshair", ".lg.crosshair",	XrmoptionNoArg,  (char *) "False"},
{"-initLUTWrap", ".initLUTWrap",	XrmoptionSepArg, (char *) NULL},
/* The key controls */
{"-zoomin",	".zoomIn",		XrmoptionSepArg, (char *) NULL},
{"-zoomnorm",	".zoomNorm",		XrmoptionSepArg, (char *) NULL},
{"-zoomout",	".zoomOut",		XrmoptionSepArg, (char *) NULL},
{"-zoomxin",	".x.zoomIn",		XrmoptionSepArg, (char *) NULL},
{"-zoomxout",	".x.zoomOut",		XrmoptionSepArg, (char *) NULL},
{"-zoomyin",	".y.zoomIn",		XrmoptionSepArg, (char *) NULL},
{"-zoomyout",	".y.zoomOut",		XrmoptionSepArg, (char *) NULL},
{"-helpkey",	".help",		XrmoptionSepArg, (char *) NULL},
{"-cursor",	".cursor",		XrmoptionSepArg, (char *) NULL},
{"-recenter",	".recenter",		XrmoptionSepArg, (char *) NULL},
{"-showloc",	".showLoc",		XrmoptionSepArg, (char *) NULL},
{"-quit",	".quit",		XrmoptionSepArg, (char *) NULL},
{"-showcmap",	".showCmap",		XrmoptionSepArg, (char *) NULL},
{"-showpatch",	".showPatch",		XrmoptionSepArg, (char *) NULL},
{"-row",	".row",			XrmoptionSepArg, (char *) NULL},
{"-imagePrint",	".imagePs",		XrmoptionSepArg, (char *) NULL},
{"-windowPrint", ".windowPs",		XrmoptionSepArg, (char *) NULL},
{"-invert",	".invertCmap",		XrmoptionSepArg, (char *) NULL},
{"-showsee",	".showSee",		XrmoptionSepArg, (char *) NULL},
{"-column",	".column",		XrmoptionSepArg, (char *) NULL},
{"-decreaseSlit", ".decreaseSlit",	XrmoptionSepArg, (char *) NULL},
{"-increaseSlit", ".increaseSlit",	XrmoptionSepArg, (char *) NULL},
{"-resetSlit",	".resetSlit",		XrmoptionSepArg, (char *) NULL},
{"-inhibit",	".inhibit",		XrmoptionSepArg, (char *) NULL},
{"-histogram",	".histogram",		XrmoptionSepArg, (char *) NULL},
{"-increaseLUTWrap", ".increaseLUTWrap", XrmoptionSepArg, (char *) NULL},
{"-decreaseLUTWrap", ".decreaseLUTWrap", XrmoptionSepArg, (char *) NULL},
{"-resetLUTWrap", ".resetLUTWrap",	XrmoptionSepArg, (char *) NULL},
{"-box",	".box",			XrmoptionSepArg, (char *) NULL},
{"-mouseMode",	".mouseMode",		XrmoptionSepArg, (char *) NULL},
/* the rest aren't yet implemented */
/* one each for patch, line graphics, and seeing windows */
/* only one */
{"-name",	".name",		XrmoptionSepArg, (char *) NULL},
/* one for each window */
{"-borderwidth", "*borderWidth",	XrmoptionSepArg, (char *) NULL},
{"-bw",		"*borderWidth",		XrmoptionSepArg, (char *) NULL},
{"-title",	".bm*title",		XrmoptionSepArg, (char *) NULL},
{"-bordercolor", "*borderColor",	XrmoptionSepArg, (char *) NULL},
{"-bc",		"*borderColor",		XrmoptionSepArg, (char *) NULL},
{"-icon",	"*icon",		XrmoptionSepArg, (char *) NULL},
{"-iconGeometry", ".bm*iconGeometry",	XrmoptionSepArg, (char *) NULL},
{"-iconic",	".bm*iconic",		XrmoptionNoArg,  (char *) "on"},
/* one each for bitmap, line graphics, patch, and color map windows */
/* (the seeing window is fixed size, and the location window could be */
/* resized by the program) */
{"-minGeometry",".bm*minGeometry",	XrmoptionSepArg, (char *) NULL},
{"-maxGeometry",".bm*maxGeometry",	XrmoptionSepArg, (char *) NULL},
/* for line graphics only */
{"-blank",	".lg*blank",		XrmoptionSepArg, (char *) NULL},
/* one each for patch and seeing windows */
{"-background",	"*background",		XrmoptionSepArg, (char *) NULL},
{"-bg",		"*background",		XrmoptionSepArg, (char *) NULL},
{"-foreground",	"*foreground",		XrmoptionSepArg, (char *) NULL},
{"-fg",		"*foreground",		XrmoptionSepArg, (char *) NULL},
};

/* The resource database */
static XrmDatabase resdb=NULL;

/* The command line resource database */
static XrmDatabase comdb=NULL;

/* The name of the program */
static char prog[80];

static char *GetHomeDir();

/* The parseops routine parses command line arguments */

void parsedisp(argc, argv)
int *argc;
char **argv;
{
	/* the name of the display */
	char dispname[256];
	XrmValue value;
	char *strtype[20];
	char *progname;

	void Usage();

	dispname[0] = '\0';

	if ((progname=strrchr(argv[0],'/')) == NULL) strcpy(prog,argv[0]);
	else strcpy(prog,progname+1);
	if (*argc != 1) XrmParseCommand(&comdb, fdops,
		sizeof(fdops)/sizeof(fdops[0]), prog, argc, argv);

	if (*argc != 1) Usage(argv[0]);

	/* get a display value so we can get other databases */
	if (XrmGetResource(comdb, "figdisp.display", "Figdisp.Display", strtype,
		&value) == True)
	{
		(void)strncpy(dispname, value.addr, (int) value.size);
	}

	/* Open the specified display */
	if (!(display=XOpenDisplay(dispname)))
	{
		(void)fprintf(stderr, "%s: Can't open display '%s'\n", argv[0],
			XDisplayName(dispname));
#ifndef lint
		exit(FAIL);
#endif
	}
		
	return;
}

/* The mergeops routine merges the command line options in with all the */
/* resource files */

void mergeops()
{
	XrmDatabase appdb,servdb,homedb;
	char filename[1024];
	char *env;
#ifdef PGDISP
	char *classname = "Pgdisp";
#else
	char *classname = "Figdisp";
#endif

	/* get the application defaults */
	if ((env=getenv("XAPPLRESDIR")) == NULL)
		(void)strcpy(filename, "/usr/lib/X11/app-defaults/");
	else (void)strcpy(filename, env);
	(void)strcat(filename, "/");
	(void)strcat(filename, classname);

	appdb=XrmGetFileDatabase(filename);
	XrmMergeDatabases(appdb, &resdb);

	/* get the server defaults (or .Xdefaults) */
	if (XResourceManagerString(display) != NULL)
		servdb=XrmGetStringDatabase(XResourceManagerString(display));
	else {
#ifdef VMS
		(void)strcat(filename,"SYS$LOGIN:.Xdefaults");
#else
		(void)GetHomeDir(filename);
		(void)strcat(filename,"/.Xdefaults");
#endif
		servdb=XrmGetFileDatabase(filename);
	}
	(void) XrmMergeDatabases(servdb, &resdb);

	/* get the XENVIRONMENT file or (if not defined) .Xdefaults for this */
	/* host */
	if ((env=getenv("XENVIRONMENT")) == NULL)
	{
#ifndef VMS
		int len;
		env=GetHomeDir(filename);
		len=strlen(env);
#ifdef solaris
		sysinfo (SI_HOSTNAME, env+len, 1024 - len);
#else
		(void)gethostname(env+len,1024-len);
#endif
#endif
	}
	if (env != NULL)
	{
		homedb=XrmGetFileDatabase(env);
		(void) XrmMergeDatabases(homedb, &resdb);
	}

	/* merge in the command line */
	(void) XrmMergeDatabases(comdb, &resdb);

	return;
}

/* The extractops routine extracts the options into a form the program can */
/* use. */

void extractops()
{
	char *strtype[20];
	XrmValue value;
	int flags;
	char resource[80];

	(void)sprintf(resource, "%s.bm.geometry", prog);
	if (XrmGetResource(resdb, resource, "*Geometry", strtype, &value)
	    == True)
	{
		flags = XParseGeometry(value.addr, &res.bmgeo.x, &res.bmgeo.y,
			(unsigned int *)&res.bmgeo.w,
			(unsigned int *)&res.bmgeo.h);
		if (!(flags & WidthValue)) res.bmgeo.w=BM_WIDTH;
		if (!(flags & HeightValue)) res.bmgeo.h=BM_HEIGHT;
		if (!(flags & XValue)) res.bmgeo.x= -1;
		if (!(flags & YValue)) res.bmgeo.y= -1;
		if ((flags & XValue) && (flags & XNegative)) res.bmgeo.x +=
			(DisplayWidth(display,screen) - res.bmgeo.w);
		if ((flags & YValue) && (flags & YNegative)) res.bmgeo.y +=
			(DisplayHeight(display,screen) - res.bmgeo.h);
	} else {
		res.bmgeo.w=BM_WIDTH;
		res.bmgeo.h=BM_HEIGHT;
		res.bmgeo.x=res.lggeo.y= -1;
	}

	(void)sprintf(resource, "%s.lg.geometry", prog);
	if (XrmGetResource(resdb, resource, "*Geometry", strtype, &value)
	    == True)
	{
		flags = XParseGeometry(value.addr, &res.lggeo.x, &res.lggeo.y,
			(unsigned int *)&res.lggeo.w,
			(unsigned int *)&res.lggeo.h);
		if (!(flags & WidthValue)) res.lggeo.w=BM_WIDTH;
		if (!(flags & HeightValue)) res.lggeo.h=BM_HEIGHT;
		if (!(flags & XValue)) res.lggeo.x= -1;
		if (!(flags & YValue)) res.lggeo.y= -1;
		if ((flags & XValue) && (flags & XNegative)) res.lggeo.x +=
			(DisplayWidth(display,screen) - res.lggeo.w);
		if ((flags & YValue) && (flags & YNegative)) res.lggeo.y +=
			(DisplayHeight(display,screen) - res.lggeo.h);
	} else {
		res.lggeo.w=LG_WIDTH;
		res.lggeo.h=LG_HEIGHT;
		res.lggeo.x=res.lggeo.y= -1;
	}

	(void)sprintf(resource, "%s.showhelp", prog);
	if (XrmGetResource(resdb, resource, "*Showhelp", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.showhelp=0;
		else res.showhelp=1;
	} else res.showhelp=1;

	/* see if we should use the line graphics crosshair cursor */
	(void)sprintf(resource, "%s.lg.crosshair", prog);
	if (XrmGetResource(resdb, resource, "*Crosshair", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.lgcross=0;
		else res.lgcross=1;
	} else res.lgcross=0;

	/* Get the default dimensions for the patch window */
	(void)sprintf(resource, "%s.patchgeometry", prog);
	if (XrmGetResource(resdb, resource, "*Patchgeometry", strtype, &value)
	    == True)
	{
		flags = XParseGeometry(value.addr, &res.pgeo.x, &res.pgeo.y,
			(unsigned int *)&res.pgeo.w,
			(unsigned int *)&res.pgeo.h);
		if (!(flags & WidthValue)) res.pgeo.w=BM_WIDTH;
		if (!(flags & HeightValue)) res.pgeo.h=BM_HEIGHT;
		if (!(flags & XValue)) res.pgeo.x= -1;
		if (!(flags & YValue)) res.pgeo.y= -1;
		if ((flags & XValue) && (flags & XNegative)) res.pgeo.x +=
			(DisplayWidth(display,screen) - res.pgeo.w);
		if ((flags & YValue) && (flags & YNegative)) res.pgeo.y +=
			(DisplayHeight(display,screen) - res.pgeo.h);
	} else res.pgeo.w=res.pgeo.h=res.pgeo.x=res.pgeo.y= -1;

	/* Get the default dimensions for the color map window */
	(void)sprintf(resource, "%s.cmapgeometry", prog);
	if (XrmGetResource(resdb, resource, "Cmapgeometry", strtype, &value)
	    == True)
	{
		flags = XParseGeometry(value.addr, &res.cgeo.x, &res.cgeo.y,
			(unsigned int *)&res.cgeo.w,
			(unsigned int *)&res.cgeo.h);
		if (!(flags & WidthValue)) res.cgeo.w=CM_WIDTH;
		if (!(flags & HeightValue)) res.cgeo.h=CM_HEIGHT;
		if (!(flags & XValue)) res.cgeo.x= -1;
		if (!(flags & YValue)) res.cgeo.y= -1;
		if ((flags & XValue) && (flags & XNegative)) res.cgeo.x +=
			(DisplayWidth(display,screen) - res.cgeo.w);
		if ((flags & YValue) && (flags & YNegative)) res.cgeo.y +=
			(DisplayHeight(display,screen) - res.cgeo.h);
	} else {
		res.cgeo.x=res.cgeo.y= -1;
		res.cgeo.w=CM_WIDTH;
		res.cgeo.h=CM_HEIGHT;
	}

	/* Get the default dimensions for the location window */
	(void)sprintf(resource, "%s.locationgeometry", prog);
	if (XrmGetResource(resdb, resource, "*Locationgeometry", strtype,
		&value) == True)
	{
		flags = XParseGeometry(value.addr, &res.lgeo.x, &res.lgeo.y,
			(unsigned int *)&res.lgeo.w,
			(unsigned int *)&res.lgeo.h);
		if (!(flags & WidthValue)) res.lgeo.w=LOC_WIDTH;
		if (!(flags & HeightValue)) res.lgeo.h=LOC_HEIGHT;
		if (!(flags & XValue)) res.lgeo.x= -1;
		if (!(flags & YValue)) res.lgeo.y= -1;
		if ((flags & XValue) && (flags & XNegative)) res.lgeo.x +=
			(DisplayWidth(display,screen) - res.lgeo.w);
		if ((flags & YValue) && (flags & YNegative)) res.lgeo.y +=
			(DisplayHeight(display,screen) - res.lgeo.h);
	} else {
		res.lgeo.x=res.lgeo.y= -1;
		res.lgeo.w=LOC_WIDTH;
		res.lgeo.h=LOC_HEIGHT;
	}

	/* Get the area to be used for histogram equalization */
	(void)sprintf(resource, "%s.histgeometry", prog);
	if (XrmGetResource(resdb, resource, "*histgeometry", strtype, &value)
	    == True)
	{
		flags = XParseGeometry(value.addr, &res.histgeo.x,
			&res.histgeo.y, (unsigned int *)&res.histgeo.w,
			(unsigned int *)&res.histgeo.h);
		if (!(flags & WidthValue)) res.histgeo.w=HIST_WIDTH;
		if (!(flags & HeightValue)) res.histgeo.h=HIST_HEIGHT;
	} else {
		res.histgeo.w=HIST_WIDTH;
		res.histgeo.h=HIST_HEIGHT;
	}

	/* now get the maximum colors to use in the default cmap */
	(void)sprintf(resource, "%s.bm.maxcolors", prog);
	if (XrmGetResource(resdb, resource, "*Maxcolors", strtype, &value)
	    == True)
	{
		if ((res.maxcolors=atoi(value.addr)) > BM_COLORS)
		{
			(void)fprintf(stderr,
				"Maxcolors reduced to maximum of %d\n",
				BM_COLORS);
			res.maxcolors=BM_COLORS;
		} else if (res.maxcolors < 0) {
			(void)fprintf(stderr,
				"Maxcolors changed from %d to %d\n",
				res.maxcolors, BM_MAX_SH_COLS),
			res.maxcolors=BM_MAX_SH_COLS;
		}
	} else res.maxcolors=BM_MAX_SH_COLS;

	/* now get the minimum colors to use in the default cmap */
	(void)sprintf(resource, "%s.bm.mincolors", prog);
	if (XrmGetResource(resdb, resource, "*Mincolors", strtype, &value)
	    == True)
	{
		if ((res.mincolors=atoi(value.addr)) > BM_COLORS)
		{
			(void)fprintf(stderr,
				"Mincolors reduced to maximum of %d\n",
				BM_COLORS);
			res.mincolors=BM_COLORS;
		} else if (res.mincolors < 0) {
			(void)fprintf(stderr,
				"Mincolors changed from %d to %d\n",
				res.mincolors, BM_MIN_SH_COLS),
			res.mincolors=BM_MIN_SH_COLS;
		}
	} else res.mincolors=BM_MIN_SH_COLS;

	/* now get the maximum colors to use in a private cmap */
	(void)sprintf(resource, "%s.bm.private.maxcolors", prog);
	if (XrmGetResource(resdb, resource, "*Private*Maxcolors", strtype,
		&value) == True)
	{
		if ((res.maxpcolors=atoi(value.addr)) > BM_COLORS)
		{
			(void)fprintf(stderr,
				"Maxpcolors reduced to maximum of %d\n",
				BM_COLORS);
			res.maxpcolors=BM_COLORS;
		} else if (res.maxpcolors < 0) {
			(void)fprintf(stderr,
				"Maxpcolors changed from %d to %d\n",
				res.maxpcolors, BM_COLORS),
			res.maxpcolors=BM_COLORS;
		}
	} else res.maxpcolors=BM_COLORS;

	/* now get the minimum colors to use in a private cmap */
	(void)sprintf(resource, "%s.bm.private.mincolors", prog);
	if (XrmGetResource(resdb, resource, "*Private*Mincolors", strtype,
		&value) == True)
	{
		if ((res.minpcolors=atoi(value.addr)) > BM_COLORS)
		{
			(void)fprintf(stderr,
				"Minpcolors reduced to maximum of %d\n",
				BM_COLORS);
			res.minpcolors=BM_COLORS;
		} else if (res.minpcolors < 0) {
			(void)fprintf(stderr,
				"Minpcolors changed from %d to %d\n",
				res.minpcolors, BM_MIN_COLORS),
			res.minpcolors=BM_MIN_COLORS;
		}
	} else res.minpcolors=BM_MIN_COLORS;

	/* Now get the number of colors to use for the line graphics window. */
	/* See the routine getvisuals() for information on how this is done. */
	(void)sprintf(resource, "%s.lineColors", prog);
	if (XrmGetResource(resdb, resource, "*LineColors", strtype, &value)
	    == True)
	{
		if ((res.lgcolors=atoi(value.addr)) < 2)
		{
			(void)fprintf(stderr,
				"LineColors increased to minimum of %d\n",
				LG_MIN_COLORS);
			res.lgcolors=LG_MIN_COLORS;
		}
	} else res.lgcolors=LG_COLORS;

	/* Now get the number of times to wrap the LUT initially */
	(void)sprintf(resource, "%s.initLUTWrap", prog);
	if (XrmGetResource(resdb, resource, "*InitLUTWrap", strtype, &value)
	    == True)
	{
		if ((res.initwrap=atoi(value.addr)) < 1)
		{
			(void)fprintf(stderr,
				"InitLUTWRap increased to minimum of 1\n");
			res.initwrap=1;
		}
	} else res.initwrap=INIT_LUT_WRAP;

	/* now get the key for zooming in */
	(void)sprintf(resource, "%s.zoomIn", prog);
	if (XrmGetResource(resdb, resource, "*ZoomIn", strtype, &value) == True)
		res.keys[ZOOMIN]=XStringToKeysym(value.addr);
	else res.keys[ZOOMIN]=KEY_ZOOM_IN;

	/* now get the key for resetting zoom factor */
	(void)sprintf(resource, "%s.zoomNorm", prog);
	if (XrmGetResource(resdb, resource, "*ZoomNorm", strtype, &value)
	    == True) res.keys[ZOOMNORM]=XStringToKeysym(value.addr);
	else res.keys[ZOOMNORM]=KEY_ZOOM_0;

	/* now get the key for zooming out */
	(void)sprintf(resource, "%s.zoomOut", prog);
	if (XrmGetResource(resdb, resource, "*ZoomOut", strtype, &value)
	    == True) res.keys[ZOOMOUT]=XStringToKeysym(value.addr);
	else res.keys[ZOOMOUT]=KEY_ZOOM_OUT;

	/* now get the key for zooming X in */
	(void)sprintf(resource, "%s.x.zoomIn", prog);
	if (XrmGetResource(resdb, resource, "*X*ZoomIn", strtype, &value)
	    == True) res.keys[ZOOMXIN]=XStringToKeysym(value.addr);
	else res.keys[ZOOMXIN]=KEY_ZOOMX_IN;

	/* now get the key for zooming X out */
	(void)sprintf(resource, "%s.x.zoomOut", prog);
	if (XrmGetResource(resdb, resource, "*X*ZoomOut", strtype, &value)
	    == True) res.keys[ZOOMXOUT]=XStringToKeysym(value.addr);
	else res.keys[ZOOMXOUT]=KEY_ZOOMX_OUT;

	/* now get the key for zooming Y in */
	(void)sprintf(resource, "%s.y.zoomIn", prog);
	if (XrmGetResource(resdb, resource, "*Y*ZoomIn", strtype, &value)
	    == True) res.keys[ZOOMYIN]=XStringToKeysym(value.addr);
	else res.keys[ZOOMYIN]=KEY_ZOOMY_IN;

	/* now get the key for zooming Y out */
	(void)sprintf(resource, "%s.y.zoomOut", prog);
	if (XrmGetResource(resdb, resource, "*Y*ZoomOut", strtype, &value)
	    == True) res.keys[ZOOMYOUT]=XStringToKeysym(value.addr);
	else res.keys[ZOOMYOUT]=KEY_ZOOMY_OUT;

	/* now get the key for printhing help */
	(void)sprintf(resource, "%s.help", prog);
	if (XrmGetResource(resdb, resource, "*Help", strtype, &value) == True)
		res.keys[HELP]=XStringToKeysym(value.addr);
	else res.keys[HELP]=KEY_HELP;

	/* now get the key for toggling cursor output */
	(void)sprintf(resource, "%s.cursor", prog);
	if (XrmGetResource(resdb, resource, "*Cursor", strtype, &value) == True)
		res.keys[CURSOR]=XStringToKeysym(value.addr);
	else res.keys[CURSOR]=KEY_CUR_TOG;

	/* now get the key for recentering the image */
	(void)sprintf(resource, "%s.recenter", prog);
	if (XrmGetResource(resdb, resource, "*Recenter", strtype, &value)
	    == True) res.keys[RECENTER]=XStringToKeysym(value.addr);
	else res.keys[RECENTER]=KEY_RECENTER;

	/* now get the key for toggling the location window */
	(void)sprintf(resource, "%s.showLoc", prog);
	if (XrmGetResource(resdb, resource, "*ShowLoc", strtype, &value)
	    == True) res.keys[SHOWLOC]=XStringToKeysym(value.addr);
	else res.keys[SHOWLOC]=KEY_LOC_TOG;

	/* now get the key for quitting */
	(void)sprintf(resource, "%s.quit", prog);
	if (XrmGetResource(resdb, resource, "*Quit", strtype, &value) == True)
		res.keys[QUIT]=XStringToKeysym(value.addr);
	else res.keys[QUIT]=KEY_QUIT;

	/* now get the key for toggline the color map window */
	(void)sprintf(resource, "%s.showCmap", prog);
	if (XrmGetResource(resdb, resource, "*ShowCmap", strtype, &value)
	    == True) res.keys[SHOWCM]=XStringToKeysym(value.addr);
	else res.keys[SHOWCM]=KEY_CMAP;

	/* now get the key for showing the patch window */
	(void)sprintf(resource, "%s.showPatch", prog);
	if (XrmGetResource(resdb, resource, "*ShowPatch", strtype, &value)
	    == True) res.keys[SHOWPAT]=XStringToKeysym(value.addr);
	else res.keys[SHOWPAT]=KEY_PAT_TOG;

	/* now get the key for producing a row plot */
	(void)sprintf(resource, "%s.row", prog);
	if (XrmGetResource(resdb, resource, "*Row", strtype, &value) == True)
		res.keys[ROW]=XStringToKeysym(value.addr);
	else res.keys[ROW]=KEY_ROW;

	/* now get the key for printing the entire image */
	(void)sprintf(resource, "%s.imagePs", prog);
	if (XrmGetResource(resdb, resource, "*ImagePS", strtype, &value)
	    == True) res.keys[IMPS]=XStringToKeysym(value.addr);
	else res.keys[IMPS]=KEY_IMPS;

	/* now get the key for printing the window */
	(void)sprintf(resource, "%s.windowPs", prog);
	if (XrmGetResource(resdb, resource, "*WindowPS", strtype, &value)
	    == True) res.keys[WINPS]=XStringToKeysym(value.addr);
	else res.keys[WINPS]=KEY_WINPS;

	/* now get the key for inverting the color map */
	(void)sprintf(resource, "%s.invertCmap", prog);
	if (XrmGetResource(resdb, resource, "*InvertCmap", strtype, &value)
	    == True) res.keys[INVERT]=XStringToKeysym(value.addr);
	else res.keys[INVERT]=KEY_INVERT;

	/* now get the key for calculating the seeing */
	(void)sprintf(resource, "%s.showSee", prog);
	if (XrmGetResource(resdb, resource, "*ShowSee", strtype, &value)
	    == True) res.keys[SEEING]=XStringToKeysym(value.addr);
	else res.keys[SEEING]=KEY_SEE_TOG;

	/* now get the key for producing a column plot */
	(void)sprintf(resource, "%s.column", prog);
	if (XrmGetResource(resdb, resource, "*Column", strtype, &value) == True)
		res.keys[COL]=XStringToKeysym(value.addr);
	else res.keys[COL]=KEY_COL;

	/* now get the key for decreasing the slit width */
	(void)sprintf(resource, "%s.decreaseSlit", prog);
	if (XrmGetResource(resdb, resource, "*DecreaseSlit", strtype, &value)
	    == True) res.keys[DECSLIT]=
			XStringToKeysym(value.addr);
	else res.keys[DECSLIT]=KEY_SLDEC;

	/* now get the key for increasing the slit width */
	(void)sprintf(resource, "%s.increaseSlit", prog);
	if (XrmGetResource(resdb, resource, "*IncreaseSlit", strtype, &value)
	    == True) res.keys[INCSLIT]=XStringToKeysym(value.addr);
	else res.keys[INCSLIT]=KEY_SLINC;

	/* now get the key for resetting the slit width */
	(void)sprintf(resource, "%s.resetSlit", prog);
	if (XrmGetResource(resdb, resource, "*ResetSlit", strtype, &value)
	    == True) res.keys[RESSLIT]=XStringToKeysym(value.addr);
	else res.keys[RESSLIT]=KEY_SLRES;

	/* get the key for inhibiting other key interpretations */
	(void)sprintf(resource, "%s.inhibit", prog);
	if (XrmGetResource(resdb, resource, "*Inhibit", strtype, &value)
	    == True) res.keys[INHIBIT]=XStringToKeysym(value.addr);
	else res.keys[INHIBIT]=KEY_INHIBIT;

	/* now get the key to toggle use of histogram equalization */
	(void)sprintf(resource, "%s.histogram", prog);
	if (XrmGetResource(resdb, resource, "*Histogram", strtype, &value)
		== True) res.keys[HISTOGRAM]=XStringToKeysym(value.addr);
	else res.keys[HISTOGRAM]=KEY_HIST;

	/* now the key for increasing the LUT wrap */
	(void)sprintf(resource, "%s.increaseLUTWrap", prog);
	if (XrmGetResource(resdb, resource, "*.increaseLUTWrap",strtype,&value)
		== True) res.keys[INCLUTWRAP]=XStringToKeysym(value.addr);
	else res.keys[INCLUTWRAP]=KEY_LWRAP_INC;

	/* now the key for decreasing the LUT wrap */
	(void)sprintf(resource, "%s.decreaseLUTWrap", prog);
	if (XrmGetResource(resdb, resource, "*.decreaseLUTWrap",strtype,&value)
		== True) res.keys[DECLUTWRAP]=XStringToKeysym(value.addr);
	else res.keys[DECLUTWRAP]=KEY_LWRAP_DEC;

	/* now the key for resetting the LUT wrap */
	(void)sprintf(resource, "%s.resetLUTWrap", prog);
	if (XrmGetResource(resdb, resource, "*.ResetLUTWrap",strtype,&value)
		== True) res.keys[RESLUTWRAP]=XStringToKeysym(value.addr);
	else res.keys[RESLUTWRAP]=KEY_LWRAP_RES;

	/* now get the key to toggle the mouse mode */
	(void)sprintf(resource, "%s.mouseMode", prog);
	if (XrmGetResource(resdb, resource, "*MouseMode", strtype, &value)
		== True) res.keys[MOUSEMODE]=XStringToKeysym(value.addr);
	else res.keys[MOUSEMODE]=KEY_MOUSEMODE;

	/* now get the key to get image stats within box */
	(void)sprintf(resource, "%s.box", prog);
	if (XrmGetResource(resdb, resource, "*Box", strtype, &value)
		== True) res.keys[DOBOX]=XStringToKeysym(value.addr);
	else res.keys[DOBOX]=KEY_DOBOX;

	/* Get the font to use */
	res.textfont=NULL;
	(void)sprintf(resource, "%s.font", prog);
	if (XrmGetResource(resdb, resource, "*Font", strtype, &value) == True)
	{
		if ((res.textfont=XLoadQueryFont(display, value.addr)) == NULL)
			(void)fprintf(stderr,
				"Font %s not found, using default of %s\n",
				value.addr, DEFAULT_FONT);
	}
	if (res.textfont == NULL)
		res.textfont=XLoadQueryFont(display, DEFAULT_FONT);
	
	/* Get the PostScript output file (if any) */
	res.psfile=NULL;
	(void)sprintf(resource, "%s.psFile", prog);
	if (XrmGetResource(resdb, resource, "*PsFile", strtype, &value) == True)
	{
		if ((res.psfile=malloc(value.size)) == NULL)
		{
			(void)fprintf(stderr,
				"Could not get memory for output file name.\n");
			(void)fprintf(stderr,
				"Output will be sent to the printer.\n");
		} else if (!strncmp(value.addr,"direct", sizeof("direct"))) {
			/* we want output to be direct to the printer */
			free(res.psfile);
			res.psfile=NULL;
		} else (void)strncpy(res.psfile, value.addr, (int)value.size);
	}

	/* get the printer to use for output */
	res.printer=NULL;
	(void)sprintf(resource, "%s.printer", prog);
	if (XrmGetResource(resdb, resource, "*Printer", strtype, &value)
	    == True)
	{
		if ((res.printer=malloc(value.size)) == NULL)
		{
			(void)fprintf(stderr,
				"Could not get memory for printer name.\n");
		} else if (!strncmp(value.addr,"PRINTER", sizeof("PRINTER"))) {
			/* go to the default (PRINTER environment var) */
			free(res.printer);
			res.printer=NULL;
		} else (void)strncpy(res.printer, value.addr, (int)value.size);
	}

	/* Get the id for this copy of the display */
	(void)sprintf(resource, "%s.id", prog);
	if (XrmGetResource(resdb, resource, "*Id", strtype, &value) == True)
		res.id=atoi(value.addr);
	else res.id=0;

	/* Get the visual class to use for line graphics */
	(void)sprintf(resource, "%s.visual", prog);
	res.visclass=AnyVis;
	if (XrmGetResource(resdb, resource, "*Visual", strtype, &value) == True)
	{
		if (!strncmp(value.addr, "GrayScale", sizeof("GrayScale")))
			res.visclass=GrayScale;
		else if (!strncmp(value.addr, "PseudoColor",
			sizeof("PseudoColor"))) res.visclass=PseudoColor;
#ifdef PGDISP
		else if (!strncmp(value.addr, "DirectColor",
			sizeof("DirectColor"))) res.visclass=DirectColor;
		else if (!strncmp(value.addr, "StaticGray",
			sizeof("StaticGray"))) res.visclass=StaticGray;
		else if (!strncmp(value.addr, "StaticColor",
			sizeof("StaticColor"))) res.visclass=StaticColor;
		else if (!strncmp(value.addr, "TrueColor",
			sizeof("TrueColor"))) res.visclass=TrueColor;
#endif
		else if (!strncmp(value.addr, "Any", sizeof("Any")))
			res.visclass=AnyVis;
		else if (!strncmp(value.addr, "Default", sizeof("Default")))
			res.visclass=DefaultVis;
		else {
			(void)fprintf(stderr, "Invalid visual %s specified.\n",
				value.addr);
			(void)fprintf(stderr,"Using default of Any\n");
		}
	}

	/* now get the number of microseconds to sleep between checks for */
	/* the existance of a client program.  See waitevent.c for more */
	/* details. */
	(void)sprintf(resource, "%s.sleepTime", prog);
	if (XrmGetResource(resdb, resource, "*SleepTime", strtype, &value)
	    == True)
	{
		if ((res.sleeptime=atoi(value.addr)) < MIN_USLEEP_TIME)
		{
			res.sleeptime=MIN_USLEEP_TIME;
			(void)fprintf(stderr,
				"SleepTime increased to minimumm of %d\n",
				MIN_USLEEP_TIME);
		}
	} else res.sleeptime=USLEEP_TIME;

	/* see if we should force the location window pixels to be square. */
	res.forcesquare=0;
	(void)sprintf(resource, "%s.forceSquare", prog);
	if (XrmGetResource(resdb, resource, "*ForceSquare", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "True", (int)value.size) == 0)
			res.forcesquare=1;
	}

	/* now get the number of colors to copy from the default colormap to */
	/* a private color map.  This is so things like title bars will be */
	/* the same between both color maps. */
	(void)sprintf(resource, "%s.saveColors", prog);
	if (XrmGetResource(resdb, resource,"*SaveColors", strtype, &value)
	    == True)
	{
		if ((res.savecolors=atoi(value.addr)) < 0) res.savecolors=0;
	} else res.savecolors=SAVE_COLORS;

	/* now get the number of colors to leave available in the default */
	/* colormap.  This is so applications started after figdisp will */
	/* be able to use the default colormap. */
	(void)sprintf(resource, "%s.leaveColors", prog);
	if (XrmGetResource(resdb, resource, "*LeaveColors", strtype, &value)
	    == True)
	{
		if ((res.leavecolors=atoi(value.addr)) < 0) res.leavecolors=0;
	} else res.leavecolors=LEAVE_COLORS;

	/* does the user want the line plots to always go from left to right? */
	res.lefttoright=0;
	(void)sprintf(resource, "%s.line.leftToRight", prog);
	if (XrmGetResource(resdb, resource, "*LeftToRight", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "True", (int)value.size) == 0)
			res.lefttoright=1;
	}

	/* Does the user want the line plots to always increase with the */
	/* appropriate coordinate? */
	res.ascendcoord=0;
	(void)sprintf(resource, "%s.line.ascending", prog);
	if (XrmGetResource(resdb, resource, "*Ascending", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "True", (int)value.size) == 0)
			res.ascendcoord=1;
	}


	/* Does the use want line plots to be in histogram form? */
	res.plothist=0;
	(void)sprintf(resource, "%s.line.histogram", prog);
	if (XrmGetResource(resdb, resource, "*Line*Histogram", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "True", (int)value.size) == 0)
			res.plothist=1;
	}

	/* Does the user want column plots to always go bottom to top? */
	res.bottotop=1;
	(void)sprintf(resource, "%s.col.bottomToTop", prog);
	if (XrmGetResource(resdb, resource, "*Col*BottomToTop", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.bottotop=0;
	}

	/* Does the user want column plots to always follow ascending y? */
	res.ascendy=1;
	(void)sprintf(resource, "%s.col.ascending", prog);
	if (XrmGetResource(resdb, resource, "*Ascending", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.ascendy=0;
	}

	/* Does the user want row plots to always go left to right? */
	res.rowltor=1;
	(void)sprintf(resource, "%s.row.leftToRight", prog);
	if (XrmGetResource(resdb, resource, "*LeftToRight", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.rowltor=0;
	}

	/* Does the user want row plots to always follow ascending x? */
	res.ascendx=1;
	(void)sprintf(resource, "%s.row.ascending", prog);
	if (XrmGetResource(resdb, resource, "*Ascending", strtype, &value)
	    == True)
	{
		if (strncmp(value.addr, "False", (int)value.size) == 0)
			res.ascendx=0;
	}

	return;
}

static char *usemsg[]={
"[-display DISP] [-geometry WxH[+x+y]] [-bmGeometry WxH[+x+y]]\n",
"\t[-lgGeometry WxH[+x+y]] [-patchGeometry WxH[+x+y]]\n",
"\t[-cmapGeometry WxH[+x+y]] [-locationGeometry WxH[+x+y]] [-colors #]\n",
"\t[-maxColors #] [-minColors #] [-privateColors #] [-maxPrivateColors #]\n",
"\t[-minPrivateColors #] [-help] [-nohelp] [-zoomin key] [-zoomnorm key]\n",
"\t[-zoomout key] [-helpkey key] [-cursor key] [-recenter key]\n",
"\t[-showloc key] [-quit key] [-showcmap key] [-showpatch key] [-row key]\n",
"\t[-imagePrint key] [-windowPrint key] [-invert key] [-showsee key]\n",
"\t[-column key] [-decreaseSlit key] [-increaseSlit key] [-resetSlit key]\n",
"\t[-zoomxin key] [-zoomxout key] [-zoomyin key] [-zoomyout key]\n",
"\t[-inhibit key] [-font font] [-fn font] [-P printer] [-printer printer]\n",
"\t[-psFile output.ps] [-sleepTime usecs] [-forceSquare] [-noforceSquare]\n",
"\t[-saveColors #] [-leftToRight] [-noleftToRight] [-ascendingX]\n",
"\t[-noascendingX] [-rowLeftToRight] [-norowLeftToRight] [-ascendingY]\n",
"\t[-noascendingY] [-bottomToTop] [-nobottomToTop] [-leaveColors #]\n",
"\t[-ascendingCoord] [-noascendingCoord] [-id #] [-lineColors #]\n",
"\t[-visual Vis] [-lgCrosshair] [-nolgCrosshair] [-histogram key]\n",
"\t[-histogramGeometry WxH] [-plothist] [-noplothist] [-initLUTWrap #]\n",
"\t[-increaseLUTWrap key] [-decreaseLUTWrap key] [-box key] [-mouseMode key]\n",
"",
};
void Usage(prog)
char *prog;
{
	int i=0;

	(void)printf("Usage:\n%s ",prog);
	while(*usemsg[i] != '\0')
		(void)printf("%s",usemsg[i++]);

	return;
}

#ifndef VMS
/* striaght from the X manual */

static char *GetHomeDir(dest)
char *dest;
{
	int uid;

	extern char *getenv();
	extern int getuid();
	extern struct passwd *getpwuid();
	struct passwd *pw;
	register char *ptr;

	if ((ptr = getenv("HOME")) != NULL) {
		(void)strcpy(dest, ptr);
	} else {
		if ((ptr = getenv("USER")) != NULL) {
			pw = getpwnam(ptr);
		} else {
			uid = getuid();
			pw = getpwuid(uid);
		}
		if (pw) {
			(void)strcpy(dest, pw->pw_dir);
		} else {
			*dest = ' ';
		}
	}
	return dest;
}
#endif
