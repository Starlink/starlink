/* This file contains the definitions (and formats) for the command types, */
/* as well as any other constants needed for programs which access the */
/* Figaro/PGPLOT X Window display server.  It should be included by any */
/* program which accesses the Figaro/PGPLOT X Window display server.  All */
/* arguments are short (16-bit) except for the pixel values for the bitmap */
/* write command.  If an odd number of pixels is written in the bitmap write */
/* command, the next command must be aligned on the next half-word (16-bit) */
/* boundary.  Note that all points are given from the top left corner of the */
/* PIXMAP, not the window.  This is so the point specification never changes */
/* no matter what is done to the window size.  Only one command which returns */
/* a value to the requesting program (TOK_LG_MAX_DIM, for example) is allowed */
/* in a single transfer.  If more than one are specified the final request is */
/* honored.  The command does not need to be the last command in a sequence. */
/* A call to TOK_SET_LG_SIZE must not precede TOK_LG_DEF_SIZE in a command */
/* buffer.  If it does, the old size of the window MAY (not will) be */
/* returned. */

/* Sam Southard, Jr. */
/* Created: 2-Nov-1990 */
/*  8-Nov-1990	SNS/CIT	NAME_PROG, NAME_INCRATOM, NAME_DATAATOM, and */
/*			NAME_SELATOM added. */
/* 14-Nov-1990	SNS/CIT	TOK_DRAW_LINE now takes four arguments. */
/*			TOK_DRAW_POLY no longer uses the curent cursor */
/*			location as a starting point.  TOK_LG_MAX_DIM through */
/*			TOK_FILL_RECT added. */
/* 15-Nov-1990	SNS/CIT	All commands except the bitmap-write (not added yet) */
/*			modified to accept all short arguments. */
/* 19-Nov-1990	SNS/CIT	TOK_SET_LG_CURS and TOK_LG_GET_CURS combined into */
/*			TOK_LG_CURS. */
/* 20-Nov-1990	SNS/CIT	TOK_LG_LINE_WID added. */
/* 12-Dec-1990	SNS/CIT	TOK_SHOW_BM_WIN through TOK_SET_BM_SIZE added. */
/* 17-Dec-1990	SNS/CIT	TOK_SET_BM_LUT description changed for better PGPLOT */
/*			support.  TOK_SET_BM_LUT and TOK_SET_LG_LUT now */
/*			function differently, which should be changed in the */
/*			future. */
/* 18-Dec-1990	SNS/CIT	TOK_BM_CURS split into TOK_BM_SET_CURS and */
/*			TOK_BM_GET_CURS for easier TVPCKG interface. */
/* 22-Apr-1991	SNS/CIT	TOK_SET_BM_CSCALE and TOK_SET_BM_DSCALE added. */
/* 29-Apr-1991	SNS/CIT	TOK_BM_ZOOMPAN added. */
/* 17-Jun-1991	SNS/CIT	TOK_BM_FLUSH added. */
/*  1-Aug-1991	SNS/CIT	TOM_BM_LINE added. */
/* 14-Aug-1991	SNS/CIT	Commands no longer begin with TOK_ */
/* 20-Aug-1991	SNS/CIT	COMBUFLEN no longer needed. */
/* 17-Oct-1991	SNS/CIT	SET_BM_SIZE and BM_WRITE now take the number of bits */
/*			in a pixel */
/* 26-Feb-1992	SNS/CIT	LG_PIXLINE and SET_LG_CSCALE added */
/* 25-Jun-1992	SNS/CIT	SET_BM_LUT now takes the number of bits in a pixel. */
/*  9-Jul-1992	SNS/CIT	Argument definitions for SET_BM_SIZE and SET_LG_SIZE */
/*			reconciled.  They now both take the new size, while */
/*			SET_LG_LUT had been taking the maximum coordinates */
/*			and the comments in this file claimed that both of */
/*			them took the maximum coordinates. */
/* 22-Sep-1992	SNS/CIT	special value definition added to BM_ZOOMPAN comment. */
/*			Added definitions of SET_BM_SH_SIZE and BM_SH_UPDATE. */
/* 23-Sep-1992	SNS/CIT	SET_BM_DSCALE changed to use ASCII strings. */
/* 27-Sep-1992	SNS/CIT	SET_LG_CSCALE changed to use ASCII strings. */
/*  5-Oct-1992	ARC/HI	Added DO_BOX and DO_AUTOSCALE commands. */
/* 14-Oct-1992	SNS/CIT	RCS id string now added if INC_HEADER_RCS #define'd. */
/*			Now protected from double inclusion */
/*  4-Nov-1992	SNS/CIT	AUTOSCALE now takes ASCII-encoded floats which range */
/*			from 0.0 to 100.0 */

#ifndef INC_COMMANDS_H
#define INC_COMMANDS_H

#ifndef lint
#ifdef INC_HEADER_RCS
static char commands_h_rcsid[]="@(#)$Id$";
#endif
#endif

/* some constants needed for programs accessing the display server */
#define NAME_PROG	"figdisp" /* The name of the program.  It is used to */
				/* ensure that only one copy of the program */
				/* is running on each screen. */
#define NAME_INCRATOM	"figdispincr" /* The name of the incremental atom */
#define NAME_DATAATOM	"figdispdata" /* The name of the data atom */
#define NAME_SELATOM	"figdispsel" /* The name of the selection atom */

/* The command tokens */
#define RESET		0	/* reset the server.  Does not clear either  */
				/* the line or bitmap graphics screens.  Sets */
				/* the line graphics line characteristics and */
				/* the line graphics LUTs to their default */
				/* states. */
#define SHOW_LG_WIN	1	/* takes one argument: 0 means hide the line */
				/* graphics window, anything else means show */
				/* the line graphics window */
#define SET_LG_LUT	2	/* set the line graphics look up tables.  The */
				/* first argument is the starting LUT entry, */
				/* the next argument is the number of entries */
				/* to change, and the remaining arguments are */
				/* red, green, and blue (in that order) */
				/* values for the LUT entries */
#define LG_CURS		3	/* set and return the line graphics cursor */
				/* location.  The first argument is the */
				/* number of pixels from the left side of the */
				/* image.  The second is the number of pixels */
				/* from the top of the screen.  Once a key */
				/* has been pressed or a mouse button */
				/* clicked, three short elements are */
				/* returned.  The first two are the position */
				/* of the cursor.  The third element is the */
				/* key or button that was pressed.  The high */
				/* byte is 0 if a key was pressed and non- */
				/* zero if a button was clicked.  If a button */
				/* was pressed the second byte is the number */
				/* of the button which was pressed, from left */
				/* to right with 0 being the left-most */
				/* button.  If a key was pressed the second */
				/* byte is the ASCII code of the */
				/* corresponding character. */
#define SET_LG_COL	4	/* set the color index for the line graphics */
				/* The only argument is the color index to */
				/* use.  Only the lower 4 bits of the */
				/* argument are relevant. */
#define DRAW_LINE	5	/* draw a line from the position specified */
				/* by the x and y coordinates of the first */
				/* points and the x and y coordinates of the */
				/* second point.  */
#define DRAW_POLY	6	/* draw a poly-line.  The first argument is */
				/* the number of points.  The remaining */
				/* arguments are X,Y pairs in the format of */
				/* the DRAW_LINE command. */
#define CLR_LG_WIN	7	/* clear the line graphics window */
#define LG_MAX_DIM	8	/* Get the line graphics window maximum */
				/* This returns all information required for */
				/* OPCODE 2 in PGPLOT.  The first four */
				/* elements the minimum and maximum x values */
				/* and the mimumum and maximum y values.  The */
				/* next two elements are the minimum and */
				/* maximum allowed color indices */
#define LG_SCALE	9	/* Returns information required for OPCODE 3 */
				/* in PGPLOT.  The elements are the width and */
				/* height of the screen in millimeters and */
				/* The width and height of the screen in */
				/* pixels.  */
#define LG_DEF_SIZE	10	/* Returns the default display size, which is */
				/* the current window size.  The elements of */
				/* this command are the current minimum and */
				/* maximum x values and the current minimum */
				/* and maximum y values for the window. */
#define SET_LG_SIZE	11	/* Sets the size of the line graphics window. */
				/* The arguments to this command are the new */
				/* x and y sizes.  If they exceed the */
				/* compile-time maximums or are less than the */
				/* compile-time minimums the compile time */
				/* values are used instead. */
#define DRAW_DOT	12	/* draw a dot at the specified X,Y location. */
#define FILL_POLY	13	/* Draw a filled polygon.  The first argument */
				/* is the number of points in the polygon. */
				/* The remaining arguments are the X,Y */
				/* points. */
#define FILL_RECT	14	/* Draw a filled retangle.  The first two */
				/* arguments are the X,Y coordinates of the */
				/* lower left point and the next two points */
				/* are the X,Y coordinates of the upper right */
				/* point. */
#define LG_LINE_WID	15	/* set the line width.  The argument is the */
				/* number of pixels the line width should be */
#define SHOW_BM_WIN	16	/* takes one argument: 0 means hide the */
				/* bitmap graphics window, anything else */
				/* means show the bitmap graphics window */
#define SET_BM_LUT	17	/* set the bitmap graphics look up tables.  */
				/* The first argument is the number of bits */
				/* used to number the LUT entries (e.g. 8 if */
				/* there are 256 LUT entries and 16 if there */
				/* are 65536 LUT entries.  The next */
				/* arguments are the starting LUT entry, the */
				/* number of entries to change, and the */
				/* fourth argument is a bitfield representing */
				/* the LUTs to change.  If the fourth */
				/* argument is 0 than the all the LUTs are */
				/* affected and the remaining arguments are */
				/* red, green, and blue (in that order) */
				/* values for the LUT entries.  If the fourth */
				/* argument is 1 than only the red values are */
				/* affected, if it is 2 than the green values */
				/* are affected, and if it is 4 than the blue */
				/* values are affected.  If it is a */
				/* combination of 1, 2, and 4 than each of */
				/* values is used for all appropriate LUTs. */
				/* For example, if the sequence was 3 */
				/* followed by 15 than both the red and green */
				/* LUT values would be set to 15.  Note that */
				/* this is very different from the */
				/* interpretation of SET_LG_LUT. */
#define BM_SET_CURS	18	/* Set the current cursor location.  This has */
				/* no visible effect until BM_GET_CURS */
				/* is sent, when the window is raied and the */
				/* pointer is warped to the appropriate */
				/* place.  Takes two arguments, the X and Y */
				/* coordinates of the cursor. */
#define BM_GET_CURS	19	/* return the bitmap graphics cursor */
				/* location.  The return values are the same */
				/* as LG_CURS. */
#define CLR_BM_WIN	20	/* clear the bitmap graphics window */
#define BM_MAX_DIM	21	/* Get the bitmap graphics window maximum */
				/* The first four elements the minimum and */
				/* maximum x values and the mimumum and */
				/* maximum y values.  The next two elements */
				/* are the minimum and maximum allowed color */
				/* indices */
#define BM_DEF_SIZE	22	/* Returns the default display size, which is */
				/* the current image size.  The elements of */
				/* this command are the current minimum and */
				/* maximum x values and the current minimum */
				/* and maximum y values for the image. */
#define SET_BM_SIZE	23	/* Sets the size of the bitmap graphics */
				/* window.  The arguments to this command are */
				/* the new x and y sizes and the new number */
				/* of bits in a pixel (8 or 16).  If they */
				/* exceed the compile-time maximums or are */
				/* less than the compile-time minimums the */
				/* compile time values are used instead. */
#define BM_WRITE	24	/* write a bitmap to the bitmap image.  This */
				/* command takes a varying number of */
				/* arguments.  The first argument is the */
				/* number of bits in a pixel (8 or 16).  The */
				/* next two arguments are the X and Y */
				/* coordinates of the upper left corner of */
				/* the area to be affected.  The next two */
				/* arguments are the width and height of the */
				/* affected area.  The remaining arguments */
				/* are the data values.  If there are 8 bits */
				/* per pixels the data values are all chars. */
				/* If the data for this command is split up */
				/* between more than one X transfers, than */
				/* all but the last transfer must have an */
				/* even number of bytes.  The last transfer */
				/* may have an odd number if necessary.  Note */
				/* this this is automatically taken care of */
				/* if there are 16 bits per pixel. */
#define SET_BM_CSCALE 25	/* Set the X and Y scaling and offset values */
				/* so that the built-in cursor can be used. */
				/* The arguments to this command are the x */
				/* multiplier, the x divisor, the x offset, */
				/* the y multiplier, the y divisor, and the */
				/* y offset.  See the documentation for */
				/* further details */
				/* scaling factors and adding the offsets. */
#define SET_BM_DSCALE 26	/* Set the data scaling and offset values */
				/* so that the built-in cursor can be used. */
				/* This command takes two ASCII strings as */
				/* arguments, both NULL terminated and in the */
				/* %g format (from printf).  They are the */
				/* scaling factor and the offset to apply to */
				/* the data.  See the documentation for */
				/* further details. */
#define BM_ZOOMPAN	27	/* Set the zoom factor.  The first two */
				/* arguments are the X and Y coordinates for */
				/* the new center of the displayed portion, */
				/* and the next two arguments are the power */
				/* of two for X & Y to zoom to.  Setting the */
				/* X or Y coordinate (or both) to -1 leaves */
				/* the center of the displayed image as is. */
				/* Setting either of the the zoom factors to */
				/* a value greater than 30 leaves them as is. */
#define BM_FLUSH	28	/* flush all commands to the screen */
#define BM_LINE		29	/* Draw a line in the bitmap graphics area. */
				/* The arguments to this command are the */
				/* number of bits per pixel used for the */
				/* data value, X & Y coordinates of the two */
				/* points (x1, y1, x2, y2) followed by the */
				/* data value (0-255 or 0-65535) to use to */
				/* draw the line. */
#define LG_PIXLINE	30	/* Draw a line of pixels into the line */
				/* graphics windows (PGPLOT opcode 26).  The */
				/* arguments to this command are the number */
				/* of pixels, the starting X and Y */
				/* coordinates, and the list of pixels */
#define SET_LG_CSCALE	31	/* Set the line graphics cursor scale so that */
				/* the built-in cursor can be used.  The four */
				/* arguments to this command are the NULL- */
				/* terminated ASCII strings in the %g format */
				/* (see printf).  They are the X offset and */
				/* scaling factor followed by the Y values. */
				/* See the documentation for further details. */
#define SET_BM_SH_SIZE	32	/* Sets the size of the bitmap graphics */
				/* window and uses data from a shared memory */
				/* buffer.  The arguments to this command are */
				/* the new x and y sizes, the new number of */
				/* bits in a pixel (8 or 16), the type of */
				/* shared memory to use, and a variable */
				/* number of other parameters, depending on */
				/* the type of shared memory being used.  The */
				/* shared memory type is very system */
				/* dependant.  So far the following types, */
				/* with the mentioned arguments, are */
				/* defined: */
				/* Type 1: SunOS 4.1.2 shmget-type. */
				/* This type has an extra two argument, the */
				/* high 16-bit word of the shared memory */
				/* identifier suitable for use with the shmat */
				/* call and the low 16-bit word.  This */
				/* command also returns a two parameter */
				/* buffer, the first word of which is */
				/* SET_BM_SH_SIZE and the second of which is */
				/* 1 if the shared memory mapping was */
				/* successful and 0 if it was not.  If this */
				/* command is not successful, the SET_BM_SIZE */
				/* command should be used.  See the */
				/* programmer's manual for more information */
				/* on this command. */
#define BM_SH_UPDATE	33	/* This command tells figdisp to update */
				/* internal structures (such as windows) */
				/* based on the changed contents of the */
				/* shared memory area from SET_BM_SH_SIZE. */
				/* The first two arguments are the X and Y */
				/* coordinates of the upper left corner of */
				/* the area to be affected.  The next two */
				/* arguments are the width and height of the */
				/* affected area.  This command is a shared */
				/* memory version of the BM_WRITE command and */
				/* only works after a SET_BM_SH_SIZE command */
				/* has completed successfully since the last */
				/* time the the SET_BM_SIZE command was used */
				/* (it is ignored otherwise).  The BM_WRITE */
				/* command may also be used with shared */
				/* memory images, but it is not as fast. */
#define DO_BOX		34	/* Compute image statistics within the user */
				/* defined rectangle.  This command has no */
				/* arguments. */
#define DO_AUTODISP	35	/* Linear ramp between percantile range.  The */
				/* arguments to this command are the two */
				/* ASCII strings, the lower and upper bounds */
				/* percentiles (between 0.0 and 100.0) to be */
				/* used to scale data. */
#define DO_HISTEQ	36	/* Histogram Equalization */
#define FIGDISP_IDLE	37	/* This command returns itself when figdisp */
				/* is idle.  It is useful for synchronizing */
				/* commands, such as CLR_BM_WIN and BM_LINE, */
				/* which cause the figdisp program to alter */
				/* the image data.  If this is not used when */
				/* shared memory is in use, the image could */
				/* get corrupted. */
#define	FIGDISP_POINTS	38	/* This command takes the number of points, */
				/* then the number of bits per pixel, then */
				/* numpoints triplets of x, y, and value. */
#endif /* INC_COMMANDS_H */
