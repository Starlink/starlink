/* This file contains the definitions of the error/status messages generated */
/* by the Figaro display server */

/* Sam Southard, Jr. */
/* Created: 1-Nov-1990 */
/*  2-Nov-1990	SNS/CIT	MSG_NODATAOWN, MSG_BADGETPROP, and MSG_BADDATATYPE */
/*			added. */
/*  7-Nov-1990	SNS/CIT	MSG_BADSELOWN and MSG_NOVISUAL added. */
/*  8-Nov-1990	SNS/CIT	MSG_BADDISPOPEN added. */
/* 20-Nov-1990	SNS/CIT	MSG_REPLYNOTSENT added. */
/* 10-Dec-1990	SNS/CIT	MSG_NOLOCK added. */
/* 11-Dec-1990	SNS/CIT	MSG_NOBM_VISUAL added.  MSG_NOCOLORS generalized. */
/*  3-Apr-1991	SNS/CIT	MSG_OLD_LOST, MSG_CHOP_X, and MSG_CHOP_Y added. */
/* 20-Aug-1991	SNS/CIT	MSG_LOADFONT added. */
/* 23-Aug-1991	SNS/CIT	MSG_LWOPENERR added. */
/*  3-Mar-1992	SNS/CIT	MSG_NOCOLORS updated to include visual possibility. */
/*  9-Mar-1992	SNS/CIT	MSG_TRYRESIZE & MSG_BADRESIZE added. */
/* 24-Jun-1992	SNS/CIT	MSG_SMALLHIST added. */
/* 26-Jun-1992	SNS/CIT	MSG_LGWINTOOSMALL added. */

/* If the screen already contains a copy of this server */
#define MSG_ALREADYRUNNING "There is aready a display on your screen!\n"

/* If we could not obtain a locking atom */
#define MSG_BADLOCKATOM "Could not create locking atom!\n"

/* If we could not own the locking atom (locking will not work) */
#define MSG_NOLOCK "Could not own locking atom!  Locking will not work!\n"

/* If we could not obtain an atom for the incremental type */
#define MSG_BADINCRATOM "Could not create incremental type atom!\n"

/* If we could not obtain a data atom */
#define MSG_BADDATAATOM "Could not create data atom!\n"

/* If we could not obtain a selection atom */
#define MSG_BADSELATOM "Could not create selection atom!\n"

/* If the display does not support the requested pixmap depth */
#define MSG_BADPIXMAPDEPTH "Your server does not support the required depth!\n"

/* If the selection atom seems to have no owner */
#define MSG_NOSELOWN "The selection atom has no owner!\n"

/* If there was an error getting data from a property */
#define MSG_BADGETPROP "Could not get the data from the data atom!\n"

/* If we received a bad data type */
#define MSG_BADDATATYPE "Display server received an unsupported data type!\n"

/* If we can't own the selection atom */
#define MSG_BADSELOWN "Could not get ownership of selection atom!\n"

/* If we can't allocate colors in the color map */
#define MSG_NOCOLORS \
	"Could not allocate colormap entries in the requested visual!\n"

/* If we can't get memory */
#define MSG_MALLOC "Could not allocate memory!\n"

/* If we can't open the display */
#define MSG_BADDISPOPEN "Could not open the standard display!\n"

/* If we can't send a reply to the client (because he went away) */
#define MSG_REPLYNOTSENT "Could not return a reply!\n"

/* If the X11 display server does not support a bitmap visual */
#define MSG_NOBMVISUAL \
	"The X Display server does not support an appropriate visual!\n"

/* If we must delete the old image before copying whatever fits to the new */
#define MSG_OLD_LOST \
	"Memory requirements forced loss of image data."

/* If we must chop the X dimension to fit into memory. */
#define MSG_CHOP_X \
	"Memory requirements forced a smaller image width."

/* If we must chop the Y dimension to fit into memory. */
#define MSG_CHOP_Y \
	"Memory requirements forced a smaller image height."

/* if an XLoadQueryFont fails */
#define MSG_FONTLOAD "Unable to load font!\n"

/* if the open of the laser printer file fails */
#define MSG_LWOPENERR	"Unable to open laser printer file!\n"

/* if we are about to try to resize the window */
#define MSG_TRYRESIZE \
	"Trying to resize the window.  Some window managers just ignore a\n\
program's attempts to resize a window, so you may have to do it yourself\n"

/* if a resize appears to fail */
#define MSG_BADRESIZE \
	"Hmm...I detect that a resize request went unhonored.  Things may be\n\
a little wierd unless you make this window smaller\n"

/* If the area used to calculate the histogram is too small */
#define MSG_SMALLHIST \
	"The area used to calculate the histogram is too small!\n"

#define MSG_LGWINTOOSMALL \
	"The line graphics window is too small for a plot\n"
