/*
 *  Name:
 *     grf_tkcan.c
 *
 *  Purpose:
 *     Implement the grf module using a Tk canvas.
 *
 *  Description:
 *     This file implements the low level graphics functions required by
 *     the rest of AST, by evaluating suitable Tk canvas commands.
 *
 *  Authors:
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *
 *  Notes:
 *     This module requires the use of the rtd_mark, rtd_word
 *     and rtd_segment canvas items (these are distributed as
 *     part of GAIA).
 *
 *     Before using any AST routines to plot anything you must call
 *     the astTk_Init function will the names of the Tcl interpreter
 *     and the canvas to be drawn into. You can control the removal
 *     of elements using the astTk_Tag routine. This sets the tag to
 *     be added to any elements created after that time (note that
 *     the tag cannot be greater than 32 characters and is reset
 *     at each call to astTk_Init to "ast_element").
 *
 *  Copyright:
 *     Copyright (C) 1997-1998 Central Laboratory of the Research Councils
 *
 *  History:
 *     1-AUG-1997 (PDRAPER):
 *        Original version, based on grp_pgplot.c.
 *     18-NOV-1997 (PDRAPER):
 *        Added changes for version 0.9 AST.
 */

/* Macros */
/* ====== */
#define astCLASS                 /* Make protected functions available */

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "ast.h"                 /* AST functions and macros */
#include "ast_error.h"           /* Error reporting facilities */
#include "grf.h"                 /* Interface to this module */

/* C header files. */
/* --------------- */
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include "tcl.h"                 /* Tcl functions and definitions */

/* Constants. */
/* ========== */
#ifdef R2D
#undef R2D
#endif
#define R2D 57.29578             /* Radians to degrees factor */
#define CMDLEN 512               /* Maximum length of Tcl message string */
#define TAGLEN 32                /* Maximum length of a canvas tag */
#define WIDLEN 132               /* Maximum length of widget name */

/* Local Macros */
/* ============ */
#define MAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MIN(a,b) ( (a) < (b) ? (a) : (b) )

/* Function Prototypes. */
/* ==================== */

static int textBBox ( double x, double y, char *text, char *anchor,
                      double angle, float *xb, float *yb );

static int textAnchor ( const char *just, char *anchor );

void RtdWordLastBBox ( double *xb, double *yp );

/* Static variables. */
/* ================= */
static Tcl_Interp *Interp = NULL;  /* Name of the Tcl interpreter */
static char Canvas[WIDLEN];        /* Buffer for containing canvas
                                      name. */
static char Segment[TAGLEN];       /* Tk tag of the existing segment
                                      item. */
static int NewSegment = 1;         /* True when a new segment is
                                      needed (at first call to create
                                      a line and when the default tag
                                      is changed. */
static int HaveScale = 0;          /* Scale just initialised once */
static float Scale = 1.0;          /* Pixels per MM */

/* Define a structure to contain the configuration information (line
   widths, types etc.). */
typedef struct configInfo {
  double style;
  double width;
  double size;
  int font;
  int colour;
  char tag[TAGLEN];
} configInfo;

static configInfo ConfigInfo;

/* Define a list of possible fonts (bit clunky for X). These are
   typical fonts a variable width, fixed width and a symbol font.
   The slants are normal and slanted (r and o/i). The final
   fonts are just a gratuitously big one and the fixed font.
*/
static char *Fonts[19] = {
"-adobe-helvetica-medium-r-normal--*-140-*-*-*-*-*-*",
"-adobe-helvetica-medium-o-normal--*-140-*-*-*-*-*-*",
"-adobe-helvetica-bold-r-normal--*-140-*-*-*-*-*-*",
"-adobe-helvetica-bold-o-normal--*-140-*-*-*-*-*-*",
"-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*",
"-adobe-helvetica-medium-o-normal--*-120-*-*-*-*-*-*",
"-adobe-helvetica-bold-r-normal--*-120-*-*-*-*-*-*",
"-adobe-helvetica-bold-o-normal--*-120-*-*-*-*-*-*",
"-adobe-times-medium-r-normal--*-120-*-*-*-*-*-*",
"-adobe-times-medium-i-normal--*-120-*-*-*-*-*-*",
"-adobe-times-bold-r-normal--*-120-*-*-*-*-*-*",
"-adobe-times-bold-i-normal--*-120-*-*-*-*-*-*",
"-adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*",
"-adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*",
"-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*",
"-adobe-courier-bold-o-*-*-*-120-*-*-*-*-*-*",
"-adobe-symbol-medium-r-normal-*-*-120-*-*-*-*-*-*",
"-adobe-helvetica-bold-r-*-*-20-*-*-*-*-*-*-*",
"fixed"};

/*  Define a list of possible colours (this is more or less like PGPLOT).
 * 0   White                  1.00, 1.00, 1.00
 * 1   Black                  0.00, 0.00, 0.00
 * 2   Red                    1.00, 0.00, 0.00
 * 3   Green                  0.00, 1.00, 0.00
 * 4   Blue                   0.00, 0.00, 1.00
 * 5   Cyan (Green + Blue)    0.00, 1.00, 1.00
 * 6   Magenta (Red + Blue)   1.00, 0.00, 1.00
 * 7   Yellow  (Red + Green)  1.00, 1.00, 0.00
 * 8   Red + Yellow (Orange)  1.00, 0.50, 0.00
 * 9   Green + Yellow         0.50, 1.00, 0.00
 *10   Green + Cyan           0.00, 1.00, 0.50
 *11   Blue + Cyan            0.00, 0.50, 1.00
 *12   Blue + Magenta         0.50, 0.00, 1.00
 *13   Red + Magenta          1.00, 0.00, 0.50
 *14   Dark Gray              0.33, 0.33, 0.33
 *15   Light Gray             0.66, 0.66, 0.66
 */
static char *Colours[16] = {
"#fff", "#000", "#f00", "#0f0", "#00f", "#0ff", "#f0f", "#ff0", "#f80",
"#8f0", "#0f8", "#08f", "#80f", "#f08", "#512751275127", "#a8b4a8b4a8b4"};

/* Externally visible functions. */
/* ============================= */
/* These implement the "grf" interface in terms of Tk canvas primitives
   and add the necessary initialisation functions. */

int astTk_Init( Tcl_Interp *theinterp, const char *thecanvas ) {
/*
 *+
 *  Name:
 *     astTk_Init

 *  Purpose:
 *     Establishes the Tcl interpreter and canvas that are used
 *     to draw the graphics.

 *  Synopsis:
 *     int astTk_Init( theinterp, thecanvas )

 *  Description:
 *      This routine records the name of the Tcl interpreter and the
 *      canvas that is used to draw the graphics. It should be invoked
 *      at least once before any attempts to draw graphics are made
 *      from within the normal ast plotting mechanisms.

 *  Parameters:
 *     theinterp
 *        The Tcl interpreter that was used to create the canvas.
 *     thecanvas
 *        The Tk name of the canvas (i.e. that used by the canvas
 *        command).

 *  Returned Value:
 *     A value of 0 is returned if an error occurs, and 1 is returned
 *     otherwise.

 *-
 */
  /*  Record the names of the canvas and its interpreter */
  if ( thecanvas != NULL ) {
    (void) strncpy( Canvas, thecanvas, WIDLEN - 1);
  } else {
    astError( AST__GRFER, "NULL canvas name not allowed\n");
    return 0;
  }
  if ( theinterp != NULL ) {
    Interp = theinterp;
  } else {
    astError( AST__GRFER, "NULL Tcl interpreter not allowed\n");
    return 0;
  }

  /*  Set the default configuration information. */
  ConfigInfo.style = 1.0;
  ConfigInfo.width = 1.0;
  ConfigInfo.size = 1.0;
  ConfigInfo.font = 0;
  ConfigInfo.colour = 0;
  (void) strcpy( ConfigInfo.tag, "ast_element" );

  /*  Need a line segment item. */
  NewSegment = 1;
  Segment[0] = '\0';

  /*  Return here if all O.K. */
  return 1;
}

void astTk_Tag( const char *newtag ) {
/*
 *+
 *  Name:
 *     astTk_Tag

 *  Purpose:
 *     Sets the canvas tag.

 *  Synopsis:
 *     void astTk_Tag( char *newtag )

 *  Description:
 *     This routine modifies the string used to tag any new canvas
 *     items. This facility is intended to be used to identify any
 *     elements associated with specific calls to AST routines (such as
 *     astGrid) so that they can be subsequently modified (or deleted
 *     as a whole). It is therefore important that the tag is reset at
 *     appropriate times (by setting the tag to NULL) so that further
 *     canvas items are not mis-identified.

 *  Parameters:
 *     newtag
 *        The tag to be associated with any canvas items created from
 *        this point onwards. If set to NULL then the tag will be
 *        reset to the default value "ast_element".

 *-
 */
  if ( newtag != NULL ) {
    (void) strncpy( ConfigInfo.tag, newtag, TAGLEN - 1);
  } else {
    (void) strcpy( ConfigInfo.tag, "ast_element" );
  }

  /*  Need a line segment item. */
  NewSegment = 1;
}


int astGFlush( void ){
/*
*+
*  Name:
*     astGFlush

*  Purpose:
*     Flush all pending graphics to the output device.

*  Synopsis:
*     #include "grf.h"
*     int astGFlush( void )

*  Description:
*     This function ensures that the display device is up-to-date,
*     by flushing any pending graphics to the output device.

*  Parameters:
*     None.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*-
*/
  if ( Interp != NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }
  if ( Tcl_Eval( Interp, "update idletasks\n" ) == TCL_OK ) {
    return 1;
  } else {
    return 0;
  }
}

int astGLine( int n, const float *x, const float *y ){
/*
*+
*  Name:
*     astGLine

*  Purpose:
*     Draw a polyline (i.e. a set of connected lines).

*  Synopsis:
*     #include "grf.h"
*     int astGLine( int n, const float *x, const float *y )

*  Description:
*     This function displays lines joining the given positions.
*     For efficiency all lines are created as part of the same
*     canvas segment item (so that when orientation changes,
*     rescaling etc. are performed on the canvas as a whole all
*     "lines" are done together), until either the tag is changed
*     a line related configuration option or a re-initialisation
*     of the canvas name is made.

*  Parameters:
*     n
*        The number of positions to be joined together.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Nothing is done if "n" is less than 2, or if a NULL pointer is
*     given for either "x" or "y".

*-
*/
  int i, j;
  int need;
  int have;
  int used;
  char buffer[CMDLEN];
  char *coords = NULL;

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  /*  Create a string for storing the eventual segment creation command.
   *  This may become very long as all the coordinates will be stored
   *  in it, so we will increase it as necessary.
   */
  coords = (char *) malloc( CMDLEN );
  coords[0] = '\0';
  have = CMDLEN;
  used = 0;
  if( n > 1 && x && y ) {
    for ( i = 0, j = 1; i < n - 1; i++, j++ ) {
      (void) sprintf( buffer, " %f %f %f %f", (double) x[i],
                      (double) y[i], (double) x[j], (double) y[j] );
      need = strlen( buffer );
      if ( ( need + used + 2 ) > have ) {
        have = have + CMDLEN;
        coords = (char *) realloc( (void *) coords, (size_t) have );
      }
      (void) strcat( coords, buffer );
      used += need + 1;
    }

    /*  If using an existing segment then add these new values,
        otherwise create a new segment and record its tag. */
    if ( NewSegment ) {

      /*  Finish the command by adding the canvas name and all the
          required options. */
      (void) sprintf( buffer, " -fill %s -width %f -tag %s \n",
               Colours[ConfigInfo.colour], ConfigInfo.width, ConfigInfo.tag );
      if ( Tcl_VarEval( Interp, Canvas, " create rtd_segment ", coords, buffer,
                        (char *) NULL ) != TCL_OK ) {

        /*  Failed in creation attempt, so issue an error, release the
            workspace and make sure that a new segment item is
            created next time. */
        astError( AST__GRFER, "astGLine: failed to create line (%s)", Interp->result );
        free( (void *) coords );
        NewSegment = 1;
        return 0;

      } else {

        /*  Record the name of the item we have just created. */
        (void) strncpy( Segment, Interp->result, TAGLEN - 1 );
        NewSegment = 0;
      }
    } else {

      /* Use the existing segment. */
      if ( Tcl_VarEval( Interp, Canvas, " coords ", Segment, " add ",
                        coords, (char *) NULL ) != TCL_OK ) {

        /*  Failed in creation attempt, so issue an error, release the
            workspace and make sure that a new segment item is
            created next time. */
        astError( AST__GRFER, "astGLine: failed to append line segments (%s)", Interp->result );
        free( (void *) coords );
        NewSegment = 1;
        return 0;
      }

    }
    free( (void *) coords );
  }
  return 1;
}

int astGMark( int n, const float *x, const float *y, int type ){
/*
*+
*  Name:
*     astGMark

*  Purpose:
*     Draw a set of markers.

*  Synopsis:
*     #include "grf.h"
*     int astGMark( int n, const float *x, const float *y, int type )

*  Description:
*     This function displays markers at the given positions.

*  Parameters:
*     n
*        The number of markers to draw.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.
*     type
*        An integer which can be used to indicate the type of marker symbol
*        required.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Nothing is done if "n" is less than 1, or if a NULL pointer is
*     given for either "x" or "y".

*-
*/
  char *shape;
  int i;
  char buffer[CMDLEN];

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  if( n > 0 && x && y ) {

    /* Convert marker type into one known to the rtd_mark item. If the
       value is out of range then the user gets a circle. */
    switch (type) {
    case 1:
      shape = "dot";
      break;
    case 2:
      shape = "cross";
      break;
    case 3:
      shape = "plus";
      break;
    case 4:
      shape = "square";
      break;
    case 5:
      shape = "circle";
      break;
    case 6:
      shape = "diamond";
      break;
    case 7:
      shape = "triangle";
      break;
    default:
      shape = "circle";
    }

    /* Create string with initialising command then add the
       coordinates of the mark and its type, finally evaluate the
       command. */
    for ( i = 0; i < n; i++ ) {
      (void) sprintf( buffer, "%s create rtd_mark %f %f -type %s -size %d \
-width %d -outline %s -fill %s -tag %s \n",
                      Canvas, (double) x[i], (double) y[i], shape,
                      (int) ConfigInfo.size, (int) ConfigInfo.width,
                      Colours[ConfigInfo.colour],
                      Colours[ConfigInfo.colour], ConfigInfo.tag );
      if ( Tcl_Eval( Interp, buffer ) != TCL_OK ) {
        astError( AST__GRFER, "astGLine: failed to create mark (%s)", Interp->result );
        return 0;
      }
    }
  } else {
    return 0;
  }
   return 1;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
/*
*+
*  Name:
*     astGText

*  Purpose:
*     Draw a character string.

*  Synopsis:
*     #include "grf.h"
*     int astGText( const char *text, float x, float y, const char *just,
*                   float upx, float upy )

*  Description:
*     This function displays a character string at a given position
*     using a specified justification and up-vector.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", or 'B' for "bottom", and specifies the
*        vertical location of the reference position. Note, "bottom"
*        corresponds to the base-line of normal text. Some characters
*        (eg "y", "g", "p", etc) descend below the base-line. The second
*        character may be 'L' for "left", 'C' for "centre", or 'R'
*        for "right", and specifies the horizontal location of the
*        reference position. If the string has less than 2 characters
*        then 'C' is used for the missing characters.
*     upx
*        The x component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        left to right on the screen.
*     upy
*        The y component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        bottom to top on the screen.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Any graphics within the rotated box enclosing the text are erased.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*     -  Both "upx" and "upy" being zero causes an error.
*     -  Any unrecognised character in "just" causes an error.

*-
*/

  /* Local Variables: */
  char anchor[3];
  char buffer[CMDLEN];
  float angle;

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  /* Check that there is something to draw. */
  if( text && text[ 0 ] != 0 ){

    /* Translate the justification string into an Tk anchor string. */
    if ( ! textAnchor( just, anchor ) ) {
      return 0;
    }

    /* If either axis is reversed, reverse the supplied up-vector components
       so that they refer to the world-coordinates axes??*/

    /* Get the angle between the text base-line and horizontal. */
    angle = atan2( -(double) upx, (double) upy ) * R2D;

    /* Now display the text. */
    (void) sprintf ( buffer, "%s create rtd_word %f %f -word {%s} -angle %f \
-anchor %s -scale %f -font %s -fill %s -tag %s \n",
                     Canvas, (double) x, (double) y, text, angle, anchor,
                     ConfigInfo.size, Fonts[ConfigInfo.font],
                     Colours[ConfigInfo.colour],
                     ConfigInfo.tag );
    if ( Tcl_Eval( Interp, buffer ) != TCL_OK ) {
      astError( AST__GRFER, "astGText: Failed to draw text." );
      return 0;
    }
  }

  /* Return. */
  return 1;
}

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){
/*
*+
*  Name:
*     astGTxExt

*  Purpose:
*     Get the extent of a character string.

*  Synopsis:
*     #include "grf.h"
*     int astGTxExt( const char *text, float x, float y, const char *just,
*                   float upx, float upy, float *xb, float *yb )

*  Description:
*     This function returns the corners of a box which would enclose the
*     supplied character string if it were displayed using astGText.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", or 'B' for "bottom", and specifies the
*        vertical location of the reference position. The second
*        character may be 'L' for "left", 'C' for "centre", or 'R'
*        for "right", and specifies the horizontal location of the
*        reference position. If the string has less than 2 characters
*        then 'C' is used for the missing characters.
*     upx
*        The x component of the up-vector for the text.
*     upy
*        The y component of the up-vector for the text.
*     xb
*        An array of 4 elements in which to return the x coordinate of
*        each corner of the bounding box.
*     yb
*        An array of 4 elements in which to return the y coordinate of
*        each corner of the bounding box.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  A NULL value for "just" causes a value of "CC" to be used.
*     -  Both "upx" and "upy" being zero causes an error.
*     -  Any unrecognised character in "just" causes an error.
*     -  Zero is returned for all bounds of the box if an error occurs.

*-
*/

  /* Local Variables: */
  char anchor[3];
  float angle;
  int i;

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  /* Initialise the returned values to indicate no box available. */
  for( i = 0; i < 4; i++ ){
    xb[i] = 0.0f;
    yb[i] = 0.0f;
  }

  /* Check that there is something to draw. */
  if( text && text[ 0 ] != 0 ){

    /* Translate the justification string into an Tk anchor string. */
    if ( ! textAnchor( just, anchor ) ) {
      return 0;
    }

    /* If either axis is reversed, reverse the supplied up-vector components
       so that they refer to the world-coordinates axes?? */

    /* Get the angle between the text base-line and horizontal. */
    angle = atan2( -(double) upx, (double) upy ) * R2D;

    /* Now get the bounding box. */
    if ( ! textBBox ( (double) x, (double) y, (char *)text, anchor,
                      (double) angle, xb, yb ) ) {
      return 0;
    }
  }

  /* Return. */
  return 1;
}

int astGQch( float *chv, float *chh ){
/*
*+
*  Name:
*     astGQch

*  Purpose:
*     Return the character height in world cooridnates.

*  Synopsis:
*     #include "grf.h"
*     int astGQch( float *chv, float *chh )

*  Description:
*     This function returns the heights of characters drawn vertically and
*     horizontally in world coordinates.

*  Parameters:
*     chv
*        A pointer to the float which is to receive the height of
*        characters drawn vertically. This will be an increment in the X
*        axis.
*     chh
*        A pointer to the float which is to receive the height of
*        characters drawn horizontally. This will be an increment in the Y
*        axis.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*-
*/

  /* Local Variables: */
  float xbox[4], ybox[4];

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  /* Get the bounding box for a horizontal text string with good up
     and down span. */
  if ( ! textBBox ( 0.0, 0.0, "ABCD", "c", 0.0, xbox, ybox ) ) {
    return 0;
  } else {
    *chh = xbox[1] - xbox[0];
    if ( *chh < 0 ) *chh = -(*chh);
  }

  /* Same for vertical text. */
  if ( ! textBBox ( 0.0, 0.0, "ABCD", "c", 90.0, xbox, ybox ) ) {
    return 0;
  } else {
    *chv = ybox[3] - ybox[0];
    if ( *chv < 0 ) *chv = -(*chv);
  }

  /* Return. */
  return 1;
}

int astGAttr( int attr, double value, double *old_value, int prim ){
/*
*+
*  Name:
*     astGAttr

*  Purpose:
*     Enquire or set a graphics attribute value.

*  Synopsis:
*     #include "grf.h"
*     int int astGAttr( int attr, double value, double *old_value, int prim )

*  Description:
*     This function returns the current value of a specified graphics
*     attribute, and optionally establishes a new value. The supplied
*     value is converted to an integer value if necessary before use.

*  Parameters:
*     attr
*        An integer value identifying the required attribute. The
*        following symbolic values are defined in grf.h:
*
*           GRF__STYLE  - Line style.
*           GRF__WIDTH  - Line width.
*           GRF__SIZE   - Character and marker size scale factor.
*           GRF__FONT   - Character font.
*           GRF__COLOUR - Colour index.
*     value
*        A new value to store for the attribute. If this is AST__BAD
*        no value is stored.
*     old_value
*        A pointer to a double in which to return the attribute value.
*        If this is NULL, no value is returned.
*     prim
*        The sort of graphics primative to be drawn with the new attribute.
*        Identified by the following values defined in grf.h:
*           GRF__LINE
*           GRF__MARK
*           GRF__TEXT

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:

*-
*/

  int ival;
  double dval;

  if ( Interp == NULL ) {
    astError( AST__GRFER, "Tk graphics system not initialised\n");
    return 0;
  }

  /* If required retrieve the current line style, and set a new line
     style. The Tk line does not have any "styles" as recognised here
     so always set this to 1.0. It may be possible to use stipples to
     get other effects if really necessary. */
  if( attr == GRF__STYLE ){
    if( old_value ) *old_value = ConfigInfo.style;

    if( value != AST__BAD ){

      /* Need a new segment item if the value has changed. */
      if ( value != ConfigInfo.style ) {
        NewSegment = 1;
      }
      ConfigInfo.style = 1.0;

    }

  } else if( attr == GRF__WIDTH ){
    /* If required retrieve the current line width, and set a new line width.
       Line width is scaled betwen 0.0 (minimum thickness) and 1.0 (maximum
       thickness). */
    if( old_value ) *old_value = ConfigInfo.width / 200.0;

    if( value != AST__BAD ){
      dval = MAX( 0.0, MIN( 200.0, ( value * 200.0 ) ) );

      /* Need a new segment item if the value has changed. */
      if ( dval != ConfigInfo.width ) {
        NewSegment = 1;
      }
      ConfigInfo.width = MAX( 0.0, MIN( 200.0, ( value * 200.0 ) ) );
    }

  } else if( attr == GRF__SIZE ){
    /* If required retrieve the current character size, and set a new size.
       The attribute value should be a factor by which to multiply the
       default character size. */
    if( old_value ) *old_value = ConfigInfo.size;

    if( value != AST__BAD ){
      ConfigInfo.size = value;
    }

  } else if( attr == GRF__FONT ){
    /* If required retrieve the current character font, and set a new font. */
    if( old_value ) *old_value = (double) ConfigInfo.font;

    if( value != AST__BAD ) {
      ConfigInfo.font = MAX( 0, MIN( 18, (int) value ) );
    }

  } else if( attr == GRF__COLOUR ){
    /* If required retrieve the current colour index, and set a new colour
       index. */
    if( old_value ) *old_value = (double) ConfigInfo.colour;

    if( value != AST__BAD ){
      ival = MAX( 0, MIN( 15, (int) value ) );

      /* Need a new segment item if the value has changed. */
      if ( ival != ConfigInfo.colour ) {
        NewSegment = 1;
      }
      ConfigInfo.colour = ival;
    }
  } else {
    /* Give an error message for any other attribute value. */
    astError( AST__GRFER, "astGAttr: Unknown graphics attribute '%d' "
                "requested.", attr );
    return 0;
  }

  /* Return. */
   return 1;
}

int astGAxScale( float *alpha, float *beta ){
/*
*+
*  Name:
*     astGAxScale

*  Purpose:
*     Get the axis scales.

*  Synopsis:
*     #include "grf.h"
*     int astGAxScale( float *alpha, float *beta )

*  Description:
*     This function returns two values (one for each axis) which scale
*     increments on the corresponding axis into a "normal" coordinate
*     system in which:
*        1 - The axes have equal scale in terms of (for instance)
*            millimetres per unit distance.
*        2 - X values increase from left to right.
*        3 - Y values increase from bottom to top.

*  Parameters:
*     alpha
*        A pointer to the location at which to return the scale for the
*        X axis (i.e. Xnorm = alpha*Xworld).
*     beta
*        A pointer to the location at which to return the scale for the
*        Y axis (i.e. Ynorm = beta*Yworld).

*  Returned Value:
*     A value of 0 is returned if an error occurrs, and 1 is returned
*     otherwise.

*-
*/
  /*  Local variables */
  char buffer[CMDLEN];
  double s1, s2;

  /*  For a Tk canvas the Y axis runs from upper left to lower left
   *  and the X axis from upper left to upper right. The scales are
   *  always square. So we determine these just once and make
   *  speedup assumptions. */
  if ( ! HaveScale ) {
    HaveScale = 1;

    /* Get the pixels per MM figure by offsetting from 10m to 20m
     * along the X axis */
    (void) sprintf ( buffer, "%s canvasx 10m \n", Canvas );
    if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {
      s1 = atof( Interp->result );
      (void) sprintf ( buffer, "%s canvasx 20m \n", Canvas );
      if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {
        s2 = atof( Interp->result );
        Scale = (float)( ( MAX( s2, s1 ) - MIN( s2, s1 ) ) / 10.0 );
      }
    }
  }
  *alpha = Scale;
  *beta = -Scale;
  return 1;
}

static int textBBox ( double x, double y, char *text, char *anchor,
                      double angle, float *xb, float *yb ) {
/*
 *+
 *  Name:
 *     textBBox

 *  Purpose:
 *     Return the bounding box of the given text.

 *  Description:
 *     This routine returns a bounding box as returned by astGTxExt.
 *     This is achieved by creating a word with the necessary
 *     parameters and then querying its bounding box. The text is then
 *     deleted from the canvas before proceeding.

 */

  /*  Local Variables. */
  char buffer[CMDLEN];
  double xbd[4];
  double ybd[4];

  /*  First display the text  */
  (void) sprintf ( buffer, "%s create rtd_word %f %f -word {%s} -angle %f \
-anchor %s -scale %f -font %s -tag grf_word_temp \n",
            Canvas, x, y, text, angle, anchor, ConfigInfo.size,
            Fonts[ConfigInfo.font] );
  if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {

    /*  Now get the bounding box of the text (we use a back door to
        get this as Tk only returns the axis aligned box and we want a
        proper one) and then remove it */
    (void) sprintf ( buffer, "%s bbox %s \n", Canvas, Interp->result );
    if ( Tcl_Eval( Interp, buffer ) == TCL_OK ) {
      RtdWordLastBBox( xbd, ybd );
      xb[0] = (float) xbd[0];
      xb[1] = (float) xbd[1];
      xb[2] = (float) xbd[2];
      xb[3] = (float) xbd[3];
      yb[0] = (float) ybd[0];
      yb[1] = (float) ybd[1];
      yb[2] = (float) ybd[2];
      yb[3] = (float) ybd[3];

      /*  Remove the word. */
      (void) sprintf ( buffer, "%s delete grf_word_temp \n", Canvas );
      (void) Tcl_Eval( Interp, buffer );
    } else {
      (void) sprintf ( buffer, "%s delete grf_word_temp \n", Canvas );
      (void) Tcl_Eval( Interp, buffer );
      return 0;
    }
  }
  return 1;
}


static int textAnchor ( const char *just, char *anchor ) {

/*
 *+
 *  Name:
 *     textAnchor

 *  Purpose:
 *     Convert an AST justification string into a Tk anchor position.

 *  Notes:
 *     The arrangement is actually top-bottom, left-right flipped.

 */

  anchor[0] = ' ', anchor[1] = ' ', anchor[2] = '\0';
  if( just ) {
    switch ( just[0] ) {
    case 'T':
      anchor[0] = 's'; break;
    case 'C':
      anchor[0] = 'c'; break;
    case 'B':
      anchor[0] = 'n'; break;
    }

    switch ( just[1] ) {
    case 'L':
      if ( anchor[0] == 'c' ) {
        anchor[0] = 'e';
      } else {
        anchor[1] = 'e';
      }
      break;
    case 'R':
      if ( anchor[0] == 'c' ) {
        anchor[0] = 'w';
      } else {
        anchor[1] = 'w';
      }
      break;
    case 'C':
      break;
    default:
      astError( AST__GRFER, "astGText: Justification string '%s' is "
                "invalid.", just );
      return 0;
    }
  } else {
    anchor[0] = 'c';
  }
  return 1;
}
