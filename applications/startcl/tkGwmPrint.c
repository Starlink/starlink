#include <config.h>

#include <stdio.h>
#include <math.h>

#include <stdlib.h>
#if HAVE_MALLOC_H
#  include <malloc.h>
#endif

#include "tcl.h"
#include "tk.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "tkGwm_sys.h"
#include "gwm_sys.h"

static void DoPs(ClientData clientData);
static void DoInkjet(ClientData clientData);
static void DoJpeg(ClientData clientData);
static void init_printer(FILE*);
static void reset_printer(FILE*);
static void get_mask(int [NINKS][NPLANES][NBITS]);
static void get_inks(XColor*, int[MAXDOTS][MAXCOLOURS]);
static void get_ink1(XColor*, int, int*, int*, int*, int*, int*);
static void rgb_2_hsv(float, float, float, float*, float*, float*l);
static void get_rgb(XColor*, int, float*, float*, float*);
static float min3f(float, float, float);
static float max3f(float, float, float);
static void send_nxo(FILE*, XImage*, int*, int*, int*, float*, float*);
static void get_locx(int, int, float, int[MAXX]);
static void handle_row(FILE*, XImage*, int[NINKS][NPLANES][NBITS],
    int[MAXDOTS][MAXCOLOURS], int[MAXX], int, int, float, float, int);
static void get_planes( XImage*, int[NINKS][NPLANES][NBITS],
    int[MAXDOTS][MAXCOLOURS], int[MAXX], int, int, int,
    float, int*, int[MAXX_BYTES], int[MAXX_BYTES], int[MAXX_BYTES]);
static void clear_planes(int, int[MAXX_BYTES], int[MAXX_BYTES],
    int [MAXX_BYTES]);
static void handle_plane(FILE*, int, int[MAXX_BYTES]);
static int encode(int[MAXX_BYTES], int, int[MAXRLE]);
static void send_plane(FILE*, int, int, int, int[MAXX_BYTES]);


#if !HAVE_ROUND
static double round( double x )
{
if ( x >= 0 )
   return ( x - floor(x) < 0.5 ? floor(x) : ceil(x) );
else
   return ( ceil(x) - x < 0.5 ? ceil(x) : floor(x) );
}
#endif

/*
**  Macros used by the ink jet printing code
*/
#define BITTEST(val, iBit) ((val & bitSet[iBit]))
#if !defined(MAX)
#define MAX(x, y) ((x > y) ? x : y)
#endif
#if !defined(MIN)
#define MIN(x, y) ((x < y) ? x : y)
#endif

int tkgwmStartPrint(Tcl_Interp *interp, Gwm *gwmPtr, char *filename)
{
    char *fullname;
    int nc, i, j, k;
    int epsf, portrait;
    float aspect, scale, xpaper, ypaper;
    long int bgpix;
    Tcl_DString buffer;
    int length;
    int quality;
    long pixel;

/*
 *  Check that we aren't already in the process of printing
 */
    if (gwmPtr->printing) {
	Tcl_AppendResult(interp, "widget is still printing previous request",
		 (char *) NULL);
	return TCL_ERROR;
    }

/*
**  Open output file
*/
    fullname = Tcl_TildeSubst(interp, filename, &buffer);
    if ( !fullname ) return TCL_ERROR;
    gwmPtr->out = fopen( fullname, "wb");
    if ( !gwmPtr->out ) {
	Tcl_AppendResult(interp, "couldn't open \"", fullname,
		"\"", (char *) NULL);
        Tcl_DStringFree( &buffer );
	return TCL_ERROR;
    }
    Tcl_DStringFree( &buffer );
    fseek( gwmPtr->out, 0L, 0);

/*
**  Get a copy of the colour table
*/
    if (gwmPtr->info->visclass == DirectColor ||
          gwmPtr->info->visclass == TrueColor ) {

/*
**  Decomposed colour model - get each colour component seperately.
*/

/*
**  Find first pixel value for each colour.
*/
       for ( gwmPtr->rbase = 1;
              (gwmPtr->vinfo->red_mask & gwmPtr->rbase) == 0;
              gwmPtr->rbase *= 2 );
       for ( gwmPtr->gbase = 1;
              (gwmPtr->vinfo->green_mask & gwmPtr->gbase) == 0;
              gwmPtr->gbase *= 2 );
       for ( gwmPtr->bbase = 1;
              (gwmPtr->vinfo->blue_mask & gwmPtr->bbase) == 0;
              gwmPtr->bbase *= 2 );

/*
**  Load the colour table array with the appropriate pixel values and get
**  the colour tables.
*/
       for ( nc = 0, pixel = 0; pixel <= gwmPtr->vinfo->red_mask; nc++ ) {
          pixel += gwmPtr->rbase;
       }
       gwmPtr->ctr = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pixel = 0; pixel <= gwmPtr->vinfo->red_mask; i++ ) {
          gwmPtr->ctr[i].pixel = pixel;
          pixel += gwmPtr->rbase;
       }
       XQueryColors( gwmPtr->display, gwmPtr->info->cmap, gwmPtr->ctr, nc );

       for ( nc = 0, pixel = 0; pixel <= gwmPtr->vinfo->green_mask; nc++ ) {
          pixel += gwmPtr->gbase;
       }
       gwmPtr->ctg = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pixel = 0; pixel <= gwmPtr->vinfo->green_mask; i++ ) {
          gwmPtr->ctg[i].pixel = pixel;
          pixel += gwmPtr->gbase;
       }
       XQueryColors( gwmPtr->display, gwmPtr->info->cmap, gwmPtr->ctg, nc );

       for ( nc = 0, pixel = 0; pixel <= gwmPtr->vinfo->blue_mask; nc++ ) {
          pixel += gwmPtr->bbase;
       }
       gwmPtr->ctb = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0, pixel = 0; pixel <= gwmPtr->vinfo->blue_mask; i++ ) {
          gwmPtr->ctb[i].pixel = pixel;
          pixel += gwmPtr->bbase;
       }
       XQueryColors( gwmPtr->display, gwmPtr->info->cmap, gwmPtr->ctb, nc );

/*
**  Force the background to requested colour
*/
       if ( gwmPtr->printbg ) {
           bgpix = gwmPtr->info->ctable[0];
           pixel = (bgpix & gwmPtr->vinfo->red_mask ) / gwmPtr->rbase;
           gwmPtr->ctr[pixel].red = gwmPtr->printbg->red;
           pixel = (bgpix & gwmPtr->vinfo->green_mask ) / gwmPtr->gbase;
           gwmPtr->ctg[pixel].green = gwmPtr->printbg->green;
           pixel = (bgpix & gwmPtr->vinfo->blue_mask ) / gwmPtr->bbase;
           gwmPtr->ctb[pixel].blue = gwmPtr->printbg->blue;
       }


    } else {

/*
**  Undecomposed colour model - get colours for all possible pixel values.
*/
       nc =  DisplayCells( gwmPtr->display,  DefaultScreen( gwmPtr->display ) );
       gwmPtr->ct = (XColor*)malloc( sizeof(XColor) * nc );
       for ( i = 0; i < nc; i++ ) gwmPtr->ct[i].pixel = i;
       XQueryColors( gwmPtr->display, gwmPtr->info->cmap, gwmPtr->ct, nc );

/*
**  Force the background to requested colour
*/
       if ( gwmPtr->printbg ) {
           bgpix = gwmPtr->info->ctable[0];
           gwmPtr->ct[bgpix].red = gwmPtr->printbg->red;
           gwmPtr->ct[bgpix].green = gwmPtr->printbg->green;
           gwmPtr->ct[bgpix].blue = gwmPtr->printbg->blue;
       }

    }
/*
**  Get a copy of the pixmap
*/
    gwmPtr->image = XGetImage( gwmPtr->display, gwmPtr->info->pix_id, 0, 0,
	gwmPtr->width, gwmPtr->height, AllPlanes, XYPixmap );

    length = strlen(gwmPtr->printformat);
    if ( strncmp(gwmPtr->printformat, "HPinkjet", length) == 0 ) {
	for (i=0; i<NINKS; i++) {
	    for (j=0; j<NPLANES; j++) {
		for (k=0; k<NBITS; k++) {
		    gwmPtr->mask[i][j][k] = 0;
		}
	    }
	}
        init_printer( gwmPtr->out );
        get_mask( gwmPtr->mask );
        get_inks( gwmPtr->ct, gwmPtr->inks);
        send_nxo( gwmPtr->out, gwmPtr->image, &gwmPtr->nyi, &gwmPtr->nxi,
		&gwmPtr->nxo, &gwmPtr->expand, &gwmPtr->dimin);
        get_locx( gwmPtr->nxi, gwmPtr->nxo, gwmPtr->dimin, gwmPtr->locx);
        gwmPtr->nyo = round (gwmPtr->nyi*gwmPtr->expand);
        gwmPtr->yo = 6;
	Tcl_DoWhenIdle(DoInkjet, gwmPtr);
    }
    else if ( strncmp(gwmPtr->printformat, "JPEG", length) == 0 ) {
#if HAVE_JPEGLIB_H

    /*
    **  Allocate and initialise compression structures
    */
        gwmPtr->cinfo = (struct jpeg_compress_struct*)
            malloc( sizeof(struct jpeg_compress_struct) );
        gwmPtr->jerr = (struct jpeg_error_mgr*)
            malloc( sizeof(struct jpeg_error_mgr) );
        gwmPtr->cinfo->err = jpeg_std_error(gwmPtr->jerr);
        jpeg_create_compress(gwmPtr->cinfo);
        jpeg_stdio_dest(gwmPtr->cinfo, gwmPtr->out);
        gwmPtr->cinfo->image_width = gwmPtr->width;
        gwmPtr->cinfo->image_height = gwmPtr->height;
        gwmPtr->cinfo->input_components = 3;
        gwmPtr->cinfo->in_color_space = JCS_RGB;
        jpeg_set_defaults(gwmPtr->cinfo);

    /*
    **  Set the quality and progressive
    */
        quality = gwmPtr->quality < 0 ? 0 :
             ( gwmPtr->quality > 100 ? 100 : gwmPtr->quality );
        jpeg_set_quality(gwmPtr->cinfo, quality, TRUE);
        if ( gwmPtr->progressive ) jpeg_simple_progression(gwmPtr->cinfo);

    /*
    **  Allocate buffer for one scanline
    */
        gwmPtr->buffer = (char*)malloc( gwmPtr->width * 3 * sizeof(JSAMPLE));

    /*
    **  Start compression
    */
        jpeg_start_compress(gwmPtr->cinfo, TRUE);

    /*
    **  Initialise line counter for work routine
    */
        gwmPtr->j = 0;

    /*
    **  register work routine
    */
	Tcl_DoWhenIdle(DoJpeg, gwmPtr);

#else /* !(HAVE_JPEGLIB_H) */
        /* what's the best thing to do here?  Just jump out? */
	Tcl_AppendResult(interp,
                         "Can't produce JPEGs -- unavailable at build time",
                         (char *) NULL);
	return TCL_ERROR;
#endif
    }
    else
    {
    /*
    **  Must be postscript
    */
	gwmPtr->colour = (strncmp(gwmPtr->printformat, "colour_ps", length) ==
	    0) || (strncmp(gwmPtr->printformat, "colour_postscript", length) ==
	    0) || (strncmp(gwmPtr->printformat, "colour_eps", length) == 0);
	epsf = (strncmp(gwmPtr->printformat, "eps", length) == 0) ||
	    (strncmp(gwmPtr->printformat, "bw_eps", length) == 0) ||
	    (strncmp(gwmPtr->printformat, "colour_eps", length) == 0);

    /*
    **  Allocate enough space for a line of postscript
    */
        if ( gwmPtr->colour )
            gwmPtr->buffer = (char*)malloc( gwmPtr->width * 6  + 1);
        else
            gwmPtr->buffer = (char*)malloc( gwmPtr->width * 2  + 1);

    /*
    **  Convert paper size from mm to postscript points (1/72 inch)
    */
	xpaper = (float)gwmPtr->paperwidth * 2.83465;
	ypaper = (float)gwmPtr->paperheight * 2.83465;

    /*
    **  Compare the aspect ratio of the picture with that of the paper
    **  size and decide whether to plot the picture in portrait or landscape
    **  mode.
    */
        aspect = (float)gwmPtr->height / (float)gwmPtr->width;
        portrait = aspect >= 1.0;

    /*
    **  Compute a scaling factor to fit the plot onto the paper
    */
        if ( portrait )
        {
            if ( aspect < ypaper/xpaper )
                scale = xpaper / (float)gwmPtr->width;
            else
                scale = ypaper / (float)gwmPtr->height;
        }
        else
        {
            if ( aspect < xpaper/ypaper )
                scale = ypaper / (float)gwmPtr->width;
            else
                scale = xpaper / (float)gwmPtr->height;
        }

    /*
    **  Write postscript preamble
    */
        fprintf( gwmPtr->out, "%%!PS-Adobe-2.1\n" );
        fprintf( gwmPtr->out, "%%%%Creator: mxrefresh V1.0\n" );
        fprintf( gwmPtr->out, "%%%%Pages: 1\n" );
        fprintf( gwmPtr->out, "%%%%DocumentFonts:\n" );
        if (epsf) fprintf( gwmPtr->out, "%%%%BoundingBox: %f %f %f %f\n",
	    0.0, 0.0, (float)gwmPtr->width * scale,
	    (float)gwmPtr->height * scale);
        fprintf( gwmPtr->out, "%%%%EndComments\n" );
        if ( !epsf )
        {
        /*
        **  Not EPSF
        */
            fprintf( gwmPtr->out, "clippath pathbbox pop pop translate\n" );
        }
        fprintf( gwmPtr->out, "%%%%EndProlog\n" );
        fprintf( gwmPtr->out, "%%%%Page ? 1\n" );
        fprintf( gwmPtr->out, "[ %f 0.0 0.0 %f 0.0 0.0 ] concat\n", scale, scale );
        if (!portrait)
            fprintf( gwmPtr->out, "[ 0.0 1.0 -1.0 0.0 %f 0.0 ] concat\n",
                xpaper / scale );
        fprintf( gwmPtr->out, "%d %d 8", gwmPtr->width, gwmPtr->height);
        fprintf( gwmPtr->out, " [ %f %f %f %f %f %f ]",
            1.0, 0.0, 0.0, 1.0, 0.0, 0.0 );
        fprintf( gwmPtr->out, "{currentfile %d string readhexstring pop}false",
            gwmPtr->width );
        if ( gwmPtr->colour )
            fprintf( gwmPtr->out, " 3" );
        else
            fprintf( gwmPtr->out, " 1" );
        fprintf( gwmPtr->out, " colorimage\n" );

    /*
    **  Initialise line counter for work routine
    */
        gwmPtr->j = gwmPtr->height - 1;

    /*
    **  register work routine
    */
	Tcl_DoWhenIdle(DoPs, gwmPtr);
    }

/*
 *  Set printing in progress flag
 */
    gwmPtr->printing = 1;
    return TCL_OK;
}

static void DoPs(ClientData clientData)
{
/*
**  Work routine for writing a line of postscript
*/
    Gwm *gwmPtr = (Gwm*)clientData;
    int i, k, inten;
    long rpix, gpix, bpix, pix;

    if ( gwmPtr->j >= 0 )
    {
    /*
    **  For each pixel in the line
    */
        for ( i = 0, k = 0; i < gwmPtr->width; i++ )
        {
            pix = XGetPixel( gwmPtr->image, i, gwmPtr->j );
            if (gwmPtr->colour)
            {
                if (gwmPtr->info->visclass == DirectColor ||
                      gwmPtr->info->visclass == TrueColor ) {
                    rpix = (pix & gwmPtr->vinfo->red_mask) / gwmPtr->rbase;
                    gpix = (pix & gwmPtr->vinfo->green_mask) / gwmPtr->gbase;
                    bpix = (pix & gwmPtr->vinfo->blue_mask) / gwmPtr->bbase;
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctr[rpix].red >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctr[rpix].red >> 8) & 0xf];
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctg[gpix].green >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctg[gpix].green >> 8) & 0xf];
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctb[bpix].blue >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                      "0123456789ABCDEF"[(gwmPtr->ctb[bpix].blue >> 8) & 0xf];
                } else {
                    gwmPtr->buffer[k++] =
                       "0123456789ABCDEF"[(gwmPtr->ct[pix].red >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                       "0123456789ABCDEF"[(gwmPtr->ct[pix].red >> 8) & 0xf];
                    gwmPtr->buffer[k++] =
                        "0123456789ABCDEF"[(gwmPtr->ct[pix].green >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                         "0123456789ABCDEF"[(gwmPtr->ct[pix].green >> 8) & 0xf];
                    gwmPtr->buffer[k++] =
                        "0123456789ABCDEF"[(gwmPtr->ct[pix].blue >> 12) & 0xf];
                    gwmPtr->buffer[k++] =
                        "0123456789ABCDEF"[(gwmPtr->ct[pix].blue >> 8) & 0xf];
               }
            }
            else
            {
            /*
            **  Convert the pixel value to a B/W intensity
            */
                if (gwmPtr->info->visclass == DirectColor ||
                      gwmPtr->info->visclass == TrueColor ) {
                    rpix = (pix & gwmPtr->vinfo->red_mask) / gwmPtr->rbase;
                    gpix = (pix & gwmPtr->vinfo->green_mask) / gwmPtr->gbase;
                    bpix = (pix & gwmPtr->vinfo->blue_mask) / gwmPtr->bbase;
                    inten = (int)( (float)(gwmPtr->ctr[rpix].red   >> 8) +
                        (float)(gwmPtr->ctg[gpix].green >> 8) +
                        (float)(gwmPtr->ctb[bpix].blue  >> 8) ) /3.0;
                } else {
                    inten = (int)( (float)(gwmPtr->ct[pix].red   >> 8) +
                        (float)(gwmPtr->ct[pix].green >> 8) +
                        (float)(gwmPtr->ct[pix].blue  >> 8) ) /3.0;
                }

            /*
            **  Write it to the file in Hex
            */
                gwmPtr->buffer[k++] = "0123456789ABCDEF"[(inten >> 4) & 0xf];
                gwmPtr->buffer[k++] = "0123456789ABCDEF"[inten & 0xf];
            }
        }
        gwmPtr->buffer[k] = '\0';
        fputs( gwmPtr->buffer, gwmPtr->out);
        fprintf( gwmPtr->out, " \n");

    /*
    **  Decrement line counter
    */
        gwmPtr->j--;

    /*
     *  Arrange to be called again
     */
	Tcl_DoWhenIdle(DoPs, gwmPtr);
    }
    else
    {

    /*
    **  Write trailer
    */
        fprintf( gwmPtr->out, "showpage\n" );
        fprintf( gwmPtr->out, "%%%%Trailer\n" );

    /*
    **  Free the line buffer
    */
        free( gwmPtr->buffer );

    /*
    **  Close file and clean up
    */
        fclose( gwmPtr->out );
        if (gwmPtr->info->visclass == DirectColor ||
             gwmPtr->info->visclass == TrueColor ) {
           free( (void*)gwmPtr->ctr );
           free( (void*)gwmPtr->ctg );
           free( (void*)gwmPtr->ctb );
        } else {
           free( (void*)gwmPtr->ct );
        }

    /*
     *  Clear the printing in progress flag and set the print variable
     */
	gwmPtr->printing = 0;
	Tcl_Release((ClientData) gwmPtr);
	Tcl_SetVar(gwmPtr->interp, gwmPtr->printvar, "1",
		TCL_GLOBAL_ONLY);
    }
}
static void DoInkjet(ClientData clientData)
/*
*+
*  Name :
*     inkjet
*
*  Purpose :
*     support routines for printing a pixmap in HP Inkjet format
*
*  Language :
*     C
*
*  Invocation :
*
*  Description :
*  Authors :
*     DLT: David Terrett (Starlink RAL)
*       Derived from code by Rob Dickens (Jodrell Bank)
*/
{
    Gwm *gwmPtr = (Gwm*)clientData;

    if ( gwmPtr->yo < gwmPtr->nyo )
    {
        handle_row( gwmPtr->out, gwmPtr->image, gwmPtr->mask, gwmPtr->inks,
	    gwmPtr->locx, gwmPtr->nyi, gwmPtr->nxo, gwmPtr->expand,
	    gwmPtr->dimin, gwmPtr->yo);
        gwmPtr->yo++;
    /*
     *  Arrange to be called again
     */
	Tcl_DoWhenIdle(DoInkjet, gwmPtr);
    }
    else
    {
        reset_printer ( gwmPtr->out);

    /*
    **  Close file and clean up
    */
        fclose( gwmPtr->out );
        if (gwmPtr->info->visclass == DirectColor ||
             gwmPtr->info->visclass == TrueColor ) {
           free( (void*)gwmPtr->ctr );
           free( (void*)gwmPtr->ctg );
           free( (void*)gwmPtr->ctb );
        } else {
           free( (void*)gwmPtr->ct );
        }

    /*
     *  Clear the printing in progress flag and set the print variable
     */
	gwmPtr->printing = 0;
	Tcl_Release((ClientData) gwmPtr);
	Tcl_SetVar(gwmPtr->interp, gwmPtr->printvar, "1", TCL_GLOBAL_ONLY);
    }
}


/************************************************************************/
/*   init_printer                                                       */
/************************************************************************/

void init_printer ( FILE *out)
{
   fprintf (out, "%s", "\033%8\033&k2S\033*t180R\033*r3U\033*r0A");
   /*                      ^     ^       ^         ^       ^        */
   /*                      |     |       |         |       |        */
   /*            HP protocol     |       |         |       |        */
   /*  Print pitch = 18 chars/inch       |         |       |        */
   /*         Graphics res = 180 dots/inch         |       |        */
   /*                Use 3 planes (-> 8 colours/dot)       |        */
   /*                             Prepare to receive data...        */
   /*****************************************************************/
}

/************************************************************************/
/*   reset_printer                                                       */
/************************************************************************/

void reset_printer (FILE *out)
{
    fprintf (out, "%s", "\033*rB\033&k0W");
    /*                      ^      ^        */
    /*                      |      |        */
    /*           HP text mode      |        */
    /*                   single pass        */
    /****************************************/
}


/************************************************************************/
/*   get_mask                                                           */
/*                                                                      */
/*   Precompute bit mask, where each non-zero entry has one of bits 0   */
/*   through 7 set.                                                     */
/************************************************************************/

void get_mask ( int mask[NINKS][NPLANES][NBITS])
{
    int iPlane;
    int bitSet[NBITS] = {1, 2, 4, 8, 16, 32, 64, 128};
    int rawColour[NINKS] =
    {  4,    5,       1,   3,      2,     6,    0,     7};
    /**^*****^********^****^*******^******^*****^******^******/
    /* blue, magenta, red, yellow, green, cyan, black, white */
    /*********************************************************/

    for (iPlane=0; iPlane<NPLANES; iPlane++)
    {
	int iInk;

	for (iInk=0; iInk<NINKS; iInk++) if (BITTEST(rawColour[iInk], iPlane))
	{
	    int iBit;

	    for (iBit=0; iBit<NBITS; iBit++)
		mask[iInk][iPlane][iBit] = bitSet[NBITS - (iBit + 1)];
	}
    }
}


/************************************************************************/
/*   assign inks array                                                  */
/************************************************************************/

void get_inks (XColor *colourmap, int inks[MAXDOTS][MAXCOLOURS])
{
    int index;

    for (index=0; index<MAXCOLOURS; index++)
    {
	int ink1, ink2, n1, n2, nb, k;

	get_ink1 (colourmap, index, &ink1, &ink2, &n1, &n2, &nb);
	for (k=0; k<MAXDOTS; k++)
	{
	    if (k < n1) inks[k][index] = ink1;
            else if (k < n1+n2) inks[k][index] = ink2;
            else if (k < n1+n2+nb) inks[k][index] = 6;   /* black */
            else inks[k][index] = 7;                     /* white */
	}
    }
}


/************************************************************************/
/*   calc various ink-related values                                    */
/************************************************************************/

void get_ink1 (XColor *colourmap, int index, int *p_ink1, int *p_ink2,
    int *p_n1, int *p_n2, int *p_nb)
{
    float hue, sat, val;
    {
	float red, green, blue;

	get_rgb (colourmap, index, &red, &green, &blue);
	rgb_2_hsv (red, green, blue, &hue, &sat, &val);
    }
    *p_nb = MAXDOTS*(1.0-val);
    {
	int ink1 = *p_ink1 = hue/60.0;

	*p_ink2 = (ink1+1)%6;
    }
    {
	int ncol = (int)round(MAXDOTS*sat*val);
	float p2 = (fmod (hue, 60.0))/60.0;
	int n2 = *p_n2 = (int)round(ncol*p2);

	*p_n1 = ncol - n2;
    }
}


/************************************************************************/
/*   Return normalised r g b values, as obtained from colourmap         */
/************************************************************************/

void get_rgb (XColor *colourmap, int index, float *p_red, float *p_green,
    float *p_blue)
{
    *p_red   = (float)(colourmap[index].red)/65535.0;
    *p_green   = (float)(colourmap[index].green)/65535.0;
    *p_blue   = (float)(colourmap[index].blue)/65535.0;
}


/************************************************************************/
/* C  version of Clive Page's SUBROUTINE GRXHSV, to convert red, green, */
/* blue to hue, sat, val (from algorithm in Foley & van Dam's           */
/* "Fundamentals of Interactive Graphics", p615).                       */
/************************************************************************/

void rgb_2_hsv (float red, float green, float blue, float *p_hue,
    float *p_sat, float *p_val)
{
    float vmax = *p_val = max3f (red, green, blue);
    float vmin = min3f (red, green, blue);
    float diff = vmax - vmin;
    float sat = *p_sat = (vmax > 0.0) ? diff/vmax : 0.0;

    if (sat == 0.0)
	*p_hue = 0.0;    /* though actually undefined */
    else
    {
	float rc = (vmax-red)/diff;
	float gc = (vmax-green)/diff;
	float bc = (vmax-blue)/diff;
	float temp;

	if (red >= vmax)
	    temp = bc - gc;
	else if (green >= vmax)
	    temp = 2 + rc - bc;
	else if (blue >= vmax)
	    temp = 4 + gc - rc;
	*p_hue = fmod ( 60.0*(temp + 2.0), 360.0);
    }
}


/************************************************************************/
/*  returns max of three reals                                          */
/************************************************************************/

float max3f (float x, float y, float z)
{
   x = MAX(x, y);
   x = MAX(x, z);
   return (x);
}


/************************************************************************/
/*  returns min of three reals                                          */
/************************************************************************/

float min3f (float x, float y, float z)
{
   x = MIN(x, y);
   x = MIN(x, z);
   return (x);
}


/************************************************************************/
/*  Get various parameters, including number of output pixels per row,  */
/*  which is then output.                                               */
/************************************************************************/

void send_nxo (FILE *out, XImage *image, int *p_nyi, int *p_nxi,
    int *p_nxo, float *p_expand, float *p_dimin)
{
    *p_nyi = image->height;
    *p_nxi = image->width;
    {
	float tx = MAXX/(float) *p_nxi;
	float ty = MAXY/(float) *p_nyi;

	*p_expand = min3f (tx, ty, 10.0);
    }
    *p_dimin = 1.0/(*p_expand);
    {
	int temp = (int)round(*p_expand*(*p_nxi));

	*p_nxo = MIN (MAXX, temp);
    }
    fprintf (out, "%s%i%s", "\033*r", *p_nxo, "S");
}


/************************************************************************/
/*  get locx, where xi = locx[xo]x                                      */
/************************************************************************/
void get_locx (int nxi, int nxo, float dimin, int locx[MAXX])
{
    int j;

    for (j = 0;  j < nxo;  j++) locx[j] = MIN (nxi - 1, (int)round(j*dimin) );
}

/************************************************************************/
/* Generates the LJ250 representation of a row of pixels from the screen*/
/************************************************************************/

void handle_row (FILE *out, XImage *image, int mask[NINKS][NPLANES][NBITS],
    int inks[MAXDOTS][MAXCOLOURS], int locx[MAXX], int nyi, int nxo,
    float expand, float dimin, int yo)
{

	/********************************/
	/* loop over printer pixel rows */
	/********************************/
	int plane0[MAXX_BYTES],
          plane1[MAXX_BYTES],
          plane2[MAXX_BYTES],
          nWords;

	get_planes (image, mask, inks, locx, nyi, nxo, yo,
		  dimin, &nWords, plane0, plane1, plane2);
	handle_plane (out, nWords, plane0);
	handle_plane (out, nWords, plane1);
	handle_plane (out, nWords, plane2);
	fprintf (out, "\033*b0W");          /* send graphics linefeed */
}


/************************************************************************/
/* Return three planes to be sent to the printer, given a row of pixels */
/* from  the screen.                                                    */
/************************************************************************/

void get_planes ( XImage *image, int mask[NINKS][NPLANES][NBITS],
    int inks[MAXDOTS][MAXCOLOURS], int locx[MAXX], int nyi, int nxo, int yo,
    float dimin, int *p_nWords, int plane0[MAXX_BYTES],
    int plane1[MAXX_BYTES], int plane2[MAXX_BYTES])
{
    int yi = MIN (nyi - 1, (int)round(yo*dimin));
    /*******************************************/
    /* yi = current rasterfile pixel row index */
    /*******************************************/
    int modyo = yo%4;
    int iWord;
    int xo;
    int matrix[4][4] =
	{{5, 1, 4, 0},
	 {3, 7, 2, 6},
	 {4, 0, 5, 1},
	 {2, 6, 3, 7}};

    clear_planes (nxo, plane0, plane1, plane2);
    for (xo = 0;  xo < nxo;  xo++)
    {
	/********************************/
	/* loop over printer pixel cols */
	/********************************/
	int modxo = xo & 3;
	int idot = matrix[modxo][modyo];
	int iBit = xo & 7;
	int lvl, ink;

        lvl = XGetPixel( image, locx[xo],  yi);
	ink = inks[idot][lvl];
	iWord = xo >> 3;
	plane0[iWord] |= mask[ink][0][iBit];
	plane1[iWord] |= mask[ink][1][iBit];
	plane2[iWord] |= mask[ink][2][iBit];
    }
    *p_nWords = iWord + 1;
}


/************************************************************************/
/* just resets the plane0/1/2 arrays to zero                            */
/************************************************************************/
void clear_planes (int nxo, int plane0[MAXX_BYTES], int plane1[MAXX_BYTES],
   int plane2[MAXX_BYTES])
{
    int i;
    int count = nxo/8;

    for (i = 0;  i < count;  i++)
	plane0[i] = plane1[i] = plane2[i] = 0;
}


/************************************************************************/
/*  Get run-length-encoded plane, and output whichever is most compact. */
/************************************************************************/
void handle_plane (FILE *out, int nBytes, int plane[MAXX_BYTES])
{
    int rle_plane[MAXRLE];
    int nBytes_rle = encode (plane, nBytes, rle_plane);
    static int was_rle = 0;
    int use_rle = nBytes_rle < nBytes;
    int change = use_rle != was_rle;

    if (use_rle)
	send_plane (out, change, use_rle, nBytes_rle, rle_plane);
	/* run-length encoded version more compact */
    else
	send_plane (out, change, use_rle, nBytes,         plane);
	/* none encoded version more compact */
    if (change)
	was_rle = was_rle ? 0 : 1;
}


/************************************************************************/
/* create the run-length-encoded version of array plane                 */
/************************************************************************/
int encode (int plane[MAXX_BYTES], int nBytes, int rle_plane[MAXRLE])
{
    int index = 0;
    int count = 0;
    int i;

    for (i = 1;  i < nBytes;  i++)
    {
	if (plane[i] == plane[i-1])
            count++;
	else
	{
	    rle_plane[index++] = count;
	    rle_plane[index++] = plane[i-1];
	    count = 0;
	}
    }
    rle_plane[index++] = count;
    rle_plane[index] = plane[i-1];
    return (index + 1);
}


/************************************************************************/
/*  Output plane i (bit i of every pixel in a given row).               */
/************************************************************************/

void send_plane (FILE *out, int change, int is_rle, int nBytes,
    int plane[MAXX_BYTES])
{
    if (change)
	fprintf (out, "%s%d%s", "\033*b", (int) is_rle, "M");
    fprintf (out, "%s%d%s", "\033*b", nBytes, "V");
    {
	int i;

	for (i = 0;  i < nBytes; i++)
	{
	    fputc (plane[i], out);
	}
    }
}
#if HAVE_JPEGLIB_H
static void DoJpeg(ClientData clientData)
{
/*
**  Work routine for writing a compressing a line of jpeg
*/
    Gwm *gwmPtr = (Gwm*)clientData;
    int i, k;
    unsigned long pix, rpix, gpix, bpix;
    JSAMPROW row_pointer[1];

    if ( gwmPtr->j < gwmPtr->height )
    {
    /*
    **  For each pixel in the line
    */
        for ( i = 0, k = 0; i < gwmPtr->width; i++ )
        {
           pix = XGetPixel( gwmPtr->image, i, gwmPtr->j );

           if (gwmPtr->info->visclass == DirectColor ||
                 gwmPtr->info->visclass == TrueColor ) {

               rpix = (pix & gwmPtr->vinfo->red_mask) / gwmPtr->rbase;
               gpix = (pix & gwmPtr->vinfo->green_mask) / gwmPtr->gbase;
               bpix = (pix & gwmPtr->vinfo->blue_mask) / gwmPtr->bbase;
               gwmPtr->buffer[k++] = (gwmPtr->ctr[rpix].red >> 8) & 0xff;
               gwmPtr->buffer[k++] = (gwmPtr->ctg[gpix].green >> 8) & 0xff;
               gwmPtr->buffer[k++] = (gwmPtr->ctb[bpix].blue >> 8) & 0xff;
            } else {
               gwmPtr->buffer[k++] = (gwmPtr->ct[pix].red >> 8) & 0xff;
               gwmPtr->buffer[k++] = (gwmPtr->ct[pix].green >> 8) & 0xff;
               gwmPtr->buffer[k++] = (gwmPtr->ct[pix].blue >> 8) & 0xff;
            }
        }

    /*
    ** send to compression engine
    */
        row_pointer[0] = (JSAMPROW)gwmPtr->buffer;
        jpeg_write_scanlines(gwmPtr->cinfo, row_pointer, 1);

    /*
    **  Decrement line counter
    */
        gwmPtr->j++;

    /*
     *  Arrange to be called again
     */
	Tcl_DoWhenIdle(DoJpeg, gwmPtr);
    }
    else
    {

    /*
    **  Finish compression
    */
        jpeg_finish_compress(gwmPtr->cinfo);

    /*
    **  Close file and clean up
    */
        fclose( gwmPtr->out );
        jpeg_destroy_compress( gwmPtr->cinfo );
        if (gwmPtr->info->visclass == DirectColor ||
             gwmPtr->info->visclass == TrueColor ) {
           free( (void*)gwmPtr->ctr );
           free( (void*)gwmPtr->ctg );
           free( (void*)gwmPtr->ctb );
        } else {
           free( (void*)gwmPtr->ct );
        }
        free( (void*)gwmPtr->buffer );
        free( (void*)gwmPtr->jerr );
        free( (void*)gwmPtr->cinfo );

    /*
     *  Clear the printing in progress flag and set the print variable
     */
	gwmPtr->printing = 0;
	Tcl_Release((ClientData) gwmPtr);
	Tcl_SetVar(gwmPtr->interp, gwmPtr->printvar, "1",
		TCL_GLOBAL_ONLY);
    }
}
#endif
