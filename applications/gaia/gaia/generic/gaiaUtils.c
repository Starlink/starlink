/*+
 *   Name:
 *      gaiaUtils

 *   Purpose:
 *      Utility routines for GAIA.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include <gaiaUtils.h>
#include <sae_par.h>
#include <ems.h>
#include <ems_par.h>

#define TCL_OK 0
#define TCL_ERROR 1

/*
 *  Name:
 *     gaiaUtilsErrMessage
 *
 *  Purpose:
 *     Copy the current EMS error message into a dynamic string for returning. 
 *     Make sure status is set before calling this and release the memory used
 *     for the string when complete. On return status is reset to SAI__OK and 
 *     the error stack is empty.
 */
char *gaiaUtilsErrMessage()
{
    char *result_copy = NULL;
    char *result_message = NULL;
    char buffer[EMS__SZMSG+1];
    char param[EMS__SZPAR+1];
    int buffer_length = 0;
    int mess_length = 0;
    int param_length = 0;
    int status_copy;

    /* Need to get error message in one pass. Since we don't know how
     * big this is going to be we need to realloc memory used */

    /* Recover error status from EMS */
    emsStat( &status_copy );

    /* Loop until all error has been read */
    while ( status_copy != SAI__OK ) {

        /* Load error message and start reading it */
        emsEload( param, &param_length, buffer, &buffer_length, &status_copy );

        /* If all read then stop */
        if ( status_copy != SAI__OK ) {

            /* Else initialise or increase the result buffer to make room for
             * new part, plus 2 for newline and null. */
            mess_length += ( buffer_length + 2 );
            result_copy = (char *) realloc( (void *) result_message,
                                            (size_t) mess_length );
            if ( result_copy == NULL ) {
                
                /* Realloc failed so use the fragment already recovered in
                 * result_message */
                status_copy = SAI__OK;
            }
            else {
                /* Realloc suceeded */
                result_message = result_copy;
                
                /* Add a newline to end of string to wrap last line, unless
                 * this is first, in which initialise it */
                if ( mess_length == ( buffer_length + 2 ) ) {
                    result_message[0] = '\0';
                }
                else {
                    strcat( result_message, "\n" );
                }
                
                /* Concatenate buffer into result string */
                strncat( result_message, buffer, buffer_length );
            }
        }
    }
    return result_message;
}

/**
 * Get an AST frameset that describes the coordinates of a given axis.
 * Axes are in the AST sense, i.e. start at 1. The offset value is a shift
 * that should be applied to the GRID coordinates (useful when an NDF data
 * component has been sectioned outside of the NDF library, this is the
 * origin).
 */
int gaiaUtilsGtAxisWcs( AstFrameSet *fullwcs, int axis, int offset,
                        AstFrameSet **iwcs, char **error_mess )
{
   AstFrame *tmpframe;
   AstMapping *joined;
   int iaxes[1];
   int nin;
   int nout;

   astBegin;

   /* Determine the current number of input and output axes and
    * take a copy of the full frameset. */
   nin = astGetI( fullwcs, "Nin" );
   nout = astGetI( fullwcs, "Nout" );
   *iwcs = astCopy( fullwcs );

   /* The requested axis must be valid, if not we adopt the
    * default of axis 1. */
   iaxes[0] = axis;
   if ( iaxes[0] > nin ) {
       iaxes[0] = 1;
   }
   else if ( iaxes[0] < 1 ) {
       iaxes[0] = 1;
   }
   
   /* If the base frame has more than one axis then select the given one.
    * This is easy, just pick a frame with the appropriate axes and put it
    * back, note that we have to pick the current frame, so swap things 
    * around a little. */
   if ( nin != 1 ) {
       astInvert( *iwcs );
       joined = NULL;
       tmpframe = astPickAxes( *iwcs, 1, iaxes, &joined );
       astAddFrame( *iwcs, AST__CURRENT, joined, tmpframe );
       astInvert( *iwcs );

       /* If we have an offset to apply to the GRID coordinates (these will be
        * the base frame). Then add in a ShiftMap. */
       if ( offset != 0 ) {
           double shift[1];
           AstShiftMap *map;
           shift[0] = (double) -offset;
           map = astShiftMap( 1, shift, "" );
           astRemapFrame( *iwcs, AST__BASE, map );
           astAnnul( map );
       }
   }

   /* Select an axis in the current frame and tack this onto the
    * end. Same procedure as above, just no inversion. */
   if ( nout != 1 ) {
       joined = NULL;
       tmpframe = astPickAxes( *iwcs, 1, iaxes, &joined );
       astAddFrame( *iwcs, AST__CURRENT, joined, tmpframe );
   }

   /* Export the WCS and report if any errors occurred. */
   astExport( *iwcs );
   astEnd;
   if ( ! astOK ) {
       *error_mess = strdup( "Failed to extract AST frameset for an axis" );
       astClearStatus;
       return TCL_ERROR;
   }
   return TCL_OK;
}

/**
 * Query the equivalent current coordinate of a base coordinate along the
 * given axis.
 *
 * Returns a coordinate value in *coord that may be formatted and trailed by
 * the axis units and label. The position along the axis is identified using
 * ncoords pixel coordinates, where ncoords must match the dimensionality of
 * the FrameSet (at least up to ncoords axes, axes beyond that are handled as
 * dummies). The coordinate returned is matched to the requested axis by being
 * the coordinate with the largest change wrt to a single pixel step. We need
 * to use this logic as the relationship between the base frame axes and the
 * current frame ones may not be straight-forward. Note that the value
 * returned by the coord argument should be immediately copied.
 */
int gaiaUtilsQueryCoord( AstFrameSet *frameset, int axis, double *coords, 
                         int trailed, int formatted, int ncoords, char **coord,
                         char **error_mess )
{
    char *label;
    char *units;
    double *coords_in;
    double diff;
    double out1[7];
    double out2[7];
    double static_coords_in[7];
    double tmp;
    int base;
    int caxis;
    int current;
    int i;
    int ncoords_in;
    int ncoords_out;
    static char buf[256];              /* Static as may be returned */
    static char lcoord[30];            /* Static as may be returned */


    /* 
     * Check dimensionality, if more than ncoords then pad up, if less 
     * then drop them off. 
     */
    current = astGetI( frameset, "Current" );
    base = astGetI( frameset, "Base" );
    astSetI( frameset, "Current", base );
    ncoords_in = astGetI( frameset, "naxes" );
    astSetI( frameset, "Current", current );
    if ( ncoords_in == ncoords ) {
        coords_in = coords;
    }
    else if ( ncoords_in > ncoords ) {
        coords_in = static_coords_in;
        memcpy( coords_in, coords, ncoords * sizeof( double ) );
        for ( i = ncoords; i < ncoords_in; i++ ) {
            coords_in[i] = AST__BAD;
        }
    }

    /*  Transform the position from base coordinates to current coordinates
     *  for this position, and a position one units offset along the given
     *  axis. */
    ncoords_out = astGetI( frameset, "naxes" );

    coords_in[axis-1] += 1.0;
    astTranN( frameset, 1, ncoords_in, 1, coords_in, 1, ncoords_out, 1, out1 );
    coords_in[axis-1] -= 1.0;
    astTranN( frameset, 1, ncoords_in, 1, coords_in, 1, ncoords_out, 1, out2 );

    /*  Select the axis with the largest shift as equivalent */
    caxis = axis;
    diff = 0.0;
    for ( i = 0; i < ncoords_out; i++ ) {
        tmp = fabs( out1[i] - out2[i] );
        if ( tmp > diff ) {
            caxis = i + 1;
            diff = tmp;
        }
    }

    /*  Format the value along that axis, if requested, otherwise just write
     *  out the double. */
    if ( formatted ) {
        *coord = (char *) astFormat( frameset, caxis, out2[caxis - 1] );
    }
    else {
        sprintf( lcoord, "%.17g", out2[caxis -1] );
        *coord = lcoord;
    }

    if ( ! astOK ) {
        astClearStatus;
        *coord = NULL;
        *error_mess = strdup( "Failed to convert to world coordinate" );
        return TCL_ERROR;
    }

    /*  Add the axis units and label if requested. */
    if ( trailed ) {
        sprintf( buf, "unit(%d)", caxis );
        units = (char *) astGetC( frameset, buf );
        sprintf( buf, "label(%d)", caxis );
        label = (char *) astGetC( frameset, buf );
        sprintf( buf, "%s %s (%s)", *coord, label, units );
        *coord = buf;
    }
    if ( ! astOK ) {
        astClearStatus;
    }
    return TCL_OK;
}
