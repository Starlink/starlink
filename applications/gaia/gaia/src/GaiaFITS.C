/*+
 *   Name:
 *      GaiaFITS

 *   Purpose:
 *      Primitive access to generalised FITS for GAIA.

 *   Language:
 *      C++

 *   Notes:
 *      C++ so we can easily make use of Skycat classes, not an actual class
 *      itself (although that is a good idea longer term).

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      05-APR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <cstdio>
#include <cstring>
#include "StarFitsIO.h"
extern "C" {
#include <ast.h>
}

/* Standard return values, success and failure to match TCL */
#define TCL_OK 0
#define TCL_ERROR 1

/* Maximum dimensions */
#define MAX_DIM 7

/**
 * Open a FITS file and move to the indicated extension. Returns a StarFitsIO
 * instance that is backing the file access.
 */
StarFitsIO *GaiaFITSOpen( const char* filename, int hdu )
{
    StarFitsIO *fitsio = StarFitsIO::read( filename, Mem::FILE_DEFAULTS );
    if ( fitsio != NULL ) {
        if ( fitsio->setHDU( hdu ) != 0 ) {
            delete fitsio;
            return NULL;
        }
    }
    return fitsio;
}

/**
 * Close a FITS file. Just deletes the underlying object, it does the real
 * work.
 */
int GaiaFITSClose( StarFitsIO *fitsio )
{
    delete fitsio;
    return TCL_OK;
}

/**
 * Parse a FITS specification into a filename and a HDU number. "name" should
 * be at least as long as spec.
 */
int GaiaFITSParseName( const char *spec, char *name, int name_length, 
                       int *hdu )
{
    // Copy input to output.
    strncpy( name, spec, name_length );

    // Look for a FITS extension [n].
    char *left = strrchr( name, '[' );
    char *right = strrchr( name, ']' );

    // If found terminate the name string and try to read the integer.
    if ( left && right ) {
        left = '\0';
        right = '\0';
        sscanf( left + 1, "%d", *hdu );
    }
    else {
        /* Primary HDU is 1. */
        *hdu = 1;
    }
    return TCL_OK;
}

/**
 * Return the Mem object for the data section of the current HDU.
 */
int GaiaFITSDataMem( StarFitsIO *fitsio, Mem **dataPtr )
{
    *dataPtr = new Mem( fitsio->data() );
    return TCL_OK;
}

/**
 * Unmap a data part.
 */
void GaiaFITSUnmap( Mem *dataPtr )
{
    delete dataPtr;
}

/**
 * Get the WCS as an AstFrameSet from the current HDU headers. Also returns
 * the array dimensions. The dims array should have MAX_DIM elements. The
 * number of dimensions returned is ndims.
 */
int GaiaFITSGtWcs( StarFitsIO *fitsio, AstFrameSet **iwcs, 
                   int dims[], int *ndims )
{
    astBegin;

    Mem mem = fitsio->header();
    char *header = (char *)mem.ptr();
    int lheader = mem.size();

    // Read headers using a FITS channel.
    AstFitsChan *fitschan = astFitsChan( NULL, NULL, "" );
    char card[81];
    char *ptr = (char *) header;
    int ncard = (int) lheader / 80;
    for ( int i = 0 ; i < ncard; i++, ptr += 80 ) {
        memcpy( card, (void *)ptr, (size_t) 80 );
        card[80] = '\0';

        //  Read all cards up to, but not including, the END card.
        if ( ! ( card[0] == 'E' && card[1] == 'N' && card[2] == 'D'
                 && ( card[3] == '\0' || card[3] == ' ' ) ) ) {
            astPutFits( fitschan, card, 0 );
            if ( !astOK ) {
                //  If an error occurs with a card, just continue, it's almost
                //  certainly something trivial like a formatting problem.
                astClearStatus;
            }
        }
        else {
            break;
        }
    }

    // Look for the image dimensions.
    astClear( fitschan, "Card" );
    int i = 0;
    while( astFindFits( fitschan, "NAXIS%d", card, 1 ) ) {
        if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
            sscanf( ++ptr, "%d", &dims[i++] );
        }
    }
    *ndims = i;

    // Now try to read in the FITS headers to create a frameset
    astClear( fitschan, "Card" );
    *iwcs = (AstFrameSet *) astRead( fitschan );

    if ( *iwcs == AST__NULL ) {
        astClearStatus;
        astEnd;
        return TCL_ERROR;
    }

    astExport( *iwcs );
    astEnd;
    return TCL_OK;
}

/** 
 * Get the value of a FITS keyword.
 */
int GaiaFITSHGet( StarFitsIO *fitsio, char *keyword, char *value, 
                  int value_length )
{
    char *result = fitsio->get( keyword, value, value_length );
    if ( result[0] == '\0' ) {
        return TCL_ERROR;
    }
    return TCL_OK;
}
