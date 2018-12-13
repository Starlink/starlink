/*+
 *  Name:
 *     GaiaFITS

 *  Purpose:
 *     Primitive access to generalised FITS for GAIA.

 *  Language:
 *     C++

 *  Notes:
 *     C++ so we can easily make use of Skycat classes, not an actual class
 *     itself (although that is a good idea longer term).

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper, Starlink - University of Durham

 *  History:
 *     05-APR-2006 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string>
#include <sstream>
#include <cstdio>
#include <unistd.h>
#include <cstring>
#include "StarFitsIO.h"
extern "C" {
#include <ast.h>
}
#include "gaiaUtils.h"

/* Standard return values, success and failure to match TCL */
static const int TCL_OK = 0;
static const int TCL_ERROR = 1;

/* Maximum dimensions */
static const int MAX_DIM = 7;

//  Number of characters in a FITS header card.
static const int FITSCARD = 80;

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
        *right = '\0';
        *left = '\0';
        sscanf( left + 1, "%d", hdu );
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
 *  Local function to extract the context from a FITS channel
 *  set up handle -TAB requests.
 */ 
static void staticLoadTabTable( AstFitsChan *chan, const char *extname,
                                int extver, int extlevel, int *status )
{
    /*  Get the instance and call the read member. */
    StarFitsIO *instance = (StarFitsIO *) astChannelData;
    *status = 0;
    instance->loadTabTable( chan, extname, extver, extlevel, status );

    /* Local status is 0 for success, AST wants that to be 1. */
    if ( *status == 0 ) {
        *status = 1;
    }
}

/**
 * Get the WCS as an AstFrameSet from the current HDU headers. Also returns
 * the array dimensions. The dims array should have MAX_DIM elements. The
 * number of dimensions returned is ndims. If no valid WCS is found a dummy
 * one (with the correct dimensionality) connecting GRID and PIXEL is
 * returned, this will include the pixel origins, if any LBOUND keywords are
 * present.
 */
int GaiaFITSGtWcs( StarFitsIO *fitsio, AstFrameSet **iwcs,
                   int dims[], int *ndims )
{
    astBegin;

    Mem mem = fitsio->header();
    char *header = (char *)mem.ptr();
    size_t lheader = mem.size();

    // Read headers using a FITS channel.
    AstFitsChan *fitschan = NULL;
    int ncard = (int) ( lheader / (size_t) FITSCARD );
    gaiaUtilsGtFitsChan( (char *) header, ncard, &fitschan );

    // Look for the image dimensions.
    astClear( fitschan, "Card" );
    int i = 0;
    char card[FITSCARD+1];
    char *ptr;
    while( astFindFits( fitschan, "NAXIS%d", card, 1 ) ) {
        if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
            sscanf( ++ptr, "%d", &dims[i++] );
        }
    }
    *ndims = i;

    // Prepare to handle the -TAB format.
    astSetI( fitschan, "TabOK", 1 );
    astTableSource( fitschan, staticLoadTabTable );

    // Store the StarFitsIO pointer for context.
    astPutChannelData( fitschan, fitsio );

    // Now try to read in the FITS headers to create a frameset.
    astClear( fitschan, "Card" );
    *iwcs = (AstFrameSet *) astRead( fitschan );

    if ( *iwcs == AST__NULL ) {
        if ( ! astOK ) astClearStatus;

        // No (valid) frameset, just create a basic one with a GRID and PIXEL
        // domain. Check for LBOUND values, these are used by CONVERT to set
        // the NDF origin.
        double dlbnd[MAX_DIM];
        for ( i = 0; i < MAX_DIM; i++ ) {
            dlbnd[i] = 0.0;
        }

        astClear( fitschan, "Card" );
        int lbnd;
        i = 0;
        while( astFindFits( fitschan, "LBOUND%d", card, 1 ) ) {
            if ( ( ptr = strstr( card, "=" ) ) != (char *)NULL ) {
                sscanf( ++ptr, "%d", &lbnd );
                dlbnd[i] = (double) ( lbnd - 1 );
                i++;
            }
        }

        AstFrame *f1 = astFrame( *ndims, "Domain=GRID" );
        AstFrame *f2 = astFrame( *ndims, "Domain=PIXEL" );
        AstShiftMap *map = astShiftMap( *ndims, dlbnd, " " );
        *iwcs = astFrameSet( f1, " " );
        astAddFrame( *iwcs, AST__BASE, map, f2 );
    }

    astExport( *iwcs );
    astEnd;
    return TCL_OK;
}

/**
 * Create a FITS file and write a data array, plus WCS, to its primary
 * extension.
 */
int GaiaFITSCreate( const char* filename, void *data,
                    AstFrameSet *wcs, int bitpix, double bscale,
                    double bzero, long blank, const char *object,
                    const char *units, int naxis, long naxes[] )
{
    /* Delete old file if it already exists */
    unlink( filename );

    int status = 0;
    fitsfile *fptr;
    if ( fits_create_file( &fptr, filename, &status ) != 0 ) {
        return TCL_ERROR;
    }

    /* Create the primary image extension */
    if ( fits_create_img( fptr, bitpix, naxis, naxes, &status ) != 0 ) {
        return TCL_ERROR;
    }

    /* Convert bitpix into a datatype */
    int needbscale = 0;
    int datatype = TBYTE;
    switch (bitpix) {
    case 8:
    case -8:
        datatype = TBYTE;
        needbscale = 1;
        break;
    case 16:
        datatype = TSHORT;
        needbscale = 1;
        break;
    case -16:
        datatype = TUSHORT;
        needbscale = 1;
        break;
    case 32:
        datatype = TINT;
        needbscale = 1;
        break;
    case -32:
        datatype = TFLOAT;
        break;
    case 64:
        datatype = TLONGLONG;
        needbscale = 1;
        break;
    case -64:
        datatype = TDOUBLE;
        break;
    }

    /*  Set the OBJECT and BUNIT values */
    if ( object != NULL && object[0] != '\0' ) {
        fits_write_key_str( fptr, "OBJECT", (char *) object, "Data object",
                            &status );
    }
    if ( units != NULL && units[0] != '\0' ) {
        fits_write_key_str( fptr, "BUNIT", (char *) units, "Data units",
                            &status );
    }

    /*  Add any BSCALE, BZERO and BLANK values */
    if ( needbscale ) {
        fits_write_key( fptr, TDOUBLE, "BSCALE", &bscale, "Data scale",
                        &status );
        fits_write_key( fptr, TDOUBLE, "BZERO", &bzero, "Data zero point",
                        &status );
        fits_write_key( fptr, TLONG, "BLANK", &blank, "Blank value",
                        &status );

        /* Do not apply these when writing data as these are implicit. */
        fits_set_bscale( fptr, 1.0, 0.0, &status );
    }

    /* Write WCS into a FITS channel. Attempt to write headers using a
     * FITS-WCS encoding, if that fails we use a Native encoding */
    if ( wcs != NULL ) {
        AstFitsChan *chan = astFitsChan( NULL, NULL, " " );
        astSet( chan, "Encoding=FITS-WCS" );
        int nwrite = astWrite( chan, wcs );
        if ( !astOK || nwrite == 0 ) {
            astClearStatus;
            astSet( chan, "Encoding=Native" );
            nwrite = astWrite( chan, wcs );
        }

        /* Now read the channel and write cards to the FITS file. */
        char card[FITSCARD+1];
        astClear( chan, "Card" );
        int ncard = astGetI( chan, "Ncard" );
        for ( int i = 0; i < ncard; i++ ) {
            astFindFits( chan, "%f", card, 1 );
            fits_write_record( fptr, card, &status );
        }
        astAnnul( chan );
    }

    /*  Finally write the data array to the file. */
    long nel = 1L;
    for ( int i = 0; i < naxis; i++ ) {
        nel *= naxes[i];
    }
    if ( fits_write_img( fptr, datatype, 1L, nel, data, &status ) != 0 ) {
        return TCL_ERROR;
    }

    fits_close_file( fptr, &status );

    return TCL_OK;
}

/**
 * Get the value of a FITS keyword. Returns the empty string if not found.
 */
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, char *value,
                  int value_length )
{
    fitsio->get( keyword, value, value_length );
    return TCL_OK;
}

/**
 * Get the integer value of a FITS keyword. Returns TCL_ERROR if not found.
 */
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, int *value )
{
    if ( fitsio->get( keyword, *value ) == 0 ) {
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Get the double precision value of a FITS keyword. Returns TCL_ERROR if not
 * found.
 */
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, double *value )
{
    if ( fitsio->get( keyword, *value ) == 0 ) {
        return TCL_OK;
    }
    return TCL_ERROR;
}

/**
 * Write a FITS card as a keyword, value, comment set. The type should be
 * "char" or "numeric". If the later then the value will be written without
 * quotes.
 */
int GaiaFITSHPut( StarFitsIO *fitsio, const char *keyword, const char *value,
                  const char *comment, const char *type )
{
    char card[81];
    if ( type[0] == 'n' ) {
        /* Numeric value, no quotes */
        if ( strlen( value ) > 20 ) {
            sprintf( card, "%-8.8s= %s / %s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s= %20.20s / %s", keyword, value, comment );
        }
    }
    else {
        /* Char */
        if ( strlen( value ) > 18 ) {
            sprintf( card, "%-8.8s= '%s' / %s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s= '%18.18s' / %s", keyword, value, comment );
        }
    }
    fitsio->putcard( card );
    return TCL_OK;
}

/**
 * Write a FITS card as a keyword, value, comment set.
 */
int GaiaFITSHPutCard( StarFitsIO *fitsio, const char *card )
{
    fitsio->putcard( card );
    return TCL_OK;
}


/*
 * Return a string containing a Tcl list with various meta-data descriptions
 * of the extensions in the given FITS file. The meta-data values are:
 *
 * "HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3 CRPIX1 CRPIX2"
 *
 * Note that if an extension contains a compressed image the string
 * "COMPRESSED_IMAGE" will be part of the extname.
 */
int GaiaFITSHduList( StarFitsIO* fits, string &result )
{
    int numHDUs = fits->getNumHDUs();
    if ( numHDUs <= 0 ) {
        //  No HDUs.
        return TCL_OK;
    }

    //  Save current HDU, then loop through all HDUs to get info
    int curHDU = fits->getHDUNum();
    ostringstream os;
    int status = 0;
    int count = 0;
    for ( int i = 1; i <= numHDUs; i++ ) {
        if ( fits->setHDU( i ) != 0 ) {
            status++;
            break;
        }
        const char* type = fits->getHDUType();
        if ( !type ) {
            status++;
            break;
        }

        //  Get these keyword values and default to "".
        char extName[80];
        int naxis, naxis1, naxis2, naxis3;
        double crpix1, crpix2;
        fits->get( "EXTNAME", extName, 80 );
        fits->get( "NAXIS", naxis );
        fits->get( "NAXIS1", naxis1 );
        fits->get( "NAXIS2", naxis2 );
        fits->get( "NAXIS3", naxis3 );
        fits->get( "CRPIX1", crpix1 );
        fits->get( "CRPIX2", crpix2 );

        //  Is this compressed? ZIMAGE will be T or extname already contains
        //  the COMPRESSED_IMAGE string.
        if ( strstr( extName, "COMPRESSED_IMAGE" ) == NULL ) {
            int zimage = 0;
            fits->get( "ZIMAGE", zimage );
            if ( zimage ) {
                strcat( extName, "(COMPRESSED_IMAGE)" );
            }
        }

        os << "{"
           << i
           << " " << type
           << " {" << extName << "}"
           << " {" << naxis << "}"
           << " {" << naxis1 << "}"
           << " {" << naxis2 << "}"
           << " {" << naxis3 << "}"
           << " {" << crpix1 << "}"
           << " {" << crpix2 << "}"
           << "} ";
        count++;
    }
    if ( count > 0 ) {
        fits->setHDU( curHDU );
        if ( status == 0 ) {
            result = os.str();
            return TCL_OK;
        }
    }
    return TCL_ERROR;
}
