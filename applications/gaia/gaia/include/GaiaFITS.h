#ifndef _GAIAFITS_INCLUDED_
#define _GAIAFITS_INCLUDED_

/*
 *  External prototypes and definitions for GaiaFITS.C.
 */

/* Open a FITS file and position at a given HDU */
StarFitsIO *GaiaFITSOpen( const char* filename, int hdu );

/* Close a FITS file */
int GaiaFITSClose( StarFitsIO *fitsio );

/* Parse a FITS specification into a file name and HDU */
int GaiaFITSParseName( const char *spec, char *name, int name_length,
                       int *hdu );

/* Access the data of a FITS HDU */
int GaiaFITSDataMem( StarFitsIO *fitsio, Mem **dataPtr );

/* Unmap the data of a FITS HDU */
void GaiaFITSUnmap( Mem *dataPtr );

/* Get the FITS WCS from the FITS headers and the associated data HDU
   dimensions */
int GaiaFITSGtWcs( StarFitsIO *fitsio, AstFrameSet **iwcs,
                   int dims[], int *ndims );

/* Get a FITS card value */
int GaiaFITSHGet( StarFitsIO *fitsio, char *keyword, char *value,
                  int value_length );
int GaiaFITSHGet( StarFitsIO *fitsio, char *keyword, int *value );

#endif
