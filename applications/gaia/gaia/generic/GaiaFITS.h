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
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, char *value,
                  int value_length );
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, int *value );
int GaiaFITSHGet( StarFitsIO *fitsio, const char *keyword, double *value );

/* Write a FITS card */
int GaiaFITSHPutCard( StarFitsIO *fitsio, const char *card );

/* Write a FITS card based on character kvc */
int GaiaFITSHPut( StarFitsIO *fitsio, const char *keyword, const char *value,
                  const char *comment, const char *type );

/* Create a FITS file from a data array and a WCS */
int GaiaFITSCreate( const char* filename, void *data,
                    AstFrameSet *wcs, int bitpix, double bscale,
                    double bzero, long blank, const char *object,
                    const char *units, int naxis, long naxes[] );

/* Meta-list of MEF contents */
int GaiaFITSHduList( StarFitsIO *fitsio, string &result );

#endif
