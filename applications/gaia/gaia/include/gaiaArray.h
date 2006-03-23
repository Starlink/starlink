#ifndef _GAIAARRAY_INCLUDED_
#define _GAIAARRAY_INCLUDED_
 
/*
 *  External prototypes and definitions for gaiaArray.c.
 */

/* Enumeration for the support data types, these are supposed to match the HDS
   ones. */
enum { HDS_UNKNOWN = -1, 
       HDS_UBYTE, HDS_BYTE, HDS_UWORD, HDS_WORD, HDS_INTEGER, HDS_REAL,
       HDS_DOUBLE };

#ifdef __cplusplus
extern "C" {
#endif
    /* Convert an HDS type into one of the enums */
    int gaiaArrayHDSType( char *typePtr );
    

    /* Convert an array of data into double precision */
    void gaiaArrayToDouble( void *inPtr, int nel, int type, double badValue, 
                            double *outPtr );

    /*  Get a section of an existing array */
    void gaiaArraySection( void *dataPtr, char dataType, int ndim, int dims[],
                           int lbnd[], int ubnd[], void **sectionPtr );

#ifdef __cplusplus
}
#endif

#endif
