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

    /* Get an image section from a cube */
    void gaiaArrayImageFromCube( void *inPtr, char type, int dims[3],
                             int axis, int index, void **outPtr,
                                 int cnfmalloc );
        
    /* Get a spectrum (line of data) from a cube */
    void gaiaArraySpectrumFromCube( void *inPtr, char type, int dims[3],
                                    int axis, int index1, int index2,
                                    int cnfmalloc, void **outPtr, int *nel );

    /* Get strides for indexing an ND array */ 
    void gaiaArrayGetStrides( int ndims, int dims[], int strides[] );

#ifdef __cplusplus
}
#endif

#endif
