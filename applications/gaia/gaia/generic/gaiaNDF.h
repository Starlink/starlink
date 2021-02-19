#ifndef _GAIANDF_INCLUDED_
#define _GAIANDF_INCLUDED_

/*
 *  External prototypes for gaiaNDF.c.
 */

/*  Maximum character length of NDF filename or HDS path */
#define MAXNDFNAME 256

#ifdef __cplusplus
extern "C" {
#endif

#include <ast.h>
#include <star/hds.h>

/*  Single NDF interface */
/*  ==================== */

   /*  Open an NDF */
   int gaiaAccessNDF( const char *filename, int *type, int *width, int *height,
                      char **header, int *header_length, int *ndfid,
                      char **error_message );
   /*  Write an NDF */
   int gaiaWriteNDF( const char *filename, int type, int width, int height,
                     void *data, int ndfid, const char *component,
                     const char *header, size_t lheader,
                     char **error_message );
   /*  Free an NDF */
   int gaiaFreeNDF( int ndfid );

   /*  Copy NDF data component */
   int gaiaCopyComponent( int ndfid, void **data, const char* component,
                          char **error_mess );

   /*  Map an NDF data component */
   int gaiaMapNDF( int ndfid, void **data, const char *component,
                   const char *type, char **error_message );

   /*  Create a very simple NDF. */
   int gaiaCreateNDF( const char *filename, int ndim, int lbnd[], int ubnd[],
                      const char *type, AstFrameSet *wcs, int *indf,
                      char **error_mess );

   /*  Create a new NDF by selective copying of an existing NDF */
    int gaiaCopyNDF( const char *filename, int indf, const char *clist,
                     int ndim, int lbndf[], int ubnd[], const char *type,
                     AstFrameSet *wcs, int axis, int *ondf, char **error_mess );

/*  Multiple NDFs per container interface */
/*  ===================================== */

   /*  Initialise multiple NDF access */
   int gaiaInitMNDF( const char *name, int deepsearch, void **handle,
                     char **error_mess );

   /*  Check component of NDF exists */
   int gaiaCheckMNDF( const void *handle, int index, const char *component );

   /*  Get info about NDF */
   void gaiaGetInfoMNDF( const void *handle, int index, char **name,
                         int *type, int *width, int *height,
                         char **header, int *hlen, int *ndfid,
                         int *hasvar, int *hasqual );

   /*  Get access to NDF data component */
   int gaiaGetMNDF( const void *handle, int index, const char *component,
                    void **data, char **error_mess );

   /*  Release all NDFs and resources */
   void gaiaReleaseMNDF( const void *handle );

   /*  Release a single NDF */
   void gaiaFreeMNDF( void *handle, int index );

   /*  Return number of NDFs available. */
   int gaiaCountMNDFs( const void *handle );

   /*  Set readonly status of component */
   void gaiaSetReadMNDF( const void *handle, int index, int readonly );

   /*  Get readonly status of component */
   int gaiaGetReadMNDF( const void *handle, int index );

   /*  Get the NDF identifier */
   int gaiaGetIdMNDF( const void *handle, int index );

   /* Create a cloned copy */
   void *gaiaCloneMNDF( const void *handle );

/*
 *  Straight-forward NDF access, with no 2D bias.
 *  =============================================
 */

    /* Open an NDF and return the identifier */
    int gaiaNDFOpen( char *ndfname, int *ndfid, char **error_mess );

    /* Close an NDF */
    int gaiaNDFClose( int *ndfid );

    /* Get the data type of a component */
    int gaiaNDFType( int ndfid, const char* component, char *type,
                     int type_length, char **error_mess );

    /* Get the value of a character component */
    int gaiaNDFCGet( int ndfid, const char* component, char *value,
                     int value_length, char **error_mess );

    /* Set the value of a character component */
    int gaiaNDFCPut( int ndfid, const char* component, const char *value,
                     char **error_mess );

    /* Query the dimensions of an opened NDF */
    int gaiaNDFQueryDims( int ndfid, int ndimx, int dims[], int *ndim,
                          char **error_mess );

    /* Query the pixel bounds of an opened NDF */
    int gaiaNDFQueryBounds( int ndfid, int ndimx, int lbnd[], int ubnd[],
                            int *ndim, char **error_mess );

    /* Map in an array component */
    int gaiaNDFMap( int ndfid, char *type, const char *access,
                    const char* component, void **data, int *el,
                    char **error_mess );

    /*  Unmap an NDF data component */
    int gaiaNDFUnmap( int ndfid, const char *component, char **error_mess );


    /* Get the NDF WCS component as an AST frameset */
    int gaiaNDFGtWcs( int ndfid, AstFrameSet **iwcs, char **error_mess );

    /* Get a frameset for a specified axis */
    int gaiaNDFGtAxisWcs( int ndfid, int axis, AstFrameSet **iwcs,
                          char **error_mess );

    /* Check if an NDF component exists */
    int gaiaNDFExists( int ndfid, const char *component, int *exists,
                       char **error_mess );

    /* Get a FITS channel for the NDF. */
    int gaiaNDFGetFitsChan( int ndfid, AstFitsChan **fitschan,
                         char **error_mess );

    /* Write a FITS extension using a FITS channel. */
    int gaiaNDFWriteFitsChan( int ndfid, AstFitsChan *fitschan,
                              char **error_mess );

    /* Check if an NDF is opened with write access */
    int gaiaNDFCanWrite( int ndfid );

    /* Check if an extension exists */
    int gaiaNDFExtensionExists( int ndfid, const char *name );

    /* Get value of a property in a named extension */
    int gaiaNDFGetProperty( int ndfid, const char *extension,
                            const char *name, char *value,
                            int value_length, char **error_mess );

    /* Get value of a property in a named extension as double precision */
    int gaiaNDFGetDoubleProperty( int ndfid, const char *extension,
                                  const char *name, double *value,
                                  char **error_mess );

    /* Get dimensions of a property in a named extension */
    int gaiaNDFGetPropertyDims( int ndfid, const char *extension,
                                const char *name, hdsdim dims[], int *ndim,
                                char **error_mess );

    /* Search a given HDS path for the first child that is an NDF. */
    int gaiaNDFFindChild( const char *name, int *ndfid, char **error_mess );

    /* Enerumerate the number of siblings an NDF has. */
    int gaiaNDFSiblingSearch( int ndfid, int deepsearch, int *nsiblings,
                              char **props );

#ifdef __cplusplus
}
#endif

#endif
