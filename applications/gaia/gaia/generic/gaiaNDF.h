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

/*  Simple NDF interface */
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
   int gaiaCopyNDF( int ndfid, void **data, const char *component,
                    char **error_message );

   /*  Map an NDF data component */
   int gaiaMapNDF( int ndfid, void **data, const char *component,
                   char **error_message );

   /*  Unmap an NDF data component */
   int gaiaNDFUnmap( int ndfid, const char *component, char **error_mess );


/*  Multiple NDFs per container interface */
/*  ===================================== */

   /*  Initialise multiple NDF access */
   int gaiaInitMNDF( const char *name, void **handle, char **error_mess );

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
 *  These are only used to query NDF bounds, so that we can section up cubes,
 *  plus some toys for playing around (see gaiaNDFTcl.c).
 */

    /* Open an NDF and return the identifier */
    int gaiaSimpleOpenNDF( char *ndfname, int *ndfid, char **error_mess );

    /* Close an NDF */
    int gaiaSimpleCloseNDF( int *ndfid );

    /* Get the data type of a component */
    int gaiaSimpleTypeNDF( int ndfid, const char* component, char *type, 
                           int type_length, char **error_mess );

    /* Get the value of a character component */
    int gaiaSimpleCGetNDF( int ndfid, const char* component, char *value, 
                           int value_length, char **error_mess );

    /* Query the dimensions of an opened NDF */
    int gaiaSimpleQueryDims( int ndfid, int ndimx, int dims[], int *ndim, 
                             char **error_mess );

    /* Query the pixel bounds of an opened NDF */
    int gaiaSimpleQueryBounds( int ndfid, int ndimx, int lbnd[], int ubnd[], 
                               int *ndim, char **error_mess );

    /* Query the coordinate of a position along the given axis. */
    int gaiaSimpleQueryCoord( int ndfid, int axis, double *coords, 
                              int ncoords, int trailed, char **coord,
                              char **error_mess );

    /* Map in an array component */
    int gaiaSimpleMapNDF( int ndfid, char *type, const char* component, 
                          void **data, int *el, char **error_mess );

    /* Get the NDF WCS component as an AST frameset */
    int gaiaSimpleWCSNDF( int ndfid, AstFrameSet **iwcs, char **error_mess );

    /* Get a frameset for a specified axis */
    int gaiaSimpleAxisWCSNDF( int ndfid, int axis, int offset, 
                              AstFrameSet **iwcs, char **error_mess );

#ifdef __cplusplus
}
#endif

#endif
