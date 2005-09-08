#include "dat_par.h"

void idlDatMapv( const char *loc,
              const char *type,
              const char *mode,
              void * *ptr,
              int *actval,
              int *status );

void idlDatAnnul( char *loc,
               int *status );

void idlDatCell( const char *loc1,
              int ndim,
              const int *sub,
              char *loc2,
              int *status );

void idlDatClen( const char *loc,
              int *clen,
              int *status );

void idlDatClone( const char *loc1,
               char *loc2,
               int *status );

void idlDatFind( const char *loc1,
              const char *name,
              char *loc2,
              int *status );

void idlDatIndex( const char *loc1,
               int index,
               char *loc2,
               int *status );

void idlDatName( const char *loc,
              char *name,
              int *status );

void idlDatNcomp( const char *loc,
               int *ncomp,
               int *status );

void idlDatNew( const char *loc,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             int *status );

void idlDatPrim( const char *loc,
              int *reply,
              int *status );

void idlDatPrmry( int set,
               char *loc,
               int *prmry,
               int *status );

void idlDatShape( const char *loc,
               int ndimx,
               int *dim,
               int *ndim,
               int *status );

void idlDatSize( const char *loc,
              int *size,
              int *status );

void idlDatSlice( const char *loc1,
               int ndim,
               const int *diml,
               const int *dimu,
               char *loc2,
               int *status );

void idlDatState( const char *loc,
               int *reply,
               int *status );

void idlDatStruc( const char *loc,
               int *reply,
               int *status );

void idlDatUnmap( const char *loc,
               int *status );

void idlDatVec( const char *loc1,
             char *loc2,
             int *status );

void idlDatPut( const char *loc,
             const char *type,
             int ndim,
             const int *dim,
             void *value,
             int *status );

void idlDatPutc( const char *loc,
              int ndim,
              const int *dim,
              char *const *value,
              int value_length,
              int *status );

void idlDatType( const char *loc,
              char *type,
              int *status );

void idlHdsNew( const char *file,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             char *loc,
             int *status );

void idlHdsOpen( const char *file,
              const char *mode,
              char *loc,
              int *status );
