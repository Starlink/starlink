#include "dat_par.h"
void cmpGet0c( const char *loc,
               const char *name,
               char *value,
               int value_length,
               int *status );

void cmpGet0d( const char *loc,
               const char *name,
               double *value,
               int *status );

void cmpGet0i( const char *loc,
               const char *name,
               int *value,
               int *status );

void cmpGet0l( const char *loc,
               const char *name,
               int *value,
               int *status );

void cmpGet0r( const char *loc,
               const char *name,
               float *value,
               int *status );

void cmpGet1c( const char *loc,
               const char *name,
               int elx,
               char *const *value,
               int value_length,
               int *el,
               int *status );

void cmpGet1d( const char *loc,
               const char *name,
               int elx,
               double *value,
               int *el,
               int *status );

void cmpGet1i( const char *loc,
               const char *name,
               int elx,
               int *value,
               int *el,
               int *status );

void cmpGet1l( const char *loc,
               const char *name,
               int elx,
               int *value,
               int *el,
               int *status );

void cmpGet1r( const char *loc,
               const char *name,
               int elx,
               float *value,
               int *el,
               int *status );

void cmpGetnc( const char *loc,
               const char *name,
               int ndim,
               const int *maxd,
               char *const *value,
               int value_length,
               int *actd,
               int *status );

void cmpGetnd( const char *loc,
               const char *name,
               int ndim,
               const int *maxd,
               double *value,
               int *actd,
               int *status );

void cmpGetni( const char *loc,
               const char *name,
               int ndim,
               const int *maxd,
               int *value,
               int *actd,
               int *status );

void cmpGetnl( const char *loc,
               const char *name,
               int ndim,
               const int *maxd,
               int *value,
               int *actd,
               int *status );

void cmpGetnr( const char *loc,
               const char *name,
               int ndim,
               const int *maxd,
               float *value,
               int *actd,
               int *status );

void cmpGetvc( const char *loc,
               const char *name,
               int elx,
               char *const *value,
               int value_length,
               int *el,
               int *status );

void cmpGetvd( const char *loc,
               const char *name,
               int elx,
               double *value,
               int *el,
               int *status );

void cmpGetvi( const char *loc,
               const char *name,
               int elx,
               int *value,
               int *el,
               int *status );

void cmpGetvl( const char *loc,
               const char *name,
               int elx,
               int *value,
               int *el,
               int *status );

void cmpGetvr( const char *loc,
               const char *name,
               int elx,
               float *value,
               int *el,
               int *status );

void cmpLen( const char *loc,
             const char *name,
             int *len,
             int *status );

void cmpMapn( const char *loc,
              const char *name,
              const char *type,
              const char *mode,
              int ndim,
              void * *ptr,
              int *dims,
              int *status );

void cmpMapv( const char *loc,
              const char *name,
              const char *type,
              const char *mode,
              void * *ptr,
              int *actval,
              int *status );

void cmpMod( const char *loc,
             const char *name,
             const char *type,
             int ndim,
             const int *dims,
             int *status );

void cmpModc( const char *loc,
              const char *name,
              int len,
              int ndim,
              const int *dims,
              int *status );

void cmpPrim( const char *loc,
              const char *name,
              int *prim,
              int *status );

void cmpPut0c( const char *loc,
               const char *name,
               const char *value,
               int *status );

void cmpPut0d( const char *loc,
               const char *name,
               double value,
               int *status );

void cmpPut0i( const char *loc,
               const char *name,
               int value,
               int *status );

void cmpPut0l( const char *loc,
               const char *name,
               int value,
               int *status );

void cmpPut0r( const char *loc,
               const char *name,
               float value,
               int *status );

void cmpPut1c( const char *loc,
               const char *name,
               int nval,
               char *const *value,
               int value_length,
               int *status );

void cmpPut1d( const char *loc,
               const char *name,
               int nval,
               const double *value,
               int *status );

void cmpPut1i( const char *loc,
               const char *name,
               int nval,
               const int *value,
               int *status );

void cmpPut1l( const char *loc,
               const char *name,
               int nval,
               const int *value,
               int *status );

void cmpPut1r( const char *loc,
               const char *name,
               int nval,
               const float *value,
               int *status );

void cmpPutnc( const char *loc,
               const char *name,
               int ndim,
               const int *dimx,
               char *const *value,
               int value_length,
               const int *dim,
               int *status );

void cmpPutnd( const char *loc,
               const char *name,
               int ndim,
               const int *dimx,
               const double *value,
               const int *dim,
               int *status );

void cmpPutni( const char *loc,
               const char *name,
               int ndim,
               const int *dimx,
               const int *value,
               const int *dim,
               int *status );

void cmpPutnl( const char *loc,
               const char *name,
               int ndim,
               const int *dimx,
               const int *value,
               const int *dim,
               int *status );

void cmpPutnr( const char *loc,
               const char *name,
               int ndim,
               const int *dimx,
               const float *value,
               const int *dim,
               int *status );

void cmpPutvc( const char *loc,
               const char *name,
               int nval,
               char *const *value,
               int value_length,
               int *status );

void cmpPutvd( const char *loc,
               const char *name,
               int nval,
               const double *value,
               int *status );

void cmpPutvi( const char *loc,
               const char *name,
               int nval,
               const int *value,
               int *status );

void cmpPutvl( const char *loc,
               const char *name,
               int nval,
               const int *value,
               int *status );

void cmpPutvr( const char *loc,
               const char *name,
               int nval,
               const float *value,
               int *status );

void cmpShape( const char *loc,
               const char *name,
               int maxdim,
               int *dims,
               int *ndim,
               int *status );

void cmpSize( const char *loc,
              const char *name,
              int *size,
              int *status );

void cmpStruc( const char *loc,
               const char *name,
               int *struc,
               int *status );

void cmpType( const char *loc,
              const char *name,
              char *type,
              int *status );

void cmpUnmap( const char *loc,
               const char *name,
               int *status );

void datCcopy( const char *sloc,
               const char *dloc,
               const char *cname,
               char *cloc,
               int *status );

void datCctyp( int size,
               char *type );

void datErdsc( const char *loc,
               int *status );

void datErdsn( const char *loc,
               const char *cmp,
               int *status );

void datErtxt( const char *text,
               int *status );

void datGet0c( const char *loc,
               char *value,
               int value_length,
               int *status );

void datGet0d( const char *loc,
               double *value,
               int *status );

void datGet0i( const char *loc,
               int *value,
               int *status );

void datGet0l( const char *loc,
               int *value,
               int *status );

void datGet0r( const char *loc,
               float *value,
               int *status );

void datGet1c( const char *loc,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void datGet1d( const char *loc,
               int maxval,
               double *values,
               int *actval,
               int *status );

void datGet1i( const char *loc,
               int maxval,
               int *values,
               int *actval,
               int *status );

void datGet1l( const char *loc,
               int maxval,
               int *values,
               int *actval,
               int *status );

void datGet1r( const char *loc,
               int maxval,
               float *values,
               int *actval,
               int *status );

void datGetnc( const char *loc,
               int ndim,
               const int *maxd,
               char *const *values,
               int values_length,
               int *actd,
               int *status );

void datGetnd( const char *loc,
               int ndim,
               const int *maxd,
               double *values,
               int *actd,
               int *status );

void datGetni( const char *loc,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status );

void datGetnl( const char *loc,
               int ndim,
               const int *maxd,
               int *values,
               int *actd,
               int *status );

void datGetnr( const char *loc,
               int ndim,
               const int *maxd,
               float *values,
               int *actd,
               int *status );

void datGetvc( const char *loc,
               int maxval,
               char *const *values,
               int values_length,
               int *actval,
               int *status );

void datGetvd( const char *loc,
               int maxval,
               double *values,
               int *actval,
               int *status );

void datGetvi( const char *loc,
               int maxval,
               int *values,
               int *actval,
               int *status );

void datGetvl( const char *loc,
               int maxval,
               int *values,
               int *actval,
               int *status );

void datGetvr( const char *loc,
               int maxval,
               float *values,
               int *actval,
               int *status );

void datMapn( const char *loc,
              const char *type,
              const char *mode,
              int ndim,
              void * *ptr,
              int *dims,
              int *status );

void datMapv( const char *loc,
              const char *type,
              const char *mode,
              void * *ptr,
              int *actval,
              int *status );

void datMsg( const char *token,
             const char *loc );

void datNew0( const char *loc,
              const char *comp,
              const char *type,
              int *status );

void datNew0c( const char *loc,
               const char *comp,
               int size,
               int *status );

void datNew0d( const char *loc,
               const char *name,
               int *status );

void datNew0i( const char *loc,
               const char *name,
               int *status );

void datNew0l( const char *loc,
               const char *name,
               int *status );

void datNew0r( const char *loc,
               const char *name,
               int *status );

void datNew1( const char *loc,
              const char *comp,
              const char *type,
              int len,
              int *status );

void datNew1c( const char *loc,
               const char *comp,
               int size,
               int len,
               int *status );

void datNew1d( const char *loc,
               const char *name,
               int len,
               int *status );

void datNew1i( const char *loc,
               const char *name,
               int len,
               int *status );

void datNew1l( const char *loc,
               const char *name,
               int len,
               int *status );

void datNew1r( const char *loc,
               const char *name,
               int len,
               int *status );

void datNewc( const char *loc,
              const char *comp,
              int size,
              int ndim,
              const int *dims,
              int *status );

void datPrec( const char *loc,
              int *nbyte,
              int *status );

void datPut0c( const char *loc,
               const char *value,
               int *status );

void datPut0d( const char *loc,
               double value,
               int *status );

void datPut0i( const char *loc,
               int value,
               int *status );

void datPut0l( const char *loc,
               int value,
               int *status );

void datPut0r( const char *loc,
               float value,
               int *status );

void datPut1c( const char *loc,
               int nval,
               char *const *values,
               int values_length,
               int *status );

void datPut1d( const char *loc,
               int nval,
               const double *values,
               int *status );

void datPut1i( const char *loc,
               int nval,
               const int *values,
               int *status );

void datPut1l( const char *loc,
               int nval,
               const int *values,
               int *status );

void datPut1r( const char *loc,
               int nval,
               const float *values,
               int *status );

void datPutnc( const char *loc,
               int ndim,
               const int *dimx,
               char *const *values,
               int values_length,
               int *dim,
               int *status );

void datPutnd( const char *loc,
               int ndim,
               const int *dimx,
               const double *values,
               const int *dim,
               int *status );

void datPutni( const char *loc,
               int ndim,
               const int *dimx,
               const int *values,
               const int *dim,
               int *status );

void datPutnl( const char *loc,
               int ndim,
               const int *dimx,
               const int *values,
               const int *dim,
               int *status );

void datPutnr( const char *loc,
               int ndim,
               const int *dimx,
               const float *values,
               const int *dim,
               int *status );

void datPutvc( const char *loc,
               int nval,
               char *const *values,
               int values_length,
               int *status );

void datPutvd( const char *loc,
               int nval,
               const double *values,
               int *status );

void datPutvi( const char *loc,
               int nval,
               const int *values,
               int *status );

void datPutvl( const char *loc,
               int nval,
               const int *values,
               int *status );

void datPutvr( const char *loc,
               int nval,
               const float *values,
               int *status );

void datRef( const char *loc,
             char *ref,
             int ref_length,
             int *lref,
             int *status );

void datTune( const char *name,
              int value,
              int *status );

void datMap( const char *loc,
             const char *type,
             const char *mode,
             int ndim,
             const int *dim,
             void * *pntr,
             int *status );

void datMapc( const char *loc,
              const char *mode,
              int ndim,
              const int *dim,
              void * *pntr,
              int *status );

void datMapd( const char *loc,
              const char *mode,
              int ndim,
              const int *dim,
              void * *pntr,
              int *status );

void datMapi( const char *loc,
              const char *mode,
              int ndim,
              const int *dim,
              void * *pntr,
              int *status );

void datMapl( const char *loc,
              const char *mode,
              int ndim,
              const int *dim,
              void * *pntr,
              int *status );

void datMapr( const char *loc,
              const char *mode,
              int ndim,
              const int *dim,
              void * *pntr,
              int *status );

void datAlter( const char *loc,
               int ndim,
               int dim,
               int *status );

void datAnnul( char *loc,
               int *status );

void datBasic( const char *loc,
               const char *mode,
               void * *pntr,
               int *len,
               int *status );

void datCell( const char *loc1,
              int ndim,
              const int *sub,
              char *loc2,
              int *status );

void datClen( const char *loc,
              int *clen,
              int *status );

void datClone( const char *loc1,
               char *loc2,
               int *status );

void datCoerc( const char *loc1,
               int ndim,
               char *loc2,
               int *status );

void datCopy( const char *loc1,
              char *loc2,
              const char *name,
              int *status );

void datDrep( const char *loc,
              char *format,
              int format_length,
              char *order,
              int order_length,
              int *status );

void datErase( const char *loc,
               const char *name,
               int *status );

void datErmsg( int statu,
               int *length,
               char *msg,
               int msg_length );

void datFind( const char *loc1,
              const char *name,
              char *loc2,
              int *status );

void datIndex( const char *loc1,
               int index,
               char *loc2,
               int *status );

void datLen( const char *loc,
             int *len,
             int *status );

void datMould( const char *loc,
               int ndim,
               const int *dim,
               int *status );

void datMove( char *loc1,
              char *loc2,
              const char *name,
              int *status );

void datName( const char *loc,
              char *name,
              int *status );

void datNcomp( const char *loc,
               int *ncomp,
               int *status );

void datNew( const char *loc,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             int *status );

void datParen( const char *loc1,
               char *loc2,
               int *status );

void datPrim( const char *loc,
              int *reply,
              int *status );

void datPrmry( int set,
               char *loc,
               int *prmry,
               int *status );

void datRefct( const char *loc,
               int *refct,
               int *status );

void datRenam( const char *loc,
               const char *name,
               int *status );

void datReset( const char *loc,
               int *status );

void datRetyp( const char *loc,
               const char *type,
               int *status );

void datShape( const char *loc,
               int ndimx,
               int *dim,
               int *ndim,
               int *status );

void datSize( const char *loc,
              int *size,
              int *status );

void datSlice( const char *loc1,
               int ndim,
               const int *diml,
               const int *dimu,
               char *loc2,
               int *status );

void datState( const char *loc,
               int *reply,
               int *status );

void datStruc( const char *loc,
               int *reply,
               int *status );

void datTemp( const char *type,
              int ndim,
              const int *dim,
              char *loc,
              int *status );

void datThere( const char *loc,
               const char *name,
               int *reply,
               int *status );

void datType( const char *loc,
              char *type,
              int *status );

void datUnmap( const char *loc,
               int *status );

void datValid( const char *loc,
               int *reply,
               int *status );

void datVec( const char *loc1,
             char *loc2,
             int *status );

void datWhere( const char *loc,
               int *block,
               int *offset,
               int *status );

void datGet( const char *loc,
             const char *type,
             int ndim,
             const int *dim,
             void *value,
             int *status );

void datGetc( const char *loc,
              int ndim,
              const int *dim,
              char *const *value,
              int value_length,
              int *status );

void datGetd( const char *loc,
              int ndim,
              const int *dim,
              double *value,
              int *status );

void datGeti( const char *loc,
              int ndim,
              const int *dim,
              int *value,
              int *status );

void datGetl( const char *loc,
              int ndim,
              const int *dim,
              int *value,
              int *status );

void datGetr( const char *loc,
              int ndim,
              const int *dim,
              float *value,
              int *status );

void datPut( const char *loc,
             const char *type,
             int ndim,
             const int *dim,
             void *value,
             int *status );

void datPutc( const char *loc,
              int ndim,
              const int *dim,
              char *const *value,
              int value_length,
              int *status );

void datPutd( const char *loc,
              int ndim,
              const int *dim,
              const double *value,
              int *status );

void datPuti( const char *loc,
              int ndim,
              const int *dim,
              const int *value,
              int *status );

void datPutl( const char *loc,
              int ndim,
              const int *dim,
              const int *value,
              int *status );

void datPutr( const char *loc,
              int ndim,
              const int *dim,
              const float *value,
              int *status );

void hdsCopy( const char *loc,
              const char *file,
              const char *name,
              int *status );

void hdsErase( const char *loc,
               int *status );

void hdsEwild( int *iwld,
               int *status );

void hdsFlush( const char *group,
               int *status );

void hdsFree( const char *loc,
              int *status );

void hdsGroup( const char *loc,
               char *group,
               int *status );

void hdsGtune( const char *param,
               int *value,
               int *status );

void hdsLink( const char *loc,
              const char *group,
              int *status );

void hdsLock( const char *loc,
              int *status );

void hdsNew( const char *file,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             char *loc,
             int *status );

void hdsOpen( const char *file,
              const char *mode,
              char *loc,
              int *status );

void hdsShow( const char *topic,
              int *status );

void hdsState( int *state,
               int *status );

void hdsStop( int *status );

void hdsTrace( const char *loc,
               int *nlev,
               char *path,
               int path_length,
               char *file,
               int file_length,
               int *status );

void hdsTune( const char *param,
              int value,
              int *status );

void hdsWild( const char *fspec,
              const char *mode,
              int *iwld,
              char *loc,
              int *status );

