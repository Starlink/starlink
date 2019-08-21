/* Protect against multiple inclusion */
#ifndef STAR_CMP_H_INCLUDED
#define STAR_CMP_H_INCLUDED

void cmpLen( HDSLoc *struc, const char *comp, size_t *len, int *status );
void cmpMapN(HDSLoc* struc, const char *name, const char *type,
             const char *mode, int ndim, void **pntr, hdsdim dims[], int *status);
void cmpMapV(HDSLoc* struc, const char *name, const char *type,
             const char *mode, void **pntr, size_t *actval, int *status);
void cmpMod( HDSLoc *struc, const char *comp, const char *type,
             int ndim, const hdsdim *dims, int *status );
void cmpModC( HDSLoc *struc, const char *comp, size_t len,
             int ndim, const hdsdim *dims, int *status );
void cmpPrim( HDSLoc *struc, const char *comp, hdsbool_t *prim, int *status );

void cmpPut0C( HDSLoc *struc, const char *comp, const char *value, int *status );
void cmpPut0D( HDSLoc *struc, const char *comp, double value, int *status );
void cmpPut0I( HDSLoc *struc, const char *comp, int value, int *status );
void cmpPut0L( HDSLoc *struc, const char *comp, int value, int *status );
void cmpPut0R( HDSLoc *struc, const char *comp, float value, int *status );
void cmpGet0C( HDSLoc *struc, const char *comp, char *value, size_t value_length, int *status );
void cmpGet0D( HDSLoc *struc, const char *comp, double *value, int *status );
void cmpGet0I( HDSLoc *struc, const char *comp, int *value, int *status );
void cmpGet0L( HDSLoc *struc, const char *comp, int *value, int *status );
void cmpGet0R( HDSLoc *struc, const char *comp, float *value, int *status );
void cmpPut1C( HDSLoc *struc, const char *comp, size_t nval, const char *values[], int *status );
void cmpPut1D( HDSLoc *struc, const char *comp, size_t nval, const double values[], int *status );
void cmpPut1I( HDSLoc *struc, const char *comp, size_t nval, const int values[], int *status );
void cmpPut1L( HDSLoc *struc, const char *comp, size_t nval, const hdsbool_t values[], int *status );
void cmpPut1R( HDSLoc *struc, const char *comp, size_t nval, const float values[], int *status );
void cmpPutVC( HDSLoc *struc, const char *comp, size_t nval, const char *values[], int *status );
void cmpPutVD( HDSLoc *struc, const char *comp, size_t nval, const double values[], int *status );
void cmpPutVI( HDSLoc *struc, const char *comp, size_t nval, const int values[], int *status );
void cmpPutVL( HDSLoc *struc, const char *comp, size_t nval, const hdsbool_t values[], int *status );
void cmpPutVR( HDSLoc *struc, const char *comp, size_t nval, const float values[], int *status );

void cmpGet1C( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize, char *buffer, char *pntrs[], size_t *actval, int *status );
void cmpGet1D( HDSLoc *struc, const char *comp, size_t maxval, double values[], size_t *actval, int *status );
void cmpGet1I( HDSLoc *struc, const char *comp, size_t maxval, int values[], size_t *actval, int *status );
void cmpGet1L( HDSLoc *struc, const char *comp, size_t maxval, int values[], size_t *actval, int *status );
void cmpGet1R( HDSLoc *struc, const char *comp, size_t maxval, float values[], size_t *actval, int *status );
void cmpGetVC( HDSLoc *struc, const char *comp, size_t maxval, size_t bufsize, char *buffer, char *pntrs[], size_t *actval, int *status);
void cmpGetVD( HDSLoc *struc, const char *comp, size_t maxval, double values[], size_t *actval, int *status );
void cmpGetVI( HDSLoc *struc, const char *comp, size_t maxval, int values[], size_t *actval, int *status );
void cmpGetVL( HDSLoc *struc, const char *comp, size_t maxval, int values[], size_t *actval, int *status );
void cmpGetVR( HDSLoc *struc, const char *comp, size_t maxval, float values[], size_t *actval, int *status );

void cmpShape( HDSLoc *struc, const char *comp, int maxdim, hdsdim dims[], int *actdim, int *status );
void cmpSize( HDSLoc *struc, const char *comp, size_t *size, int *status );
void cmpStruc( HDSLoc *strucloc, const char *comp, hdsbool_t *struc, int *status );
void cmpType( HDSLoc *struc, const char *comp, char type[DAT__SZTYP+1], int *status );
void cmpUnmap(HDSLoc* struc, const char *name, int *status);

/* STAR_CMP_H_INCLUDED */
#endif

