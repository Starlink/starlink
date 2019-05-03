/* Protect against multiple inclusion */
#ifndef STAR_CMP_H_INCLUDED
#define STAR_CMP_H_INCLUDED

void cmpMod( HDSLoc *struc, const char *comp, const char *type,
             int ndim, const hdsdim *dims, int *status );
void cmpModC( HDSLoc *struc, const char *comp, size_t len,
             int ndim, const hdsdim *dims, int *status );

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
void cmpType( HDSLoc *struc, const char *comp, char type[DAT__SZTYP+1], int *status );

/* STAR_CMP_H_INCLUDED */
#endif

