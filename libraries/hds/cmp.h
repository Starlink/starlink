/* Protect against multiple inclusion */
#ifndef STAR_CMP_H_INCLUDED
#define STAR_CMP_H_INCLUDED

void cmpMod( HDSLoc *struc, const char *comp, const char *type,
             int ndim, const hdsdim *dims, int *status );

void cmpPut0C( HDSLoc *struc, const char *comp, const char *value, int *status );
void cmpPut0D( HDSLoc *struc, const char *comp, double value, int *status );
void cmpPut0I( HDSLoc *struc, const char *comp, int value, int *status );
void cmpPut0L( HDSLoc *struc, const char *comp, int value, int *status );
void cmpPut0R( HDSLoc *struc, const char *comp, float value, int *status );

/* STAR_CMP_H_INCLUDED */
#endif

