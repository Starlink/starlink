#ifndef POLSUB_DEFINED
#define POLSUB_DEFINED
#include "ast.h"

/* The name of the environment variable used to get the number of worker
  threads to use. */
#define POLPACK__THREADS "POLPACK_THREADS"


void pol1Pa2gr( AstFrameSet *iwcs, int axis, int npos, const double *gx0,
                const double *gy0, double *angle, int *status );

void pol1Rotqu( int nrow, int ncol, AstFrameSet *wcs, float angle,
                int var, const double *qin, const double *uin,
                double *qout, double *uout, int *status );

#endif
