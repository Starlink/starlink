#ifndef POLSUB_DEFINED
#define POLSUB_DEFINED
#include "ast.h"

/* The name of the environment variable used to get the number of worker
  threads to use. */
#define POLPACK__THREADS "POLPACK_THREADS"

void pol1Pa2gr( AstMapping *map, AstFrame *frm, int axis, int npos,
                const double *gx0, const double *gy0, double *angle,
                int *status );

void pol1Rotrf( int nrow, int ncol, AstFrameSet *wcs, AstFrameSet *twcs,
                int ifrm, int iaxis, const double *qin,
                const double *uin, double *qout, double *uout,
                const double *qinv, const double *uinv, double *qoutv,
                double *uoutv, AstMapping **map, int *status );


#endif
