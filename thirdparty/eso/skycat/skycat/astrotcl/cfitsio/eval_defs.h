#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#if defined(__sgi) || defined(__hpux)
#include <alloca.h>
#endif
#ifdef sparc
#include <malloc.h>
#endif
#include "fitsio2.h"

#define MAXDIMS      5
#define MAXSUBS     10
#define CONST_OP -1000

typedef struct {
                  int  type;
                  long nelem;
                  int  naxis;
                  long naxes[MAXDIMS];
                                } DataInfo;

typedef struct {
                  long   nelem;
                  int    naxis;
                  long   naxes[MAXDIMS];
                  char   *undef;
                  union {
                         double dbl;
                         long   lng;
                         char   log;
                         char   str[256];
                         double *dblptr;
                         long   *lngptr;
                         char   *logptr;
                         char   **strptr;
                         void   *ptr;
		  } data;
                                } lval;

typedef struct Node {
                  int    operation;
                  void   (*DoOp)(struct Node *this);
                  int    nSubNodes;
                  int    SubNodes[MAXSUBS];
                  int    type;
                  lval   value;
                                } Node;

typedef struct {
                  fitsfile    *def_fptr;

                  int         compressed;
                  int         timeCol;
                  int         parCol;
                  int         valCol;

                  char        *expr;
                  int         index;
                  int         is_eobuf;

                  Node        *Nodes;
                  int         nNodes;
                  int         nNodesAlloc;
                  
                  long        firstRow;
                  long        nRows;

                  int         nCols;
                  iteratorCol *colData;
                  DataInfo    *colInfo;
                  char        **colNulls;

                  int         datatype;

                  int         status;
                                } ParseData;

typedef enum {
                  rnd_fct = 1001,
                  sum_fct,
                  nelem_fct,
                  sin_fct,
                  cos_fct,
                  tan_fct,
                  asin_fct,
                  acos_fct,
                  atan_fct,
                  sinh_fct,
                  cosh_fct,
                  tanh_fct,
                  exp_fct,
                  log_fct,
                  log10_fct,
                  sqrt_fct,
                  abs_fct,
                  atan2_fct,
                  ceil_fct,
                  floor_fct,
                  round_fct,
                  near_fct,
                  circle_fct,
                  box_fct,
                  elps_fct,
                  isnull_fct,
                  defnull_fct,
                  gtifilt_fct,
                  regfilt_fct,
                  row_fct
                                } funcOp;

extern ParseData gParse;

#ifdef __cplusplus
extern "C" {
#endif

   int  ffparse(void);
   int  fflex(void);
   void ffrestart(FILE*);
   int  ffbuildcolumn( char *ColName, int *ColNum );
   int  ffallocatecol( int nCol, int *status );

   void Evaluate_Node( int thisNode );
   void Reset_Parser ( long firstRow, long rowOffset, long nRows );

#ifdef __cplusplus
    }
#endif
