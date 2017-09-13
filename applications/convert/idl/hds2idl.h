
#include "export.h"
#include "dat_par.h"
#include "star/hds.h"

/* Constants for HDS routines */

#define MAXDIMS DAT__MXDIM

/* Protoypes for internal HDS to IDL to HDS routines */

char **getstringarray( int ndims, hdsdim *dims, IDL_STRING *data );
void retstringarray( char ** );

void getobjectdetails( IDL_VPTR var, void *data, char **taglist,
                    char hdstype[], int *numtags,
                    int *ndims, hdsdim dims[], int *elt_len, int *status );

void hdsstructwrite( HDSLoc *toploc,
                     void *data,
                     char **taglist,
                     int numtags,
                     int ndims,
                     hdsdim dims[],
                     IDL_VPTR var,
                     int *status );

void hdsprimwrite( HDSLoc *toploc,
                     char *hdstype,
                     int ndims,
                     hdsdim dims[],
                     void *data,
                     int *status );

void getobjectdetails( IDL_VPTR var, void *data, char **taglist,
                    char hdstype[], int *numtags,
                    int *ndims, hdsdim dims[], int *elt_len, int *status );

char** tagstrip( char *prefix, char** taglist );

int checkarr( char *comp, char name[], int *slice, int *ndims,
              hdsdim starts[], hdsdim ends[], int *status );

void getcomp(
     char *objname, const char *acmode, HDSLoc ** objloc, int *status );
UCHAR getidltype( const char *hdstype );
IDL_StructDefPtr idlstructarrdef(
   HDSLoc *sloc, char *name, int ndims, hdsdim dims[], int *status );

IDL_StructDefPtr idlstructdef( HDSLoc *sloc, int *status );
void idlstructfill( HDSLoc *sloc, IDL_SREF sref, int *status );
void idlprimfill( HDSLoc * objloc, IDL_VPTR var, void *tdata, int *status );

IDL_VPTR hds2idl( int argc, IDL_VPTR argv[] );

int typecheck ( char * , UCHAR );
