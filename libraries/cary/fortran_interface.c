#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "ary.h"
#include "star/hds_fortran.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int, or as a pointer. */
typedef union IdUnion {
   int i;
   unsigned u;
   void *pointer;
} IdUnion;

IdUnion work;
#define aryP2I(ary) (work.pointer=(ary),work.i)
#define aryI2P(iary) (work.i=(iary),work.pointer)

F77_SUBROUTINE(ary_annul)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary = aryI2P(*IARY);
   aryAnnul( &ary, STATUS );
   *IARY = aryP2I(ary);
}


F77_SUBROUTINE(ary_find)( CHARACTER(LOC),
                          CHARACTER(NAME),
                          INTEGER(IARY),
                          INTEGER(STATUS)
                          TRAIL(LOC)
                          TRAIL(NAME) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary;
   char name[DAT__SZNAM+1];
   HDSLoc *loc = NULL;

   datImportFloc( LOC, LOC_length, &loc, STATUS );
   cnfImpn( NAME, NAME_length, DAT__SZNAM, name );

   aryFind( loc, name, &ary, STATUS );

   *IARY = aryP2I(ary);
}

F77_SUBROUTINE(ary_ftype)( INTEGER(IARY),
                           CHARACTER(FTYPE),
                           INTEGER(STATUS)
                           TRAIL(FTYPE) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)
   char ftype[ARY__SZFTP+1];

   aryFtype( aryI2P(*IARY), char ftype[ARY__SZFTP+1], int *status );
   cnfExprt( ftype, FTYPE, FTYPE_length );

}


F77_SUBROUTINE(ary_imprt)( CHARACTER(LOC),
                          INTEGER(IARY),
                          INTEGER(STATUS)
                          TRAIL(LOC) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary;
   HDSLoc *loc = NULL;

   datImportFloc( LOC, LOC_length, &loc, STATUS );

   aryImprt( loc, &ary, STATUS );

   *IARY = aryP2I(ary);
}

F77_SUBROUTINE(ary_loc)( INTEGER(IARY),
                         CHARACTER(LOC),
                         INTEGER(STATUS)
                         TRAIL(LOC) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;

   aryLoc( aryI2P(*IARY), &loc, STATUS );
   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );

}


F77_SUBROUTINE(ary_msg)( CHARACTER(TOKEN),
                         INTEGER(IARY)
                         TRAIL(TOKEN) ) {
   GENPTR_CHARACTER(TOKEN)
   GENPTR_INTEGER(IARY)

   char *token = cnfCreim( TOKEN, TOKEN_length );
   aryMsg( token, aryI2P(*IARY) );
   cnfFree( token );
}


F77_SUBROUTINE(ary_same)( INTEGER(IARY1),
                          INTEGER(IARY2),
                          LOGICAL(SAME),
                          LOGICAL(ISECT),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_LOGICAL(SAME)
   GENPTR_LOGICAL(ISECT)
   GENPTR_INTEGER(STATUS)

   int same, isect;

   arySame( aryI2P(*IARY1), aryI2P(*IARY2), &same, &isect, STATUS );
   F77_EXPORT_LOGICAL( same, *SAME );
   F77_EXPORT_LOGICAL( isect, *ISECT );

}







/*

void aryBad( Ary *ary, int check, int *bad, int *status );
void aryBase( Ary *ary1, Ary **ary2, int *status );
void aryBound( Ary *ary, int ndimx, hdsdim *lbnd, hdsdim *ubnd, int *ndim, int *status );
void aryClone( Ary *ary1, Ary **ary2, int *status );
void aryCmplx( Ary *ary, int *cmplx, int *status );
void aryCopy( Ary *ary1, AryPlace **place, Ary **ary2, int *status );
void aryDelet( Ary **ary, int *status );
void aryDelta( Ary *ary1, int zaxis, const char *type, float minrat, AryPlace **place, float *zratio, Ary **ary2, int *status );
void aryDim( Ary *ary, int ndimx, hdsdim *dim, int *ndim, int *status );
void aryDupe( Ary *iary1, AryPlace **place, Ary **iary2, int *status );
void aryForm( Ary *ary, char form[ARY__SZFRM+1], int *status );
void aryFtype( Ary *ary,  char ftype[ARY__SZFTP+1], int *status );
void aryGtdlt( Ary *ary, int *zaxis, char ztype[DAT__SZTYP+1], float *zratio, int *status );
void aryIsacc( Ary *ary, const char *access, int *isacc, int *status );
void aryIsbas( Ary *ary, int *base, int *status );
void aryIsmap( Ary *ary, int *mapped, int *status );
void aryIstmp( Ary *ary, int *temp, int *status );
void aryMap( Ary *ary, const char *type, const char *mmod, void **pntr, size_t *el, int *status );
void aryMapz( Ary *ary, const char *type, const char *mmod, void **rpntr, void **ipntr, size_t *el, int *status );
void aryNdim( Ary *ary, int *ndim, int *status );
void aryNew( const char *ftype, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, AryPlace **place, Ary **ary, int *status );
void aryNewp( const char *ftype, int ndim, const hdsdim *ubnd, AryPlace **place, Ary **ary, int *status );
void aryNoacc( const char *access, Ary *ary, int *status );
void aryOffs( Ary *ary1, Ary *ary2, int mxoffs, hdsdim *offs, int *status );
void aryPlace( HDSLoc *loc, const char *name, AryPlace **place, int *status );
void aryReset( Ary *ary, int *status );
void arySbad( int bad, Ary *ary, int *status );
void arySbnd( int ndim, const hdsdim *lbnd, const hdsdim *ubnd, Ary *ary, int *status );
void arySctyp( Ary *ary, char type[ARY__SZTYP+1], int *status );
void arySect( Ary *ary1, int ndim, const hdsdim *lbnd, const hdsdim *ubnd, Ary **ary2, int *status );
void aryShift( int nshift, const hdsdim *shift, Ary *ary, int *status );
void arySize( Ary *ary, size_t *npix, int *status );
void arySsect( Ary *ary1, Ary *ary2, Ary **ary3, int *status );
void aryState( Ary *ary, int *state, int *status );
void aryStype( const char *ftype, Ary *ary, int *status );
void aryTemp( AryPlace **place, int *status );
void aryType( Ary *ary, char type[ARY__SZTYP+1], int *status );
void aryUnlock( Ary *ary, int *status );
void aryUnmap( Ary *ary, int *status );
void aryVerfy( Ary *ary, int *status );
nclude "ary_cgen.h"

*/
