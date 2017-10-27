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

#define ARY__NOID 0
#define aryA2I(ary) (ary?(work.pointer=(ary),work.i):ARY__NOID)
#define aryI2A(iary) (((iary)!=ARY__NOID)?(work.i=(iary),work.pointer):NULL)

#define ARY__NOPL 0
#define aryP2I(place) (place?(work.pointer=(place),work.i):ARY__NOPL)
#define aryI2P(iplace) (((iplace)!=ARY__NOPL)?(work.i=(iplace),work.pointer):NULL)










F77_SUBROUTINE(ary_annul)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary = aryI2A(*IARY);
   aryAnnul( &ary, STATUS );
   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_bad)( INTEGER(IARY),
                         LOGICAL(CHECK),
                         LOGICAL(BAD),
                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(CHECK)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)
   int bad;
   aryBad( aryI2A(*IARY), F77_ISTRUE(*CHECK)?1:0, &bad, STATUS );
   *BAD = bad ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_base)( INTEGER(IARY1),
                          INTEGER(IARY2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)
   Ary *ary2;
   aryBase( aryI2A(*IARY1), &ary2, STATUS );
   *IARY2 = aryA2I(ary2);
}

F77_SUBROUTINE(ary_bound)( INTEGER(IARY),
                           INTEGER(NDIMX),
                           INTEGER8_ARRAY(LBND),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(NDIM),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];
   aryBound( aryI2A(*IARY), ARY__MXDIM, lbnd, ubnd, NDIM, STATUS );

   n = ( *NDIMX < ARY__MXDIM ) ? *NDIMX : ARY__MXDIM;
   for( i = 0; i < n; i++ ) {
      LBND[ i ] = lbnd[ i ];
      UBND[ i ] = ubnd[ i ];
   }

   for( ; i < *NDIMX; i++ ) {
      LBND[ i ] = 1;
      UBND[ i ] = 1;
   }
}

F77_SUBROUTINE(ary_clone)( INTEGER(IARY1),
                          INTEGER(IARY2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)
   Ary *ary2;
   aryClone( aryI2A(*IARY1), &ary2, STATUS );
   *IARY2 = aryA2I(ary2);
}

F77_SUBROUTINE(ary_cmplx)( INTEGER(IARY),
                           LOGICAL(CMPLX),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(CMPLX)
   GENPTR_INTEGER(STATUS)
   int cmplx;
   aryCmplx( aryI2A(*IARY), &cmplx, STATUS );
   *CMPLX = cmplx ? F77_TRUE : F77_FALSE;
}


void aryCopy( Ary *ary1, AryPlace **place, Ary **ary2, int *status );
F77_SUBROUTINE(ary_copy)( INTEGER(IARY1),
                          INTEGER(PLACE),
                          INTEGER(IARY2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)
   Ary *ary2;
   AryPlace *place = aryI2P(*PLACE);
   aryCopy( aryI2A(*IARY1), &place, &ary2, STATUS );
   *PLACE = aryP2I(place);
   *IARY2 = aryA2I(ary2);
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

   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_ftype)( INTEGER(IARY),
                           CHARACTER(FTYPE),
                           INTEGER(STATUS)
                           TRAIL(FTYPE) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)
   char ftype[ARY__SZFTP+1];

   aryFtype( aryI2A(*IARY), ftype, STATUS );
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

   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_loc)( INTEGER(IARY),
                         CHARACTER(LOC),
                         INTEGER(STATUS)
                         TRAIL(LOC) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;

   aryLoc( aryI2A(*IARY), &loc, STATUS );
   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );

}

F77_SUBROUTINE(ary_msg)( CHARACTER(TOKEN),
                         INTEGER(IARY)
                         TRAIL(TOKEN) ) {
   GENPTR_CHARACTER(TOKEN)
   GENPTR_INTEGER(IARY)

   char *token = cnfCreim( TOKEN, TOKEN_length );
   aryMsg( token, aryI2A(*IARY) );
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

   arySame( aryI2A(*IARY1), aryI2A(*IARY2), &same, &isect, STATUS );
   F77_EXPORT_LOGICAL( same, *SAME );
   F77_EXPORT_LOGICAL( isect, *ISECT );

}







/*

void aryDelet( Ary **ary, int *status );
void aryDelta( Ary *ary1, int zaxis, const char *type, float minrat, AryPlace **place, float *zratio, Ary **ary2, int *status );
void aryDim( Ary *ary, int ndimx, hdsdim *dim, int *ndim, int *status );
void aryDupe( Ary *iary1, AryPlace **place, Ary **iary2, int *status );
void aryForm( Ary *ary, char form[ARY__SZFRM+1], int *status );
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
