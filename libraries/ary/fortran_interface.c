#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "sae_par.h"
#include "mers.h"
#include "ary.h"
#include "ary_err.h"
#include "star/hds_fortran.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int, or as a pointer. */
typedef union IdUnion {
   int i;
   unsigned u;
   void *pointer;
} IdUnion;


IdUnion work1;
IdUnion work2;
IdUnion work3;

#define ARY__NOID 0
#define aryI2A(iary) (((iary)!=ARY__NOID)?(work1.i=(iary),work1.pointer):NULL)
#define aryI2A2(iary) (((iary)!=ARY__NOID)?(work2.i=(iary),work2.pointer):NULL)
#define aryA2I(ary) (ary?(work3.pointer=(ary),work3.i):ARY__NOID)

IdUnion work4;
IdUnion work5;

#define ARY__NOPL 0
#define aryP2I(place) (place?(work4.pointer=(place),work4.i):ARY__NOPL)
#define aryI2P(iplace) (((iplace)!=ARY__NOPL)?(work5.i=(iplace),work5.pointer):NULL)

#define CHECK_DIM( IVAL, HDSDIMVAL, FUNC, IARY ) \
   if( *STATUS == SAI__OK && (hdsdim) (IVAL) != (HDSDIMVAL) ) { \
      aryMsg( "A", aryI2A(*IARY) ); \
      *STATUS = ARY__TOOBIG; \
      errRep( " ", #FUNC ": Array ^A is too large.", STATUS ); \
   }










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
   GENPTR_CHARACTER(FTYPE)
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

   arySame( aryI2A(*IARY1), aryI2A2(*IARY2), &same, &isect, STATUS );

   F77_EXPORT_LOGICAL( same, *SAME );
   F77_EXPORT_LOGICAL( isect, *ISECT );
}

F77_SUBROUTINE(ary_delet)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary = aryI2A(*IARY);

   aryDelet( &ary, STATUS );

   *IARY = aryA2I(ary);
}



F77_SUBROUTINE(ary_delta)( INTEGER(IARY1),
                           INTEGER(ZAXIS),
                           CHARACTER(TYPE),
                           REAL(MINRAT),
                           INTEGER(PLACE),
                           REAL(ZRATIO),
                          INTEGER(IARY2),
                          INTEGER(STATUS)
                          TRAIL(TYPE) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(ZAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_REAL(MINRAT)
   GENPTR_INTEGER(PLACE)
   GENPTR_REAL(ZRATIO)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)

   char type[ DAT__SZTYP + 1 ];
   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   Ary *ary2;
   AryPlace *place = aryI2P(*PLACE);

   aryDelta( aryI2A(*IARY1), *ZAXIS, type, *MINRAT, &place, ZRATIO,
             &ary2, STATUS );

   *PLACE = aryP2I(place);
   *IARY2 = aryA2I(ary2);
}

F77_SUBROUTINE(ary_dupe)( INTEGER(IARY1),
                          INTEGER(PLACE),
                          INTEGER(IARY2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)
   Ary *ary2;
   AryPlace *place = aryI2P(*PLACE);

   aryDupe( aryI2A(*IARY1), &place, &ary2, STATUS );

   *PLACE = aryP2I(place);
   *IARY2 = aryA2I(ary2);
}

F77_SUBROUTINE(ary_form)( INTEGER(IARY),
                          CHARACTER(FORM),
                          INTEGER(STATUS)
                          TRAIL(FORM) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(FORM)
   GENPTR_INTEGER(STATUS)
   char form[ARY__SZFRM+1];

   aryForm( aryI2A(*IARY), form, STATUS );

   cnfExprt( form, FORM, FORM_length );
}

F77_SUBROUTINE(ary_gtdlt)( INTEGER(IARY),
                           INTEGER(ZAXIS),
                           CHARACTER(ZTYPE),
                           REAL(ZRATIO),
                           INTEGER(STATUS)
                           TRAIL(ZTYPE) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(ZAXIS)
   GENPTR_CHARACTER(ZTYPE)
   GENPTR_REAL(ZRATIO)
   GENPTR_INTEGER(STATUS)
   char ztype[DAT__SZTYP+1];

   aryGtdlt( aryI2A(*IARY), ZAXIS, ztype, ZRATIO, STATUS );

   cnfExprt( ztype, ZTYPE, ZTYPE_length );
}

F77_SUBROUTINE(ary_isacc)( INTEGER(IARY),
                           CHARACTER(ACCESS),
                           LOGICAL(ISACC),
                           INTEGER(STATUS)
                           TRAIL(ACCESS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(ACCESS)
   GENPTR_LOGICAL(ISACC)
   GENPTR_INTEGER(STATUS)
   int isacc;

   char access[ ARY__SZACC + 1 ];
   cnfImpn( ACCESS, ACCESS_length, ARY__SZACC, access );

   aryIsacc( aryI2A(*IARY), access, &isacc, STATUS );
   *ISACC = isacc ? F77_TRUE : F77_FALSE;

}

F77_SUBROUTINE(ary_isbas)( INTEGER(IARY),
                           LOGICAL(BASE),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(BASE)
   GENPTR_INTEGER(STATUS)
   int base;

   aryIsbas( aryI2A(*IARY), &base, STATUS );
   *BASE = base ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_ismap)( INTEGER(IARY),
                           LOGICAL(MAPPED),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(MAPPED)
   GENPTR_INTEGER(STATUS)
   int mapped;

   aryIsmap( aryI2A(*IARY), &mapped, STATUS );
   *MAPPED = mapped ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_istmp)( INTEGER(IARY),
                           LOGICAL(TEMP),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(TEMP)
   GENPTR_INTEGER(STATUS)
   int temp;

   aryIstmp( aryI2A(*IARY), &temp, STATUS );
   *TEMP = temp ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_ndim)( INTEGER(IARY),
                          INTEGER(NDIM),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   aryNdim( aryI2A(*IARY), NDIM, STATUS );

}

F77_SUBROUTINE(ary_noacc)( CHARACTER(ACCESS),
                           INTEGER(IARY),
                           INTEGER(STATUS)
                           TRAIL(ACCESS) ) {
   GENPTR_CHARACTER(ACCESS)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   char access[ ARY__SZACC + 1 ];
   cnfImpn( ACCESS, ACCESS_length, ARY__SZACC, access );

   aryNoacc( access, aryI2A(*IARY), STATUS );
}

F77_SUBROUTINE(ary_place)( CHARACTER(LOC),
                           CHARACTER(NAME),
                           INTEGER(IPLACE),
                           INTEGER(STATUS)
                           TRAIL(LOC)
                           TRAIL(NAME) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(IPLACE)
   GENPTR_INTEGER(STATUS)

   AryPlace *place;
   char name[DAT__SZNAM+1];
   HDSLoc *loc = NULL;

   datImportFloc( LOC, LOC_length, &loc, STATUS );
   cnfImpn( NAME, NAME_length, DAT__SZNAM, name );

   aryPlace( loc, name, &place, STATUS );

   *IPLACE = aryP2I(place);
}

F77_SUBROUTINE(ary_reset)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   aryReset( aryI2A(*IARY), STATUS );

}

F77_SUBROUTINE(ary_sbad)( LOGICAL(BAD),
                          INTEGER(IARY),
                          INTEGER(STATUS) ) {
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   arySbad( (*BAD == F77_TRUE), aryI2A(*IARY), STATUS );
}


F77_SUBROUTINE(ary_sctyp)( INTEGER(IARY),
                           CHARACTER(TYPE),
                           INTEGER(STATUS)
                           TRAIL(TYPE) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(STATUS)
   char type[DAT__SZTYP+1];

   arySctyp( aryI2A(*IARY), type, STATUS );
   cnfExprt( type, TYPE, TYPE_length );

}

F77_SUBROUTINE(ary_ssect)( INTEGER(IARY1),
                           INTEGER(IARY2),
                           INTEGER(IARY3),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(IARY3)
   GENPTR_INTEGER(STATUS)
   Ary *ary3;

   arySsect( aryI2A(*IARY1), aryI2A2(*IARY2), &ary3, STATUS );

   *IARY3 = aryA2I(ary3);
}

F77_SUBROUTINE(ary_state)( INTEGER(IARY),
                           LOGICAL(STATE),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(STATE)
   GENPTR_INTEGER(STATUS)
   int state;

   aryState( aryI2A(*IARY), &state, STATUS );
   *STATE = state ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_stype)( CHARACTER(FTYPE),
                           INTEGER(IARY),
                           INTEGER(STATUS)
                           TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)
   char ftype[ ARY__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, ARY__SZFTP, ftype );

   aryStype( ftype, aryI2A(*IARY), STATUS );

}

F77_SUBROUTINE(ary_temp)( INTEGER(IPLACE),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IPLACE)
   GENPTR_INTEGER(STATUS)
   AryPlace *place;

   aryTemp( &place, STATUS );

   *IPLACE = aryP2I(place);
}

F77_SUBROUTINE(ary_type)( INTEGER(IARY),
                          CHARACTER(TYPE),
                          INTEGER(STATUS)
                          TRAIL(TYPE) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(STATUS)
   char type[DAT__SZTYP+1];

   aryType( aryI2A(*IARY), type, STATUS );

   cnfExprt( type, TYPE, TYPE_length );
}

F77_SUBROUTINE(ary_unmap)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   aryUnmap( aryI2A(*IARY), STATUS );

}

F77_SUBROUTINE(ary_valid)( INTEGER(IARY),
                           LOGICAL(VALID),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_LOGICAL(VALID)
   GENPTR_INTEGER(STATUS)

   *VALID = aryValid( aryI2A(*IARY), STATUS ) ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ary_verfy)( INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   aryVerfy( aryI2A(*IARY), STATUS );

}


#define MAKE_GTSZ(CT,FT,FTYPE) \
F77_SUBROUTINE(ary_gtsz##FT)( INTEGER(IARY), \
                              FTYPE(SCALE), \
                              FTYPE(ZERO), \
                              INTEGER(STATUS) ) { \
   GENPTR_INTEGER(IARY) \
   GENPTR_##FTYPE(SCALE) \
   GENPTR_##FTYPE(ZERO) \
   GENPTR_INTEGER(STATUS) \
\
   aryGtsz##CT( aryI2A(*IARY), SCALE, ZERO, STATUS ); \
}

MAKE_GTSZ(B, b, BYTE)
MAKE_GTSZ(D, d, DOUBLE)
MAKE_GTSZ(F, r, REAL)
MAKE_GTSZ(I, i, INTEGER)
MAKE_GTSZ(K, k, INTEGER8)
MAKE_GTSZ(UB, ub, UBYTE)
MAKE_GTSZ(UW, uw, UWORD)
MAKE_GTSZ(W, w, WORD)

#undef MAKE_GTSZ


#define MAKE_PTSZ(CT,FT,FTYPE) \
F77_SUBROUTINE(ary_ptsz##FT)( INTEGER(IARY), \
                              FTYPE(SCALE), \
                              FTYPE(ZERO), \
                              INTEGER(STATUS) ) { \
   GENPTR_INTEGER(IARY) \
   GENPTR_##FTYPE(SCALE) \
   GENPTR_##FTYPE(ZERO) \
   GENPTR_INTEGER(STATUS) \
\
   aryPtsz##CT( aryI2A(*IARY), *SCALE, *ZERO, STATUS ); \
}

MAKE_PTSZ(B, b, BYTE)
MAKE_PTSZ(D, d, DOUBLE)
MAKE_PTSZ(F, r, REAL)
MAKE_PTSZ(I, i, INTEGER)
MAKE_PTSZ(K, k, INTEGER8)
MAKE_PTSZ(UB, ub, UBYTE)
MAKE_PTSZ(UW, uw, UWORD)
MAKE_PTSZ(W, w, WORD)

#undef MAKE_PTSZ





/* -------  Routines with 64 bit arguments -------------- */

F77_SUBROUTINE(ary_boundk)( INTEGER(IARY),
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

F77_SUBROUTINE(ary_dimk)( INTEGER(IARY),
                          INTEGER(NDIMX),
                          INTEGER8_ARRAY(DIM),
                          INTEGER(NDIM),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER8_ARRAY(DIM)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim dim[ ARY__MXDIM ];

   aryDim( aryI2A(*IARY), ARY__MXDIM, dim, NDIM, STATUS );

   n = ( *NDIMX < ARY__MXDIM ) ? *NDIMX : ARY__MXDIM;
   for( i = 0; i < n; i++ ) DIM[ i ] = dim[ i ];
   for( ; i < *NDIMX; i++ ) DIM[ i ] = 1;
}

F77_SUBROUTINE(ary_mapk)( INTEGER(IARY),
                          CHARACTER(TYPE),
                          CHARACTER(MMOD),
                          INTEGER(PNTR),
                          INTEGER8(EL),
                          INTEGER(STATUS)
                          TRAIL(TYPE)
                          TRAIL(MMOD) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ ARY__SZMMD + 1 ];
   void *pntr = NULL;
   size_t el;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, ARY__SZMMD, mmod );

   aryMap( aryI2A(*IARY), type, mmod, &pntr, &el, STATUS );

   *PNTR = cnfFptr( pntr );
   *EL = el;
}

F77_SUBROUTINE(ary_mapzk)( INTEGER(IARY),
                           CHARACTER(TYPE),
                           CHARACTER(MMOD),
                           INTEGER(RPNTR),
                           INTEGER(IPNTR),
                           INTEGER8(EL),
                           INTEGER(STATUS)
                           TRAIL(TYPE)
                           TRAIL(MMOD) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(RPNTR)
   GENPTR_INTEGER(IPNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ ARY__SZMMD + 1 ];
   void *rpntr = NULL;
   void *ipntr = NULL;
   size_t el;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, ARY__SZMMD, mmod );

   aryMapz( aryI2A(*IARY), type, mmod, &rpntr, &ipntr, &el, STATUS );

   *RPNTR = cnfFptr( rpntr );
   *IPNTR = cnfFptr( ipntr );
   *EL = el;
}

F77_SUBROUTINE(ary_newk)( CHARACTER(FTYPE),
                          INTEGER(NDIM),
                          INTEGER8_ARRAY(LBND),
                          INTEGER8_ARRAY(UBND),
                          INTEGER(PLACE),
                          INTEGER(IARY),
                          INTEGER(STATUS)
                          TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   char ftype[ ARY__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, ARY__SZFTP, ftype );
   int i;
   Ary *ary;
   AryPlace *place = aryI2P(*PLACE);

   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];
   int ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   aryNew( ftype, *NDIM, lbnd, ubnd, &place, &ary, STATUS );

   *PLACE = aryP2I(place);
   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_newpk)( CHARACTER(FTYPE),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(PLACE),
                           INTEGER(IARY),
                           INTEGER(STATUS)
                           TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   char ftype[ ARY__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, ARY__SZFTP, ftype );
   Ary *ary;
   int i;
   AryPlace *place = aryI2P(*PLACE);

   hdsdim ubnd[ARY__MXDIM];
   int ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) ubnd[ i ] = UBND[ i ];

   aryNewp( ftype, *NDIM, ubnd, &place, &ary, STATUS );

   *PLACE = aryP2I(place);
   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_offsk)( INTEGER(IARY1),
                           INTEGER(IARY2),
                           INTEGER(MXOFFS),
                           INTEGER8_ARRAY(OFFS),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(MXOFFS)
   GENPTR_INTEGER8_ARRAY(OFFS)
   GENPTR_INTEGER(STATUS)

   int i, mxoffs;
   hdsdim offs[ ARY__MXDIM ];

   aryOffs( aryI2A(*IARY1), aryI2A2(*IARY2), ARY__MXDIM, offs, STATUS );

   mxoffs = ( *MXOFFS < ARY__MXDIM ) ? *MXOFFS : ARY__MXDIM;
   for( i = 0; i < mxoffs; i++ ) OFFS[ i ] = offs[ i ];
   for( ; i < *MXOFFS; i++ ) OFFS[ i ] = 1;
}

F77_SUBROUTINE(ary_sbndk)( INTEGER(NDIM),
                           INTEGER8_ARRAY(LBND),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];

   ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   arySbnd( ndim, lbnd, ubnd, aryI2A(*IARY), STATUS );
}

F77_SUBROUTINE(ary_sectk)( INTEGER(IARY1),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(LBND),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(IARY2),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)

   Ary *ary2;
   int i, ndim;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];

   ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   arySect( aryI2A(*IARY1), ndim, lbnd, ubnd, &ary2, STATUS );

   *IARY2 = aryA2I(ary2);

}

F77_SUBROUTINE(ary_shiftk)( INTEGER(NSHIFT),
                            INTEGER8_ARRAY(SHIFT),
                            INTEGER(IARY),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(NSHIFT)
   GENPTR_INTEGER8_ARRAY(SHIFT)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   int i, nshift;
   hdsdim shift[ARY__MXDIM];
   nshift = ( *NSHIFT < ARY__MXDIM ) ? *NSHIFT : ARY__MXDIM;
   for( i = 0; i < nshift; i++ ) shift[ i ] = SHIFT[ i ];

   aryShift( nshift, shift, aryI2A(*IARY), STATUS );

}

F77_SUBROUTINE(ary_sizek)( INTEGER(IARY),
                           INTEGER8(NPIX),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER8(NPIX)
   GENPTR_INTEGER(STATUS)
   size_t npix;

   arySize( aryI2A(*IARY), &npix, STATUS );

   *NPIX = npix;
}




/* -------  Routines with 32 bit arguments -------------- */

F77_SUBROUTINE(ary_bound)( INTEGER(IARY),
                           INTEGER(NDIMX),
                           INTEGER_ARRAY(LBND),
                           INTEGER_ARRAY(UBND),
                           INTEGER(NDIM),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];
   aryBound( aryI2A(*IARY), ARY__MXDIM, lbnd, ubnd, NDIM, STATUS );

   n = ( *NDIMX < ARY__MXDIM ) ? *NDIMX : ARY__MXDIM;
   for( i = 0; i < n; i++ ) {
      LBND[ i ] = (int) lbnd[ i ];
      UBND[ i ] = (int) ubnd[ i ];
      CHECK_DIM( LBND[ i ], lbnd[ i ], ARY_BOUND, IARY )
   }

   for( ; i < *NDIMX; i++ ) {
      LBND[ i ] = 1;
      UBND[ i ] = 1;
   }
}

F77_SUBROUTINE(ary_dim)( INTEGER(IARY),
                         INTEGER(NDIMX),
                         INTEGER_ARRAY(DIM),
                         INTEGER(NDIM),
                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER_ARRAY(DIM)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim dim[ ARY__MXDIM ];

   aryDim( aryI2A(*IARY), ARY__MXDIM, dim, NDIM, STATUS );

   n = ( *NDIMX < ARY__MXDIM ) ? *NDIMX : ARY__MXDIM;
   for( i = 0; i < n; i++ ) {
      DIM[ i ] = (int) dim[ i ];
      CHECK_DIM( DIM[ i ], dim[ i ], ARY_DIM, IARY )
   }
   for( ; i < *NDIMX; i++ ) DIM[ i ] = 1;
}


F77_SUBROUTINE(ary_map)( INTEGER(IARY),
                         CHARACTER(TYPE),
                         CHARACTER(MMOD),
                         INTEGER(PNTR),
                         INTEGER(EL),
                         INTEGER(STATUS)
                         TRAIL(TYPE)
                         TRAIL(MMOD) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ ARY__SZMMD + 1 ];
   void *pntr = NULL;
   size_t el;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, ARY__SZMMD, mmod );

   aryMap( aryI2A(*IARY), type, mmod, &pntr, &el, STATUS );

   *PNTR = cnfFptr( pntr );

   *EL = (int) el;
   CHECK_DIM( *EL, el, ARY_MAP, IARY )
}

F77_SUBROUTINE(ary_mapz)( INTEGER(IARY),
                          CHARACTER(TYPE),
                          CHARACTER(MMOD),
                          INTEGER(RPNTR),
                          INTEGER(IPNTR),
                          INTEGER(EL),
                          INTEGER(STATUS)
                          TRAIL(TYPE)
                          TRAIL(MMOD) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(RPNTR)
   GENPTR_INTEGER(IPNTR)
   GENPTR_INTEGER(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ ARY__SZMMD + 1 ];
   void *rpntr = NULL;
   void *ipntr = NULL;
   size_t el;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, ARY__SZMMD, mmod );

   aryMapz( aryI2A(*IARY), type, mmod, &rpntr, &ipntr, &el, STATUS );

   *RPNTR = cnfFptr( rpntr );
   *IPNTR = cnfFptr( ipntr );
   *EL = (int) el;
   CHECK_DIM( *EL, el, ARY_MAP, IARY )
}

F77_SUBROUTINE(ary_new)( CHARACTER(FTYPE),
                         INTEGER(NDIM),
                         INTEGER_ARRAY(LBND),
                         INTEGER_ARRAY(UBND),
                         INTEGER(PLACE),
                         INTEGER(IARY),
                         INTEGER(STATUS)
                         TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   char ftype[ ARY__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, ARY__SZFTP, ftype );
   int i;
   Ary *ary;
   AryPlace *place = aryI2P(*PLACE);

   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];
   int ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   aryNew( ftype, *NDIM, lbnd, ubnd, &place, &ary, STATUS );

   *PLACE = aryP2I(place);
   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_newp)( CHARACTER(FTYPE),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(UBND),
                          INTEGER(PLACE),
                          INTEGER(IARY),
                          INTEGER(STATUS)
                          TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   char ftype[ ARY__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, ARY__SZFTP, ftype );
   Ary *ary;
   int i;
   AryPlace *place = aryI2P(*PLACE);

   hdsdim ubnd[ARY__MXDIM];
   int ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) ubnd[ i ] = (hdsdim) UBND[ i ];

   aryNewp( ftype, *NDIM, ubnd, &place, &ary, STATUS );

   *PLACE = aryP2I(place);
   *IARY = aryA2I(ary);
}

F77_SUBROUTINE(ary_offs)( INTEGER(IARY1),
                          INTEGER(IARY2),
                          INTEGER(MXOFFS),
                          INTEGER_ARRAY(OFFS),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(MXOFFS)
   GENPTR_INTEGER_ARRAY(OFFS)
   GENPTR_INTEGER(STATUS)

   int i, mxoffs;
   hdsdim offs[ ARY__MXDIM ];

   aryOffs( aryI2A(*IARY1), aryI2A2(*IARY2), ARY__MXDIM, offs, STATUS );

   mxoffs = ( *MXOFFS < ARY__MXDIM ) ? *MXOFFS : ARY__MXDIM;
   for( i = 0; i < mxoffs; i++ ) {
      OFFS[ i ] = offs[ i ];
      CHECK_DIM( OFFS[ i ],  offs[ i ], ARY_OFFS, IARY1 )
   }
   for( ; i < *MXOFFS; i++ ) OFFS[ i ] = 1;
}


F77_SUBROUTINE(ary_sbnd)( INTEGER(NDIM),
                          INTEGER_ARRAY(LBND),
                          INTEGER_ARRAY(UBND),
                          INTEGER(IARY),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];

   ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   arySbnd( ndim, lbnd, ubnd, aryI2A(*IARY), STATUS );
}

F77_SUBROUTINE(ary_sect)( INTEGER(IARY1),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(LBND),
                          INTEGER_ARRAY(UBND),
                          INTEGER(IARY2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(IARY2)
   GENPTR_INTEGER(STATUS)

   Ary *ary2;
   int i, ndim;
   hdsdim lbnd[ARY__MXDIM];
   hdsdim ubnd[ARY__MXDIM];

   ndim = ( *NDIM < ARY__MXDIM ) ? *NDIM : ARY__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   arySect( aryI2A(*IARY1), ndim, lbnd, ubnd, &ary2, STATUS );

   *IARY2 = aryA2I(ary2);

}

F77_SUBROUTINE(ary_shift)( INTEGER(NSHIFT),
                           INTEGER_ARRAY(SHIFT),
                           INTEGER(IARY),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(NSHIFT)
   GENPTR_INTEGER_ARRAY(SHIFT)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   int i, nshift;
   hdsdim shift[ARY__MXDIM];
   nshift = ( *NSHIFT < ARY__MXDIM ) ? *NSHIFT : ARY__MXDIM;
   for( i = 0; i < nshift; i++ ) shift[ i ] = (hdsdim) SHIFT[ i ];

   aryShift( nshift, shift, aryI2A(*IARY), STATUS );

}

F77_SUBROUTINE(ary_size)( INTEGER(IARY),
                          INTEGER(NPIX),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(NPIX)
   GENPTR_INTEGER(STATUS)
   size_t npix;

   arySize( aryI2A(*IARY), &npix, STATUS );

   *NPIX = npix;
   CHECK_DIM( *NPIX, npix, ARY_SIZE, IARY )
}












