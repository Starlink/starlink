#include <pthread.h>
#include "f77.h"
#include "ndf_ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "ndf_err.h"
#include "star/hds_fortran.h"

#define CHECK_DIM( IVAL, HDSDIMVAL, FUNC, INDF ) \
   if( *STATUS == SAI__OK && (hdsdim) (IVAL) != (HDSDIMVAL) ) { \
      ndfMsg( "A", *INDF ); \
      *STATUS = NDF__TOOBIG; \
      errRep( " ", #FUNC ": NDF ^A is too large - use NDF V2.", STATUS ); \
   }


/* Prototypes for obsolete functions that are not defined in ndf.h. These
   still need an F77 wrapper for the sake of legacy code. */
void ndfTrace_( int newflg, int *oldflg );
#define ndfTrace  ndfTrace_

/* A pointer to the F77 subroutine supplied to ndf_hout. */
static void ( *Ndf_f77_routin )( INTEGER(NLINES),
                                 CHARACTER_ARRAY(TEXT),
                                 INTEGER(STATUS)
                                 TRAIL(TEXT) );

/* A pthread mutex is used to ensure only one thread is accessing the
   above pointer at any one time. */
static pthread_mutex_t  Ndf_Routin_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Prototype for the C service routine that is called by ndfHout, and
   which invokes the F77 service routine pointed to by global variable
   "routin". */
void ndf1RoutinWrap( int nlines, char *const text[], int *status );




F77_SUBROUTINE(ndf_acget)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           CHARACTER(VALUE),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(VALUE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(STATUS)
   char *comp = cnfCreim( COMP, COMP_length );
   char *value = malloc( VALUE_length+1 );
   if( value ) {
      strcpy( value, "<null>" );
      ndfAcget_( *INDF, comp, *IAXIS, value, VALUE_length+1, STATUS );
      if( strcmp( value, "<null>" ) ) {
         cnfExprt( value, VALUE, VALUE_length );
      }
      free( value );
   }
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_aclen)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(LENGTH),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(LENGTH)
   GENPTR_INTEGER(STATUS)
   size_t length;

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAclen_( *INDF, comp, *IAXIS, &length, STATUS );
   *LENGTH = length;
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_acput)( CHARACTER(VALUE),
                           INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(STATUS)
                           TRAIL(VALUE)
                           TRAIL(COMP) ) {
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)

   char *value = cnfCreim( VALUE, VALUE_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfAcput_( value, *INDF, comp, *IAXIS, STATUS );
   cnfFree( value );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_acre)( INTEGER(INDF),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   ndfAcre_( *INDF, STATUS );
}

F77_SUBROUTINE(ndf_aform)( INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(IAXIS),
                          CHARACTER(FORM),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(FORM) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_CHARACTER(FORM)
   GENPTR_INTEGER(STATUS)
   char form[NDF__SZFRM+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAform_( *INDF, comp, *IAXIS, form, sizeof(form), STATUS );
   cnfFree( comp );

   cnfExprt( form, FORM, FORM_length );
}

F77_SUBROUTINE(ndf_amap)( INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(IAXIS),
                          CHARACTER(TYPE),
                          CHARACTER(MMOD),
                          INTEGER_ARRAY(PNTR),
                          INTEGER(EL),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(TYPE)
                          TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER_ARRAY(PNTR)
   GENPTR_INTEGER(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *pntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipntr;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfAmap_( *INDF, comp, *IAXIS, type, mmod, pntr, &el, STATUS );

   cnfFree( comp );

   for( ipntr = 0; ipntr < 4; ipntr++ ) {
      if( pntr[ ipntr ] ) PNTR[ ipntr ] = cnfFptr( pntr[ ipntr ] );
   }

   *EL = (int) el;
   CHECK_DIM( *EL, el, NDF_AMAP, INDF )
}

F77_SUBROUTINE(ndf_amapk)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           CHARACTER(TYPE),
                           CHARACTER(MMOD),
                           INTEGER_ARRAY(PNTR),
                           INTEGER8(EL),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(TYPE)
                           TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER_ARRAY(PNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *pntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipntr;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfAmap_( *INDF, comp, *IAXIS, type, mmod, pntr, &el, STATUS );

   cnfFree( comp );

   for( ipntr = 0; ipntr < 4; ipntr++ ) {
      if( pntr[ ipntr ] ) PNTR[ ipntr ] = cnfFptr( pntr[ ipntr ] );
   }

   *EL = el;
}

F77_SUBROUTINE(ndf_annul)( INTEGER(INDF),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   ndfAnnul_( INDF, STATUS );
}

F77_SUBROUTINE(ndf_anorm)( INTEGER(INDF),
                           INTEGER(IAXIS),
                           LOGICAL(NORM),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(IAXIS)
   GENPTR_LOGICAL(NORM)
   GENPTR_INTEGER(STATUS)
   int norm;
   ndfAnorm_( *INDF, *IAXIS, &norm, STATUS );
   *NORM = norm ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_arest)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   ndfArest_( *INDF, comp, *IAXIS, STATUS );
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_asnrm)( LOGICAL(NORM),
                           INTEGER(INDF),
                           INTEGER(IAXIS),
                           INTEGER(STATUS) ) {
   GENPTR_LOGICAL(NORM)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)
   ndfAsnrm_( F77_ISTRUE(*NORM)?1:0, *INDF, *IAXIS, STATUS );
}

F77_SUBROUTINE(ndf_astat)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           LOGICAL(STATE),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_LOGICAL(STATE)
   GENPTR_INTEGER(STATUS)
   int state;

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAstat_( *INDF, comp, *IAXIS, &state, STATUS );
   cnfFree( comp );
   *STATE = state ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_astyp)( CHARACTER(FTYPE),
                           INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(STATUS)
                           TRAIL(FTYPE)
                           TRAIL(COMP) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)
   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAstyp_( ftype, *INDF, comp, *IAXIS, STATUS );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_atype)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           CHARACTER(TYPE),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(TYPE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(STATUS)
   char type[DAT__SZTYP+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAtype_( *INDF, comp, *IAXIS, type, sizeof(type), STATUS );
   cnfFree( comp );

   cnfExprt( type, TYPE, TYPE_length );
}

F77_SUBROUTINE(ndf_aunmp)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   ndfAunmp_( *INDF, comp, *IAXIS, STATUS );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_bad)( INTEGER(INDF),
                         CHARACTER(COMP),
                         LOGICAL(CHECK),
                         LOGICAL(BAD),
                         INTEGER(STATUS)
                         TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_LOGICAL(CHECK)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   int bad;

   ndfBad_( *INDF, comp, F77_ISTRUE(*CHECK)?1:0, &bad, STATUS );
   *BAD = bad ? F77_TRUE : F77_FALSE;

   cnfFree( comp );
}

F77_SUBROUTINE(ndf_base)( INTEGER(INDF1),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)
   ndfBase_( *INDF1, INDF2, STATUS );
}

F77_SUBROUTINE(ndf_bb)( INTEGER(INDF),
                        UBYTE(BADBIT),
                        INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_UBYTE(BADBIT)
   GENPTR_INTEGER(STATUS)
   ndfBb_( *INDF, BADBIT, STATUS );
}

F77_SUBROUTINE(ndf_begin)( ){
   ndfBegin_();
}

F77_SUBROUTINE(ndf_block)(INTEGER(INDF1),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(MXDIM),
                          INTEGER(IBLOCK),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(MXDIM)
   GENPTR_INTEGER(IBLOCK)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   int i;
   hdsdim mxdim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) mxdim[ i ] = (hdsdim) MXDIM[ i ];

   ndfBlock_( *INDF1, *NDIM, mxdim, *IBLOCK, INDF2, STATUS );

}

F77_SUBROUTINE(ndf_blockk)(INTEGER(INDF1),
                          INTEGER(NDIM),
                          INTEGER8_ARRAY(MXDIM),
                          INTEGER(IBLOCK),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(MXDIM)
   GENPTR_INTEGER(IBLOCK)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)
   int i;

   hdsdim mxdim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) mxdim[ i ] = (hdsdim) MXDIM[ i ];

   ndfBlock_( *INDF1, *NDIM, mxdim, *IBLOCK, INDF2, STATUS );

}

F77_SUBROUTINE(ndf_cget)( INTEGER(INDF),
                          CHARACTER(COMP),
                          CHARACTER(VALUE),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(VALUE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(STATUS)
   char *comp = cnfCreim( COMP, COMP_length );
   char *value = malloc( VALUE_length+1 );
   if( value ) {
      strcpy( value, "<null>" );
      ndfCget_( *INDF, comp, value, VALUE_length+1, STATUS );
      if( strcmp( value, "<null>" ) ) {
         cnfExprt( value, VALUE, VALUE_length );
      }
      free( value );
   }
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_chunk)(INTEGER(INDF1),
                          INTEGER(MXPIX),
                          INTEGER(ICHUNK),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(MXPIX)
   GENPTR_INTEGER(ICHUNK)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   hdsdim mxpix = (hdsdim) *MXPIX;
   ndfChunk_( *INDF1, mxpix, *ICHUNK, INDF2, STATUS );
}

F77_SUBROUTINE(ndf_chunkk)(INTEGER(INDF1),
                          INTEGER8(MXPIX),
                          INTEGER(ICHUNK),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER8(MXPIX)
   GENPTR_INTEGER(ICHUNK)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   hdsdim mxpix = (hdsdim) *MXPIX;
   ndfChunk_( *INDF1, mxpix, *ICHUNK, INDF2, STATUS );
}

F77_SUBROUTINE(ndf_clen)( INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(LENGTH),
                          INTEGER(STATUS)
                          TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(LENGTH)
   GENPTR_INTEGER(STATUS)
   size_t length;

   char *comp = cnfCreim( COMP, COMP_length );
   ndfClen_( *INDF, comp, &length, STATUS );
   *LENGTH = length;
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_clone)( INTEGER(INDF1),
                           INTEGER(INDF2),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)
   ndfClone_( *INDF1, INDF2, STATUS );
}

F77_SUBROUTINE(ndf_cmplx)( INTEGER(INDF),
                           CHARACTER(COMP),
                           LOGICAL(CMPLX),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_LOGICAL(CMPLX)
   GENPTR_INTEGER(STATUS)
   char *comp = cnfCreim( COMP, COMP_length );
   int cmplx;
   ndfCmplx_( *INDF, comp, &cmplx, STATUS );
   *CMPLX = cmplx ? F77_TRUE : F77_FALSE;
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_cmsg)( CHARACTER(TOKEN),
                          INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(STATUS)
                          TRAIL(TOKEN)
                          TRAIL(COMP) ) {
   GENPTR_CHARACTER(TOKEN)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *token = cnfCreim( TOKEN, TOKEN_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfCmsg_( token, *INDF, comp, STATUS );
   cnfFree( comp );
   cnfFree( token );
}

F77_SUBROUTINE(ndf_copy)( INTEGER(INDF1),
                          INTEGER(PLACE),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)
   ndfCopy_( *INDF1, PLACE, INDF2, STATUS );
}

F77_SUBROUTINE(ndf_cput)( CHARACTER(VALUE),
                          INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(STATUS)
                          TRAIL(VALUE)
                          TRAIL(COMP) ) {
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *value = cnfCreim( VALUE, VALUE_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfCput_( value, *INDF, comp, STATUS );
   cnfFree( value );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_end)( INTEGER(STATUS) ) {
   GENPTR_INTEGER(STATUS)
   ndfEnd_( STATUS );
}

F77_SUBROUTINE(ndf_hend)( INTEGER(STATUS) ) {
   GENPTR_INTEGER(STATUS)
   ndfHend_( STATUS );
}

F77_SUBROUTINE(ndf_find)( CHARACTER(LOC),
                          CHARACTER(NAME),
                          INTEGER(INDF),
                          INTEGER(STATUS)
                          TRAIL(LOC)
                          TRAIL(NAME) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;
   if( strncmp( DAT__ROOT, LOC, LOC_length ) ) {
      datImportFloc( LOC, LOC_length, &loc, STATUS );
   }
   char *name = cnfCreim( NAME, NAME_length );
   ndfFind_( loc, name, INDF, STATUS );
   cnfFree( name );
}

F77_SUBROUTINE(ndf_ftype)( INTEGER(INDF),
                           CHARACTER(COMP),
                           CHARACTER(FTYPE),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(FTYPE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(STATUS)
   char ftype[NDF__SZFTP+1];
   char *comp = cnfCreim( COMP, COMP_length );

   ndfFtype_( *INDF, comp, ftype, sizeof(ftype), STATUS );
   cnfExprt( ftype, FTYPE, FTYPE_length );

   cnfFree( comp );
}

F77_SUBROUTINE(ndf_happn)( CHARACTER(APPN),
                           INTEGER(STATUS)
                           TRAIL(APPN) ) {
   GENPTR_CHARACTER(APPN)
   GENPTR_INTEGER(STATUS)
   char *appn = cnfCreim( APPN, APPN_length );
   ndfHappn_( appn, STATUS );
   cnfFree( appn );
}

F77_SUBROUTINE(ndf_hcre)( INTEGER(INDF),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   ndfHcre_( *INDF, STATUS );
}

F77_SUBROUTINE(ndf_hdef)( INTEGER(INDF),
                          CHARACTER(APPN),
                          INTEGER(STATUS)
                          TRAIL(APPN) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(APPN)
   GENPTR_INTEGER(STATUS)
   char *appn = cnfCreim( APPN, APPN_length );
   ndfHdef_( *INDF, appn, STATUS );
   cnfFree( appn );
}

F77_SUBROUTINE(ndf_hecho)( INTEGER(NLINES),
                           CHARACTER_ARRAY(TEXT),
                           INTEGER(STATUS)
                           TRAIL(TEXT) ) {

   GENPTR_INTEGER(NLINES)
   GENPTR_CHARACTER_ARRAY(TEXT)
   GENPTR_INTEGER(STATUS)
   int i;

   char **parray = calloc( *NLINES, sizeof(*parray) );
   if( parray ) {
      for( i = 0; i < *NLINES; i++ ) parray[ i ] = malloc( TEXT_length + 1 );
      cnfImprtap( TEXT, TEXT_length, parray, TEXT_length + 1, 1, NLINES );
      ndfHecho_( *NLINES, parray, STATUS );
      for( i = 0; i < *NLINES; i++ ) free( parray[ i ] );
      free( parray );
   }
}

F77_SUBROUTINE(ndf_hinfo)( INTEGER(INDF),
                           CHARACTER(ITEM),
                           INTEGER(IREC),
                           CHARACTER(VALUE),
                           INTEGER(STATUS)
                           TRAIL(ITEM)
                           TRAIL(VALUE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(ITEM)
   GENPTR_INTEGER(IREC)
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(STATUS)
   char *item = cnfCreim( ITEM, ITEM_length );
   char *value = malloc( VALUE_length + 1 );
   if( value ) {
      strcpy( value, "<null>" );
      ndfHinfo_( *INDF, item, *IREC, value, VALUE_length + 1,
                 STATUS );
      if( strcmp( value, "<null>" ) ) {
         cnfExprt( value, VALUE, VALUE_length );
      }
      free( value );
   }
   cnfFree( item );
}

F77_SUBROUTINE(ndf_hndlr)( CHARACTER(EVNAME),
                           NdfEventHandler HANDLR,
                           LOGICAL(SET),
                           INTEGER(STATUS)
                           TRAIL(EVNAME) ) {
   GENPTR_CHARACTER(EVNAME)
   GENPTR_LOGICAL(SET)
   GENPTR_INTEGER(STATUS)
   char *evname = cnfCreim( EVNAME, EVNAME_length );
   ndfHndlr_( evname, HANDLR, F77_ISTRUE(*SET)?1:0, STATUS );
   cnfFree( evname );
}

F77_SUBROUTINE(ndf_hnrec)( INTEGER(INDF),
                           INTEGER(NREC),
                           INTEGER(STATUS) ){
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NREC)
   GENPTR_INTEGER(STATUS)
   ndfHnrec_( *INDF, NREC, STATUS );
}

F77_SUBROUTINE(ndf_hout)( INTEGER(INDF),
                          INTEGER(IREC),
                          void (* ROUTIN)(),
                          INTEGER(STATUS) ){
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(IREC)

/* Store a pointer to the supplied F77 service routine in a global
   variable so that the C service routine, ndf1RoutinWrap, can call it.
   Use a mutex to serialise access to this global variable. */
   pthread_mutex_lock( &Ndf_Routin_mutex );
   Ndf_f77_routin = (void (*)( INTEGER(NLINES),
                               CHARACTER_ARRAY(TEXT),
                               INTEGER(STATUS)
                               TRAIL(TEXT) ) ) ROUTIN;

/* Call the C function to output the lines of History text, telling it to
   use ndf1RoutinWrap to write out each line. */
   ndfHout_( *INDF, *IREC, ndf1RoutinWrap, STATUS );

/* Unlock the mutex. */
   pthread_mutex_unlock( &Ndf_Routin_mutex );
}

/* This function invokes the F77 ndfHout service function whose pointer is
   stored in global variable Ndf_f77_routin in order to write out lines
   of history text. */
void ndf1RoutinWrap( int nlines, char *const text[], int *status ){
   DECLARE_INTEGER(NLINES);
   DECLARE_CHARACTER_ARRAY_DYN(TEXT);
   DECLARE_INTEGER(STATUS);
   int len;
   int i;

   if( *status != SAI__OK ) return;

/* Find the length of the longest supplied line of text. */
   TEXT_length = 0;
   for( i = 0; i < nlines; i++ ) {
      len = strlen( text[ i ] );
      if( len > TEXT_length ) TEXT_length = len;
   }

/* Allocate room for the array of space-padded F77 strings. */
   TEXT = malloc( TEXT_length*nlines );
   if( TEXT ){

/* Export the C null-terminated string array to the F77 space-padded string
   array. */
      F77_EXPORT_CHARACTER_ARRAY_P(text,TEXT,TEXT_length,nlines)

/* Export the other supplied C values to equivalent F77 values. */
      NLINES = nlines;
      STATUS = *status;

/* Call the F77 service routine. */
      (*Ndf_f77_routin)( INTEGER_ARG(&NLINES),
                         CHARACTER_ARRAY_ARG(TEXT),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(TEXT) );

/* Free the F77 copy of the text lines. */
      free( TEXT );

/* Import the new F77 status to C. */
      *status = STATUS;
   }
}

F77_SUBROUTINE(ndf_hput)( CHARACTER(HMODE),
                          CHARACTER(APPN),
                          LOGICAL(REPL),
                          INTEGER(NLINES),
                          CHARACTER_ARRAY(TEXT),
                          LOGICAL(TRANS),
                          LOGICAL(WRAP),
                          LOGICAL(RJUST),
                          INTEGER(INDF),
                          INTEGER(STATUS)
                          TRAIL(HMODE)
                          TRAIL(APPN)
                          TRAIL(TEXT) ) {

   GENPTR_CHARACTER(HMODE)
   GENPTR_CHARACTER(APPN)
   GENPTR_LOGICAL(REPL)
   GENPTR_INTEGER(NLINES)
   GENPTR_CHARACTER_ARRAY(TEXT)
   GENPTR_LOGICAL(TRANS)
   GENPTR_LOGICAL(WRAP)
   GENPTR_LOGICAL(RJUST)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   int i;

   char *hmode = cnfCreim( HMODE, HMODE_length );
   char *appn = cnfCreim( APPN, APPN_length );

   char **parray = calloc( *NLINES, sizeof(*parray) );
   if( parray ) {
      for( i = 0; i < *NLINES; i++ ) parray[ i ] = malloc( TEXT_length + 1 );
      cnfImprtap( TEXT, TEXT_length, parray, TEXT_length + 1, 1, NLINES );

      ndfHput_( hmode, appn, F77_ISTRUE(*REPL)?1:0, *NLINES, parray,
                F77_ISTRUE(*TRANS)?1:0, F77_ISTRUE(*WRAP)?1:0,
                F77_ISTRUE(*RJUST)?1:0, *INDF, STATUS );

      for( i = 0; i < *NLINES; i++ ) free( parray[ i ] );
      free( parray );
   }

   cnfFree( hmode );
   cnfFree( appn );
}

F77_SUBROUTINE(ndf_hsdat)( CHARACTER(DATE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(DATE) ) {
   GENPTR_CHARACTER(DATE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *date = cnfCreim( DATE, DATE_length );
   ndfHsdat_( date, *INDF, STATUS );
   cnfFree( date );
}

F77_SUBROUTINE(ndf_hsmod)( CHARACTER(HMODE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(HMODE) ) {
   GENPTR_CHARACTER(HMODE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *hmode = cnfCreim( HMODE, HMODE_length );
   ndfHsmod_( hmode, *INDF, STATUS );
   cnfFree( hmode );
}

F77_SUBROUTINE(ndf_loc)( INTEGER(INDF),
                         CHARACTER(MODE),
                         CHARACTER(LOC),
                         INTEGER(STATUS)
                         TRAIL(MODE)
                         TRAIL(LOC) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(MODE)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;
   char *mode = cnfCreim( MODE, MODE_length );

   ndfLoc_( *INDF, mode, &loc, STATUS );
   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );
   cnfFree( mode );
}

F77_SUBROUTINE(ndf_mbad)( LOGICAL(BADOK),
                          INTEGER(INDF1),
                          INTEGER(INDF2),
                          CHARACTER(COMP),
                          LOGICAL(CHECK),
                          LOGICAL(BAD),
                          INTEGER(STATUS)
                          TRAIL(COMP) ) {
   GENPTR_LOGICAL(BADOK)
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_CHARACTER(COMP)
   GENPTR_LOGICAL(CHECK)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   int bad;

   ndfMbad_( F77_ISTRUE(*BADOK)?1:0, *INDF1, *INDF2, comp,
             F77_ISTRUE(*CHECK)?1:0, &bad, STATUS );
   *BAD = bad ? F77_TRUE : F77_FALSE;

   cnfFree( comp );
}

F77_SUBROUTINE(ndf_mbadn)( LOGICAL(BADOK),
                           INTEGER(N),
                           INTEGER_ARRAY(INDFS),
                           CHARACTER(COMP),
                           LOGICAL(CHECK),
                           LOGICAL(BAD),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_LOGICAL(BADOK)
   GENPTR_INTEGER(N)
   GENPTR_INTEGER_ARRAY(INDFS)
   GENPTR_CHARACTER(COMP)
   GENPTR_LOGICAL(CHECK)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   int bad;

   ndfMbadn_( F77_ISTRUE(*BADOK)?1:0, *N, INDFS, comp,
              F77_ISTRUE(*CHECK)?1:0, &bad, STATUS );
   *BAD = bad ? F77_TRUE : F77_FALSE;

   cnfFree( comp );
}

F77_SUBROUTINE(ndf_mbnd)( CHARACTER(OPTION),
                          INTEGER(INDF1),
                          INTEGER(INDF2),
                          INTEGER(STATUS)
                          TRAIL(OPTION) ) {
   GENPTR_CHARACTER(OPTION)
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   char *option = cnfCreim( OPTION, OPTION_length );
   ndfMbnd_( option, INDF1, INDF2, STATUS );
   cnfFree( option );
}

F77_SUBROUTINE(ndf_mbndn)( CHARACTER(OPTION),
                           INTEGER(N),
                           INTEGER(NDFS),
                           INTEGER(STATUS)
                           TRAIL(OPTION) ) {
   GENPTR_CHARACTER(OPTION)
   GENPTR_INTEGER(N)
   GENPTR_INTEGER(NDFS)
   GENPTR_INTEGER(STATUS)

   char *option = cnfCreim( OPTION, OPTION_length );
   ndfMbndn_( option, *N, NDFS, STATUS );
   cnfFree( option );
}

F77_SUBROUTINE(ndf_msg)( CHARACTER(TOKEN),
                         INTEGER(INDF)
                         TRAIL(TOKEN) ) {
   GENPTR_CHARACTER(TOKEN)
   GENPTR_INTEGER(INDF)
   char *token = cnfCreim( TOKEN, TOKEN_length );
   ndfMsg_( token, *INDF );
   cnfFree( token );
}

F77_SUBROUTINE(ndf_mtype)( CHARACTER(TYPLST),
                           INTEGER(INDF1),
                           INTEGER(INDF2),
                           CHARACTER(COMP),
                           CHARACTER(ITYPE),
                           CHARACTER(DTYPE),
                           INTEGER(STATUS)
                           TRAIL(TYPLST)
                           TRAIL(COMP)
                           TRAIL(ITYPE)
                           TRAIL(DTYPE) ) {
   GENPTR_CHARACTER(TYPLST)
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(ITYPE)
   GENPTR_CHARACTER(DTYPE)
   GENPTR_INTEGER(STATUS)
   char itype[NDF__SZTYP+1];
   char dtype[NDF__SZFTP+1];

   char *typlst = cnfCreim( TYPLST, TYPLST_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfMtype_( typlst, *INDF1, *INDF2, comp, itype, sizeof(itype),
              dtype, sizeof(dtype), STATUS );
   cnfFree( comp );
   cnfFree( typlst );

   cnfExprt( itype, ITYPE, ITYPE_length );
   cnfExprt( dtype, DTYPE, DTYPE_length );
}

F77_SUBROUTINE(ndf_mtypn)( CHARACTER(TYPLST),
                           INTEGER(N),
                           INTEGER(NDFS),
                           CHARACTER(COMP),
                           CHARACTER(ITYPE),
                           CHARACTER(DTYPE),
                           INTEGER(STATUS)
                           TRAIL(TYPLST)
                           TRAIL(COMP)
                           TRAIL(ITYPE)
                           TRAIL(DTYPE) ) {
   GENPTR_CHARACTER(TYPLST)
   GENPTR_INTEGER(N)
   GENPTR_INTEGER(NDFS)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(ITYPE)
   GENPTR_CHARACTER(DTYPE)
   GENPTR_INTEGER(STATUS)
   char itype[NDF__SZTYP+1];
   char dtype[NDF__SZFTP+1];

   char *typlst = cnfCreim( TYPLST, TYPLST_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfMtypn_( typlst, *N, NDFS, comp, itype, sizeof(itype),
              dtype, sizeof(dtype), STATUS );
   cnfFree( comp );
   cnfFree( typlst );

   cnfExprt( itype, ITYPE, ITYPE_length );
   cnfExprt( dtype, DTYPE, DTYPE_length );
}

F77_SUBROUTINE(ndf_open)( CHARACTER(LOC),
                          CHARACTER(NAME),
                          CHARACTER(MODE),
                          CHARACTER(STAT),
                          INTEGER(INDF),
                          INTEGER(PLACE),
                          INTEGER(STATUS)
                          TRAIL(LOC)
                          TRAIL(NAME)
                          TRAIL(MODE)
                          TRAIL(STAT) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_CHARACTER(NAME)
   GENPTR_CHARACTER(MODE)
   GENPTR_CHARACTER(STAT)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;

   if( strncmp( DAT__ROOT, LOC, LOC_length ) ) {
      datImportFloc( LOC, LOC_length, &loc, STATUS );
   }

   char *name = cnfCreim( NAME, NAME_length );
   char *mode = cnfCreim( MODE, MODE_length );
   char *stat = cnfCreim( STAT, STAT_length );

   ndfOpen_( loc, name, mode, stat, INDF, PLACE, STATUS );

   cnfFree( name );
   cnfFree( mode );
   cnfFree( stat );

}

F77_SUBROUTINE(ndf_same)( INTEGER(INDF1),
                          INTEGER(INDF2),
                          LOGICAL(SAME),
                          LOGICAL(ISECT),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(INDF2)
   GENPTR_LOGICAL(SAME)
   GENPTR_LOGICAL(ISECT)
   GENPTR_INTEGER(STATUS)

   int same, isect;

   ndfSame_( *INDF1, *INDF2, &same, &isect, STATUS );

   F77_EXPORT_LOGICAL( same, *SAME );
   F77_EXPORT_LOGICAL( isect, *ISECT );
}

F77_SUBROUTINE(ndf_delet)( INTEGER(INDF),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   ndfDelet_( INDF, STATUS );
}

F77_SUBROUTINE(ndf_form)( INTEGER(INDF),
                          CHARACTER(COMP),
                          CHARACTER(FORM),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(FORM) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(FORM)
   GENPTR_INTEGER(STATUS)
   char form[NDF__SZFRM+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfForm_( *INDF, comp, form, sizeof(form), STATUS );
   cnfFree( comp );

   cnfExprt( form, FORM, FORM_length );
}

F77_SUBROUTINE(ndf_gtdlt)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(ZAXIS),
                           CHARACTER(ZTYPE),
                           REAL(ZRATIO),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(ZTYPE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(ZAXIS)
   GENPTR_CHARACTER(ZTYPE)
   GENPTR_REAL(ZRATIO)
   GENPTR_INTEGER(STATUS)
   char ztype[DAT__SZTYP+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfGtdlt_( *INDF, comp, ZAXIS, ztype, sizeof( ztype ), ZRATIO, STATUS );
   cnfFree( comp );

   cnfExprt( ztype, ZTYPE, ZTYPE_length );
}

F77_SUBROUTINE(ndf_imprt)( CHARACTER(LOC),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(LOC) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;
   if( strncmp( DAT__ROOT, LOC, LOC_length ) ) {
      datImportFloc( LOC, LOC_length, &loc, STATUS );
   }

   ndfFind_( loc, " ", INDF, STATUS );
}

F77_SUBROUTINE(ndf_isacc)( INTEGER(INDF),
                           CHARACTER(ACCESS),
                           LOGICAL(ISACC),
                           INTEGER(STATUS)
                           TRAIL(ACCESS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(ACCESS)
   GENPTR_LOGICAL(ISACC)
   GENPTR_INTEGER(STATUS)
   int isacc;

   char access[ NDF__SZACC + 1 ];
   cnfImpn( ACCESS, ACCESS_length, NDF__SZACC, access );

   ndfIsacc_( *INDF, access, &isacc, STATUS );
   *ISACC = isacc ? F77_TRUE : F77_FALSE;

}

F77_SUBROUTINE(ndf_isbas)( INTEGER(INDF),
                           LOGICAL(BASE),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_LOGICAL(BASE)
   GENPTR_INTEGER(STATUS)
   int base;

   ndfIsbas_( *INDF, &base, STATUS );
   *BASE = base ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_istmp)( INTEGER(INDF),
                           LOGICAL(TEMP),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_LOGICAL(TEMP)
   GENPTR_INTEGER(STATUS)
   int temp;

   ndfIstmp_( *INDF, &temp, STATUS );
   *TEMP = temp ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_nbloc)(INTEGER(INDF),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(MXDIM),
                          INTEGER(NBLOCK),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(MXDIM)
   GENPTR_INTEGER(NBLOCK)
   GENPTR_INTEGER(STATUS)

   int i;
   hdsdim mxdim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) mxdim[ i ] = (hdsdim) MXDIM[ i ];

   ndfNbloc_( *INDF, *NDIM, mxdim, NBLOCK, STATUS );

}

F77_SUBROUTINE(ndf_nblock)(INTEGER(INDF),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(MXDIM),
                           INTEGER(NBLOCK),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(MXDIM)
   GENPTR_INTEGER(NBLOCK)
   GENPTR_INTEGER(STATUS)

   int i;
   hdsdim mxdim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) mxdim[ i ] = (hdsdim) MXDIM[ i ];

   ndfNbloc_( *INDF, *NDIM, mxdim, NBLOCK, STATUS );

}

F77_SUBROUTINE(ndf_noacc)( CHARACTER(ACCESS),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(ACCESS) ) {
   GENPTR_CHARACTER(ACCESS)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char access[ NDF__SZACC + 1 ];
   cnfImpn( ACCESS, ACCESS_length, NDF__SZACC, access );

   ndfNoacc_( access, *INDF, STATUS );
}

F77_SUBROUTINE(ndf_place)( CHARACTER(LOC),
                           CHARACTER(NAME),
                           INTEGER(IPLACE),
                           INTEGER(STATUS)
                           TRAIL(LOC)
                           TRAIL(NAME) ) {
   GENPTR_CHARACTER(LOC)
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(IPLACE)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;
   if( strncmp( DAT__ROOT, LOC, LOC_length ) ) {
      datImportFloc( LOC, LOC_length, &loc, STATUS );
   }

   char *name = cnfCreim( NAME, NAME_length );
   ndfPlace_( loc, name, IPLACE, STATUS );
   cnfFree( name );
}

F77_SUBROUTINE(ndf_ptwcs)( INTEGER(IWCS),
                           INTEGER(INDF),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IWCS)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   ndfPtwcs_( astI2P( *IWCS ), *INDF, STATUS );
}

F77_SUBROUTINE(ndf_reset)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   ndfReset_( *INDF, comp, STATUS );
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_sbad)( LOGICAL(BAD),
                          INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(STATUS)
                          TRAIL(COMP) ) {
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   ndfSbad_( (*BAD == F77_TRUE), *INDF, comp, STATUS );
   cnfFree( comp );
}

F77_SUBROUTINE(ndf_sbb)( UBYTE(BADBIT),
                         INTEGER(INDF),
                         INTEGER(STATUS) ) {
   GENPTR_UBYTE(BADBIT)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   ndfSbb_( *BADBIT, *INDF, STATUS );
}

F77_SUBROUTINE(ndf_scopy)( INTEGER(INDF1),
                           CHARACTER(CLIST),
                           INTEGER(PLACE),
                           INTEGER(INDF2),
                           INTEGER(STATUS)
                           TRAIL(CLIST) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_CHARACTER(CLIST)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   char *clist = cnfCreim( CLIST, CLIST_length );
   ndfScopy_( *INDF1, clist, PLACE, INDF2, STATUS );
   cnfFree( clist );
}

F77_SUBROUTINE(ndf_sqmf)( LOGICAL(QMF),
                          INTEGER(INDF),
                          INTEGER(STATUS) ) {
   GENPTR_LOGICAL(QMF)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   ndfSqmf_( F77_ISTRUE(*QMF)?1:0, *INDF, STATUS );
}

F77_SUBROUTINE(ndf_gtune)( CHARACTER(TPAR),
                           INTEGER(VALUE),
                           INTEGER(STATUS)
                           TRAIL(TPAR) ) {
   GENPTR_CHARACTER(TPAR)
   GENPTR_INTEGER(VALUE)
   GENPTR_INTEGER(STATUS)

   char *tpar = cnfCreim( TPAR, TPAR_length );
   ndfGtune_( tpar, VALUE, STATUS );
   cnfFree( tpar );
}

F77_SUBROUTINE(ndf_gtwcs)( INTEGER(INDF),
                           INTEGER(IWCS),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(IWCS)
   GENPTR_INTEGER(STATUS)
   AstFrameSet *wcs;
   ndfGtwcs_( *INDF, &wcs, STATUS );
   *IWCS = astP2I( wcs );
}

F77_SUBROUTINE(ndf_sctyp)( INTEGER(INDF),
                           CHARACTER(COMP),
                           CHARACTER(TYPE),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(TYPE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(STATUS)
   char type[DAT__SZTYP+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfSctyp_( *INDF, comp, type, sizeof(type), STATUS );
   cnfFree( comp );

   cnfExprt( type, TYPE, TYPE_length );

}

F77_SUBROUTINE(ndf_state)( INTEGER(INDF),
                           CHARACTER(COMP),
                           LOGICAL(STATE),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_LOGICAL(STATE)
   GENPTR_INTEGER(STATUS)
   int state;

   char *comp = cnfCreim( COMP, COMP_length );
   ndfState_( *INDF, comp, &state, STATUS );
   cnfFree( comp );
   *STATE = state ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_stype)( CHARACTER(FTYPE),
                           INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(STATUS)
                           TRAIL(FTYPE)
                           TRAIL(COMP) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)
   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );

   char *comp = cnfCreim( COMP, COMP_length );
   ndfStype_( ftype, *INDF, comp, STATUS );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_temp)( INTEGER(IPLACE),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(IPLACE)
   GENPTR_INTEGER(STATUS)

   ndfTemp_( IPLACE, STATUS );
}

F77_SUBROUTINE(ndf_trace)( LOGICAL(NEWFLG),
                           LOGICAL(OLDFLG) ) {
   GENPTR_LOGICAL(NEWFLG)
   GENPTR_LOGICAL(OLDFLG)
   int oldflg;

   ndfTrace_( F77_ISTRUE(*NEWFLG)?1:0, &oldflg );
   *OLDFLG = oldflg ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_tune)( INTEGER(VALUE),
                          CHARACTER(TPAR),
                          INTEGER(STATUS)
                          TRAIL(TPAR) ) {
   GENPTR_INTEGER(VALUE)
   GENPTR_CHARACTER(TPAR)
   GENPTR_INTEGER(STATUS)

   char *tpar = cnfCreim( TPAR, TPAR_length );
   ndfTune_( *VALUE, tpar, STATUS );
   cnfFree( tpar );
}

F77_SUBROUTINE(ndf_type)( INTEGER(INDF),
                          CHARACTER(COMP),
                          CHARACTER(TYPE),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(TYPE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(STATUS)
   char type[DAT__SZTYP+1];

   char *comp = cnfCreim( COMP, COMP_length );
   ndfType_( *INDF, comp, type, sizeof(type), STATUS );
   cnfFree( comp );

   cnfExprt( type, TYPE, TYPE_length );
}

F77_SUBROUTINE(ndf_unmap)( INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(STATUS)
                           TRAIL(COMP) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   ndfUnmap_( *INDF, comp, STATUS );
   cnfFree( comp );

}

F77_SUBROUTINE(ndf_valid)( INTEGER(INDF),
                           LOGICAL(VALID),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_LOGICAL(VALID)
   GENPTR_INTEGER(STATUS)
   int valid;

   ndfValid_( *INDF, &valid, STATUS );
   *VALID = valid ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_xdel)( INTEGER(INDF),
                          CHARACTER(XNAME),
                          INTEGER(STATUS)
                          TRAIL(XNAME) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_INTEGER(STATUS)

   char *xname = cnfCreim( XNAME, XNAME_length );
   ndfXdel_( *INDF, xname, STATUS );
   cnfFree( xname );
}

F77_SUBROUTINE(ndf_xgt0c)( INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(CMPT),
                           CHARACTER(VALUE),
                           INTEGER(STATUS)
                           TRAIL(XNAME)
                           TRAIL(CMPT)
                           TRAIL(VALUE) ){
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(CMPT)
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(STATUS)
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *cmpt = cnfCreim( CMPT, CMPT_length );
   char *value = malloc( VALUE_length+1 );
   if( value ) {
      strcpy( value, "<null>" );
      ndfXgt0c_( *INDF, xname, cmpt, value, VALUE_length+1, STATUS );
      if( strcmp( value, "<null>" ) ) {
         cnfExprt( value, VALUE, VALUE_length );
      }
      free( value );
   }
   cnfFree( cmpt );
   cnfFree( xname );
}

F77_SUBROUTINE(ndf_xgt0l)( INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(CMPT),
                           LOGICAL(VALUE),
                           INTEGER(STATUS)
                           TRAIL(XNAME)
                           TRAIL(CMPT) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(CMPT)
   GENPTR_LOGICAL(VALUE)
   GENPTR_INTEGER(STATUS)
   int value;

   char *xname = cnfCreim( XNAME, XNAME_length );
   char *cmpt = cnfCreim( CMPT, CMPT_length );
   ndfXgt0l_( *INDF, xname, cmpt, &value, STATUS );
   *VALUE = value ? F77_TRUE : F77_FALSE;
   cnfFree( cmpt );
   cnfFree( xname );
}


#define MAKE_XGT0(CT,FT,FTYPE) \
F77_SUBROUTINE(ndf_xgt0##FT)( INTEGER(INDF), \
                              CHARACTER(XNAME), \
                              CHARACTER(CMPT), \
                              FTYPE(VALUE), \
                              INTEGER(STATUS) \
                              TRAIL(XNAME) \
                              TRAIL(CMPT) ) { \
   GENPTR_INTEGER(INDF) \
   GENPTR_CHARACTER(XNAME) \
   GENPTR_CHARACTER(CMPT) \
   GENPTR_##FTYPE(VALUE) \
   GENPTR_INTEGER(STATUS) \
\
   char *xname = cnfCreim( XNAME, XNAME_length ); \
   char *cmpt = cnfCreim( CMPT, CMPT_length ); \
   ndfXgt0##CT##_( *INDF, xname, cmpt, VALUE, STATUS ); \
   cnfFree( cmpt ); \
   cnfFree( xname ); \
}

MAKE_XGT0(B, b, BYTE)
MAKE_XGT0(D, d, DOUBLE)
MAKE_XGT0(F, r, REAL)
MAKE_XGT0(I, i, INTEGER)
MAKE_XGT0(K, k, INTEGER8)
MAKE_XGT0(UB, ub, UBYTE)
MAKE_XGT0(UW, uw, UWORD)
MAKE_XGT0(W, w, WORD)

#undef MAKE_XGT0


F77_SUBROUTINE(ndf_xloc)( INTEGER(INDF),
                         CHARACTER(XNAME),
                         CHARACTER(MODE),
                         CHARACTER(LOC),
                         INTEGER(STATUS)
                         TRAIL(XNAME)
                         TRAIL(MODE)
                         TRAIL(LOC) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(MODE)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *loc = NULL;
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *mode = cnfCreim( MODE, MODE_length );

   ndfXloc_( *INDF, xname, mode, &loc, STATUS );
   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );
   cnfFree( xname );
   cnfFree( mode );
}

F77_SUBROUTINE(ndf_xiary)( INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(CMPT),
                           CHARACTER(MODE),
                           INTEGER(IARY),
                           INTEGER(STATUS)
                           TRAIL(XNAME)
                           TRAIL(CMPT)
                           TRAIL(MODE) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(CMPT)
   GENPTR_CHARACTER(MODE)
   GENPTR_INTEGER(IARY)
   GENPTR_INTEGER(STATUS)

   Ary *ary = NULL;
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *cmpt = cnfCreim( CMPT, CMPT_length );
   char *mode = cnfCreim( MODE, MODE_length );

   ndfXiary_( *INDF, xname, cmpt, mode, &ary, STATUS );
   *IARY = aryA2I(ary);

   cnfFree( xname );
   cnfFree( cmpt );
   cnfFree( mode );
}

F77_SUBROUTINE(ndf_xname)( INTEGER(INDF),
                           INTEGER(N),
                           CHARACTER(XNAME),
                           INTEGER(STATUS)
                           TRAIL(XNAME) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(N)
   GENPTR_CHARACTER(XNAME)
   GENPTR_INTEGER(STATUS)
   char xname[DAT__SZNAM+1];

   ndfXname_( *INDF, *N, xname, sizeof(xname), STATUS );
   cnfExprt( xname, XNAME, XNAME_length );

}

F77_SUBROUTINE(ndf_xnew)( INTEGER(INDF),
                          CHARACTER(XNAME),
                          CHARACTER(TYPE),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(DIM),
                          CHARACTER(LOC),
                          INTEGER(STATUS)
                          TRAIL(XNAME)
                          TRAIL(TYPE)
                          TRAIL(LOC) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(DIM)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)
   int i;

   HDSLoc *loc = NULL;
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *type = cnfCreim( TYPE, TYPE_length );

   hdsdim dim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) dim[ i ] = (hdsdim) DIM[ i ];

   ndfXnew_( *INDF, xname, type, *NDIM, dim, &loc, STATUS );

   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );
   cnfFree( xname );
   cnfFree( type );
}

F77_SUBROUTINE(ndf_xnewk)( INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(TYPE),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(DIM),
                           CHARACTER(LOC),
                           INTEGER(STATUS)
                           TRAIL(XNAME)
                           TRAIL(TYPE)
                           TRAIL(LOC) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(DIM)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)
   int i;

   HDSLoc *loc = NULL;
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *type = cnfCreim( TYPE, TYPE_length );

   hdsdim dim[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) dim[ i ] = (hdsdim) DIM[ i ];

   ndfXnew_( *INDF, xname, type, *NDIM, dim, &loc, STATUS );

   datExportFloc( &loc, 1, LOC_length, LOC, STATUS );
   cnfFree( xname );
   cnfFree( type );
}

F77_SUBROUTINE(ndf_xnumb)( INTEGER(INDF),
                           INTEGER(NEXTN),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NEXTN)
   GENPTR_INTEGER(STATUS)

   ndfXnumb_( *INDF, NEXTN, STATUS );
}

F77_SUBROUTINE(ndf_xpt0c)( CHARACTER(VALUE),
                           INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(CMPT),
                           INTEGER(STATUS)
                           TRAIL(VALUE)
                           TRAIL(XNAME)
                           TRAIL(CMPT) ) {
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(CMPT)
   GENPTR_INTEGER(STATUS)
   char *value = cnfCreim( VALUE, VALUE_length );
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *cmpt = cnfCreim( CMPT, CMPT_length );
   ndfXpt0c_( value, *INDF, xname, cmpt, STATUS );
   cnfFree( cmpt );
   cnfFree( xname );
   cnfFree( value );
}

F77_SUBROUTINE(ndf_xpt0l)( LOGICAL(VALUE),
                           INTEGER(INDF),
                           CHARACTER(XNAME),
                           CHARACTER(CMPT),
                           INTEGER(STATUS)
                           TRAIL(XNAME)
                           TRAIL(CMPT) ) {
   GENPTR_LOGICAL(VALUE)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_CHARACTER(CMPT)
   GENPTR_INTEGER(STATUS)
   char *xname = cnfCreim( XNAME, XNAME_length );
   char *cmpt = cnfCreim( CMPT, CMPT_length );
   ndfXpt0l_( F77_ISTRUE(*VALUE)?1:0, *INDF, xname, cmpt, STATUS );
   cnfFree( cmpt );
   cnfFree( xname );
}

#define MAKE_XPT0(CT,FT,FTYPE) \
F77_SUBROUTINE(ndf_xpt0##FT)( FTYPE(VALUE), \
                              INTEGER(INDF), \
                              CHARACTER(XNAME), \
                              CHARACTER(CMPT), \
                              INTEGER(STATUS) \
                              TRAIL(XNAME) \
                              TRAIL(CMPT) ) { \
   GENPTR_##FTYPE(VALUE) \
   GENPTR_INTEGER(INDF) \
   GENPTR_CHARACTER(XNAME) \
   GENPTR_CHARACTER(CMPT) \
   GENPTR_INTEGER(STATUS) \
\
   char *xname = cnfCreim( XNAME, XNAME_length ); \
   char *cmpt = cnfCreim( CMPT, CMPT_length ); \
   ndfXpt0##CT##_( *VALUE, *INDF, xname, cmpt, STATUS ); \
   cnfFree( cmpt ); \
   cnfFree( xname ); \
}

MAKE_XPT0(B, b, BYTE)
MAKE_XPT0(D, d, DOUBLE)
MAKE_XPT0(F, r, REAL)
MAKE_XPT0(I, i, INTEGER)
MAKE_XPT0(K, k, INTEGER8)
MAKE_XPT0(UB, ub, UBYTE)
MAKE_XPT0(UW, uw, UWORD)
MAKE_XPT0(W, w, WORD)

#undef MAKE_XPT0

F77_SUBROUTINE(ndf_xstat)( INTEGER(INDF),
                           CHARACTER(XNAME),
                           LOGICAL(THERE),
                           INTEGER(STATUS)
                           TRAIL(XNAME) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(XNAME)
   GENPTR_LOGICAL(THERE)
   GENPTR_INTEGER(STATUS)
   int there;

   char *xname = cnfCreim( XNAME, XNAME_length );
   ndfXstat_( *INDF, xname, &there, STATUS );
   cnfFree( xname );
   *THERE = there ? F77_TRUE : F77_FALSE;
}

F77_SUBROUTINE(ndf_zdelt)( INTEGER(INDF1),
                           CHARACTER(COMP),
                           REAL(MINRAT),
                           INTEGER(ZAXIS),
                           CHARACTER(TYPE),
                           INTEGER(PLACE),
                           INTEGER(INDF2),
                           REAL(ZRATIO),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(TYPE) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_CHARACTER(COMP)
   GENPTR_REAL(MINRAT)
   GENPTR_INTEGER(ZAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF2)
   GENPTR_REAL(ZRATIO)
   GENPTR_INTEGER(STATUS)

   char *comp = cnfCreim( COMP, COMP_length );
   char *type = cnfCreim( TYPE, TYPE_length );

   ndfZdelt_( *INDF1, comp, *MINRAT, *ZAXIS, type, PLACE, \
              INDF2, ZRATIO, STATUS );

   cnfFree( comp );
   cnfFree( type );
}

#define MAKE_GTSZ(CT,FT,FTYPE) \
F77_SUBROUTINE(ndf_gtsz##FT)( INTEGER(INDF), \
                              CHARACTER(COMP), \
                              FTYPE(SCALE), \
                              FTYPE(ZERO), \
                              INTEGER(STATUS) \
                              TRAIL(COMP) ) { \
   GENPTR_INTEGER(INDF) \
   GENPTR_CHARACTER(COMP) \
   GENPTR_##FTYPE(SCALE) \
   GENPTR_##FTYPE(ZERO) \
   GENPTR_INTEGER(STATUS) \
\
   char *comp = cnfCreim( COMP, COMP_length ); \
   ndfGtsz##CT##_( *INDF, comp, SCALE, ZERO, STATUS ); \
   cnfFree( comp ); \
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
F77_SUBROUTINE(ndf_ptsz##FT)( FTYPE(SCALE), \
                              FTYPE(ZERO), \
                              INTEGER(INDF), \
                              CHARACTER(COMP), \
                              INTEGER(STATUS) \
                              TRAIL(COMP) ) { \
   GENPTR_##FTYPE(SCALE) \
   GENPTR_##FTYPE(ZERO) \
   GENPTR_INTEGER(INDF) \
   GENPTR_CHARACTER(COMP) \
   GENPTR_INTEGER(STATUS) \
\
   char *comp = cnfCreim( COMP, COMP_length ); \
   ndfPtsz##CT##_( *SCALE, *ZERO, *INDF, comp, STATUS ); \
   cnfFree( comp ); \
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

F77_SUBROUTINE(ndf_boundk)( INTEGER(INDF),
                            INTEGER(NDIMX),
                            INTEGER8_ARRAY(LBND),
                            INTEGER8_ARRAY(UBND),
                            INTEGER(NDIM),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   ndfBound_( *INDF, NDF__MXDIM, lbnd, ubnd, NDIM, STATUS );

   n = ( *NDIMX < NDF__MXDIM ) ? *NDIMX : NDF__MXDIM;
   for( i = 0; i < n; i++ ) {
      LBND[ i ] = lbnd[ i ];
      UBND[ i ] = ubnd[ i ];
   }

   for( ; i < *NDIMX; i++ ) {
      LBND[ i ] = 1;
      UBND[ i ] = 1;
   }
}

F77_SUBROUTINE(ndf_dimk)( INTEGER(INDF),
                          INTEGER(NDIMX),
                          INTEGER8_ARRAY(DIM),
                          INTEGER(NDIM),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER8_ARRAY(DIM)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim dim[ NDF__MXDIM ];

   ndfDim_( *INDF, NDF__MXDIM, dim, NDIM, STATUS );

   n = ( *NDIMX < NDF__MXDIM ) ? *NDIMX : NDF__MXDIM;
   for( i = 0; i < n; i++ ) DIM[ i ] = dim[ i ];
   for( ; i < *NDIMX; i++ ) DIM[ i ] = 1;
}

F77_SUBROUTINE(ndf_mapk)( INTEGER(INDF),
                          CHARACTER(COMP),
                          CHARACTER(TYPE),
                          CHARACTER(MMOD),
                          INTEGER(PNTR),
                          INTEGER8(EL),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(TYPE)
                          TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *pntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipntr;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfMap_( *INDF, comp, type, mmod, pntr, &el, STATUS );

   cnfFree( comp );

   for( ipntr = 0; ipntr < 4; ipntr++ ) {
      if( pntr[ ipntr ] ) PNTR[ ipntr ] = cnfFptr( pntr[ ipntr ] );
   }

   *EL = el;
}

F77_SUBROUTINE(ndf_mapzk)( INTEGER(INDF),
                           CHARACTER(COMP),
                           CHARACTER(TYPE),
                           CHARACTER(MMOD),
                           INTEGER(RPNTR),
                           INTEGER(IPNTR),
                           INTEGER8(EL),
                           INTEGER(STATUS)
                           TRAIL(COMP)
                           TRAIL(TYPE)
                           TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(RPNTR)
   GENPTR_INTEGER(IPNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *rpntr[] = { NULL, NULL, NULL, NULL };
   void *ipntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipnt;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfMapz_( *INDF, comp, type, mmod, rpntr, ipntr, &el, STATUS );

   cnfFree( comp );

   for( ipnt = 0; ipnt < 4; ipnt++ ) {
      if( rpntr[ ipnt ] ) RPNTR[ ipnt ] = cnfFptr( rpntr[ ipnt ] );
      if( ipntr[ ipnt ] ) IPNTR[ ipnt ] = cnfFptr( ipntr[ ipnt ] );
   }

   *EL = el;
}

F77_SUBROUTINE(ndf_mapqlk)( INTEGER(INDF),
                            INTEGER(PNTR),
                            INTEGER8(EL),
                            LOGICAL(BAD),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER8(EL)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)
   int *pntr = NULL;
   size_t el;
   int bad;

   ndfMapql_( *INDF, &pntr, &el, &bad, STATUS );

   *BAD = bad ? F77_TRUE : F77_FALSE;
   *PNTR = cnfFptr( pntr );
   *EL = el;
}

F77_SUBROUTINE(ndf_newk)( CHARACTER(FTYPE),
                          INTEGER(NDIM),
                          INTEGER8_ARRAY(LBND),
                          INTEGER8_ARRAY(UBND),
                          INTEGER(PLACE),
                          INTEGER(INDF),
                          INTEGER(STATUS)
                          TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );
   int i;

   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   ndfNew_( ftype, *NDIM, lbnd, ubnd, PLACE, INDF, STATUS );

}

F77_SUBROUTINE(ndf_newpk)( CHARACTER(FTYPE),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(PLACE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );
   int i;

   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) ubnd[ i ] = UBND[ i ];

   ndfNewp_( ftype, *NDIM, ubnd, PLACE, INDF, STATUS );

}

F77_SUBROUTINE(ndf_sbndk)( INTEGER(NDIM),
                           INTEGER8_ARRAY(LBND),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(INDF),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];

   ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   ndfSbnd_( ndim, lbnd, ubnd, *INDF, STATUS );
}

F77_SUBROUTINE(ndf_sectk)( INTEGER(INDF1),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(LBND),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(INDF2),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];

   ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = LBND[ i ];
      ubnd[ i ] = UBND[ i ];
   }

   ndfSect_( *INDF1, ndim, lbnd, ubnd, INDF2, STATUS );

}

F77_SUBROUTINE(ndf_shiftk)( INTEGER(NSHIFT),
                            INTEGER8_ARRAY(SHIFT),
                            INTEGER(INDF),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(NSHIFT)
   GENPTR_INTEGER8_ARRAY(SHIFT)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   int i, nshift;
   hdsdim shift[NDF__MXDIM];
   nshift = ( *NSHIFT < NDF__MXDIM ) ? *NSHIFT : NDF__MXDIM;
   for( i = 0; i < nshift; i++ ) shift[ i ] = SHIFT[ i ];

   ndfShift_( nshift, shift, *INDF, STATUS );

}

F77_SUBROUTINE(ndf_sizek)( INTEGER(INDF),
                           INTEGER8(NPIX),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER8(NPIX)
   GENPTR_INTEGER(STATUS)
   size_t npix;

   ndfSize_( *INDF, &npix, STATUS );

   *NPIX = npix;
}




/* -------  Routines with 32 bit arguments -------------- */

F77_SUBROUTINE(ndf_bound)( INTEGER(INDF),
                           INTEGER(NDIMX),
                           INTEGER_ARRAY(LBND),
                           INTEGER_ARRAY(UBND),
                           INTEGER(NDIM),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   ndfBound_( *INDF, NDF__MXDIM, lbnd, ubnd, NDIM, STATUS );

   n = ( *NDIMX < NDF__MXDIM ) ? *NDIMX : NDF__MXDIM;
   for( i = 0; i < n; i++ ) {
      LBND[ i ] = (int) lbnd[ i ];
      UBND[ i ] = (int) ubnd[ i ];
      CHECK_DIM( LBND[ i ], lbnd[ i ], NDF_BOUND, INDF )
   }

   for( ; i < *NDIMX; i++ ) {
      LBND[ i ] = 1;
      UBND[ i ] = 1;
   }
}

F77_SUBROUTINE(ndf_dim)( INTEGER(INDF),
                         INTEGER(NDIMX),
                         INTEGER_ARRAY(DIM),
                         INTEGER(NDIM),
                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NDIMX)
   GENPTR_INTEGER_ARRAY(DIM)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER(STATUS)

   int i, n;
   hdsdim dim[ NDF__MXDIM ];

   ndfDim_( *INDF, NDF__MXDIM, dim, NDIM, STATUS );

   n = ( *NDIMX < NDF__MXDIM ) ? *NDIMX : NDF__MXDIM;
   for( i = 0; i < n; i++ ) {
      DIM[ i ] = (int) dim[ i ];
      CHECK_DIM( DIM[ i ], dim[ i ], NDF_DIM, INDF )
   }
   for( ; i < *NDIMX; i++ ) DIM[ i ] = 1;
}


F77_SUBROUTINE(ndf_map)( INTEGER(INDF),
                         CHARACTER(COMP),
                         CHARACTER(TYPE),
                         CHARACTER(MMOD),
                         INTEGER(PNTR),
                         INTEGER(EL),
                         INTEGER(STATUS)
                         TRAIL(COMP)
                         TRAIL(TYPE)
                         TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *pntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipntr;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfMap_( *INDF, comp, type, mmod, pntr, &el, STATUS );

   for( ipntr = 0; ipntr < 4; ipntr++ ) {
      if( pntr[ ipntr ] ) PNTR[ ipntr ] = cnfFptr( pntr[ ipntr ] );
   }

   cnfFree( comp );

   *EL = (int) el;
   CHECK_DIM( *EL, el, NDF_MAP, INDF )
}

F77_SUBROUTINE(ndf_mapz)( INTEGER(INDF),
                          CHARACTER(COMP),
                          CHARACTER(TYPE),
                          CHARACTER(MMOD),
                          INTEGER(RPNTR),
                          INTEGER(IPNTR),
                          INTEGER(EL),
                          INTEGER(STATUS)
                          TRAIL(COMP)
                          TRAIL(TYPE)
                          TRAIL(MMOD) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(MMOD)
   GENPTR_INTEGER(RPNTR)
   GENPTR_INTEGER(IPNTR)
   GENPTR_INTEGER(EL)
   GENPTR_INTEGER(STATUS)
   char type[ DAT__SZTYP + 1 ];
   char mmod[ NDF__SZMMD + 1 ];
   void *rpntr[] = { NULL, NULL, NULL, NULL };
   void *ipntr[] = { NULL, NULL, NULL, NULL };
   size_t el;
   int ipnt;

   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );
   cnfImpn( MMOD, MMOD_length, NDF__SZMMD, mmod );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfMapz_( *INDF, comp, type, mmod, rpntr, ipntr, &el, STATUS );

   cnfFree( comp );

   for( ipnt = 0; ipnt < 4; ipnt++ ) {
      if( rpntr[ ipnt ] ) RPNTR[ ipnt ] = cnfFptr( rpntr[ ipnt ] );
      if( ipntr[ ipnt ] ) IPNTR[ ipnt ] = cnfFptr( ipntr[ ipnt ] );
   }

   *EL = (int) el;
   CHECK_DIM( *EL, el, NDF_MAP, INDF )
}

F77_SUBROUTINE(ndf_mapql)( INTEGER(INDF),
                           INTEGER(PNTR),
                           INTEGER(EL),
                           LOGICAL(BAD),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(PNTR)
   GENPTR_INTEGER(EL)
   GENPTR_LOGICAL(BAD)
   GENPTR_INTEGER(STATUS)
   int *pntr = NULL;
   size_t el;
   int bad;

   ndfMapql_( *INDF, &pntr, &el, &bad, STATUS );

   *BAD = bad ? F77_TRUE : F77_FALSE;
   *PNTR = cnfFptr( pntr );
   *EL = (int) el;

   CHECK_DIM( *EL, el, NDF_MAP, INDF )
}

F77_SUBROUTINE(ndf_new)( CHARACTER(FTYPE),
                         INTEGER(NDIM),
                         INTEGER_ARRAY(LBND),
                         INTEGER_ARRAY(UBND),
                         INTEGER(PLACE),
                         INTEGER(INDF),
                         INTEGER(STATUS)
                         TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );
   int i;

   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfNew_( ftype, *NDIM, lbnd, ubnd, PLACE, INDF, STATUS );

}

F77_SUBROUTINE(ndf_newp)( CHARACTER(FTYPE),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(UBND),
                          INTEGER(PLACE),
                          INTEGER(INDF),
                          INTEGER(STATUS)
                          TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char ftype[ NDF__SZFTP + 1 ];
   cnfImpn( FTYPE, FTYPE_length, NDF__SZFTP, ftype );
   int i;

   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) ubnd[ i ] = (hdsdim) UBND[ i ];

   ndfNewp_( ftype, *NDIM, ubnd, PLACE, INDF, STATUS );

}


F77_SUBROUTINE(ndf_sbnd)( INTEGER(NDIM),
                          INTEGER_ARRAY(LBND),
                          INTEGER_ARRAY(UBND),
                          INTEGER(INDF),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];

   ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfSbnd_( ndim, lbnd, ubnd, *INDF, STATUS );
}

F77_SUBROUTINE(ndf_sect)( INTEGER(INDF1),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(LBND),
                          INTEGER_ARRAY(UBND),
                          INTEGER(INDF2),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   int i, ndim;
   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];

   ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfSect_( *INDF1, ndim, lbnd, ubnd, INDF2, STATUS );

}

F77_SUBROUTINE(ndf_shift)( INTEGER(NSHIFT),
                           INTEGER_ARRAY(SHIFT),
                           INTEGER(INDF),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(NSHIFT)
   GENPTR_INTEGER_ARRAY(SHIFT)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   int i, nshift;
   hdsdim shift[NDF__MXDIM];
   nshift = ( *NSHIFT < NDF__MXDIM ) ? *NSHIFT : NDF__MXDIM;
   for( i = 0; i < nshift; i++ ) shift[ i ] = (hdsdim) SHIFT[ i ];

   ndfShift_( nshift, shift, *INDF, STATUS );

}

F77_SUBROUTINE(ndf_size)( INTEGER(INDF),
                          INTEGER(NPIX),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(NPIX)
   GENPTR_INTEGER(STATUS)
   size_t npix;

   ndfSize_( *INDF, &npix, STATUS );

   *NPIX = npix;
   CHECK_DIM( *NPIX, npix, NDF_SIZE, INDF )
}



/* Function called by the NDF library to invoke an external event handler
   written in F77, and previously registered with the NDF library using
   NDF_HNDLR. */

typedef void (*F77_handler)( CHARACTER(EVNAME),
                             CHARACTER(SHORTTEXT),
                             INTEGER(STATUS)
                             TRAIL(EVNAME)
                             TRAIL(SHORTTEXT) );

void ndf1InvokeF77Handler( NdfEventHandler handler,
                           const char *evname,
                           const char *shorttext, int *status ) {

   DECLARE_CHARACTER_DYN(EVNAME);
   F77_CREATE_EXPORT_CHARACTER( evname, EVNAME );
   DECLARE_CHARACTER_DYN(SHORTTEXT);
   F77_CREATE_EXPORT_CHARACTER( shorttext, SHORTTEXT );
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( *status, STATUS );

   ( *(F77_handler) handler )( CHARACTER_ARG(EVNAME),
                               CHARACTER_ARG(SHORTTEXT),
                               INTEGER_ARG(&STATUS)
                               TRAIL_ARG(EVNAME)
                               TRAIL_ARG(SHORTTEXT) );

   F77_FREE_CHARACTER( EVNAME );
   F77_FREE_CHARACTER( SHORTTEXT );
   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(ndf_ssary)( INTEGER(IARY1),
                           INTEGER(INDF),
                           INTEGER(IARY2),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(IARY1)
   GENPTR_INTEGER(INDF)
   GENPTR_LOGICAL(IARY2)
   GENPTR_INTEGER(STATUS)
   Ary *ary2 = NULL;

   ndfSsary_( aryI2A(*IARY1), *INDF, &ary2, STATUS );

   *IARY2 = aryA2I(ary2);
}

F77_SUBROUTINE(ndf_nchnk)( INTEGER(INDF),
                           INTEGER(MXPIX),
                           INTEGER(NCHUNK),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(MXPIX)
   GENPTR_INTEGER(NCHUNK)
   GENPTR_INTEGER(STATUS)

   ndfNchnk_( *INDF, *MXPIX, NCHUNK, STATUS );

}

F77_SUBROUTINE(ndf_acmsg)( CHARACTER(TOKEN),
                           INTEGER(INDF),
                           CHARACTER(COMP),
                           INTEGER(IAXIS),
                           INTEGER(STATUS)
                           TRAIL(TOKEN)
                           TRAIL(COMP) ) {
   GENPTR_CHARACTER(TOKEN)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(IAXIS)
   GENPTR_INTEGER(STATUS)

   char *token = cnfCreim( TOKEN, TOKEN_length );
   char *comp = cnfCreim( COMP, COMP_length );
   ndfAcmsg_( token, *INDF, comp, *IAXIS, STATUS );
   cnfFree( comp );
   cnfFree( token );
}

F77_SUBROUTINE(ndf_qmf)( INTEGER(INDF),
                         INTEGER(QMF),
                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(QMF)
   GENPTR_INTEGER(STATUS)
   ndfQmf_( *INDF, QMF, STATUS );
}

F77_SUBROUTINE(ndf_hfind)( INTEGER(INDF),
                           INTEGER_ARRAY(YMDHM),
                           REAL(SEC),
                           INTEGER(EQ),
                           INTEGER(IREC),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER_ARRAY(YMDHM)
   GENPTR_REAL(SEC)
   GENPTR_INTEGER(EQ)
   GENPTR_INTEGER(IREC)
   GENPTR_INTEGER(STATUS)

   ndfHfind_( *INDF, YMDHM, *SEC, *EQ, IREC, STATUS );

}

F77_SUBROUTINE(ndf_hpurg)( INTEGER(INDF),
                           INTEGER(IREC1),
                           INTEGER(IREC2),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(IREC1)
   GENPTR_INTEGER(IREC2)
   GENPTR_INTEGER(STATUS)
   ndfHpurg_( *INDF, *IREC1, *IREC2, STATUS );
}

