#include "f77.h"
#include "ndf.h"

F77_SUBROUTINE(ndf_assoc)( CHARACTER(PARAM),
                           CHARACTER(MODE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(MODE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(MODE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   char *param = cnfCreim( PARAM, PARAM_length );
   char *mode = cnfCreim( MODE, MODE_length );

   ndfAssoc_( param, mode, INDF, STATUS );

   cnfFree( param );
   cnfFree( mode );
}

F77_SUBROUTINE(ndf_cancl)( CHARACTER(PARAM),
                           INTEGER(STATUS)
                           TRAIL(PARAM) ){
   GENPTR_CHARACTER(PARAM)
   GENPTR_INTEGER(STATUS)
   char *param = cnfCreim( PARAM, PARAM_length );
   ndfCancl_( param, STATUS );
   cnfFree( param );
}

F77_SUBROUTINE(ndf_cinp)( CHARACTER(PARAM),
                          INTEGER(INDF),
                          CHARACTER(COMP),
                          INTEGER(STATUS)
                          TRAIL(PARAM)
                          TRAIL(COMP) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_INTEGER(INDF)
   GENPTR_CHARACTER(COMP)
   GENPTR_INTEGER(STATUS)

   char *param = cnfCreim( PARAM, PARAM_length );
   char *comp = cnfCreim( COMP, COMP_length );

   ndfCinp_( param, *INDF, comp, STATUS );

   cnfFree( comp );
   cnfFree( param );
}

F77_SUBROUTINE(ndf_creat)( CHARACTER(PARAM),
                           CHARACTER(FTYPE),
                           INTEGER(NDIM),
                           INTEGER_ARRAY(LBND),
                           INTEGER_ARRAY(UBND),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *param = cnfCreim( PARAM, PARAM_length );
   char *ftype = cnfCreim( FTYPE, FTYPE_length );

   int i;

   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfCreat_( param, ftype, *NDIM, lbnd, ubnd, INDF, STATUS );

   cnfFree( param );
   cnfFree( ftype );
}

F77_SUBROUTINE(ndf_creatk)( CHARACTER(PARAM),
                            CHARACTER(FTYPE),
                            INTEGER(NDIM),
                            INTEGER8_ARRAY(LBND),
                            INTEGER8_ARRAY(UBND),
                            INTEGER(INDF),
                            INTEGER(STATUS)
                            TRAIL(PARAM)
                            TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(LBND)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *param = cnfCreim( PARAM, PARAM_length );
   char *ftype = cnfCreim( FTYPE, FTYPE_length );

   int i;

   hdsdim lbnd[NDF__MXDIM];
   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      lbnd[ i ] = (hdsdim) LBND[ i ];
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfCreat_( param, ftype, *NDIM, lbnd, ubnd, INDF, STATUS );

   cnfFree( param );
   cnfFree( ftype );
}

F77_SUBROUTINE(ndf_crep)( CHARACTER(PARAM),
                          CHARACTER(FTYPE),
                          INTEGER(NDIM),
                          INTEGER_ARRAY(UBND),
                          INTEGER(INDF),
                          INTEGER(STATUS)
                          TRAIL(PARAM)
                          TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *param = cnfCreim( PARAM, PARAM_length );
   char *ftype = cnfCreim( FTYPE, FTYPE_length );

   int i;

   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfCrep_( param, ftype, *NDIM, ubnd, INDF, STATUS );

   cnfFree( param );
   cnfFree( ftype );
}


F77_SUBROUTINE(ndf_crepk)( CHARACTER(PARAM),
                           CHARACTER(FTYPE),
                           INTEGER(NDIM),
                           INTEGER8_ARRAY(UBND),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(FTYPE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(FTYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER8_ARRAY(UBND)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)

   char *param = cnfCreim( PARAM, PARAM_length );
   char *ftype = cnfCreim( FTYPE, FTYPE_length );

   int i;

   hdsdim ubnd[NDF__MXDIM];
   int ndim = ( *NDIM < NDF__MXDIM ) ? *NDIM : NDF__MXDIM;
   for( i = 0; i < ndim; i++ ) {
      ubnd[ i ] = (hdsdim) UBND[ i ];
   }

   ndfCrep_( param, ftype, *NDIM, ubnd, INDF, STATUS );

   cnfFree( param );
   cnfFree( ftype );
}


F77_SUBROUTINE(ndf_crepl)( CHARACTER(PARAM),
                           INTEGER(PLACE),
                           INTEGER(STATUS)
                           TRAIL(PARAM) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_INTEGER(PLACE)
   GENPTR_INTEGER(STATUS)
   char *param = cnfCreim( PARAM, PARAM_length );

   ndfCrepl_( param, PLACE, STATUS );

   cnfFree( param );
}

F77_SUBROUTINE(ndf_exist)( CHARACTER(PARAM),
                           CHARACTER(MODE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(MODE) ) {
   GENPTR_CHARACTER(PARAM)
   GENPTR_CHARACTER(MODE)
   GENPTR_INTEGER(INDF)
   GENPTR_INTEGER(STATUS)
   char *param = cnfCreim( PARAM, PARAM_length );
   char *mode = cnfCreim( MODE, MODE_length );

   ndfExist_( param, mode, INDF, STATUS );

   cnfFree( param );
   cnfFree( mode );
}

F77_SUBROUTINE(ndf_prop)( INTEGER(INDF1),
                          CHARACTER(CLIST),
                          CHARACTER(PARAM),
                          INTEGER(INDF2),
                          INTEGER(STATUS)
                          TRAIL(CLIST)
                          TRAIL(PARAM) ) {
   GENPTR_INTEGER(INDF1)
   GENPTR_CHARACTER(CLIST)
   GENPTR_CHARACTER(PARAM)
   GENPTR_INTEGER(INDF2)
   GENPTR_INTEGER(STATUS)

   char *clist = cnfCreim( CLIST, CLIST_length );
   char *param = cnfCreim( PARAM, PARAM_length );

   ndfProp_( *INDF1, clist, param, INDF2, STATUS );

   cnfFree( param );
   cnfFree( clist );
}

