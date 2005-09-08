#include <string.h>
#include "dat_par.h"
#include "f77.h"
#include "hds.h"
F77_SUBROUTINE(dat_annul)( CHARACTER(loc),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatAnnul( char *loc,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_annul)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_LOCATOR(floc,loc);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_cell)( CHARACTER(loc1),
                          INTEGER(ndim),
                          INTEGER_ARRAY(sub),
                          CHARACTER(loc2),
                          INTEGER(status)
                          TRAIL(loc1)
                          TRAIL(loc2) );

void idlDatCell( const char *loc1,
              int ndim,
              const int *sub,
              char *loc2,
              int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fsub);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_INTEGER(ndim,fndim);
/*######## May need to edit the next line ######*/
   F77_CREATE_INTEGER_ARRAY(fsub,NELSsub);
/*######## May need to edit the next line ######*/
   F77_EXPORT_INTEGER_ARRAY(sub,fsub,NELSsub);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_cell)( CHARACTER_ARG(floc1),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fsub),
                       CHARACTER_ARG(floc2),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc1)
                       TRAIL_ARG(floc2) );

   F77_FREE_INTEGER(fsub);
   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_clen)( CHARACTER(loc),
                          INTEGER(clen),
                          INTEGER(status)
                          TRAIL(loc) );

void idlDatClen( const char *loc,
              int *clen,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fclen);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_clen)( CHARACTER_ARG(floc),
                       INTEGER_ARG(&fclen),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER(fclen,*clen);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_clone)( CHARACTER(loc1),
                           CHARACTER(loc2),
                           INTEGER(status)
                           TRAIL(loc1)
                           TRAIL(loc2) );

void idlDatClone( const char *loc1,
               char *loc2,
               int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_clone)( CHARACTER_ARG(floc1),
                        CHARACTER_ARG(floc2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc1)
                        TRAIL_ARG(floc2) );

   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_find)( CHARACTER(loc1),
                          CHARACTER(name),
                          CHARACTER(loc2),
                          INTEGER(status)
                          TRAIL(loc1)
                          TRAIL(name)
                          TRAIL(loc2) );

void idlDatFind( const char *loc1,
              const char *name,
              char *loc2,
              int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_CHARACTER(name,fname,fname_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_find)( CHARACTER_ARG(floc1),
                       CHARACTER_ARG(fname),
                       CHARACTER_ARG(floc2),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc1)
                       TRAIL_ARG(fname)
                       TRAIL_ARG(floc2) );

   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_index)( CHARACTER(loc1),
                           INTEGER(index),
                           CHARACTER(loc2),
                           INTEGER(status)
                           TRAIL(loc1)
                           TRAIL(loc2) );

void idlDatIndex( const char *loc1,
               int index,
               char *loc2,
               int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_INTEGER(findex);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_INTEGER(index,findex);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_index)( CHARACTER_ARG(floc1),
                        INTEGER_ARG(&findex),
                        CHARACTER_ARG(floc2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc1)
                        TRAIL_ARG(floc2) );

   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_mapv)( CHARACTER(loc),
                          CHARACTER(type),
                          CHARACTER(mode),
                          POINTER(ptr),
                          INTEGER(actval),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(type)
                          TRAIL(mode) );

void idlDatMapv( const char *loc,
              const char *type,
              const char *mode,
              void * *ptr,
              int *actval,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_CHARACTER(fmode,DAT__SZMOD);
DECLARE_POINTER(fptr);
DECLARE_INTEGER(factval);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_CHARACTER(type,ftype,ftype_length);
   F77_EXPORT_CHARACTER(mode,fmode,fmode_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_mapv)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(ftype),
                       CHARACTER_ARG(fmode),
                       POINTER_ARG(&fptr),
                       INTEGER_ARG(&factval),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(ftype)
                       TRAIL_ARG(fmode) );

   F77_IMPORT_POINTER(fptr,*ptr);
   F77_IMPORT_INTEGER(factval,*actval);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_name)( CHARACTER(loc),
                          CHARACTER(name),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(name) );

void idlDatName( const char *loc,
              char *name,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_name)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(fname),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fname) );

   F77_IMPORT_CHARACTER(fname,fname_length,name);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_ncomp)( CHARACTER(loc),
                           INTEGER(ncomp),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatNcomp( const char *loc,
               int *ncomp,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fncomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_ncomp)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fncomp),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER(fncomp,*ncomp);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_new)( CHARACTER(loc),
                         CHARACTER(name),
                         CHARACTER(type),
                         INTEGER(ndim),
                         INTEGER_ARRAY(dim),
                         INTEGER(status)
                         TRAIL(loc)
                         TRAIL(name)
                         TRAIL(type) );

void idlDatNew( const char *loc,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   for (i=ndim,nvalues=1;i;i--) nvalues*=dim[i-1];

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_CHARACTER(name,fname,fname_length);
   F77_EXPORT_CHARACTER(type,ftype,ftype_length);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fdim,ndim);
   F77_EXPORT_INTEGER_ARRAY(dim,fdim,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_new)( CHARACTER_ARG(floc),
                      CHARACTER_ARG(fname),
                      CHARACTER_ARG(ftype),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARRAY_ARG(fdim),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(floc)
                      TRAIL_ARG(fname)
                      TRAIL_ARG(ftype) );

   F77_FREE_INTEGER(fdim);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_prim)( CHARACTER(loc),
                          LOGICAL(reply),
                          INTEGER(status)
                          TRAIL(loc) );

void idlDatPrim( const char *loc,
              int *reply,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_LOGICAL(freply);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_prim)( CHARACTER_ARG(floc),
                       LOGICAL_ARG(&freply),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) );

   F77_IMPORT_LOGICAL(freply,*reply);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_prmry)( LOGICAL(set),
                           CHARACTER(loc),
                           LOGICAL(prmry),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatPrmry( int set,
               char *loc,
               int *prmry,
               int *status ) {

DECLARE_LOGICAL(fset);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_LOGICAL(fprmry);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL(set,fset);
   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_LOGICAL(*prmry,fprmry);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_prmry)( LOGICAL_ARG(&fset),
                        CHARACTER_ARG(floc),
                        LOGICAL_ARG(&fprmry),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_LOCATOR(floc,loc);
   F77_IMPORT_LOGICAL(fprmry,*prmry);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_put)( CHARACTER(loc),
                         CHARACTER(type),
                         INTEGER(ndim),
                         INTEGER_ARRAY(dim),
                         void *value,
                         INTEGER(status)
                         TRAIL(loc)
                         TRAIL(type) );

void idlDatPut( const char *loc,
             const char *type,
             int ndim,
             const int *dim,
             void *value,
             int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);

DECLARE_CHARACTER_ARRAY_DYN(cvalue);
DECLARE_DOUBLE_ARRAY_DYN(dvalue);
DECLARE_INTEGER_ARRAY_DYN(ivalue);
DECLARE_LOGICAL_ARRAY_DYN(lvalue);
DECLARE_REAL_ARRAY_DYN(rvalue);
DECLARE_WORD_ARRAY_DYN(wvalue);
DECLARE_UWORD_ARRAY_DYN(uwvalue);
DECLARE_BYTE_ARRAY_DYN(bvalue);
DECLARE_UBYTE_ARRAY_DYN(ubvalue);
void *fvalue;

DECLARE_INTEGER(fstatus);
int i,nvalues;
int value_length;

   for (i=ndim,nvalues=1;i;i--) nvalues*=dim[i-1];

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_CHARACTER(type,ftype,ftype_length);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fdim,ndim);
   F77_EXPORT_INTEGER_ARRAY(dim,fdim,ndim);

   if ( !strcmp(type,"_REAL") ) {
      F77_CREATE_REAL_ARRAY(rvalue,nvalues);
      F77_EXPORT_REAL_ARRAY(value,rvalue,nvalues);
      fvalue = (void *)rvalue;
   } else if ( !strcmp(type,"_INTEGER") ) {
      F77_CREATE_INTEGER_ARRAY(ivalue,nvalues);
      F77_EXPORT_INTEGER_ARRAY(value,ivalue,nvalues);
      fvalue = (void *)ivalue;
   } else if ( !strcmp(type,"_WORD") ) {
      F77_CREATE_WORD_ARRAY(wvalue,nvalues);
      F77_EXPORT_WORD_ARRAY(value,wvalue,nvalues);
      fvalue = (void *)wvalue;
   } else if ( !strcmp(type,"_UWORD") ) {
      F77_CREATE_UWORD_ARRAY(uwvalue,nvalues);
      F77_EXPORT_UWORD_ARRAY(value,uwvalue,nvalues);
      fvalue = (void *)uwvalue;
   } else if ( !strcmp(type,"_BYTE") ) {
      F77_CREATE_BYTE_ARRAY(bvalue,nvalues);
      F77_EXPORT_BYTE_ARRAY(value,bvalue,nvalues);
      fvalue = (void *)bvalue;
   } else if ( !strcmp(type,"_UBYTE") ) {
      F77_CREATE_UBYTE_ARRAY(ubvalue,nvalues);
      F77_EXPORT_UBYTE_ARRAY(value,ubvalue,nvalues);
      fvalue = (void *)ubvalue;
   } else if ( !strcmp(type,"_DOUBLE") ) {
      F77_CREATE_DOUBLE_ARRAY(dvalue,nvalues);
      F77_EXPORT_DOUBLE_ARRAY(value,dvalue,nvalues);
      fvalue = (void *)dvalue;
   } else if ( !strcmp(type,"_LOGICAL") ) {
      F77_CREATE_LOGICAL_ARRAY(lvalue,nvalues);
      F77_EXPORT_LOGICAL_ARRAY(value,lvalue,nvalues);
      fvalue = (void *)lvalue;
   } else if ( !strncmp(type,"_CHAR",5) ) {
      if ( strlen(type) > 5 ) {
         value_length = atoi( type+6 );
      } else {
         idlDatClen( loc, &value_length, status );
      }
      F77_CREATE_CHARACTER_ARRAY(cvalue,value_length,nvalues);
      F77_EXPORT_CHARACTER_ARRAY_P(value,cvalue,value_length,nvalues);
      fvalue = (void *)cvalue;
   }

   F77_EXPORT_INTEGER(*status,fstatus);
   F77_CALL(dat_put)( CHARACTER_ARG(floc),
                      CHARACTER_ARG(ftype),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARRAY_ARG(fdim),
                      fvalue,
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(floc)
                      TRAIL_ARG(ftype) );

   F77_FREE_INTEGER(fdim);
   if ( !strcmp(type,"_REAL") ) {
      F77_FREE_REAL(rvalue);
   } else if ( !strcmp(type,"_INTEGER") ) {
      F77_FREE_INTEGER(ivalue);
   } else if ( !strcmp(type,"_WORD") ) {
      F77_FREE_WORD(wvalue);
   } else if ( !strcmp(type,"_UWORD") ) {
      F77_FREE_UWORD(uwvalue);
   } else if ( !strcmp(type,"_BYTE") ) {
      F77_FREE_BYTE(bvalue);
   } else if ( !strcmp(type,"_UBYTE") ) {
      F77_FREE_UBYTE(ubvalue);
   } else if ( !strcmp(type,"_LOGICAL") ) {
      F77_FREE_LOGICAL(lvalue);
   } else if ( !strncmp(type,"_CHAR",5) ) {
      F77_FREE_CHARACTER(cvalue);
   } else if ( !strcmp(type,"_DOUBLE") ) {
      F77_FREE_DOUBLE(dvalue);
   }
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_putc)( CHARACTER(loc),
                          INTEGER(ndim),
                          INTEGER_ARRAY(dim),
                          CHARACTER_ARRAY(value),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(value) );

void idlDatPutc( const char *loc,
              int ndim,
              const int *dim,
              char *const *value,
              int value_length,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_CHARACTER_ARRAY_DYN(fvalue);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   for (i=ndim,nvalues=1;i;i--) nvalues*=dim[i-1];

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fdim,ndim);
   F77_EXPORT_INTEGER_ARRAY(dim,fdim,ndim);
   F77_CREATE_CHARACTER_ARRAY(fvalue,value_length-1,nvalues);
   F77_EXPORT_CHARACTER_ARRAY_P(value,fvalue,fvalue_length,nvalues);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_putc)( CHARACTER_ARG(floc),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fdim),
                       CHARACTER_ARRAY_ARG(fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fvalue) );

   F77_FREE_INTEGER(fdim);
   F77_FREE_CHARACTER(fvalue);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_shape)( CHARACTER(loc),
                           INTEGER(ndimx),
                           INTEGER_ARRAY(dim),
                           INTEGER(ndim),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatShape( const char *loc,
               int ndimx,
               int *dim,
               int *ndim,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fndimx);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(ndimx,fndimx);
   F77_CREATE_INTEGER_ARRAY(fdim,ndimx);
   F77_ASSOC_INTEGER_ARRAY(fdim,dim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_shape)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fndimx),
                        INTEGER_ARRAY_ARG(fdim),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER(fndim,*ndim);
   F77_IMPORT_INTEGER_ARRAY(fdim,dim,*ndim);
   F77_FREE_INTEGER(fdim);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_size)( CHARACTER(loc),
                          INTEGER(size),
                          INTEGER(status)
                          TRAIL(loc) );

void idlDatSize( const char *loc,
              int *size,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fsize);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_size)( CHARACTER_ARG(floc),
                       INTEGER_ARG(&fsize),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER(fsize,*size);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_slice)( CHARACTER(loc1),
                           INTEGER(ndim),
                           INTEGER_ARRAY(diml),
                           INTEGER_ARRAY(dimu),
                           CHARACTER(loc2),
                           INTEGER(status)
                           TRAIL(loc1)
                           TRAIL(loc2) );

void idlDatSlice( const char *loc1,
               int ndim,
               const int *diml,
               const int *dimu,
               char *loc2,
               int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdiml);
DECLARE_INTEGER_ARRAY_DYN(fdimu);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_INTEGER(ndim,fndim);
/*######## May need to edit the next line ######*/
   F77_CREATE_INTEGER_ARRAY(fdiml,NELSdiml);
/*######## May need to edit the next line ######*/
   F77_EXPORT_INTEGER_ARRAY(diml,fdiml,NELSdiml);
/*######## May need to edit the next line ######*/
   F77_CREATE_INTEGER_ARRAY(fdimu,NELSdimu);
/*######## May need to edit the next line ######*/
   F77_EXPORT_INTEGER_ARRAY(dimu,fdimu,NELSdimu);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_slice)( CHARACTER_ARG(floc1),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fdiml),
                        INTEGER_ARRAY_ARG(fdimu),
                        CHARACTER_ARG(floc2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc1)
                        TRAIL_ARG(floc2) );

   F77_FREE_INTEGER(fdiml);
   F77_FREE_INTEGER(fdimu);
   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_state)( CHARACTER(loc),
                           LOGICAL(reply),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatState( const char *loc,
               int *reply,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_LOGICAL(freply);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_state)( CHARACTER_ARG(floc),
                        LOGICAL_ARG(&freply),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_LOGICAL(freply,*reply);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_struc)( CHARACTER(loc),
                           LOGICAL(reply),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatStruc( const char *loc,
               int *reply,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_LOGICAL(freply);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_struc)( CHARACTER_ARG(floc),
                        LOGICAL_ARG(&freply),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_LOGICAL(freply,*reply);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_type)( CHARACTER(loc),
                          CHARACTER(type),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(type) );

void idlDatType( const char *loc,
              char *type,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_type)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(ftype),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(ftype) );

   F77_IMPORT_CHARACTER(ftype,ftype_length,type);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_unmap)( CHARACTER(loc),
                           INTEGER(status)
                           TRAIL(loc) );

void idlDatUnmap( const char *loc,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc,floc);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_unmap)( CHARACTER_ARG(floc),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc) );

   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(dat_vec)( CHARACTER(loc1),
                         CHARACTER(loc2),
                         INTEGER(status)
                         TRAIL(loc1)
                         TRAIL(loc2) );

void idlDatVec( const char *loc1,
             char *loc2,
             int *status ) {

DECLARE_CHARACTER(floc1,DAT__SZLOC);
DECLARE_CHARACTER(floc2,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR(loc1,floc1);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(dat_vec)( CHARACTER_ARG(floc1),
                      CHARACTER_ARG(floc2),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(floc1)
                      TRAIL_ARG(floc2) );

   F77_IMPORT_LOCATOR(floc2,loc2);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
