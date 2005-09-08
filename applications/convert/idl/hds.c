#include <string.h>
#include "dat_par.h"
#include "f77.h"
#include "hds.h"
F77_SUBROUTINE(hds_new)( CHARACTER(file),
                         CHARACTER(name),
                         CHARACTER(type),
                         INTEGER(ndim),
                         INTEGER_ARRAY(dim),
                         CHARACTER(loc),
                         INTEGER(status)
                         TRAIL(file)
                         TRAIL(name)
                         TRAIL(type)
                         TRAIL(loc) );

void idlHdsNew( const char *file,
             const char *name,
             const char *type,
             int ndim,
             const int *dim,
             char *loc,
             int *status ) {

DECLARE_CHARACTER_DYN(ffile);
DECLARE_CHARACTER(fname,DAT__SZNAM);
DECLARE_CHARACTER(ftype,DAT__SZTYP);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);
int i,nvalues;

   for (i=ndim,nvalues=1;i;i--) nvalues*=dim[i-1];

   F77_CREATE_CHARACTER(ffile,strlen( file ));
   F77_EXPORT_CHARACTER(file,ffile,ffile_length);
   F77_EXPORT_CHARACTER(name,fname,fname_length);
   F77_EXPORT_CHARACTER(type,ftype,ftype_length);
   F77_EXPORT_INTEGER(ndim,fndim);
   F77_CREATE_INTEGER_ARRAY(fdim,ndim);
   F77_EXPORT_INTEGER_ARRAY(dim,fdim,ndim);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(hds_new)( CHARACTER_ARG(ffile),
                      CHARACTER_ARG(fname),
                      CHARACTER_ARG(ftype),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARRAY_ARG(fdim),
                      CHARACTER_ARG(floc),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(ffile)
                      TRAIL_ARG(fname)
                      TRAIL_ARG(ftype)
                      TRAIL_ARG(floc) );

   F77_FREE_CHARACTER(ffile);
   F77_FREE_INTEGER(fdim);
   F77_IMPORT_LOCATOR(floc,loc);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(hds_open)( CHARACTER(file),
                          CHARACTER(mode),
                          CHARACTER(loc),
                          INTEGER(status)
                          TRAIL(file)
                          TRAIL(mode)
                          TRAIL(loc) );

void idlHdsOpen( const char *file,
              const char *mode,
              char *loc,
              int *status ) {

DECLARE_CHARACTER_DYN(ffile);
DECLARE_CHARACTER(fmode,DAT__SZMOD);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(ffile,strlen( file ));
   F77_EXPORT_CHARACTER(file,ffile,ffile_length);
   F77_EXPORT_CHARACTER(mode,fmode,fmode_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(hds_open)( CHARACTER_ARG(ffile),
                       CHARACTER_ARG(fmode),
                       CHARACTER_ARG(floc),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(ffile)
                       TRAIL_ARG(fmode)
                       TRAIL_ARG(floc) );

   F77_FREE_CHARACTER(ffile);
   F77_IMPORT_LOCATOR(floc,loc);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
F77_SUBROUTINE(hds_show)( CHARACTER(topic),
                          INTEGER(status)
                          TRAIL(topic) );

void idlHdsShow( const char *topic,
              int *status ) {

DECLARE_CHARACTER_DYN(ftopic);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER(ftopic,strlen( topic ));
   F77_EXPORT_CHARACTER(topic,ftopic,ftopic_length);
   F77_EXPORT_INTEGER(*status,fstatus);

   F77_CALL(hds_show)( CHARACTER_ARG(ftopic),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(ftopic) );

   F77_FREE_CHARACTER(ftopic);
   F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
