/*
 *   Name:
 *      rtdNDF
 *
 *   Purpose:
 *      Read and write an NDF.
 *
 *   Language:
 *      C
 *
 *   Notes:
 *      These routines just exist as using CNF to call Fortran from
 *      C++ seems very difficult (CNF preprocessor problems).
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *   Authors:
 *      PDRAPER: Peter W. Draper, Starlink - University of Durham
 *
 *   History:
 *      18-JAN-1995 (PDRAPER):
 *         Original version.
 *      24-JUL-1998 (PDRAPER):
 *         Changed CNF macro F77_CREATE_CHARACTER_ARRAY to new calling 
 *         sequence.
 *      {enter_changes_here}
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems_par.h"
#include "ems.h"

extern void F77_EXTERNAL_NAME(rtd_rdndf)( CHARACTER(ndfname),
                                          INTEGER(type),
                                          INTEGER(ndfid),
                                          INTEGER(width),
                                          INTEGER(height),
                                          POINTER(charPtr),
                                          INTEGER(header_length),
                                          INTEGER(status)
                                          TRAIL(ndfname));

extern void F77_EXTERNAL_NAME(rtd_wrndf)( CHARACTER(ndfname),
                                          INTEGER(type),
                                          INTEGER(ndfid),
                                          POINTER(data),
                                          INTEGER(width),
                                          INTEGER(height),
                                          CHARACTER(comp),
                                          CHARACTER_ARRAY(fhead),
                                          INTEGER(nhead),
                                          INTEGER(status )
                                          TRAIL(ndfname)
                                          TRAIL(comp)
                                          TRAIL(fhead));

extern void F77_EXTERNAL_NAME(ndf_annul)( INTEGER(ndfid),
                                          INTEGER(status ));

extern void F77_EXTERNAL_NAME(rtd_cpdat)( INTEGER(ndfid),
                                          POINTER(data),
                                          CHARACTER(comp),
                                          INTEGER(status )
                                          TRAIL(comp) );

extern F77_SUBROUTINE(rtd1_aqual)( INTEGER(ndfId), LOGICAL(grab),
                                   LOGICAL(haveQual), POINTER(q));

int rtdAccessNDF( const char *filename, int *type, int *width, int *height,
                  char **header, int *header_length, int *ndfid,
                  char **error_mess )
{
  DECLARE_CHARACTER(ndfname,132);   /* Local copy of filename (F77) */
  DECLARE_CHARACTER(tmpname,132);   /* Local copy of filename (F77) */
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_POINTER(charPtr);         /* Pointer to F77 character array */
  char *opStr;
  char *opPtr;
  int used, i, j, errcount;
  char param[EMS__SZPAR];

  /* Convert the file name into an F77 string */
  strcpy( tmpname, filename );
  cnf_exprt( tmpname, ndfname, ndfname_length );

  /* Attempt to open the NDF. */
  ems_mark_c();
  F77_CALL( rtd_rdndf )( CHARACTER_ARG(ndfname),
                         INTEGER_ARG(type),
                         INTEGER_ARG(ndfid),
                         INTEGER_ARG(width),
                         INTEGER_ARG(height),
                         POINTER_ARG(&charPtr),
                         INTEGER_ARG(header_length),
                         INTEGER_ARG(&status)
                         TRAIL_ARG(ndfname)
                         );
  if ( status != SAI__OK ) {

    /* Construct an error message that can be passed back. */
    opStr = (char *) NULL;
    used = 0;
    errcount = 1;
    while ( status != SAI__OK ) {
      opStr = (char *) realloc( (void *)opStr,
                                (size_t) EMS__SZMSG * sizeof(char) * errcount++ );
      opPtr = opStr + used;
      ems_stat_c( &status );
      ems_eload_c( param, &i, opPtr, &j, &status);
      used += j;
      opStr[used++] ='\n';
    }
    opStr[used] = '\0';
    *error_mess = opStr;
    ems_annul_c( &status );
    ems_rlse_c();
    return 0;
  }

  /* Convert the FITS headers into a C string */
  *header = cnf_creib( (char *)charPtr, 80 * (*header_length) );
  free( (void *)charPtr );
  ems_rlse_c();

  return 1;
}



int rtdWriteNDF( const char *filename, int type, int width, int height,
                 void *data , int ndfid, const char *component, 
                 const char *header, int lheader, char **error_mess )
{
  DECLARE_CHARACTER(ndfname,132);   /* Local copy of filename (F77) */
  DECLARE_CHARACTER(tmpname,132);   /* Local copy of filename (F77) */
  DECLARE_CHARACTER(comp, 20);      /* NDF component to use (F77) */
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_POINTER(dataPtr);         /* Pointer to F77 memory */
  DECLARE_CHARACTER_ARRAY_DYN(fhead);
  char *opStr;
  char *opPtr;
  int used, i, j, errcount;
  int dims[1];
  char param[EMS__SZPAR];

  /* Convert the file name into an F77 string */
  strcpy( tmpname, filename );
  cnf_exprt( tmpname, ndfname, ndfname_length );

  /* Convert the NDF component into a F77 string */
  cnf_exprt( (char *) component, comp, comp_length);

  /* Convert C pointer to Fortran pointer */
  dataPtr = (F77_POINTER_TYPE) data;

  /*  If we have a FITS header then write it out with the NDF. This
      also contains any new WCS information that might be
      available. */
  if ( lheader > 0) {

    /*  Convert the C string into a Fortran array. */
    dims[0] = lheader / 80;
    F77_CREATE_CHARACTER_ARRAY( fhead, 80, dims[0] );
    cnf_exprta( (char *) header, 80, fhead, 80, 1, dims );
  }

  /* Attempt to open the NDF. */
  ems_mark_c();
  F77_CALL( rtd_wrndf )( CHARACTER_ARG(ndfname),
                         INTEGER_ARG(&type),
                         INTEGER_ARG(&ndfid),
                         POINTER_ARG(&dataPtr),
                         INTEGER_ARG(&width),
                         INTEGER_ARG(&height),
                         CHARACTER_ARG(comp),
                         CHARACTER_ARRAY_ARG(fhead),
                         INTEGER_ARG(dims),
                         INTEGER_ARG(&status)
                         TRAIL_ARG(ndfname)
                         TRAIL_ARG(comp)
                         TRAIL_ARG(fhead)
                         );

  /* Free the header copy. */
  F77_FREE_CHARACTER( fhead );

  if ( status != SAI__OK ) {

    /* Construct an error message that can be passed back. */
    opStr = (char *) NULL;
    used = 0;
    errcount = 1;
    while ( status != SAI__OK ) {
      opStr = (char *)realloc( (void *)opStr,
                               (size_t) EMS__SZMSG * sizeof(char) * errcount++ );
      opPtr = opStr + used;
      ems_stat_c( &status );
      ems_eload_c( param, &i, opPtr, &j, &status);
      used += j;
      opStr[used++] ='\n';
    }
    opStr[used] = '\0';
    *error_mess = opStr;
    ems_annul_c( &status );
    ems_rlse_c();
    return 0;
  }
  ems_rlse_c();
  return 1;
}

int rtdFreeNDF( int ndfid )
{
  DECLARE_INTEGER(status);
  DECLARE_LOGICAL(haveQual);
  DECLARE_POINTER(qualPtr);
  DECLARE_LOGICAL(grab);
  grab = F77_FALSE;
  status = SAI__OK;
  ems_mark_c();

  /* Free any quality associated with this NDF (should be safe under
   * any circumstances). */
  if ( ndfid != 0 ) {
    F77_CALL( rtd1_aqual)( INTEGER_ARG(&ndfid), LOGICAL_ARG(&grab),
                           LOGICAL_ARG(&haveQual),
                           POINTER_ARG(&qualPtr));
  }

  /* Free the NDF */
  F77_CALL( ndf_annul ) ( INTEGER_ARG(&ndfid), INTEGER_ARG(&status) );
  if ( status != SAI__OK ) {
    ems_annul_c( &status );
  }
  ems_rlse_c();
  return 1;
}

int rtdCopyNDF( int ndfid, void **data, const char* component, 
                char **error_mess )
{
  DECLARE_POINTER(dataPtr);         /* Pointer to F77 memory */
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_CHARACTER(comp, 20);      /* F77 component name */
  char *opStr;
  char *opPtr;
  int used, i, j, errcount;
  char param[EMS__SZPAR];

  /* Convert the NDF component into a F77 string */
  cnf_exprt( (char *) component, comp, comp_length);

  /* Convert C pointer to Fortran pointer */
  dataPtr = (F77_POINTER_TYPE) *data;

  /* Copy the NDF using chunking to avoid large memory footprint. */
  ems_mark_c();
  F77_CALL( rtd_cpdat ) ( INTEGER_ARG(&ndfid), POINTER_ARG(&dataPtr),
                          CHARACTER_ARG(comp), INTEGER_ARG(&status) 
                          TRAIL_ARG(comp));
  if ( status != SAI__OK ) {

    /* Construct an error message that can be passed back. */
    opStr = (char *) NULL;
    used = 0;
    errcount = 1;
    while ( status != SAI__OK ) {
      opStr = (char *)realloc( (void *)opStr,
                               (size_t) EMS__SZMSG * sizeof(char) * errcount++ );
      opPtr = opStr + used;
      ems_stat_c( &status );
      ems_eload_c( param, &i, opPtr, &j, &status);
      used += j;
      opStr[used++] ='\n';
    }
    opStr[used] = '\0';
    *error_mess = opStr;
    ems_annul_c( &status );
    ems_rlse_c();
    return 0;
  }
  ems_rlse_c();
  return 1;
}
