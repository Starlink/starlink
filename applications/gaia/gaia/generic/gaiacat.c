/*
 *   Name:
 *      gaiacat
 *
 *   Purpose:
 *      Define modules for accessing, reading and writing CAT catalogues.
 *
 *   Language:
 *      C
 *
 *   Notes:
 *      This routine contains potentially unportable code.
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *   Authors:
 *      PDRAPER: Peter W. Draper, Starlink - University of Durham
 *
 *   History:
 *      21-SEP-1998 (PDRAPER):
 *         Original version.
 *      {enter_changes_here}
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gaiacat.h"
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems_par.h"
#include "ems.h"

extern void F77_EXTERNAL_NAME(cat_topen)( CHARACTER(name),
                                          CHARACTER(state),
                                          CHARACTER(mode),
                                          INTEGER(id),
                                          INTEGER(status)
                                          TRAIL(name) 
                                          TRAIL(state) 
                                          TRAIL(mode) );

extern void F77_EXTERNAL_NAME(gai_c2tab)( INTEGER(catId), 
                                          POINTER(dataPtr),
                                          INTEGER(numlines), 
                                          INTEGER(linelen), 
                                          INTEGER(status) );

extern void F77_EXTERNAL_NAME(cat_trlse)( INTEGER(catid),
                                          INTEGER(status) );



/*+
 *  Name:
 *     gaiaAccessCat
 *
 *  Purpose:
 *     Access a catalogue by name.
 *
 *  Arguments:
 *     filename = const char * (read)
 *        The name of the CAT library to be opened.
 *     catid = int * (write)
 *        Pointer to a int to receive the catalogue identifier.
 *     error_mess = char ** (write)
 *        Pointer to a pointer to char, which will be set to the
 *        address of a dynamic string containing an error message, if
 *        an error occurs. This should be freed by the caller, if necessary.
 *-
 */

int gaiaAccessCat( const char *filename, int *catid, char **error_mess )
{
  DECLARE_CHARACTER(name,132);      /* Local copy of filename (F77) */
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_POINTER(charPtr);         /* Pointer to F77 character array */
  DECLARE_CHARACTER(state,5);       /* State of catalogue */
  DECLARE_CHARACTER(mode,5);        /* Access mode */
  char *opStr;
  char *opPtr;
  int used, i, j, errcount;
  char param[EMS__SZPAR];

  /* Convert the file name into an F77 string */
  cnf_exprt( (char *)filename, name, name_length );

  /* Set the catalogue state and access mode */
  cnf_exprt( "OLD", state, state_length );
  cnf_exprt( "READ", mode, mode_length );

  /* Attempt to open the Catalogue. */
  status = SAI__OK;
  ems_mark_c();
  F77_CALL( cat_topen )( CHARACTER_ARG(name),
                         CHARACTER_ARG(state),
                         CHARACTER_ARG(mode),
                         INTEGER_ARG(catid),
                         INTEGER_ARG(&status)
                         TRAIL_ARG(name) TRAIL_ARG(state) TRAIL_ARG(mode) );
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
  ems_rlse_c();
  return 1;
}

/*+
 *  Name:
 *     gaiaFreeCat
 *
 *  Purpose:
 *     Free a catalogue.
 *
 *  Arguments:
 *     catId = int (read)
 *        The CAT library identifier.
 *-
 */

int gaiaFreeCat( int catId )
{
  DECLARE_INTEGER(status);
  status = SAI__OK;
  ems_mark_c();

  /* Free the catalogue */
  F77_CALL( cat_trlse ) ( INTEGER_ARG(&catId), INTEGER_ARG(&status) );
  if ( status != SAI__OK ) {
    ems_annul_c( &status );
  }
  ems_rlse_c();
  return 1;
}

/*+
 *  Name:
 *     gaiaReadCat
 *
 *  Purpose:
 *     Reads a previously opened CAT catalogue, returning the result
 *     as a single string buffer in the "tab table" format. 
 *
 *  Arguments:
 *     catId = int (read)
 *        The CAT library identifier.
 *     tabData = char ** (returned)
 *        The "tab table" format of the CAT library. This should be
 *        released by the caller when no longer required (using free()).
 *     error_mess = char ** (returned)
 *        A pointer to a pointer that will be addressed to a dynamic
 *        string, if an error is reported. This should be freed by the 
 *        caller, after the message is delivered.
 * 
 *  Notes:
 *     This routine access Fortran character arrays in a potentially
 *     unportable fashion. This is done for efficiency reasons.
 *-
 */

int gaiaReadCat( int catId, char **tabData, char **error_mess )
{
  DECLARE_POINTER(dataPtr);         /* Pointer to F77 memory */
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_INTEGER(numlines);        /* Number of elements(lines) in
                                       table */
  DECLARE_INTEGER(linelen);         /* Length of character strings */
  
  char *opStr;
  char *opPtr;
  int used, i, j, errcount;
  int need;
  char param[EMS__SZPAR];

  /* Access the catalogue, returning it as a tab table */
  status = SAI__OK;
  ems_mark_c();
  F77_CALL( gai_c2tab ) ( INTEGER_ARG(&catId), POINTER_ARG(&dataPtr),
                          INTEGER_ARG(&numlines), INTEGER_ARG(&linelen), 
                          INTEGER_ARG(&status) );
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
  } else {


    /* Convert the Fortran "array of strings" into a single C string with
       each element separated by \n */
    *tabData = (char *) dataPtr;
    opStr = (char *) dataPtr;
    for (i = 0, opStr += linelen-1; i < numlines; i++, opStr += linelen) {
      *opStr = '\n';
    }
    *opStr = '\0';
  }
  ems_rlse_c();
  return 1;
}
