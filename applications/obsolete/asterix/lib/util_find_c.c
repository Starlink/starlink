/*+
 *  Name:
 *     util_findfile_c.c
 *
 *  Purpose:
 *     Locate one or more files given a directory specification and a default
 *     file specification. The latter filters all the files found in the list
 *     defined by the former.
 *
 *  Language:
 *
 *     Starlink ANSI C
 *
 *  Description:
 *
 *  Authors:
 *
 *     David J. Allan (BHVAD::DJA)
 *
 *  History:
 *
 *     7-DEC-1992 (DJA):
 *        Original version.
 *    22-NOV-1994 (DJA):
 *        Removed nasty hack to get around Solaris 2.1 bug (DJA)
 *- */

/* Include Statements: */
#include "sae_par.h"
#include "f77.h"                       /* c <-> FORTRAN interfacing */
#include "cnf.h"                       /* c <-> FORTRAN strings */
#include "ems.h"                       /* Error handling */
#include <stdio.h>                     /* i/o handling */
#include <stdlib.h>
#include <string.h>                    /* String handling */
#include <ctype.h>                     /* Character handling */


/*
 *  VMS definitions
 */
#ifdef vms
extern void F77_EXTERNAL_NAME(lib$find_file_end)( INTEGER(ctx) );

extern int  F77_EXTERNAL_NAME(lib$find_file)( CHARACTER(wspec),
               CHARACTER(file), INTEGER(ctx), CHARACTER(deft)
               TRAIL(wspec) TRAIL(file) TRAIL(deft) );

/*
 *  UNIX definitions
 */
#else

#include "dirent.h"



int util_findfile_match( char *file, char *match )
  {
  char *str = file;
  char *pat = match;

  while ( *pat != 0)
    {
    switch (*pat)
      {
      case '*':
	pat++;
        while (*pat == '*')
	  pat++;
	if (*pat == 0) return 1;

        while (*str != 0 && *str != *pat)
          str++;

	if (*str == 0) return 0;
        break;

      case '?':
        break;

      default:
        if (*pat != *str) return 0;
	break;
      }
    pat++;
    str++;
    }

  if (*str == 0)
    return 1;
  else
    return 0;
  }


typedef
  struct
    {
    char		*wild,*deft,*cp,*dirname,*ncp;
    int                 dyn_deft,nf,dirlen,ndir;
    DIR			*dp;
    }
  ContextBlock, *ContextBlockPtr;

#define TRUE	 1
#define FALSE	 0

#endif


F77_SUBROUTINE(util_findfile_int)( CHARACTER(deft), CHARACTER(wspec),
     INTEGER(ctx), CHARACTER(file),
     INTEGER(status) TRAIL(deft) TRAIL(wspec) TRAIL(file) )
  {
  GENPTR_CHARACTER(deft)		/* Default file specification */
  GENPTR_CHARACTER(wspec)		/* Wildcard file specification */
  GENPTR_INTEGER(ctx)			/* Max number files allowed */
  GENPTR_CHARACTER(file)		/* Number of files found */
  GENPTR_INTEGER(status)		/* Inherited status */

#ifdef vms
  DECLARE_INTEGER(rval);

#else
  char                  *cp;
  struct dirent 	*dent;
  char      		*eptr;
  int			found = FALSE;
  ContextBlockPtr	lctx;
  int                   len;
  int                   dlen;
  char			*d_name;

  DECLARE_CHARACTER(ffname,132);
  DECLARE_CHARACTER(fmatch,132);
  DECLARE_LOGICAL(wasamatch);

#endif

  if ( (*status) != SAI__OK ) 		/* Check status */
    return;

#ifdef vms

  rval = F77_CALL(lib$find_file)( CHARACTER_ARG(wspec),
               CHARACTER_ARG(file), INTEGER_ARG(ctx), CHARACTER_ARG(deft)
               TRAIL(wspec) TRAIL(file) TRAIL(deft) );

  if ( !(rval&1) )
    {
    *status = SAI__ERROR;
    ems_rep_c( " ", "No more files found", status );
    }

#else

  lctx = (ContextBlockPtr) *ctx;		/* Retrieve context ptr */

  if ( ! lctx )					/* New context? */
    {
    lctx = (ContextBlockPtr) 			/* Create context block */
             malloc(sizeof(ContextBlock));

    lctx->wild = cnf_creim( wspec,
                   wspec_length );

    lctx->deft = cnf_creim( deft,               /* Copy in the default */
                   deft_length );

    eptr = getenv( lctx->deft );
    if ( eptr )                                 /* Translate from environment?*/
      {
      cnf_free( lctx->deft ); 			/* Free copy and point to */
      lctx->deft = eptr;			/* environment data */
      lctx->dyn_deft = FALSE;
      }
    else
      lctx->dyn_deft = TRUE;			/* Mark for deletion later */

    lctx->cp = lctx->deft;			/* Initialise search */
    lctx->dp = 0;				/* No current directory */
    lctx->nf = 0;
    lctx->ncp = cp;
    lctx->ndir = 0;

    *ctx = (int) lctx;				/* Set external context */
    }

  while ( !(*status) && *lctx->cp && !found )	/* While more directories */
    {
    if ( lctx->dp )                             /* Directory open? */
      {
      dent = readdir( lctx->dp );		/* Get next directory entry */
      if ( dent )				/* Anything in it? */
        {
        d_name = dent->d_name;

#ifdef sun4_Solaris
  d_name -= 2;
#endif
if ( getenv("DAVID") )
  printf("Entry : |%s|\n", d_name );

        if ( !strcmp(d_name,".") ||		/* Skip these entries */
             !strcmp(d_name,"..") ) continue;
          {
          if ( strstr( lctx->wild, "/" ) )	/* String to match. Need to */
	    {					/* prepend directory if wild */
            dlen = strlen(d_name);        	/* card contains directories */
            memcpy( ffname, lctx->dirname,	/* Join strings together */
                    lctx->dirlen );
            memcpy( ffname+lctx->dirlen,
                    d_name, dlen );
            }
          else
            {
            cnf_exprt( d_name, ffname, 		/* Just use directory */
                       ffname_length );
            }

          wasamatch = F77_EXTERNAL_NAME(util_findfile_aux)(
              CHARACTER_ARG(ffname),
              CHARACTER_ARG(wspec),
              CHARACTER_ARG(fmatch)
              TRAIL_ARG(ffname) TRAIL_ARG(wspec) TRAIL_ARG(fmatch) );

          if ( F77_ISTRUE(wasamatch) )
            {
            found = TRUE;
            lctx->nf++;
            cnf_exprt( lctx->dirname, 		/* Export the directory name */
                       file, file_length );
            cnf_exprt( d_name, 			/* Export the file name */
                       &file[lctx->dirlen],
                   file_length - lctx->dirlen );
            }
          }
        }
      else
        {
        closedir( lctx->dp );			/* Close the directory */
        lctx->dp = 0;
        if ( lctx->dyn_deft )
          {
          *status = SAI__ERROR;
          ems_rep_c( " ", "No more files found", status );
          }
        }
      }
    else                                        /* Open new directory */
      {
      if ( lctx->ndir && *lctx->ncp )		/* Advance to next one */
        {
        lctx->cp = lctx->ncp;
        while ( isspace(*(lctx->cp)) && *(lctx->cp) )
          lctx->cp++;
        }
      else if ( lctx->ndir )
        lctx->cp = lctx->ncp;

      if ( *(lctx->cp) )			/* Open the directory */
        {
        len = 0; cp = lctx->cp;			/* Count characters in this */
        while ( *cp && !isspace(*cp) )		/* directory name */
          { cp++; len++; }
        lctx->ncp = cp;

        lctx->dirname = cnf_creat( len+2 );	/* Create space for string */

        strncpy( lctx->dirname, lctx->cp, len );/* Copy the string */

        lctx->dirname[len] = '/';		/* Add directory delimiter */
        lctx->dirname[len+1] = 0;		/* And null terminate */
        lctx->dirlen = len+1;
        lctx->ndir++;

        lctx->dp = opendir( lctx->dirname );	/* Try to open it */
        if ( ! lctx->dp )
          {
          *status = SAI__ERROR;
          ems_setc_c( "DIR", lctx->dirname, len );
          ems_rep_c( " ",
             "Unable to open directory ^DIR", status );
          }
        }
      else
        *status = SAI__ERROR;
      }
    }
#endif
  }



F77_SUBROUTINE(util_findfile_end)( INTEGER(ctx), INTEGER(status) )
  {
  GENPTR_INTEGER(ctx)			/* Max number files allowed */
  GENPTR_INTEGER(status)		/* Inherited status */

#ifndef vms
  ContextBlockPtr	lctx;
#endif

  if ( (*status) != SAI__OK ) 		/* Check status */
    return;

/* Switch depending on operating system */
#ifdef vms
  F77_CALL(lib$find_file_end)( INTEGER_ARG(ctx) );

#else
  lctx = (ContextBlockPtr) *ctx;	/* Import context pointer */

  cnf_free( lctx->wild );		/* Free dynamic strings */
  if ( lctx->dyn_deft )
    cnf_free( lctx->deft );

  if ( lctx->dp )			/* Drop active directory */
    closedir( lctx->dp );

  if ( lctx->dirname )                  /* Active directory name */
    free( lctx->dirname );

  free( lctx );				/* Free the context block */

  *ctx = 0;				/* Zero external context variable */

#endif
  }
