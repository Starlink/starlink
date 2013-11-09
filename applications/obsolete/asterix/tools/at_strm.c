/*
*+
*  Name:
*     at_strm

*  Purpose:
*     Stream together input files, with include file processing

*  Language:
*     Starlink ANSI C

*  Invocation:
*     at_strm [-sSI] [-i dir]... files...

*  Description:
*     Concatenates its primary input files and writes them to standard
*     output. There are two controllable features, stripping and include
*     file processing.
* 
*     If stripping is enabled comments, defined as lines containing a
*     hash (#) is column one, are omitted from the output. Stripping is
*     on by default, but can be disabled using the -s option.
*
*     Lines of the form #include "file" can be processed in one of three
*     ways. In the default mode the include file is searched for in the
*     directory of the file currently being processed. In path mode the
*     list of directories supplied using the -i arguments is searched. 
*     The third mode is no include file processing, and this enabled using
*     the -I option. In this mode the line containing the include statement
*     is echoed to output.
*   
*     Errors are limited to those regarding include files, and appear on
*     standard error.
 
*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
      
/*
 *  Include files
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef
  enum {
    IM__NONE, IM__LOCAL, IM__PATH
    }
  IncMode;

typedef
  struct DeviceTag *DevicePtr;

typedef
  struct DeviceTag {
    FILE 	*fp;
    char	*name;
    int		line;
    DevicePtr	last;
    }
  Device;

typedef
  struct PathStrTag *PathStrPtr;

typedef
  struct PathStrTag {
    char	*name;
    PathStrPtr	last;
    }
  PathStr;

typedef
  struct {
    int		level;
    IncMode	imode;
    PathStrPtr	idirs;
    int		strip;
    Device	*cdev;
    }
  Stream;


/*
 * Drop one file off the end of a stream 
 */
Device *Close1( Stream *s ) {
  Device	*cd = s->cdev;

  if ( cd ) { 
    fclose( cd->fp );
    free( cd->name );
    s->cdev = cd->last;
    free( cd );
    s->level--;
    }
  
  return s->cdev;
  }


/*
 * Destroy a stream
 */
void Close( Stream *s ) {
  if ( s ) {
    while ( s->level > 0 )
      Close1( s );

    free( s );  
    }
  }


/*
 * Add a directory name to a path chain 
 */
void AddToPath( PathStrPtr *path, char *name ) {
  PathStrPtr pptr;
  
  pptr = (PathStrPtr) malloc( sizeof(PathStr) );
  
  pptr->name = strdup( name );
  
  pptr->last = *path;
  
  *path = pptr;
  }

/*
 * Add an environment variable value to a path structure
 */
void AddEnvToPath( PathStrPtr *path, char *eval, char *deflt ) {
  char *edup;
  char *cpos,*elast;
  int   l;
  
  if ( ! eval ) {
    AddToPath( path, deflt );
  } else {

/* Colons present? */
    cpos = strchr( eval, ':' );
    if ( cpos ) {
      
/*   Duplicate the string */    
      edup = strdup( eval );
      
/*   Loop from the end of the string. This puts path variable components */
/*   into the path structure in the correct order */
      l = strlen( edup ) - 1;
      elast = edup + l;
      while ( l >= 0 ) {
	
/*   Move back to next colon or beginning of string */
	while ( edup[l] != ':' && (l>=0) )
	  l--;

/*   Add path component to path */	
	AddToPath( path, edup + l + 1 );
	
/*   Replace colon with null to terminate next string */
        if ( l >= 0 )
	  edup[l] = 0;
        }
      
/*   Add whole environment variable */      
    } else {
      AddToPath( path, eval );
      }
    }
  }


/*
 * Add a file to a stream 
 */
Device *AddFile( Stream *s, char *name, FILE *fp ) {
  Device	*dev;
  
  dev = (Device *) malloc( sizeof(Device) );
  
  dev->fp = fp;
  dev->last = s->cdev;
  dev->name = strdup( name );
  dev->line = 0;
  
  s->cdev = dev;
  s->level++;
  
  return dev;
  }

/*
 * Create a new stream
 */
Stream *NewStream( char *name, IncMode imode, int strip ) {
  FILE	*ifp;
  Stream	*str = NULL;

  if ( strcmp( name, "-" ) )
    ifp = fopen( name, "r" );
  else {
    ifp = stdin;
    name = "*standard input*";
    }
  
/* Found file? */  
  if ( ifp ) {
    str = (Stream *) malloc( sizeof(Stream) );
    str->level = 0;
    str->cdev = NULL;
    str->strip = strip;
    str->imode = imode;
    str->idirs = NULL;
    
/* Add file to stream */
    AddFile( str, name, ifp );
    }
  
  return str;
  }

/*
 * Read a line from a stream into a buffer
 */
int GetLine( Stream *s, int blen, char *buf ) {
  int	gotaline = 0;
  Device	*dev = s->cdev;
  char		ifile[300];		/* Include file name */
  FILE		*ifp;			/* Include file handle */

/* While not got a line */  
  while ( dev && ! gotaline ) {

/* Try and read from current file */
    fgets( buf, blen, dev->fp );
    dev->line++;
    
/* Met end of line? */    
    if ( feof( dev->fp ) ) {
      dev = Close1( s );

/* Line is ok if it doesn't begin with a hash */      
    } else if ( *buf != '#' ) {
      gotaline = 1;

/* Must check lines beginning with hashes in strip mode */
    } else {
      
/*   Begins with #include? If not then the line is a comment and we go */
/*   through the loop again */
      if ( strncmp( buf, "#include", 8 ) ) {
        gotaline = ! s->strip;

/*   No include file handling so echo the line */	
      } else if ( s->imode == IM__NONE ) {
	gotaline = 1;

/*   Found the include file statement */
      } else {
	char *beg = buf + 8;
	char *end;
	int ok = 0;
	
/*   Skip white space */	
	while ( (*beg <= ' ') && *beg )
	  beg++;
	
/*   Now expect a double quote */
	if ( *beg++ == '"' ) {
	  end = beg;
 	  while ( (*end != '"') && *end )
	    end++;
  	  if ( *end == '"' ) {
	    *end = 0;
	    
/*      Look for include file using local mode? If so, we need the position */
/*      of the last / in the current filename */
	    if ( s->imode == IM__LOCAL ) {
	      char 	*spos = NULL;
	      char	*scp = dev->name;
	      
	      while ( *scp ) {
		if ( *scp == '/' )
		  spos = scp;
		scp++;
		}
	      
	      if ( spos ) {
		*spos = 0;
		sprintf( ifile, "%s/%s", dev->name, beg );
   	        ifp = fopen( ifile, "r" );
		*spos = '/';
	      } else {
   	        ifp = fopen( beg, "r" );
		}
	      
/*       Search paths of stream */	      
	    } else {
	      PathStrPtr iptr = s->idirs;

	      ifp = NULL;
	      while ( iptr && (! ifp) ) {
		sprintf( ifile, "%s/%s", iptr->name, beg );
   	        ifp = fopen( ifile, "r" );
		iptr = iptr->last;
		}
	      }
	    
/*       Got an include file */	    
	    if ( ifp )
  	      dev = AddFile( s, beg, ifp );
	    else
	      fprintf( stderr, "Unable to include file %s at line %d in file %s\n", beg, dev->line, dev->name );
	    ok = 1;
	    }
	  }
	
/*   Error report if not parsed ok */	
	if ( ! ok )
	  fprintf( stderr, "Invalid include file specification at line %d of file %s\n", dev->line, dev->name );
	}
      }
    }

  return gotaline;
  }


#define BUFLEN 2048

int main ( int argc, char *argv[] ) {
  Stream *s;
  char	buf[BUFLEN];
  int 	iargc = 1;
  int	strip = 1;
  IncMode imode = IM__LOCAL;
  PathStrPtr	path = NULL;
  
  if ( argc == 1 ) {
    printf( "Usage: at_strm [-sSI] [-i dir]... files...\n" );
    return 0;
    }
  
  while ( iargc < argc ) {
    if ( ! strcmp( argv[iargc], "-s" ) ) {
      iargc++;
      strip = 0;
    } else if ( ! strcmp( argv[iargc], "-S" ) ) {
      iargc++;
      strip = 1;
    } else if ( ! strcmp( argv[iargc], "-I" ) ) {
      iargc++;
      imode = IM__NONE;
    } else if ( ! strcmp( argv[iargc], "-i" ) ) {
      iargc++;
      AddEnvToPath( &path, argv[iargc++], NULL );
      imode = IM__PATH;
    } else {
      s = NewStream( argv[iargc], imode, strip );
      s->idirs = path;
      
      while ( GetLine( s, BUFLEN, buf ) ) {
        fputs( buf, stdout );
        }
    
      Close( s );

      iargc++;
      }
    }

  return 0;
  }
