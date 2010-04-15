void perl2argv( int * argc, char **outargv[])
{
/*
 *+

   Purpose:
     Populate a C argv structure from perl @ARGV

   Language:
     ANSI C with Perl extensions

   Description:
     This routine reads the perl variables $0 (program name) and @ARGV
     (array of supplied arguments) and stores them in "argc", "outargv"
     suitable for use in routines expectng standard C arguments.

   Arguments:
     argc  = Pointer to int (Returned)
        Number of command line arguments.
     outargv   = Pointer to Char*[] (Returned)
        Pointer to array of pointers to strings. Contains $0 and
        @ARGV. The pointer array is NULL-terminated. The memory
        associated with this array must be freed after it is used.

   Notes:
     - The contents of outargv are malloced using the perl New() macro.
       Use Safefree to free the array.

   Authors:
     Tim Jenness (TimJ)

   History:
     20-SEP-2005 (TimJ):
       Original version

   */

  SV* name; /* $0 */
  AV* argv; /* @ARGV */
  SV** val; /* pointer to member of @ARGV */
  I32 nargs; /* size of ARGV */

  int i;
  STRLEN nchars;
  char * perl_string;
  char * element;
  char ** args = NULL;
  char * dollar_zero = NULL;

  /* First $0 */
  name = perl_get_sv( "0", FALSE );
  if (name != NULL) {
    dollar_zero = SvPVbyte_nolen( name );
  }

  /* @ARGV */
  argv = (AV*) perl_get_av( "ARGV", FALSE );
  nargs = av_len( argv ) + 1;

  /* Actual size of argv is $0+scalar(@ARGV) */
  *argc = nargs + 1;

  /* some memory for the array of pointers
     Need nargs + space for $0 + space for trailing null
  */
  New( 0, args, (1+ *argc), char*);

  /* Now loop over all the args, allocating memory and copying */
  for (i = 0; i < *argc; i++) {
    nchars = 0;
    perl_string = NULL;
    if (i == 0) {
      /* $0 */
      perl_string = dollar_zero;
      nchars = strlen( dollar_zero );
    } else {
      /* ARGV */
      val = av_fetch( argv, i-1, 0);
      if ( val != NULL ) {
	perl_string = SvPVbyte( *val, nchars );
      }
    }

    /* Now have a pointer to a string or a null */
    New( 0, element, nchars+1, char );
    if ( perl_string != NULL ) {
      strncpy(element, perl_string, nchars );
    } else {
      element = '\0';
    }
    args[i] = element;
  }

  /* terminator */
  args[*argc] = NULL;

  *outargv = args;

}

/* Can only be called once we know the arguments are no longer
   required */
void freeargv( int argc, char **argv[] ) {
  int i;
  char ** args;

  /* local copy */
  args = *argv;

  /* Free all the strings */
  for (i = 0; i < argc; i++ ) {
    Safefree(args[i]);
  }

  /* Free the array */
  Safefree(args);
}
