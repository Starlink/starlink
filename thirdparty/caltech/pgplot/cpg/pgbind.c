#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/*
 * Define the program name, for use in error reporting.
 */
static char *prgnam = "pgbind";

/*
 * C prototypes are encoded as one or more consecutive lines in the
 * comment preamble of PGPLOT source code files. Such lines are
 * distinguishable from other comment lines by a special comment prefix.
 * Specify the prefix here.
 */
#define PG_PREFIX "C%"

/*
 * Specify a maximum for the reasonable length of a C prototype.
 * This is used to set the buffer size used to process prototypes.
 */
#define MAX_LINE 256

/*
 * Specify the max number of arguments expected. Given that the ANSI/ISO
 * C standard says that a compiler may set a limit as small as 31,
 * functions with more than this number of arguments are unportable
 * and should be avoided. We will enforce this limit.
 */
#define MAX_ARG 31

/*
 * Enumerate known system types.
 */
typedef enum {SYS_NON, SYS_BSD, SYS_CRAY2, SYS_VMS, SYS_MS} Systype;

/*
 * Declare a container used to record the specifics of a given
 * system.
 */
typedef struct {
  char *name;     /* Name of system template */
  Systype type;   /* The enumerated type of the system */
  char *suffix;   /* Suffix to add to subroutine name */
  int do_lower;   /* If true convert the subroutine name to lower case */
  int ltrue;      /* FORTRAN logical true */
  int lfalse;     /* FORTRAN logical false */
  char *doc;      /* Documentation of the template */
} Sysattr;

/*
 * List known system attributes.
 */
static Sysattr systable[]={
  {
    "bsd",     SYS_BSD,  "_", 1,  1, 0,
    "BSD f77 template. C string pointers are passed directly, but the length of each string is appended as an extra argument to the FORTRAN procedure call."
  },
  {
    "cray2", SYS_CRAY2,   "", 0,  1, 0,
    "Cray-2 FORTRAN template. C string pointers and lengths are combined into a single argument with the Cray fortran.h _cptofcd(pointer,length) macro."
  },
  {
    "vms",     SYS_VMS,   "", 1, -1, 0,
    "VMS FORTRAN template. C strings are passed via FORTRAN string descriptors."
  },
  {
    "ms",      SYS_MS,    "", 0, 1, 0,
    "Microsoft Powerstation Fortran + Visual C++. Each string argument is passed to the FORTRAN procedure as two adjacent arguments. The first argument is the C char * pointer of the string. The second is an int argument that contains the length of the string. In addition, the C prototype of the FORTRAN function contains a __stdcall qualifier"
  },
};

static int nsystem = sizeof(systable) / sizeof(systable[0]);

/*
 * Enumerate and list all command-line options.
 */
typedef enum {OP_NONE, OP_WRAPPER, OP_HEADER, OP_SUFFIX, OP_CASE,
	      OP_FALSE, OP_TRUE} Optype;
static struct {
  Optype type;   /* The enumeration identifier of the option */
  char *name;    /* The command-line option name (including hyphen prefix) */
  char *arg;     /* A short name for the type of any option argument */
  char *doc;     /* A short documentation string describing the option */
} options[] = {
  {
    OP_WRAPPER, "-w",      "", 
    "Write wrapper files."
  },
  {
    OP_HEADER,  "-h",      "", 
    "Write a new wrapper-library header file."
  },
  {
    OP_SUFFIX,  "-suffix", "string",
    "The suffix appended to FORTRAN symbols by the linker."
  },
  {
    OP_CASE,    "-case",   "upper|lower",
    "The typographical case given to FORTRAN symbols by the linker."
  },
  {
    OP_FALSE, "-false",      "integer",  
    "The numerical value of FORTRAN .FALSE."
  },
  {
    OP_TRUE, "-true",      "integer",  
    "The numerical value of FORTRAN .TRUE."
  },
};

static int noptions = sizeof(options) / sizeof(options[0]);

static Optype lookup_option(char *name);

/* Enumerate the supported argument data types and C type-qualifiers */

typedef enum {/* FORTRAN           C        */
  DAT_NONE,   /* Unknown           Unknown  */
  DAT_VOID,   /* (no equivalent)   (void)   */
  DAT_INT,    /* INTEGER           (int)    */
  DAT_FLT,    /* REAL              (float)  */
  DAT_DBL,    /* DOUBLE PRECISION  (double) */
  DAT_CHR,    /* CHARACTER         (char)   */
  DAT_LOG,    /* LOGICAL           (int)    */
/* Type qualifiers */
  DAT_CONST   /* C const qualifier */
} Typecode;

typedef struct {
  Typecode type;   /* Data-type enumerator */
  char *name;      /* Name of C type */
  char *ptr_cast;  /* The (type *) cast for pointers of the specified type */
} Datatype;

/*
 * List known data types.
 */
Datatype datatypes[]={
  {DAT_VOID,  "void",    "(void *)"},
  {DAT_INT,   "int",     "(int *)"},
  {DAT_FLT,   "float",   "(float *)"},
  {DAT_DBL,   "double",  "(double *)"},
  {DAT_CHR,   "char",    "(char *)"},
  {DAT_LOG,   "Logical", "(Logical *)"},
/* Type qualifiers */
  {DAT_CONST, "const",   NULL}
};
static int ndatatypes = sizeof(datatypes) / sizeof(datatypes[0]);

/*
 * Declare a type used to hold state information for the lexical
 * analyser.
 */
typedef struct {
  char prototype[MAX_LINE];/* C prototype from FORTRAN header */
  char buffer[MAX_LINE];   /* Lexical analyser work buffer */
  char *last;              /* Pointer to the last read token in prototype[] */
  char *next;              /* Pointer to the next token in prototype[] */
  char *bptr;              /* Pointer into buffer[] */
  char *fname;             /* The PGPLOT source file being processed */
  FILE *fp;                /* File pointer of file 'fname' */
  int lnum;                /* The current line number in the input file */
} Lex;

/*
 * Declare a function argument descriptor.
 */
typedef struct {
  char *name;        /* The name of the argument */
  char *cast;        /* The cast needed to remove constness of argument */
  Typecode type;     /* The data-type of the argument */
  int is_ptr;        /* True if the argument is a pointer */
  int is_const;      /* True if the argument is a const-qualified array */
  int is_scalar;     /* True if the argument is explicitly marked as scalar */
  int length_arg;    /* Argument number of associated length argument, or -1 */
} Argument;

/*
 * Declare a function descriptor.
 */
typedef struct {
  char *name;              /* The name of the function */
  Typecode type;           /* The data-type returned by the function */
  Argument args[MAX_ARG];  /* Array of 'narg' argument descriptors */
  int narg;                /* The number of arguments in args[] */
} Function;

typedef struct {
  char **files;  /* A NULL terminated list of unprocessed PGPLOT source files */
  Lex *lex;      /* Lexical analyser state container. */
  Sysattr sys;   /* The attributes of the target system */
  int do_header; /* If true write the header file of function prototypes */
  int do_wrapper;/* If true, write wrapper function files */
  char *header;  /* Name of header file */
  FILE *hfile;   /* File pointer to the open header file */
  Function fn;   /* The descriptor of the latest wrapper function */
} PGbind;

static PGbind *new_PGbind(int argc, char **argv);
static PGbind *del_PGbind(PGbind *pg);
static int parse_file(PGbind *pg, char *fname);

static Lex *new_Lex(void);
static Lex *del_Lex(Lex *lex);
static int lex_file(Lex *lex, char *fname);
static char *lex_line(Lex *lex);
static void lex_advance(Lex *lex);
static int lex_error(Lex *lex, char *msg, int next);
static Typecode read_type(Lex *lex);
static char *read_name(Lex *lex);
static int read_prototype(Lex *lex);

static int usage(void);
static void ini_Sysattr(Sysattr *sys);
static int decode_prototype(Lex *lex, Function *fn);
static int write_prototype(FILE *hfile, Function *fn);
static int write_wrapper(Sysattr *sys, Function *fn);
static char *type_name(Typecode type);
static char *pointer_cast(Typecode type);
static int wrap_line(FILE *stream, char *string, int start, int margin, int nmax);
static void write_spaces(FILE *stream, int nspace);
static int write_symbol(FILE *stream, Sysattr *sys, char *name);

/*.......................................................................
 * Create a C wrapper file or prototype header file for one or more
 * PGPLOT routines.
 */
int main(int argc, char *argv[])
{
  PGbind *pg=NULL;     /* pgbind state container */
  int waserr = 0;      /* True after any error */
  FILE *ofile;         /* file handle for the old cpgplot.c wrapper file */


/*
 * Create a new PGbind container.
 */
  pg = new_PGbind(argc, argv);
  if(pg==NULL)
    exit(usage());
    
 /* 
  * delete the previous bindings and header files, just in case
  */
  unlink("cpgplot.c");  
  /*unlink("cpgplot.h"); */ /* should we do this? */
    
/*
 * If a list of files was provided, process each of them in turn, otherwise
 * take input from stdin.
 */
  if(*pg->files) {
    while(*pg->files && !(waserr = parse_file(pg, *pg->files)))
      pg->files++;
  } else {
    waserr = parse_file(pg, NULL);
  };

  
/*
 * Clean up.
 */
  pg = del_PGbind(pg);
  exit(waserr ? EXIT_FAILURE : EXIT_SUCCESS);
}

/*.......................................................................
 * Extract marked prototypes from a given file, decode them, and
 * optionally write wrapper file(s) and append prototypes to the library
 * header file.
 *
 * Input:
 *  pg     PGbind *   The pgbind state container.
 *  fname    char *   The name of the file to be processed, or NULL to
 *                    select stdin.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int parse_file(PGbind *pg, char *fname)
{
/*
 * Connect the specified file to the lexical analyser.
 */
  if(lex_file(pg->lex, fname))
    return 1;
/*
 * Read one prototype at a time from the input file and process it.
 */
  while(read_prototype(pg->lex) == 0) {
/*
 * Decompose the prototype.
 */
    if(decode_prototype(pg->lex, &pg->fn))
      return 1;
/*
 * Write the header file prototype.
 */
    if(pg->do_header && write_prototype(pg->hfile, &pg->fn))
      return 1;
/*
 * Write the wrapper function.
 */
    if(pg->do_wrapper && write_wrapper(&pg->sys, &pg->fn))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Display program usage information on stderr.
 *
 * Output:
 *  return   int   EXIT_FAILURE - For use as exit() argument.
 */
static int usage(void)
{
  int margin;  /* The number of characters in a text margin */
  int i;
/*
 * Compile the command-line description.
 */
  fprintf(stderr, "Usage: %s template", prgnam);
  for(i=0; i<noptions; i++)
    fprintf(stderr, " [%s %s]", options[i].name, options[i].arg);
  fprintf(stderr, " [files]\n");
/*
 * List all the system types.
 */
  fprintf(stderr, "\n Where template must be one of:\n");
  for(i=0; i<nsystem; i++) {
    int op;
    Sysattr *sys = &systable[i];
    margin = fprintf(stderr, "  %-10s ", sys->name);
    wrap_line(stderr, sys->doc, margin, margin, 75);
    putc('\n', stderr);
/*
 * List the default options for the current system type.
 */
    write_spaces(stderr, margin);
    fprintf(stderr, "Default options: ");
    for(op=0; op<noptions; op++) {
      char *option = options[op].name;
      switch(options[op].type) {
      case OP_NONE:
      case OP_WRAPPER:
      case OP_HEADER:
	break;
      case OP_SUFFIX:
	fprintf(stderr, " %s \"%s\"", option, sys->suffix);
	break;
      case OP_CASE:
	fprintf(stderr, " %s %s", option, sys->do_lower ? "lower":"upper");
	break;
      case OP_FALSE:
	fprintf(stderr, " %s %d", option, sys->lfalse);
	break;
      case OP_TRUE:
	fprintf(stderr, " %s %d", option, sys->ltrue);
	break;
      };
    };
    fprintf(stderr, "\n");
  };
/*
 * List all optional arguments.
 */
  fprintf(stderr, "\n Options:\n");
  for(i=0; i<noptions; i++) {
    margin = fprintf(stderr, "  %-8s %-12s  ", options[i].name, options[i].arg);
    wrap_line(stderr, options[i].doc, margin, margin, 75);
    putc('\n', stderr);
  };
/*
 * Document input file usage.
 */
  fprintf(stderr, "\nPrototype input files:\n");
  margin = fprintf(stderr, "  files  ");
  {
    int start = margin;
    start = wrap_line(stderr, "Each file can contain zero or more C prototypes. ", start, margin, 75);
    start = wrap_line(stderr, "Each prototype consists of one or more lines, each line marked with C% in columns 1-2. ", start, margin, 75);
    start = wrap_line(stderr, "Continuation lines are heralded by a '\\' character at the end of the line being continued.\n", start, margin, 75);
    start = wrap_line(stderr, "If no files are specified, standard input is read.",  start, margin, 75);
    putc('\n', stderr);
  };
  return EXIT_FAILURE;
}

/*.......................................................................
 * Create a new state container for a specified system.
 *
 * Input:
 *  argc       int    The number of command-line arguments in argv[].
 *  argv      char ** Array of command-line arguments.
 * Output:
 *  return  PGbind *  The initialized container, or NULL on error.
 */
static PGbind *new_PGbind(int argc, char **argv)
{
  PGbind *pg=NULL;  /* Pointer to the returned container */
  int i;
/*
 * We require at least 1 command-line argument (after the program name).
 */
  if(argc < 2)
    return pg;
/*
 * Allocate a new container.
 */
  pg = (PGbind *) malloc(sizeof(PGbind));
  if(pg==NULL) {
    fprintf(stderr, "%s: Insufficient memory.\n", prgnam);
    return pg;
  };
/*
 * Pre-initialize the container at least to the point at which it can safely
 * be sent to del_PGbind(pg).
 */
  pg->files = NULL;
  pg->do_header = 0;
  pg->do_wrapper = 0;
  pg->lex = NULL;
  ini_Sysattr(&pg->sys);
  pg->header = "cpgplot.h";
  pg->hfile = NULL;
/*
 * The first argument must be the name of a recognised system class.
 */
  for(i=0; i<nsystem; i++) {
    Sysattr *sys = &systable[i];
    if(strcmp(sys->name, argv[1])==0) {
      pg->sys = *sys;
      break;
    };
  };
/*
 * System not found?
 */
  if(pg->sys.type == SYS_NON) {
    fprintf(stderr, "%s: Unrecognised template name: %s\n", prgnam, argv[1]);
    return del_PGbind(pg);
  };
/*
 * Parse command-line options.
 */
  for(i=2; i<argc && argv[i][0] == '-'; i++) {
    char *option = argv[i];
/*
 * Lookup the option.
 */
    switch(lookup_option(option)) {
/*
 * Check for the "-suffix string" option.
 */
    case OP_SUFFIX:
      if(++i < argc) {
	pg->sys.suffix = argv[i];
      } else {
	fprintf(stderr, "%s: Missing argument to the %s option.\n", prgnam,
		option);
	return del_PGbind(pg);
      };
      break;
/*
 * Check for the "-case upper|lower" option.
 */
    case OP_CASE:
      if(++i < argc) {
	char *value = argv[i];
	if(strcmp(value, "upper")==0)
	  pg->sys.do_lower = 0;
	else if(strcmp(value, "lower")==0)
	  pg->sys.do_lower = 1;
	else {
	  fprintf(stderr, "%s: Invalid combination: \"%s %s\"\n", prgnam,
		  option, value);
	  return del_PGbind(pg);
	};
      } else {
	fprintf(stderr, "%s: Missing argument to the %s option.\n", prgnam,
		option);
	return del_PGbind(pg);
      };
      break;
/*
 * Write the header prototypes?
 */
    case OP_HEADER:
      pg->do_header = 1;
      break;
/*
 * Write wrapper functions?
 */
    case OP_WRAPPER:
      pg->do_wrapper = 1;
      break;
/*
 * Override the default logical->int macro.
 */
    case OP_TRUE:
      if(++i < argc) {
	char *endp;
	pg->sys.ltrue = strtol(argv[i], &endp, 0);
	if(*endp != '\0') {
	  fprintf(stderr, "%s: The argument of %s must be a number.\n", prgnam,
		  option);
	  return del_PGbind(pg);
	};
      } else {
	fprintf(stderr, "%s: Missing argument to the %s option.\n", prgnam,
		option);
	return del_PGbind(pg);
      };
      break;
    case OP_FALSE:
      if(++i < argc) {
	char *endp;
	pg->sys.lfalse = strtol(argv[i], &endp, 0);
	if(*endp != '\0') {
	  fprintf(stderr, "%s: The argument of %s must be a number.\n", prgnam,
		  option);
	  return del_PGbind(pg);
	};
      } else {
	fprintf(stderr, "%s: Missing argument to the %s option.\n", prgnam,
		option);
	return del_PGbind(pg);
      };
      break;
    default:
      fprintf(stderr, "%s: Unrecognised \"%s\" option.\n", prgnam, option);
      return del_PGbind(pg);
    };
  };
/*
 * The remainwrite_prototypeing arguments must be the names of PGPLOT routine files.
 */
  pg->files = argv + i;
/*
 * Create the lexical analyser.
 */
  pg->lex = new_Lex();
  if(pg->lex == NULL)
    return del_PGbind(pg);
/*
 * If a new header file has been requested, create one.
 */
  if(pg->do_header) {
    pg->hfile = fopen(pg->header, "w");
    if(pg->hfile == NULL) {
      fprintf(stderr, "%s: Unable to open header file: %s\n", prgnam, pg->header);
      return del_PGbind(pg);
    };
/*
 * Write the header preamble.
 */
    fprintf(pg->hfile, "#ifndef cpgplot_h\n#define cpgplot_h\n\n");
    fprintf(pg->hfile, "#ifdef __cplusplus\n");
    fprintf(pg->hfile, "extern \"C\" {\n");
    fprintf(pg->hfile, "#endif\n\n");
    fprintf(pg->hfile, "typedef int Logical;\n\n");
  };
/*
 * Return the initialized container.
 */
  return pg;
}

/*.......................................................................
 * Cleanup and delete a PGbind state container.
 *
 * Input:
 *  pg     PGbind *  A container allocated by new_PGbind().
 * Output:
 *  return PGbind *  Allways NULL.
 */
static PGbind *del_PGbind(PGbind *pg)
{
  if(pg) {
    if(pg->hfile) {
/*
 * Write header postamble.
 */
      fprintf(pg->hfile, "\n#ifdef __cplusplus\n");
      fprintf(pg->hfile, "}\n");
      fprintf(pg->hfile, "#endif\n");
      fprintf(pg->hfile, "\n#endif\n");
      if(fclose(pg->hfile))
	fprintf(stderr, "%s: Error closing header file.\n", prgnam);
    };
/*
 * Delete the lexical analyser descriptor.
 */
    pg->lex = del_Lex(pg->lex);
  };
  return NULL;
}

/*.......................................................................
 * Search and read the next prototype from the current lexical analyser
 * input file. Prototypes are distinguishable by the FORTRAN comment prefix:
 * string assigned to PG_PREFIX (defined at the top of this file).
 *
 * Input:
 *  lex      Lex *  The lexical analyser state container.
 * Output:
 *  return   int    0 - The prototype has been copied succesfully into
 *                      lex->prototype[].
 *                  1 - No prototype found.
 */
static int read_prototype(Lex *lex)
{
  int noproto=1;      /* True until a valid prototype has been read */
  int prefix_len;     /* The length of the comment prefix string */
/*
 * Reset the prototype input buffers.
 */
  lex->prototype[0] = '\0';
  lex->buffer[0] = '\0';
  lex->last = lex->next = lex->prototype;
  lex->bptr = lex->buffer;
/*
 * Determine the length of the comment prefix that distinguishes prototype
 * comment lines in the PGPLOT source code file from other lines.
 */
  prefix_len = strlen(PG_PREFIX);
/*
 * Read the file line by line until EOF or until a line starting with
 * PG_PREFIX[] is encountered.
 */
  while(lex_line(lex) && strncmp(lex->buffer, PG_PREFIX, prefix_len) != 0)
    ;
/*
 * Did we find a prototype?
 */
  if(!feof(lex->fp) && !ferror(lex->fp)) {
    int slen=0;       /* Accumulated length of prototype */
    int finished = 0; /* True when no prorotype continuation lines remain */
/*
 * Concatenate multiple prototype lines.
 */
    do {
/*
 * Skip the comment prefix.
 */
      lex->bptr = &lex->buffer[prefix_len];
/*
 * Skip leading white-space.
 */
      while(*lex->bptr && isspace(*lex->bptr))
	lex->bptr++;
/*
 * Append the latest line to the last.
 */
      while(*lex->bptr && slen<MAX_LINE-1)
	lex->prototype[slen++] = *lex->bptr++;
/*
 * Assume that the prototype is complete, until otherwise proved.
 */
      finished = 1;
/*
 * If the last non-white-space character in the line is a \, then the line is
 * continued on the next line.
 */
      if(slen < MAX_LINE-1) {
	int endc;
	for(endc = slen-1; endc>0 && isspace(lex->prototype[endc]); endc--);
	if(lex->prototype[endc] == '\\') {
	  slen = endc;
	  finished = 0;
	};
      };
    } while(!finished && lex_line(lex) &&
	    strncmp(lex->buffer, PG_PREFIX, prefix_len)==0);
/*
 * Check for prototype buffer overflow.
 */
    if(slen >= MAX_LINE-1) {
      lex_error(lex, "Prototype too long", 0);
    } else {
      lex->prototype[slen] = '\0';
      lex->next = lex->last = lex->prototype;
      lex->bptr = &lex->buffer[0];
      lex->buffer[0] = '\0';
      noproto = 0;
    };
  };
  return noproto;
}

/*.......................................................................
 * Decode the prototype in pg->fn.prototype[] into a function name and
 * arguments.
 *
 * Input:
 *  lex      Lex *  The lexical analyser state container.
 * Input/Output:
 *  fn  Function *  The container in which to record the function
 *                  information.
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
static int decode_prototype(Lex *lex, Function *fn)
{
  int i,j;
/*
 * Read the return data-type of the function.
 */
  if((fn->type = read_type(lex)) == DAT_NONE)
    return lex_error(lex, "Bad function return type", 0);
/*
 * Only scalar return types are allowed.
 */
  if(*lex->next == '*')
    return lex_error(lex, "Pointer return types not allowed", 1);
/*
 * Get the function name.
 */
  if((fn->name = read_name(lex)) == NULL)
    return lex_error(lex, "Bad function name", 0);
/*
 * The next significant character should be an open paren.
 */
  if(*lex->next != '(')
    return lex_error(lex, "Expected '(' here", 1);
  else
    lex_advance(lex);
/*
 * Loop for all arguments up to the close paren.
 */
  for(i=0; i<MAX_ARG && *lex->next != ')'; i++) {
    Argument *arg = &fn->args[i];
/*
 * Clear the datatype and const-qualifier attributes.
 */
    arg->is_const = 0;
    arg->type = DAT_NONE;
/*
 * Read the type name and optional type-qualifier of the next argument.
 */
    while(arg->type == DAT_NONE) {
      Typecode type = read_type(lex);
      if(type == DAT_NONE)
	return lex_error(lex, "Unrecognised data-type", 0);
/*
 * Const qualifier or data-type?
 */
      if(type == DAT_CONST)
	arg->is_const = 1;  /* The type will be found in the next iteration */
      else
	arg->type = type;   /* This concludes the while() loop */
    };
/*
 * The void type is only valid when used to specify that the function
 * has no arguments.
 */
    if(arg->type == DAT_VOID) {
      if(i==0 && *lex->next==')') {
	fn->narg = 0;
	break;
      } else {
	return lex_error(lex, "void data-type illegal in this context", 0);
      };
    };
/*
 * Is the argument a pointer or a value?
 */
    if(*lex->next == '*') {
      arg->is_ptr = 1;
      lex_advance(lex);
      if(*lex->next == '*')
	return lex_error(lex, "Pointer to pointer not allowed", 1);
    } else {
      arg->is_ptr = 0;
      if(arg->is_const) {
	return lex_error(lex,
	       "Pointless const qualifier to pass-by-value argument.\n", 1);
      };
    };
/*
 * If the argument is a pointer and is const qualified, record the
 * cast needed to remove the constness for when the argument is
 * passed to the FORTRAN subroutine or passed to other functions that while
 * not modifying the respective argument are not declared with the
 * appropriate const qualifier.
 */
    arg->cast = (arg->is_ptr && arg->is_const) ? pointer_cast(arg->type) : "";
/*
 * Get the argument name.
 */
    if((arg->name = read_name(lex)) == NULL)
      return lex_error(lex, "Bad argument name", 0);
/*
 * Stop when the last argument has been seen.
 */
    if(*lex->next == ',') {
      lex_advance(lex);
    } else {
      fn->narg = i+1;
      break;
    };
  };
/*
 * Too many arguments?
 */
  if(i >= MAX_ARG)
    return lex_error(lex, "Too many arguments", 1);
/*
 * The argument list terminator must be a close paren.
 */
  if(*lex->next == '\0')
    return lex_error(lex, "Incomplete argument list", 1);
  else if(*lex->next != ')')
    return lex_error(lex, "Unexpected character", 1);
  else {
    do {
      lex_advance(lex);
    } while(*lex->next && (isspace(*lex->next) || *lex->next == ';'));
    if(*lex->next != '\0')
      return lex_error(lex, "Unexpected character follows prototype", 1);
  };
/*
 * Decode the arguments.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
/*
 * Initialize the argument attributes with defaults.
 */
    arg->length_arg = -1;
    arg->is_scalar = 0;
/*
 * Decode any extra type-specific semantics.
 */
    switch(arg->type) {
    case DAT_CHR:
/*
 * Unless the char argument is explicitly marked as scalar via an
 * "_scalar" suffix on its name, then it will be treated as a string.
 */
      {
	char *last_underscore = strrchr(arg->name, '_');
	int slen = strlen(arg->name);
	if(!arg->is_ptr ||
	   (last_underscore && strcmp(last_underscore, "_scalar")==0)) {
	  arg->is_scalar = 1;
	} else {
/*
 * See if there is a length argument associated with the string.
 */
	  for(j=0; j<fn->narg; j++) {
	    if(j!=i && strncmp(fn->args[j].name, arg->name, slen)==0 &&
	       strcmp(&fn->args[j].name[slen], "_length")==0) {
	      arg->length_arg = j;
	      break;
	    };
	  };
	};
      };
      break;
    default:
      break;
    };
  };
  return 0;
}

/*.......................................................................
 * Given the initialized descriptor pg->fn, write a C prototype in the
 * new header file, that is open for write in pg->hfile.
 *
 * Input:
 *  hfile    FILE *  The file pointer to a header file opened for writing.
 *  fn   Function *  THe descriptor of the function to be prototyped.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
static int write_prototype(FILE *hfile, Function *fn)
{
  int i;
  

  if(hfile) {
/*
 * Write the type and function name and introduce the argument list.
 */
    fprintf(hfile, "%s %s(", type_name(fn->type),  fn->name);
/*
 * Write the function arguments.
 */
    if(fn->narg==0) {
      fprintf(hfile, "void)");
    } else {
      for(i=0; i<fn->narg; i++) {
	fprintf(hfile, "%s%s%s%s%s",
		fn->args[i].is_const ? "const " : "",
		type_name(fn->args[i].type),
		fn->args[i].is_ptr ? " *" : " ",
		fn->args[i].name,
		i<fn->narg-1 ? ", ":")");
      };
    };
/*
 * End the argument list.
 */
    fprintf(hfile, ";\n");
  };
  return 0;
}

/*.......................................................................
 * Given the initialized descriptor pg->fn, write a C wrapper file.
 *
 * Input:
 *  sys   Sysattr *  A list of system attributes.
 *  fn   Function *  The descriptor of the function to be written.
 * Output:
 *  return  int    0 - OK.
 *                 1 - Error.
 */
static int write_wrapper(Sysattr *sys, Function *fn)
{
  static char buffer[MAX_LINE];
  FILE *wfile;
  int i;
/*
 * Compose the wrapper file name.
 */
  sprintf(buffer, "%s.c", "cpgplot");
/*
 * Open the wrapper file.
 */

  if((wfile = fopen(buffer, "a")) == NULL) {
    fprintf(stderr, "%s: Can't open output wrapper file: %s\n", prgnam, buffer);
    return 1;
  };
  
  
/*
 * Allow prototype vs. function definition checking by including the
 * library header file.
 */
  fprintf(wfile, "#include \"cpgplot.h\"\n");
/*
 * Extra include files are required when there are string arguments.
 */
  {
/*
 * First determine whether there are any string arguments.
 */
    int has_string = 0;
    for(i=0; i<fn->narg && !has_string; i++)
      has_string = fn->args[i].type == DAT_CHR;
/*
 * If there are any string arguments, include string.h plus any system
 * specific string header files.
 */
    if(has_string) {
      fprintf(wfile, "#include <string.h>\n");
      switch(sys->type) {
      case SYS_CRAY2:
	fprintf(wfile, "#include <fortran.h>\n");
	break;
      case SYS_VMS:
	fprintf(wfile, "#include <descrip.h>\n");
	break;
      default:
	break;
      };
    };
  };
/*
 * Declare the return type of the FORTRAN procedure.
 */
  fprintf(wfile, "extern %s ", type_name(fn->type));
  switch(sys->type) {
  case SYS_MS:
    fprintf(wfile, "__stdcall ");
    break;
  default:
    break;
  };
  write_symbol(wfile, sys, fn->name+1);
  fprintf(wfile, "();\n");
/*
 * Write the function declaration.
 */
  fprintf(wfile, "\n%s %s(", type_name(fn->type), fn->name);
/*
 * Write the function arguments.
 */
  if(fn->narg==0) {
    fprintf(wfile, "void)");
  } else {
    for(i=0; i<fn->narg; i++) {
      fprintf(wfile, "%s%s%s%s%s",
	      fn->args[i].is_const ? "const " : "",
	      type_name(fn->args[i].type),
	      fn->args[i].is_ptr ? " *" : " ",
	      fn->args[i].name,
	      i<fn->narg-1 ? ", ":")");
    };
  };
/*
 * End the argument list and start the definition block.
 */
  fprintf(wfile, "\n{\n");
/*
 * Declare intermediate variables.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
    switch(arg->type) {
    case DAT_LOG:
      fprintf(wfile, "  int l_%s = %s ? %d:%d;\n", arg->name, arg->name,
	      sys->ltrue, sys->lfalse);
      break;
    case DAT_CHR:
      fprintf(wfile, "  int len_%s = ", arg->name);
      if(arg->length_arg < 0) {
	if(arg->is_scalar)
	  fprintf(wfile, "1;\n");
	else
	  fprintf(wfile, "strlen(%s);\n", arg->name);
      } else {
	fprintf(wfile, "--(%s%s_length);\n",
		fn->args[arg->length_arg].is_ptr ? "*":"", arg->name);
      };
      if(sys->type == SYS_VMS)
	fprintf(wfile, "  struct dsc$descriptor_s dsc_%s = {0, DSC$K_DTYPE_T, DSC$K_CLASS_S, 0};\n", arg->name);
      break;
    default:
      break;
    };
  };
/*
 * Declare a temporary variable for the return value if required.
 */
  if(fn->type != DAT_VOID)
    fprintf(wfile, "  %s r_value;\n", type_name(fn->type));
/*
 * Initialize any un-initialized variables.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
    switch(arg->type) {
    case DAT_CHR:
      if(sys->type == SYS_VMS) {
	fprintf(wfile, "  dsc_%s.dsc$a_pointer = %s%s;\n",
		arg->name, arg->cast, arg->name);
	fprintf(wfile, "  dsc_%s.dsc$w_length = len_%s;\n",
		arg->name, arg->name);
      };
      break;
    default:
      break;
    };
  };
/*
 * Cache the return value of the fortran call.
 */
  fprintf(wfile, "  %s", fn->type==DAT_VOID ? "":"r_value = ");
/*
 * Write the system-specific symbol used to call the FORTRAN procedure.
 */
  write_symbol(wfile, sys, fn->name+1);
/*
 * Open the argument list.
 */
  putc('(', wfile);
/*
 * Write the FORTRAN arguments.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
/*
 * Write the argument separator.
 */
    if(i>0)
      fprintf(wfile, ", ");
/*
 * Handle the data-type specific semantics of passing each argument.
 */
    switch(arg->type) {
    case DAT_LOG:
      fprintf(wfile, "&l_%s", arg->name);
      break;
    case DAT_CHR:
      switch(sys->type) {
      case SYS_CRAY2:
	fprintf(wfile, "_cptofcd(%s%s, len_%s)",
		arg->is_ptr ? arg->cast : "&",
		arg->name, arg->name);
	break;
      case SYS_VMS:
	fprintf(wfile, "&dsc_%s", arg->name);
	break;
      case SYS_MS:
	fprintf(wfile, "%s%s, len_%s",
		arg->is_ptr ? arg->cast : "&",
		arg->name, arg->name);
	break;
      default:
	fprintf(wfile, "%s%s",
		arg->is_ptr ? arg->cast : "&",
		arg->name);
	break;
      };
      break;
    default:
      fprintf(wfile, "%s%s",
	      arg->is_ptr ? arg->cast : "&",
	      arg->name);
      break;
    };
  };
/*
 * Add any extra trailing arguments.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
    switch(arg->type) {
    case DAT_CHR:
      switch(sys->type) {
      case SYS_CRAY2:
      case SYS_VMS:
      case SYS_MS:
	break;
      default:
	fprintf(wfile, ", len_%s", arg->name);
	break;
      };
      break;
    default:
      break;
    };
  };
/*
 * Terminate the function call.
 */
  fprintf(wfile, ");\n");
/*
 * Perform required post-call operations.
 */
  for(i=0; i<fn->narg; i++) {
    Argument *arg = &fn->args[i];
/*
 * Handle the data-type specific semantics passing each argument.
 */
    switch(arg->type) {
    case DAT_CHR:
      if(arg->length_arg >= 0) {
	fprintf(wfile, "  %s%s[%s%s_length] = '\\0';\n",
		arg->is_ptr ? "":"&", arg->name,
		fn->args[arg->length_arg].is_ptr ? "*":"",
		arg->name);
      };
      break;
    case DAT_LOG:
      if(arg->is_ptr && !arg->is_const) {
	fprintf(wfile, "  *%s = l_%s==%d ? 1:0;", arg->name, arg->name,
		sys->ltrue);
      };
    default:
      break;
    };
  };
/*
 * Emit a return statement if the function returns a value.
 */
  if(fn->type != DAT_VOID)
    fprintf(wfile, "  return r_value;\n");
/*
 * Terminate the function definition.
 */
  fprintf(wfile, "}\n");
/*
 * Close the wrapper file.
 */

  if(fclose(wfile)) {
    fprintf(stderr, "%s: Error closing wrapper file: %s.c\n", prgnam, fn->name);
    return 1;
  };
  
  return 0;
}

/*.......................................................................
 * Report a lexical error along with the context of the lexical offset
 * in to the prototype being decoded.
 *
 * Input:
 *  lex    Lex *  The lexical analyser state container.
 *  msg   char *  The error message.
 *  next   int    If true, the context of the error is the next token,
 *                as pointed to by lex->next, otherwise it is the last
 *                token read, a pointed to by lex->last.
 * Output:
 *  return int    Allways 1. This intended to be used where the error
 *                return value of the parent function is 1, such that
 *                the caller can type  return lex_error(lex,"...",1);
 */
static int lex_error(Lex *lex, char *msg, int next)
{
  char *token = next ? lex->next : lex->last;
  char *start;   /* The first character of the prototype to be shown */
  int posn;      /* Number of chars of context between 'start' and 'token' */
/*
 * The context of the error will be displayed by showing up to 10
 * characters either side of the token pointer. Get the start pointer
 * of this range.
 */
  start = (token - &lex->prototype[0]) > 10 ? (token-10) : &lex->prototype[0];
/*
 * Find the number of characters from 'start' at which the token starts.
 */
  posn = (token - start) + 1;
/*
 * Display error messages.
 */
  fprintf(stderr, "%s: Error on line %d of file: %s\n", prgnam, lex->lnum,
	  lex->fname);
/*
 * Prefix the context string. Add the length of the prefix to the
 * character offset to the start of the offending token.
 */
  posn += fprintf(stderr, "%s: ", prgnam);
/*
 * Write the context string.
 */
  fprintf(stderr, "%.20s...\n", start);
/*
 * Place a caret symbol below the start of the offending token.
 */
  write_spaces(stderr, posn-1);
  putc('^', stderr);
/*
 * Present the user's error.
 */
  fprintf(stderr, "%s.\n", msg);
/*
 * Return an error status.
 */
  return 1;
}

/*.......................................................................
 * Attempt to read a type name via the lexical analyser from the next
 * unprocessed part of the prototype string, and match it with
 * a Typecode enumeration identifier.
 *
 * Input:
 *  lex         Lex *  The lexical analyser state container.
 * Output:
 *  return Typecode    The data-type read. On error DAT_NONE is returned
 *                     but no error message will have been emitted.
 */
static Typecode read_type(Lex *lex)
{
  Typecode type = DAT_NONE; /* The return value */
  int length;               /* The length of the type name */
  int i;
/*
 * Skip leading white-space.
 */
  while(*lex->next && isspace(*lex->next))
    lex->next++;
/*
 * Save the pointer to the start of the token.
 */
  lex->last = lex->next;
/*
 * Locate the end of the next identifier.
 */
  while(*lex->next && (isalnum(*lex->next) || *lex->next == '_'))
    lex->next++;
/*
 * Search for the type name in the table of recognised types.
 */
  length = (lex->next - lex->last);
  for(i=0; type==DAT_NONE && i<ndatatypes; i++) {
    if(length == strlen(datatypes[i].name) &&
       strncmp(lex->last, datatypes[i].name, length) == 0)
      type = datatypes[i].type;
  };
/*
 * Position the lex->next pointer at the start of the next token.
 */
  while(*lex->next && isspace(*lex->next))
    lex->next++;  
/*
 * Type not recognised.
 */
  return type;
}

/*.......................................................................
 * Attempt to read an identifier name via the lexical analyser from the
 * next unprocessed part of the prototype string, and return a pointer
 * to a '\0' terminated copy of it, placed in the next unused
 * part of lex->buffer[].
 *
 * Input:
 *  lex         Lex *  The lexical analyser state container.
 * Output:
 *  return Typecode    The data-type read. On error DAT_NONE is returned
 *                     but no error message will have been emitted.
 */
static char *read_name(Lex *lex)
{
  char *copy;  /* A pointer to the copy of the string in lex->buffer[] */
/*
 * Skip leading white-space.
 */
  while(*lex->next && isspace(*lex->next))
    lex->next++;
/*
 * Save the pointer to the start of the token.
 */
  lex->last = lex->next;
/*
 * Copy the identifier into lex->buffer[], starting at lex->bptr.
 */
  copy = lex->bptr;
  while(*lex->next && (isalnum(*lex->next) || *lex->next == '_'))
    *lex->bptr++ = *lex->next++;
/*
 * Terminate the copy.
 */
  *lex->bptr++ = '\0';
/*
 * Position the lex->next pointer at the start of the next token.
 */
  while(*lex->next && isspace(*lex->next))
    lex->next++;  
/*
 * If the string has zero length, signal an error by returning NULL.
 * Otherwise return a pointer to the copy.
 */
  return *copy=='\0' ? NULL : copy;
}

/*.......................................................................
 * Return the type name associated with a Typecode enumerator.
 *
 * Input:
 *  type  Typecode    The enumeration identifier to return the name of.
 * Output:
 *  return    char *  The name of the associated type, or NULL on error.
 */
static char *type_name(Typecode type)
{
  int i;
  for(i=0; i<ndatatypes; i++) {
    if(datatypes[i].type == type)
      return datatypes[i].name;
  };
  return NULL;
}

/*.......................................................................
 * Return the pointer cast for the type associated with a Typecode enumerator.
 *
 * Input:
 *  type  Typecode    The enumeration identifier to return the name of.
 * Output:
 *  return    char *  The required cast in the form "(type *)".
 */
static char *pointer_cast(Typecode type)
{
  int i;
  for(i=0; i<ndatatypes; i++) {
    if(datatypes[i].type == type)
      return datatypes[i].ptr_cast;
  };
  return NULL;
}

/*.......................................................................
 * Advance over the next un-processed character of a prototype and
 * any trailing spaces up to the start of the next token.
 *
 * Input:
 *  lex    Lex *  The lexical analyser state container.
 */
static void lex_advance(Lex *lex)
{
/*
 * The current single-character token becomes the last processed token.
 */
  lex->last = lex->next;
/*
 * Advance over the next unprocessed character.
 */
  lex->next++;
/*
 * Skip white-space up to the start of the next token.
 */
  while(*lex->next && isspace(*lex->next))
    lex->next++;
  return;
}

/*.......................................................................
 * Look up a command line option by name.
 *
 * Input:
 *  name     char *  The name of the option to look up.
 * Output:
 *  return Optype    The enumeration identifier of the new type, or
 *                   OP_NONE if not recognised. 
 */
static Optype lookup_option(char *name)
{
  int i;
  for(i=0; i<noptions; i++) {
    if(strcmp(options[i].name, name)==0)
      return options[i].type;
  };
  return OP_NONE;
}

/*.......................................................................
 * A utility function to write a given number of spaces to a given
 * stream.
 *
 * Input:
 *  nspace   int   The number of spaces to be written.
 */
static void write_spaces(FILE *stream, int nspace)
{
  while(nspace > 0)
    nspace -= fprintf(stream, "%.*s", nspace, "              ");
  return;
}

/*.......................................................................
 * Take a single line and wrap it into multiple lines, with an optional
 * margin.
 *
 * Input:
 *  stream  FILE *  The stream to write to.
 *  string  char *  The string to be wrapped.
 *  start    int    The length of the existing pefix on the first line.
 *                  This will be padded up to 'margin' with spaces.
 *  margin   int    The number of characters to pad with spaces
 *                  before writing each line.
 *  nmax     int    The number of characters per line.
 * Output:
 *  return   int    The number of characters used on the last line.
 *                  This can be used to break up a single call to wrap_line()
 *                  into multiple calls, by suplying the return value of the
 *                  previous call as the 'start' column value for the next call.
 */
static int wrap_line(FILE *stream, char *string, int start, int margin, int nmax)
{
  int nnew;       /* The number of characters to be written on the next line */
  int last=start; /* The column number of the last character written */
  int i;
/*
 * Enforce a smaller margin than the line length.
 */
  if(margin > nmax)
    margin = nmax/10;
/*
 * Write as many lines as are needed to display the whole string.
 */
  for( ; *string; string += nnew, last+=nnew, start=0) {
    last = start;
    nnew = -1;
/*
 * Write a margin if requested.
 */
    if(margin>0) {
      if(start < margin) {
	write_spaces(stderr, margin-start);
	last = start = margin;
      };
    };
/*
 * Skip leading white-space.
 */
    while(isspace(*string))
      string++;
/*
 * Locate the end of the last complete word in the string before nmax
 * characters have been seen.
 */
    for(i=0; string[i] && (i<nmax-start || nnew<0); i++) {
      if(isspace(string[i])) {
	nnew = i;
	if(string[i] == '\n')
	  break;
      };
    };
/*
 * Write the whole string if there is no space before the end of the line.
 */
    if(nnew < 0 || string[i]=='\0')
      nnew = strlen(string);
/*
 * Write the new line, but don't start a new line after the last line, so
 * as to give the caller the chance to concatenate multiple calls to
 * wrap_line().
 */
    fprintf(stream, "%.*s%s", nnew, string, string[nnew]!='\0' ? "\n":"");
  };
/*
 * Return the column number of the last character printed.
 */
  return last;
}

/*.......................................................................
 * Write a FORTRAN symbol name in the form that the local FORTRAN
 * compiler is purported to export it to the linker.
 *
 * Input:
 *  stream   FILE *  The stream to write the name to.
 *  sys   Sysattr *  A list of system atributes.
 *  name     char *  The symbol name.
 */
static int write_symbol(FILE *stream, Sysattr *sys, char *name)
{
  int c;
/*
 * Write the symbol name in the required case.
 */
  while((c = *name++))
    putc(sys->do_lower ? tolower(c):toupper(c), stream);
/*
 * Append a suffix if specified.
 */
  fprintf(stream, "%s", sys->suffix);
  return 0;
}

/*.......................................................................
 * Initialize a system attributes container with default values.
 *
 * Input:
 *  sys   Sysattr *  The container to be initialized.
 */
static void ini_Sysattr(Sysattr *sys)
{
  sys->name = "";
  sys->doc = "";
  sys->type = SYS_NON;
  sys->suffix = "";
  sys->do_lower = 0;
  sys->ltrue = 1;
  sys->lfalse = 0;
}

/*.......................................................................
 * Create a new lexical analyser state descriptor connected to stdin.
 *
 * Output:
 *  return   Lex *  The initialized descriptor, or NULL on error.
 */
static Lex *new_Lex(void)
{
  Lex *lex = NULL;
/*
 * Allocate the container.
 */
  lex = (Lex *) malloc(sizeof(Lex));
  if(lex == NULL) {
    fprintf(stderr, "%s: Insufficient memory.\n", prgnam);
    return lex;
  };
/*
 * Initialize the descriptor at least up to the point at which it can
 * safely be sent to del_Lex().
 */
  lex->prototype[0] = '\0';
  lex->buffer[0] = '\0';
  lex->last = lex->next = lex->prototype;
  lex->bptr = lex->buffer;
  lex->fp = stdin;
  lex->fname = "(stdin)";
/*
 * Return the initialized descriptor.
 */
  return lex;
}

/*.......................................................................
 * Delete a Lex descriptor and its contents. This includes closing any
 * file that is assigned to it.
 *
 * Input:
 *  lex    Lex *  A descriptor previously allocated by new_Lex().
 * Output:
 *  return Lex *  Allways NULL.
 */
static Lex *del_Lex(Lex *lex)
{
  if(lex) {
    if(lex->fp)
      fclose(lex->fp);
  };
  return NULL;
}

/*.......................................................................
 * Re-connect a lexical analyser to a new input file.
 *
 * Input:
 *  lex      Lex *   The lexical analyser descriptor.
 *  fname   char *   The name of the file to be opened, or NULL to
 *                   select stdin.
 * Output:
 *  return   int     0 - OK.
 *                   1 - Error.
 */
static int lex_file(Lex *lex, char *fname)
{
  FILE *fp = NULL;
/*
 * If a file name was given, open the associated file, otherwise
 * substitute stdin.
 */
  if(fname) {
    fp = fopen(fname, "r");
  } else {
    fp = stdin;
    fname = "(stdin)";
  };
  if(fp == NULL) {
    fprintf(stderr, "%s: Error opening: %s\n", prgnam, fname);
    return 1;
  };
/*
 * Close any existing file connected to the lexical analyser.
 */
  if(lex->fp && lex->fp != stdin)
    fclose(lex->fp);
/*
 * Instate the new file.
 */
  lex->fp = fp;
  lex->fname = fname;
  lex->lnum = 0;
  return 0;
}

/*.......................................................................
 * Read a new line from a lexical analyser input file.
 *
 * Input:
 *  lex      Lex *  The lexical analyser descriptor.
 * Output:
 *  return  char *  Pointer to the buffer lex->buffer[] containing the
 *                  line read, or NULL on EOF or other error.
 */
static char *lex_line(Lex *lex)
{
  char *buff = fgets(lex->buffer, MAX_LINE, lex->fp);
  if(buff) {
    char *cptr;
/*
 * Keep a record of the number of the line last read.
 */
    lex->lnum++;
/*
 * Check that the line fitted completely within the available buffer size.
 */
    cptr = strchr(buff, '\n');
/*
 * Discard the newline character if found.
 */
    if(cptr) {
      *cptr = '\0';
    } else {
      int c;
      fprintf(stderr, "%s: Line %d of file %s is too long.\n", prgnam,
	      lex->lnum, lex->fname);
      do {
	c = getc(lex->fp);
      } while(c != '\n' && c!= EOF);
      return NULL;
    };
  };
  return buff;
}

