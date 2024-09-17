/*
    This program is a filter that converts VMS Fortran into UNIX Fortran

    The convertion of INCLUDE statment file names is controlled by a subsitution
    file; each record contains a VMS file (or library module) specification and
    the corresponding Ultrix file specification separated by a space. The case
    of the VMS specification is ignored. If no match is found then the VMS file
    name is simple converted to lower case.

    The name of the substitution file is specified by -s<filename> on the
    command line; if not specified the name of the substitution file is
    include.sub.

    All occurrences of the "\" (backslash) character are converted to \\
    because UNIX compilers considers \ to be an escape character.

    -b<filename> specifies the object module name to be used by the -m
		 option.

    -j PAR fixup causes a statement "INCLUDE '/star/include/par_par'" to be
       added after any include statement which (after translation) includes
       the file /star/include/sae_par.

    -h HDS fixup causes a statement "INCLUDE '/star/include/dat_par'" to be
       added after any include statement which (after translation) includes
       the file /star/include/sae_par.

    -i<filename> specifies the input file name; the default is the standard
                 input.

    -m<filename> appends a line containing the name of the object module
		 and the name of the include file for each include
                 statement to the specified file. This forms a useful
                 starting point for creating a make file.

    -o<filename> specifies the output file name; the default is the standard
                 output.

    -p<prefix>   Specifies the prefix to be added to all file specification
                 not in the substitution file.

    -q           Suppresses all informational messages.

    -s<filename> specifies the name of the substitution file; the
                 default is include.sub.

    INCLUDE statements cannot contain continuation lines
*/

#include <string.h>
#include <ctype.h>
#include <stdio.h>

#define TRUE 1
#define FALSE 0

char vms_spec[256][512];		/* array of VMS file specs          */
char ultrix_spec[256][512];		/* corresponding Ultrix specs       */
int nspecs = 0;				/* number of specs                  */
int quiet = FALSE;			/* suppress informational messages  */
int make = FALSE;                       /* write "make file"                */
int hds_fixup = FALSE;			/* Add "/star/include/dat_par"?     */
int par_fixup = FALSE;			/* Add "/star/include/par_par"?     */

void incl(make_file, module, statement, prefix, sae_par)
FILE *make_file;
char module[];
char statement[];
char prefix[];
int *sae_par;
{
	int i;
	char *q1;
	char filename[63];
	char newname[63];
        char restofline[255];

/*
**  extract whatever is between the quotes eliminating spaces and converting
**  to lowercase.
*/
	q1 = strchr(statement, '\'');		/* look for first ' character */
	if (q1 == 0)				/* can't find one */
	{
		fprintf(stderr,"%% Statement \"%s\" ignored\n", statement);
		return;
	};
	for ( i = 0, q1++; *q1 != '\''; q1++)	/* find next quote */
	{
		if ( *q1 == '\0')		/* can't find one  */
		{
			fprintf(stderr,
			     "%% Unexpected end of line in statement \"%s\"\n",
				statement);
			fprintf(stderr,"%% statement ignored\n");
			return;
		};
		if ( !isspace(*q1) ) filename[i++] = tolower(*q1); /* copy non-
								 space chars */
	};
	filename[i] = '\0';			/* terminate filename */

/*
**  Save the rest of the input line
*/
        strcpy(restofline, q1+1);

/*
**  Try to match the file name with one of the VMS file specs from the
**  substitution file
*/
	for ( i = 0; i < nspecs; i++)
		if ( strcmp( filename, vms_spec[i]) == 0) break;

	if ( i == nspecs)	/* no match so prepend the prefix */
	{
		strcpy(newname, prefix);
		strcat(newname, filename);
		if (!quiet) fprintf(stderr,"%% \"%s\" replaced by \"%s\"\n",
			filename,newname);
        }
	else
	{
		strcpy(newname, ultrix_spec[i]);
		if (!quiet) fprintf(stderr,"%% \"%s\" replaced by \"%s\"\n",
			vms_spec[i],newname);
        }

/*
**  write the new include statement
*/
	sprintf(statement,"      INCLUDE '%s'%s", newname, restofline);
	if ( make ) fprintf(make_file, "%s : %s\n", module, newname);

/*
**  If the a fixup flag is set, see if '/star/include/sae_par' was included.
*/
	*sae_par = FALSE;
	if ( hds_fixup || par_fixup )
	{
	    if ( strcmp( newname, "/star/include/sae_par" ) == 0 )
	    {
		*sae_par = TRUE;
		if ( make )
		{
		    if ( hds_fixup ) fprintf(make_file, "%s : %s\n", module,
				    "/star/include/dat_par");
		    if ( par_fixup ) fprintf(make_file, "%s : %s\n", module,
				    "/star/include/par_par");
		}
	    }
	}
}

int main(argc, argv)
int argc;
char *argv[];
{
	FILE *sub_file, *input_file, *output_file, *make_file;
        int input_std = TRUE, output_std = TRUE;
	char in_line[128], out_line[128];
	int i, j , gotone, sae_par;
	char sub_file_name[63];
        char input_file_name[63];
        char output_file_name[63];
        char make_file_name[63];
        char prefix[63];
	char include[9], module[33];

	strcpy(sub_file_name,"include.sub");
	strcpy(make_file_name,"makefile");
	strcpy(include,"include'");
	strcpy(prefix,"/star/include/");
	strcpy(module,"module");

/*
**   Decode argument list
*/
	for ( i = 1; i < argc; i++ )
	{
		if (*argv[i] == '-')
		{
			switch (*(argv[i] + 1))
			{
			case 'b':
				strcpy(module,argv[i]+2);
				break;
			case 'j':
				par_fixup = TRUE;
				break;
			case 'h':
				hds_fixup = TRUE;
				break;
			case 'i':
				strcpy(input_file_name,argv[i]+2);
				input_std = FALSE;
				break;
			case 'm':
				if (strlen(argv[i]) > 2 )
				{
				    strcpy(make_file_name,argv[i]+2);
				    make = TRUE;
				}
				break;
			case 'o':
				strcpy(output_file_name,argv[i]+2);
				output_std = FALSE;
				break;
			case 'p':
				strcpy(prefix,argv[i]+2);
				break;
			case 'q':
				quiet = TRUE;
				break;
			case 's':
				strcpy(sub_file_name,argv[i]+2);
				break;
			default:
				fprintf(stderr,"%% Unknown qualifier %s\n",
					argv[i]);
			};
		}
		else
		{
			fprintf(stderr,"%% Arguments not allowed\n");
		}
	}

/*
**  Open the input and output files
*/
	if ( !input_std )
	{
	    if ((input_file = fopen(input_file_name, "r")) == 0)
		fprintf(stderr,"%% Can't open input file\n");
	}
	else
	{
	    input_file = stdin;
	}

	if ( !output_std )
	{
	    if ((output_file = fopen(output_file_name, "w")) == 0)
				fprintf(stderr,"%% Can't open output file\n");
	}
	else
	{
	    output_file = stdout;
	}
        if (make)
        {
	    if ((make_file = fopen(make_file_name, "a")) == 0)
	    {
	        fprintf(stderr,"%% Can't open output make file -m ignored\n");
	        make = FALSE;
	    }
        }

/*
**  Open the substitution file and load the arrays of file names converting
**  the VMS specifications to lower case
*/
	if (!quiet)
		fprintf(stderr,"%% Opening substitution file %s\n",
		sub_file_name);
	if ((sub_file = fopen(sub_file_name, "r")) == 0)
	{
	    fprintf(stderr,"%% Can't open substitution file\n");
	}
	else
	{
	    for ( nspecs = 0; nspecs < 256; nspecs++)
	    {
		if (fscanf(sub_file,"%s%s",vms_spec[nspecs],
		    ultrix_spec[nspecs]) == EOF) break;
		for ( i = 0; vms_spec[nspecs][i]; i++ )
		    vms_spec[nspecs][i] = tolower(vms_spec[nspecs][i]);
	    };
	    if (!quiet) fprintf(stderr,"%% %d substitutions read\n", nspecs);
	    fclose(sub_file);
	};

/*
**  Copy the input to the output
*/
	if ( (input_file != NULL) & (output_file != NULL) )
	{
	    while ( fgets(in_line,sizeof(in_line),input_file) )
	    {
		gotone = FALSE;
		for (i = 0, j = 0; in_line[i]; i++) /* look for INCLUDE */
		{
		    if ( !isspace(in_line[i]) )
		    {
			if ( tolower(in_line[i]) != include[j++]) break;
			if ( include[j] == '\0' )
			{
			    gotone = TRUE;
			    break;
			}
		    }
		}
		if (gotone) (void)incl(make_file, module, in_line, prefix,
				&sae_par);

/*
**  Look for backslash characters in non-comment lines
*/
		if ( in_line[0] != 'C' && in_line[0] != 'c' &&
			in_line[0] != '*' && in_line[0] != '!')
		{
			for (i=0, j=0; in_line[i]; i++, j++)
			{
		    		out_line[j] = in_line[i];
		    		if ( out_line[j] == '\\') out_line[++j] = '\\';
			}
/*
**  Warn about lines that have had \ added and are now more than 72 characters
**  long
*/
			if ((j > i) && (j > 73)) fprintf(stderr,
"%% *** line containing backslashes may be more than 72 characters long\n");

			out_line[j++] = '\0';
		}
		else
		{
			strcpy(out_line, in_line);
		}

/*
**  output the new line
*/
		fprintf(output_file,"%s",out_line);

/*
**  If the a fixup flag is set and the new include file name is
**  "/star/include/sae_par", add an extra statement to include the file
**  "/star/include/xxx_par" as well.
*/
		if ( sae_par )
		{
		    if (hds_fixup)
		    {
		    	fprintf( output_file,
			     "      INCLUDE '/star/include/dat_par'\n" );
		    	if (!quiet)
			fprintf(stderr,"%% Added '/star/include/dat_par'\n");
		    }
		    if (par_fixup)
		    {
		    	fprintf( output_file,
			     "      INCLUDE '/star/include/par_par'\n" );
		    	if (!quiet)
			fprintf(stderr,"%% Added '/star/include/par_par'\n");
		    }
                    sae_par = FALSE;
		}
	    }

/*
**  Write a blank line to the make file
*/
	    if (make) fprintf( make_file, "\n");
	}
}
