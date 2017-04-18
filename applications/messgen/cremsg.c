/*+
 *  Name:
 *     CREMSG

 *  Purpose:
 *     Translate a Starlink facility Fortran include file into a MESSGEN
 *     source file.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     cremsg messgen [-v] <files>

 *  Description:
 *     This program will process any number of error include files in
 *     standard format, i.e. with components:
 *        *  Message
 *              INTEGER fac__ident
 *              PARAMETER( fac__ident = number )
 *     for each error in the facility.
 *     A certain amount of flexibility is allowed. In particular, the
 *     message may be immediately before or immediately after the
 *     PARAMETER definition.
 *
 *     CREMSG will produce the corresponding MESSGEN source file, optionally
 *     printing the lines produced.

 *  Parameters:
 *     Options: One or more of:
 *              -v - Output diagnostic informations about messgen run
 *
 *     <files> - any number of message source error include file in
 *               VMS MESSAGE format

 *  Authors:
 *     AJC: A.J. Chipperfield (STARLINK)
 *     BKM: B.K. McIlwrath (STARLINK)
 *     TIMJ: Tim Jenness (JAC)
 *     {enter_new_authors_here}

 *  History:
 *     04-SEP-1994 (AJC):
 *        Prototype version.
 *     29-APR-2003 (BKM):
 *        Convert from using sys_errlist[] to strerror()
 *     24-MAR-2004 (TIMJ):
 *        Make -Wall clean. Tidy up -v output so that correct errcode
 *        is printed.
 *     18-APR-2017 (GSB):
 *        Check return values from strtok calls are not NULL.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#define MAXLINE 101

long  errcode;
unsigned int fac_code=0, fac_severity=0, fac_messno=0;
unsigned int mess_code, mess_severity=0, mess_number=1;
char severity[14];
char fac_name[10]="          ", fac_prefix[10];
char mess_id [10], mess_prefix[10];

int verify=0;


static void
process_file(char *filename)
{
/*
 * Process an error include file.
 */
    FILE *fp, *fp0;
    char buffer[MAXLINE], message[MAXLINE];
    char out_file[20];
    char *p;
    long original_errcode; /* errcode read from file */
    int line_num = 0;

/*  Open the input file */
    if ((fp = fopen(filename, "r")) == NULL) {
       fprintf(stderr, "Cannot open file %s : %s\n", filename,
             strerror(errno) );
       return;
    }
/*  Initialize the message buffer */
    message[0] = ' ';

/* Now process the records */

    while ( fgets(buffer, MAXLINE, fp) != NULL) {
    line_num++;
    switch (buffer[0]) {
    case '*':
        strcpy( message, buffer );
        break;
    default:
        p = strtok( buffer, " \t(" );
        if (p == NULL) {
          fprintf(stderr, "Error parsing line %d\n", line_num);
          return;
        }

        if ( ! strncmp( p, "PARAMETER", 9 ) ) {
/*      It's a PARAMETER definition line */
/*      Get the prefix */
           p = strtok( NULL, " \t(_" );
           if (p == NULL) {
             fprintf(stderr, "Error extracting prefix from line %d\n",
                     line_num);
             return;
           }
           strcpy( mess_prefix, p );

/*      Get the ident */
           p = strtok( NULL, " \t_=" );
           if (p == NULL) {
             fprintf(stderr, "Error extracting message ID from line %d\n",
                     line_num);
             return;
           }
           strcpy( mess_id, p );

/*      Get the error value */
           p = strtok( NULL, " \t=)" );
	   if (p == NULL) {
	     fprintf(stderr, "Error extracting error code from line %d\n",
		     line_num);
	     return;
	   }

           errcode = strtol( p, (char**)NULL, 10 );
	   original_errcode = errcode; /* cache it */
/*      and extract the required sub-values */
           mess_severity = errcode & 0x7;
           mess_number = ( errcode = errcode >> 3 ) & 0xfff;
           mess_code = ( errcode >> 13 ) & 0x7ff;
           if ( verify ) {
              printf("ERRCODE %d\n", (int)original_errcode);
              printf("   messno %d\n", mess_number);
              printf("   severity %d\n",mess_severity);
              printf("   fac_code %d\n", mess_code);
           }
/*      If it's the first definition, set up the fac_name etc */
           if ( fac_name[1] == ' ' ) {
              strcpy( fac_prefix, mess_prefix );
              strcat( fac_prefix, "__" );
              strcpy( fac_name, mess_prefix );
              fac_code = mess_code;

/*           Open the output file */
              strcpy( out_file, fac_name );
              p = out_file;
              while (*p) {
                 *p = tolower(*p);
                 p++;
              }
              strcat( out_file, "_err.msg" );
              if ((fp0 = fopen( out_file, "w")) == NULL ) {
                 fprintf(stderr, "Cannot open %s\n", out_file);
                 return;
              }

/*           Write the .TITLE */
              fprintf( fp0, ".TITLE       %s\n", fac_name );

/*           Write the .FACILITY directive */
              fprintf( fp0, ".FACILITY    %s,%d/PREFIX=%s\n\n", fac_name,
                fac_code, fac_prefix);

              if ( verify ) {
                 printf( ".TITLE       %s\n", fac_name );
                 printf( ".FACILITY    %s,%d/PREFIX=%s\n\n", fac_name,
                   fac_code, fac_prefix);
              }
           }

           if ( mess_severity != fac_severity ) {
/*         We need a new .SEVERITY directive */
              switch (mess_severity) {
              case 0:
                strcpy( severity, "WARNING" );
                break;
              case 1:
                strcpy( severity, "SUCCESS" );
                break;
              case 2:
                strcpy( severity, "ERROR" );
                break;
              case 3:
                strcpy( severity, "INFORMATIONAL" );
                break;
              case 4:
                strcpy( severity, "SEVERE" );
                break;
              default:
                fprintf(stderr, "Illegal severity value\n");
                return;
              }
              fprintf( fp0, "\n.SEVERITY\t%s\n", severity );
              if( verify ) {
                 printf( "\n.SEVERITY\t%s\n", severity );
              }
              fac_severity = mess_severity;
           }

           if ( mess_number != fac_messno + 1 ) {
/*         We need a .BASE directive */
              fprintf( fp0, "\n.BASE\t\t%d\n", mess_number );
              if( verify ) {
                 printf( "\n.BASE\t\t%d\n", mess_number );
              }
              fac_messno = mess_number;
           } else {
              fac_messno ++;
           }

           if ( mess_code != fac_code ) {
              fprintf( stderr, "Facility code error [%d != %d] at line %d\n",
		       mess_code, fac_code, line_num);
              return;
           }

/*       The PARAMETER line is OK - get the associated message
         if it didn't precede the definitions */
           if ( message[0] == ' ' ) {
              if ( fgets(buffer, MAXLINE, fp) == NULL) {
                 fprintf( stderr, "Premature end of file\n" );
                 return;
              }
           } else {
              strcpy( buffer, message );
           }
           if ( buffer[1] != '*' ){
              p = buffer + strspn(buffer,"* \t");
/*           Remove the terminating newline */
              *(p+strlen(p)-1) = '\0';
              fprintf(fp0,"%s\t<%s>\n", mess_id, p );
              if ( verify ) {
                 printf( "%s\t<%s>\n", mess_id, p );
              }
           } else {
              fprintf( stderr, "Message line doesn't follow PARAMETER\n" );
              return;
           }

         } else {
/*       It's not a PARAMETER statement. If it's not an INTEGER statement
         either, annul the last message. */
           if ( strncmp( p, "INTEGER", 7 ) ) {
              message[0] = ' ';
           }
         }
    } /* End of switch */

    } /* End of while */
/* End of file - write .END and close */
    fprintf( fp0, ".END\n");
    if ( verify ) {
       printf( ".END\n");
    }
    fclose( fp );
    fclose( fp0 );
}


int
main(int argc, char *argv[])
{
    char *prog_name;
    int i;

/* Process argument list */
    prog_name = argv[0];
    for (*argv++; *argv; *argv++)
	if (**argv == '-') {
	    i = 1;
	    while ((*argv)[i] != '\0') {
		switch ((*argv)[i]) {
		  case 'v':
		    verify = 1;
		    break;
		  default:
		    fprintf(stderr,
			"%s - unknown option %s\n", prog_name, *argv);
		    exit(1);
 		}
		i++;
	    }
	} else
	    process_file(*argv);


    exit(0);
}
