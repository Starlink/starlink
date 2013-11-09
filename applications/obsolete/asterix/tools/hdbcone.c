/*******************************************************************************

    HEASARC Database Cone (HDBCONE) utility.

    AUTHOR: Song Yom (HSTX)
    DATE:   06/94

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------
    12/94   Song Yom        added alternate host connection capability
    02/95   Edward J. Sabol output now goes to stdout unless output=file is
                                 specified as an argument

*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "csioc.h"

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS	0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE	1
#endif

#define ERROR_MSG_USAGE	"Usage: %s database ra dec radius column[,...] [outfile=filename]\n"

/*
    TCP/IP communication socket host name and port number
*/

#define HOST "legacy.gsfc.nasa.gov"
#ifdef sun
#define PORT 56839
#else
#define PORT 2014
#endif

/*
    Alternate host environment variable name
*/

#define HDBHOST "HDBHOST"

/*
    Maximum length of data buffer
*/

#define MAXLEN CSIOC_SIZE

/*
    Argument parameters
*/

#define MINARGC		6
#define MAXARGC		7
#define ARG_OUTFILE	"outfile="
#define NDATABASE	1
#define NRA		2
#define NDEC		3
#define NRADIUS		4
#define NCOLUMN		5

#if (defined(sun) && !defined(__STDC__))
main(argc,argv)
    int argc;
    char *argv[];
#else
main(int argc,char *argv[])
#endif
{
    int status = EXIT_FAILURE;
    int count;
    int sd;
    int length;
    char *ph;
    char host[MAXLEN];
    char file[MAXLEN];
    char ibuffer[MAXLEN];
    char obuffer[MAXLEN];
    FILE *fp;

/*
    Verify command line arguments, establish data communication connection
    with the server, construct and send the parameter buffer to the server,
    and check the return status.  If successful, create and populate the
    output data file with data received from the server, and terminate the
    data communication connection with the server.  Write all messages
    (informational and error) to standard error.
*/

    if (argc >= MINARGC && argc <= MAXARGC)
      {
	  /* get hostname of server */
	  if (ph = getenv(HDBHOST))
            strcpy(host,ph);
	  else
	    strcpy(host,HOST);
	  /* process args */
	  obuffer[0] = NULL;
	  for (count = 1; count < MINARGC; ++count)
	    {
		strcat(obuffer,argv[count]);
		strcat(obuffer," ");
	    }
	  obuffer[strlen(obuffer) - 1] = NULL;
	  if (argc == MAXARGC)
	    {
		if (!strncmp(argv[count],ARG_OUTFILE,strlen(ARG_OUTFILE)))
		  strcpy(file,&argv[count][strlen(ARG_OUTFILE)]);
		else
		  {
		      fprintf(stderr,ERROR_MSG_USAGE,argv[0]);
		      exit(status);
		  }
	    }
	  else
	    *file = NULL;
	  if (*file)
	    fprintf(stderr,"Trying %s ... ",host);
	  /* connect to server */
	  if (csioc_connect(host,PORT,&sd) == CSIOC_SUCCESS)
	    {
		if (*file)
		  fprintf(stderr,"connected.\n");
		if (csioc_output(sd,obuffer,strlen(obuffer) + 1) ==
		    CSIOC_SUCCESS && csioc_status(sd) == CSIOC_SUCCESS)
		  {
		      if (*file)
			{
			    fprintf(stderr,"Creating output file (%s) ... ",
				    file);
			    fp = fopen(file,"w");
			}
		      else
			fp = stdout;
		      if (fp)
			{
			    count = 0;
			    while (csioc_input(sd,ibuffer,&length)
				   == CSIOC_SUCCESS)
			      {
				  if (ibuffer[0] != '#')
				    ++count;
				  fprintf(fp,"%s\n",ibuffer);
			      }
			    fprintf(fp,"#  (%d records)\n",count);
			    fclose(fp);
			    if (*file)
			      fprintf(stderr,"%d records.\n",count);
			    status = EXIT_SUCCESS;
			}
		      else
			fprintf(stderr,"Error: Unable to create output file (%s).\n",file);
		  }
		else
		  fprintf(stderr,"%s\n",csioc_getstatus());
		csioc_disconnect(sd);
	    }
	  else
            fprintf(stderr,"Error!\n");
      }
    else
      fprintf(stderr,ERROR_MSG_USAGE,argv[0]);
    exit(status);
}
