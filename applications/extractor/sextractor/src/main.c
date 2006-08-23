 /*
 				main.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Command-line parsing.
*
*	Last modify:	18/07/2005
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"

#define		SYNTAX \
EXECUTABLE " <image> [<image2>][-c <configuration_file>][-<keyword> <value>]\n" \
"> to dump a default configuration file: " EXECUTABLE " -d \n" \
"> to dump a default extended configuration file: " EXECUTABLE " -dd \n"

extern const char       notokstr[];

/********************************** main ************************************/
int	main(int argc, char *argv[])

  {
   int		a, narg, nim, opt, opt2;
   char		**argkey, **argval, *str;

  if (argc<2)
    {
    fprintf(OUTPUT, "\n         %s  version %s (%s)\n", BANNER,MYVERSION,DATE);
    fprintf(OUTPUT, "\nby %s\n", COPYRIGHT);
    fprintf(OUTPUT, "visit %s\n", WEBSITE);
    error(EXIT_SUCCESS, "SYNTAX: ", SYNTAX);
    }
  QMALLOC(argkey, char *, argc);
  QMALLOC(argval, char *, argc);

/*default parameters */
  prefs.pipe_flag = 0;
  prefs.nimage_name = 1;
  prefs.image_name[0] = "image";
  strcpy(prefs.prefs_name, "default.sex");
  narg = nim = 0;

/* PWD: FITS extension numbers, not set by default */
  prefs.extnum[0] = -1;
  prefs.extnum[1] = -1;

  for (a=1; a<argc; a++)
    {
    if (*(argv[a]) == '-')
      {
      opt = (int)argv[a][1];
      if (strlen(argv[a])<4 || opt == '-')
        {
        opt2 = (int)tolower((int)argv[a][2]);
        if (opt == '-')
          {
          opt = opt2;
          opt2 = (int)tolower((int)argv[a][3]);
          }
        switch(opt)
          {
          case 'c':
            if (a<(argc-1))
              strcpy(prefs.prefs_name, argv[++a]);
            break;
          case 'd':
            dumpprefs(opt2=='d' ? 1 : 0);
            exit(EXIT_SUCCESS);
            break;
          case 'v':
            printf("%s version %s (%s)\n", BANNER,MYVERSION,DATE);
            exit(0);
            break;
          case 'h':
          default:
            error(EXIT_SUCCESS,"SYNTAX: ", SYNTAX);
          }
        }
      else
        {
/*------ Config parameters */
        argkey[narg] = &argv[a][1];
        argval[narg++] = argv[++a];
        }       
      }
    else
      {
/*---- The input image filename(s) */
      for(; (a<argc) && (*argv[a]!='-'); a++)
        for (str=NULL;(str=strtok(str?NULL:argv[a], notokstr)); nim++)
          if (nim<MAXIMAGE) 
            {
            char *base = NULL;
            int extension = 0;
            /* PWD: Allow the FITS extension number to be appended to the file
             * names. */ 
            base = strdup( str );
            if ( sscanf( str,"%[^[][%d]", base, &extension ) == 2 ) 
              {
              fprintf( stderr,"base: %s, extension: %d\n", base, extension );
              prefs.image_name[nim] = base;
              prefs.extnum[nim] = extension;
              }
            else 
              {
              prefs.image_name[nim] = str;
              free( base );
              }
            }
          else
            error(EXIT_FAILURE, "*Error*: Too many input images: ", str);

      prefs.nimage_name = nim;
      a--;
      }
    }

  readprefs(prefs.prefs_name, argkey, argval, narg);

  free(argkey);
  free(argval);

  makeit();

  NFPRINTF(OUTPUT, "All done");
  NPRINTF(OUTPUT, "\n");

  return EXIT_SUCCESS;
  }

