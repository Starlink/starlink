 /*
 				main.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP, Leiden observatory & ESO)
*
*	Contents:	Command-line parsing.
*
*	Last modify:	15/05/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"

#define		SYNTAX \
"sex <image_name> [-c <configuration_file>] [-<keyword> <value>]"

/********************************** main ************************************/
int	main(int argc, char *argv[])

  {
   int		a, narg, nim;
   char		**argkey, **argval, *str;

  if (argc<2)
    {
    fprintf(OUTPUT, "\n		%s  Version %s\n", BANNER, VERSION);
    fprintf(OUTPUT, "\nby  %s\n", COPYRIGHT);
    fprintf(OUTPUT, "\nFor more information: %s\n", WEBSITE);
    fprintf(OUTPUT, "Questions or suggestions? %s\n", MAILINGLIST);
    fprintf(OUTPUT, "Mailing-list requests: %s\n", MAILINGLISTREQ);
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

  for (a=1; a<argc; a++)
    {
    if (argv[a][0] == '-' && a<(argc-1))
      {
      if (strlen(argv[a])<3)
        switch((int)tolower((int)argv[a][1]))
          {
/*-------- Config filename */
          case 'c':	strcpy(prefs.prefs_name, argv[++a]);
			break;
          default:	error(EXIT_SUCCESS,"SYNTAX: ", SYNTAX);
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
        for (str=NULL;str=strtok(str?NULL:argv[a], notokstr); nim++)
          if (nim<MAXIMAGE)
            prefs.image_name[nim] = str;
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

  exit(EXIT_SUCCESS);
  }
