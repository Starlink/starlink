#include	<ctype.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include        <setjmp.h>

#include	"define.h"
#include	"globals.h"
#include        "sae_par.h"
#include        "par.h"
#include        "ndf.h"

jmp_buf         env;

void extractor( int *status ) {
int keywords=0;
char *argkey[20];
char *argval[20];
char argstr[800];
int narg;
int i, nim;
char *str;

if ( setjmp( env ) == 0 ) {

/*default parameters */
   memset( &prefs, '\0', sizeof(prefstruct) );
   prefs.nimage_name = 1;
   prefs.image_name[0] = "image";

/* See if any KEYWORDS to be specified as parameters */
   parGet0l( "KEYWORDS", &keywords, status );
/*   keywords = 0;*/
   if ( keywords ) {
/*  KEYWORDS to be given - 
 *   set pointers to names in argkey
 *   and pointers to value strings in argval
 *   Use argstr as temporary buffer
*/
      argkey[0] = argstr;
      for ( narg=0; (*status == SAI__OK) && (narg < 20); narg++ ) {
/*     Get KEYWORD name */
         parGet0c( "NAME", argkey[narg], 20, status );
         argval[narg] = argkey[narg] + strlen(argkey[narg]) + 1;
/*     Get value */
         parGet0c( "VALUE", argval[narg], 20, status );
         argkey[narg+1] = argval[narg] + strlen(argval[narg]) + 1;
         parCancl( "NAME", status );
         parCancl( "VALUE", status );
      }
      if ( *status == PAR__NULL ) errAnnul( status );
      narg--;
   } else {
      narg = 0;
   }

   if ( *status == SAI__OK ) 
      for (i=0; i<narg; i++) {
        printf("%s %s\n",argkey[i],argval[i]);
      }

/* Get configuration file name */
   parGet0c( "CONFIG", prefs.prefs_name, MAXCHAR, status );
   if ( *status == SAI__OK ) {
      readprefs(prefs.prefs_name, argkey, argval, narg);
   }

/* and image name(s) - again use argstr as buffer */
   parGet0c( "IMAGE", argstr, MAXCHAR, status );
   if ( *status == SAI__OK ) {
      for (nim=0; (str=strtok( nim?NULL:argstr, notokstr ))!=NULL; nim++) 
          if (nim<MAXIMAGE) {
/*printf("Image %d: %s\n", nim, str);*/
            prefs.image_name[nim] = str;
          } else
            error(EXIT_FAILURE, "*Error*: Too many input images: ", str);
      prefs.nimage_name = nim;

/* Now do the business */
      errStat( status );
      if ( *status == SAI__OK ) { 
         ndfBegin();
         makeit();
         errStat( status );
         ndfEnd( status );
      }
      if ( *status == SAI__OK ) {
         NFPRINTF(OUTPUT, "All done");
         NPRINTF(OUTPUT, "\n");
      }

   }

} else
/* Return from longjmp in error() */
   errStat( status );
}
