/*+
 *  Name:
 *     EXTRACTOR
 
 *  Purpose:
 *     Detects and parameterises objects on an image.
 
 *  Language:
 *     ANSI C
 
 *  Type of Module:
 *     ADAM A-task
 
 *  Invocation:
 *     extractor( status )
 
 *  Arguments:
 *     STATUS = INTEGER (Given and Returned)
 *        The global status.
 
 *  Description:
 *     This program is a version of the SExtractor program written by
 *     Emmanual Bertin, that reads NDF format (and NDF supported
 *     foreign data formats) images. 
 
 *  Usage:
 *     extractor image config
 
 *  ADAM Parameters:
 *     CONFIG = LITERAL (Read)
 *        The name of the file that contains the many program
 *        parameters (things like the threshold for object
 *        detection). This is initially a file named "default.sex"
 *        that can be found in the directory $EXTRACTOR_DIR/config. To
 *        modify the parameters used by this program, you must take a
 *        copy of this file and edit it. Guidance about the values
 *        that parameters can take may be found in this file as well
 *        as in the associated SExtractor documentation.
 *
 *        The measurements made are determined by a list of parameters
 *        in the file $EXTRACTOR_DIR/config/default.param. Again if
 *        you want measurements that are not available by default, you
 *        must take a copy of this file and edit it. Remember to
 *        also change "default.sex" to use this file (otherwise you
 *        will continue to use the system-wide defaults).
 *
 *        Quick-look modifications of parameters can be made using the
 *        KEYWORDS and NAME/VALUE parameters. 
 *
 *        [$EXTRACTOR_DIR/config/default.sex]
 *     IMAGE = LITERAL (Read)
 *        The name of the image which contains the objects you wanted
 *        detected and parameterised. If you have initialised the
 *        CONVERT package (see SUN/55) then you may process foreign
 *        formats, such as FITS and IRAF.
 *        
 *        Using this parameter you actually give two image files. The
 *        first image will be used for detection and parameterising
 *        and the second will be used to actually measure the data
 *        values. Using this method allows you to measure the same
 *        objects many images, or two use a high signal to noise image 
 *        to determine the measurement regions on a low signal to noise
 *        image 
 *        [global_data_file]
 *     KEYWORDS = _LOGICAL (Read)
 *        Whether you want to enter a series of parameter names and
 *        values interactively. If TRUE then the parameters NAME and
 *        VALUE are used to cyclically prompt for program parameters
 *        and the values you want to use. To end the cycle respond
 *        with a null symbol (!)
 *        [FALSE]
 *     NAME = LITERAL (Read)
 *        The name of a program parameter that you want to
 *        interactively associate a value with. Enter "!" when you
 *        have no more to enter.
 *        [!]
 *     VALUE = LITERAL (Read)
 *        The value of the parameter you have just specified using the 
 *        NAME prompt.
 *        [!]
 
 *  Examples:
 *     {routine_example_text}
 *        {routine_example_description}
 *     [routine_example]...
 
 *  Notes:
 *     - Read the SExtractor documentation for the meaning of the
 *     parameters that you can enter in the default.sex and
 *     default.param files. 
 
 *  Implementation deficiencies:
 *     - uses setjmp/longjmp to work around program error flow
 *       problems. This may leave open files and unfreed memory
 *       allocations. 
 
 *  References:
 *     -  MUD/165, SExtractor User's Guide (v2.0).
 
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils

 *  Authors:
 *     AJC: Alan Chipperfield (STARLINK, RAL)
 *     PWD: Peter Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     25-NOV-1998 (PDRAPER):
 *        Added prologue and comments, removed print statements.
 *     20-MAY-1999 (PDRAPER):
 *        Now deals with NDF sections as inputs (previously split at
 *        comma in parentheses)
 *     {enter_changes_here}
 
 *  Bugs:
 *     {note_any_bugs_here}

 */

  /* Include files: */
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

/* Global variables: */
jmp_buf env;          /*  Program environment when setjmp called */

void extractor( int *status ) {

  /* Declarations@ */
  int keywords=0;       /*  Prompt for interactive parameters */
  char *argkey[20];     /*  Pointers to parameter names */
  char *argval[20];     /*  Pointers to parameter values */
  char argstr[800];     /*  Storage space for parameters */
  int narg;             /*  Number of parameters */
  int i;
  int nim;              /*  Number of images given (max=2) */
  char *str;
  char *ptr;
  
  if ( setjmp( env ) == 0 ) {
    
    /* Run program. */
    
    /* Default parameters */
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
    
    /* Get configuration file name */
    parGet0c( "CONFIG", prefs.prefs_name, MAXCHAR, status );
    if ( *status == SAI__OK ) {
      readprefs(prefs.prefs_name, argkey, argval, narg);
    }
    
    /* and image name(s) - again use argstr as buffer */
    parGet0c( "IMAGE", argstr, MAXCHAR, status );
    if ( *status == SAI__OK ) {
      for (nim = 0; (str=strtok( nim?NULL:argstr, notokstr ))!=NULL; nim++) {
        if (nim<MAXIMAGE) {
          /*  Check for trailing slice. This is indicated by a 
              "(" in the string */
          if ( strstr( str, "(" ) != NULL ) {
            /*  Access up to next token and use this as final part 
                of name */
            ptr = strtok( NULL, notokstr );
            *(--ptr) = ',';
          }
          prefs.image_name[nim] = str;
        } else {
          error(EXIT_FAILURE, "*Error*: Too many input images: ", str);
        }
      }
      prefs.nimage_name = nim;
      
      /* Now do the business */
      errStat( status );
      if ( *status == SAI__OK ) { 
        ndfBegin();
        srand( 1 ); /* Needed for repeatable measurements as rand
                       function is used in gatherup */
        makeit();
        errStat( status );
        ndfEnd( status );
      }
      if ( *status == SAI__OK ) {
        NFPRINTF(OUTPUT, "All done");
        NPRINTF(OUTPUT, "\n");
      }
    }
  } else {
    /* Return from longjmp in error() -- would be nice to close any open 
       files at this point.... */
    errStat( status );
  }
}
