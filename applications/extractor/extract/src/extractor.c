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
 *     BC: Brad Cavanagh (JAC, Hawaii)
 *     PWD: Peter Draper (STARLINK - Durham University)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     25-NOV-1998 (PWD):
 *        Added prologue and comments, removed print statements.
 *     20-MAY-1999 (PWD):
 *        Now deals with NDF sections as inputs (previously split at
 *        comma in parentheses)
 *     10-MAR-2004 (PWD):
 *        Increased size of filename buffers from 20 to 256.
 *     25-JAN-2006 (PWD):
 *        Change NDF slice handling so that it works for higher dimensions. 
 *        Previously only worked for 2D images (by chance).
 *     23-AUG-2006 (BC):
 *        Replace C++-style comments with C-style.
 *     15-JUL-2008 (TIMJ):
 *        Tweak NDG API to use size_t
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


#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	"define.h"
#include	"globals.h"
#include        "sae_par.h"
#include        "par.h"
#include        "ndf.h"
#include        "star/ndg.h"
#include        "merswrap.h"
#include        "prefs.h"
#include        "adam_defs.h"

/* Maximum number of NAME/VALUE pairs */
#define MAXARGS 20
/* Maximum length of a return parameter value (NAME/VALUE/IMAGE)*/
#define VALUELEN 256

extern const char       notokstr[];

/* Global variables: */
jmp_buf env;          /*  Program environment when setjmp called */

void real_extractor( int *status ) {

    /* Declarations */
    int keywords=0;          /*  Prompt for interactive parameters */
    char *argkey[MAXARGS];   /*  Pointers to parameter names */
    char *argval[MAXARGS];   /*  Pointers to parameter values */
    char argstr[MAXARGS*VALUELEN]; /*  Storage space for name & values */
    int narg;                /*  Number of parameters */
    int nim;                 /*  Number of images given (max=2) */
    Grp *igrp;               /*  Group identifier for input NDFs */
    size_t size;                /*  Number of NDFs returned */
    int flag;                /*  Status of NDFs returned */

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
            for ( narg=0; (*status == SAI__OK) && (narg < MAXARGS); narg++ ) {
                
                /*     Get KEYWORD name */
                parGet0c( "NAME", argkey[narg], VALUELEN, status );
                argval[narg] = argkey[narg] + strlen(argkey[narg]) + 1;
                
                /*     Get value */
                parGet0c( "VALUE", argval[narg], VALUELEN, status );
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
        parGet0c( "CONFIG", prefs.prefs_name, VALUELEN, status );
        if ( *status == SAI__OK ) {
            /* And read it, using our NAME, VALUE pairs in preference to those
             * in the file */
            readprefs( prefs.prefs_name, argkey, argval, narg );
        }
        
        /* and image name(s), obtain these as an NDG group */
        igrp = NULL;
        ndgAssoc( "IMAGE", 0, &igrp, &size, &flag, status );

        if ( *status == SAI__OK ) {

            /*  Only pick first two images, if more are given. */
            nim = ( size > 1 ) ? 2: 1;
            argval[0] = argstr;
            grpGet( igrp, 1, nim, argval, VALUELEN, status );
            prefs.image_name[0] = argval[0];
            if ( nim == 2 ) {
                prefs.image_name[1] = argval[1];
            }
            prefs.nimage_name = nim;
            grpDelet( &igrp, status );
            
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
                NFPRINTF (OUTPUT, "All done" );
                NPRINTF( OUTPUT, "\n" );
            }
        }
    } else {
        /* Return from longjmp in error() -- would be nice to close any open 
           files at this point.... */
        errStat( status );
    }
}

/* Atask wrapper */

#include "f77.h"

F77_SUBROUTINE(extractor)(INTEGER(fstatus)) {
    int status;
    F77_IMPORT_INTEGER(*fstatus,status);
    real_extractor(&status);
    F77_EXPORT_INTEGER(status,*fstatus);
    return;
}
