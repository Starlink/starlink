#if !defined( HDS1_FEATURE_DEFINED ) /* Once-only latch			    */
#define HDS1_FEATURE_DEFINED 1

/*+									    */
/* Name:								    */
/*    hds1_feature.h							    */

/* Purpose:								    */
/*    Define global "feature-test" macros for HDS.			    */

/* Invocation:								    */
/*    #include "hds1_feature.h"						    */

/* Description:								    */
/*    This file should be included before any other include files in all    */
/*    HDS C routines. It performs global initialisation of the compilation  */
/*    environment, including the setting of any "feature-test" macros	    */
/*    required.								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    2-SEP-1992 (RFWS):						    */
/*       Original version.						    */
/*    1-DEC-1992 (RFWS):						    */
/*       Added _POSIX2_SOURCE definition.				    */
/*    15-JUN-1999 (RFWS):                                                   */
/*       Updated _POSIX2_SOURCE to specify the version of POSIX required    */
/*       (this facility was not originally available).                      */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Specify the version of the POSIX standard we want to work with (should   */
/* provide POSIX.1, POSIX.2 and POSIX.4, if available).                     */
#define _POSIX_C_SOURCE 199309L

/*.									    */
#endif
