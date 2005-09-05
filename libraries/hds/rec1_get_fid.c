#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if defined( vms )
void rec1_get_fid( void ){};     /* This routine is not used on VMS systems */
#else

/* C include files:                                                         */
/* ===============                                                          */
#include <errno.h>
#include <string.h>
#include <stdio.h>

/* POSIX include files:                                                     */
/* ===================                                                      */
#include <sys/types.h>
#include <sys/stat.h>

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "win_fixups.h"          /* Windows special functions               */

   void rec1_get_fid( const char *fns, struct FID *fid )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_get_fid                                                          */

/* Purpose:                                                                 */
/*    Obtain (and validate) file identification information.                */

/* Invocation:                                                              */
/*    rec1_get_fid( fns, fid )                                              */

/* Description:                                                             */
/*    This function returns information in a FID structure to identify a    */
/*    named file and also validates the file status information to ensure   */
/*    that the file is of an acceptable type. The identification provided   */
/*    is unique, so that if two files have the same File ID, then they are  */
/*    the same file.                                                        */

/* Parameters:                                                              */
/*    const char *fns                                                       */
/*       Pointer to a null terminated string giving the host file-system    */
/*       name of the file.                                                  */
/*    struct FID *fid                                                       */
/*       Pointer to a FID structure which will be filled in to provide file */
/*       identification.                                                    */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  An error will be reported if the file status information indicates */
/*    that the file name does not refer to a regular file (e.g. if it is a  */
/*    directory file or a FIFO).                                            */
/*    -  This routine is not implemented for VMS systems.                   */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    PWD: Peter W. Draper (Starlink - Durham University)                   */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    10-MAY-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    24-NOV-1992 (RFWS):                                                   */
/*       Added checks for regular files.                                    */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Do not return a function value.                                    */
/*    16-DEC-2002 (PWD):                                                    */
/*       Added windows support (problem with inodes not being available)    */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      const char *msg;           /* Pointer to error message text           */
      struct stat statbuf;       /* Buffer for file status structure        */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Obtain file status information, reporting any errors.                    */
      if ( stat( fns, &statbuf ) )
      {
         hds_gl_status = DAT__FILNF;
         ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
         ems_setc_c( "FILE", fns, EMS__SZMSG );
         ems_rep_c( "REC1_GET_FID_1",
                    "Error accessing file \'^FILE\' - ^MESSAGE",
                    &hds_gl_status );
      }

/* If the status information is OK, but indicates that this is not a        */
/* regular file, then assign an appropriate error message.                  */
      else if ( !S_ISREG( statbuf.st_mode ) )
      {
         if ( S_ISDIR( statbuf.st_mode ) )
         {
            msg = "File is a directory";
         }
         else if ( S_ISCHR( statbuf.st_mode ) )
         {
            msg = "File is a character special file";
         }
         else if ( S_ISBLK( statbuf.st_mode ) )
         {
            msg = "File is a block special file";
         }
         else if ( S_ISFIFO( statbuf.st_mode ) )
         {
            msg = "File is a pipe or a FIFO special file";
         }
         else
         {
            msg = "File is not a regular file";
         }

/* Report an error.                                                         */
         hds_gl_status = DAT__FILIN;
         ems_setc_c( "FILE", fns, EMS__SZTOK );
         ems_setc_c( "MESSAGE", msg, EMS__SZTOK );
         ems_rep_c( "REC1_GET_FID_2",
                    "Error accessing file ^FILE - ^MESSAGE.",
                    &hds_gl_status );
      }

/* If the file is OK, then initialise the File ID to zero (to clear any     */
/* "padding" which may exist in the structure).                             */
      else
      {
         (void) memset( (void *) fid, 0, sizeof( struct FID ) );

/* Copy the status block fields which uniquely identify the file into the   */
/* File ID.                                                                 */
         fid->st_ino = statbuf.st_ino;
         fid->st_dev = statbuf.st_dev;

#if defined __MING32__
         /* Windows doesn't have inodes, so we need additional information to
          * determine if a file is really the same as another file. This extra
          * bit is stored in st_rdev (which usually a duplicate of device) */
         win_get_inodes( fns, &fid->st_ino, &fid->st_rdev );
#endif
      }

/* Return the current global status value.                                  */
      return;
   }
#endif
