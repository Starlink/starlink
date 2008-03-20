#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void rec_attach_file( int expand, const char *file, INT file_len,
			 char state, char mode, struct RCL *rcl,
			 struct HAN *han )
   {
/*+									    */
/* Name:								    */
/*    rec_attach_file							    */

/* Purpose:								    */
/*    Attach a container file.						    */

/* Invocation:								    */
/*    rec_attach_file( expand, file, file_len, state, mode, rcl, han )	    */

/* Description:								    */
/*    This function attaches a handle to the top-level record in a	    */
/*    container file. The specified file state dictates whether a new (or   */
/*    scratch) file is to be created, or an existing file opened. The	    */
/*    file's reference count is not altered and remains set to zero if it   */
/*    is a new file.							    */

/* Parameters:								    */
/*    int expand							    */
/*	 If expand is non-zero, then the file name supplied will be	    */
/*	 regarded as an abbreviated form of the full name of the file and   */
/*	 will be expanded (according to the underlying operating system's   */
/*	 rules) before use. Otherwise, the file name supplied is regarded   */
/*	 as already fully expanded and will be used literally.  This	    */
/*	 mechanism is provided to allow previously-expanded file names to   */
/*	 be given, while allowing for the fact that expanding a file name   */
/*	 twice may cause the wrong file to be identified (if the underlying */
/*	 file system has changed and/or the expanded file name contains	    */
/*	 special characters, for instance).				    */
/*    const char *file							    */
/*	 Pointer to a char array containing the host file-system name of    */
/*	 the container file to be attached. It should not be null	    */
/*	 terminated.  If expand is non-zero, then leading and trailing	    */
/*	 white space will be ignored. If expand is zero, then the file name */
/*	 must be fully-expanded and white space may be significant.	    */
/*    INT file_len							    */
/*       Number of characters in the file name.				    */
/*    char state							    */
/*       State of the file: 'N' to create a new file, 'O' to open an	    */
/*	 existing (old) file, or 'S' to create a scratch file.		    */
/*    char mode								    */
/*       File access mode: 'R' for read, 'W' for write (or update).	    */
/*    struct RCL * rcl							    */
/*	 Address of a Record Control Label. For a new or scratch file, this */
/*	 must contain the relevant information which will be written to the */
/*	 file's top-level record. If an existing container file is being    */
/*	 opened, then this structure will be filled with the RCL	    */
/*	 information for the file's top level record.			    */
/*    struct HAN *han							    */
/*       Address of a record handle which will be set to identify the	    */
/*	 container file's top-level record.				    */

/* Returned Value:							    */
/*    void								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    DSB:  David S Berry (JAC)                                             */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    26-MAR-1991 (RFWS):						    */
/*	 Made into a separate module and added prologue.		    */
/*    3-APR-1991 (RFWS):						    */
/*       Added terminating null to the file name string.		    */
/*    3-APR-1991 (RFWS):						    */
/*	 Changed to call rec1_find_slot (now has modified argument list).   */
/*    4-APR-1991 (RFWS):						    */
/*       Replaced sys_read with rec1_read_file.				    */
/*    3-MAY-1991 (RFWS):						    */
/*	 Completely re-structured and simplified to call new versions of    */
/*	 rec1_create_file and rec1_open_file with machine-independence in   */
/*	 mind.								    */
/*    21-MAY-1991 (RFWS):						    */
/*	 Removed use of the HCB reserve field: no longer present.	    */
/*    24-JUL-1991 (RFWS):						    */
/*	 Added code to update the file version number if it is less than    */
/*	 the software version number and the file is being opened for the   */
/*	 first time for write or update access.				    */
/*    9-OCT-1992 (RFWS):						    */
/*       Added check to prevent re-opening of a file marked for deletion.   */
/*    14-OCT-1992 (RFWS):						    */
/*	 Converted to a void function and changed to use separate string    */
/*	 pointer and length arguments.					    */
/*    1-DEC-(1992):							    */
/*       Added the expand parameter.					    */
/*    19-OCT-1993 (RFWS):						    */
/*	 Removed restriction on re-opening a file marked for deletion,	    */
/*	 since marking a file for deletion is a useful method of creating a */
/*	 scratch file and re-opening such a file may sometimes be	    */
/*	 necessary.							    */
/*    19-APR-2004 (BKM):                                                    */
/*       Revised for 64-bit HDS files.                                      */
/*    06-MAY-2004 (BKM):                                                    */
/*       Cope with both 32-bit and 64-bit files                             */
/*    18-SEP-2006 (DSB):                                                    */
/*       Close the file if its Header Control Block information could not   */
/*       be read (when reading an existing file).                           */

/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      INT i;			 /* Loop counter			    */
      INT inalq;		 /* Initial allocated file space (blocks)   */
      INT slot;			 /* File Control Vector slot number	    */
      int newslot;		 /* Whether a new FCV slot was needed	    */
      struct HAN par;		 /* Handle to parent record		    */
      struct HCB *hcb;		 /* Pointer to HCB information		    */

/* Local Constants:							    */
      const struct RID ridprimary = { 2, 0 }; /* ID of primary file record  */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* If an existing file is not required, then create a new container file,   */
/* resetting the file allocation size.					    */
      if ( state != 'O' )
      {
         inalq = hds_gl_inalq;
	 hds_gl_inalq = hds_gl_inalq0;
         rcl->extended = hds_gl_64bit = hds_gl_c64bit;
	 rec1_create_file( expand, file, file_len, inalq, &slot, &inalq );

/* Locate the Header Control Block information.				    */
	 rec1_locate_hcb( slot, 'W', &hcb );

/* If successful, then mark the file for deletion if it is to be a scratch  */
/* file.								    */
	 if ( _ok( hds_gl_status ) )
	 {
	    rec_ga_fcv[ slot ].dele = ( state == 'S' );

/* Initialise the Header Control Block stamp, version and end-of-file	    */
/* fields.								    */
	    hcb->stamp = REC__STAMP;
            if( hds_gl_64bit ) 
               hcb->version = REC__VERSION4;
            else
               hcb->version = REC__VERSION3;
            hcb->eof = inalq;

/* Fill the free space stack with null entries (-1) and insert the current  */
/* free block state at the bottom.					    */
            for ( i = 0; i < REC__MXSTK; i++ )
	    {
	       hcb->stk[ i ].bloc = -1;
	       hcb->stk[ i ].spare = -1;
	    }
	    hcb->stk[ REC__MXSTK - 1 ].bloc = 2;
	    hcb->stk[ REC__MXSTK - 1 ].spare = inalq - 1;
	 
/* Set up the parent handle for the top level record and create this	    */
/* record.								    */
            par.slot = slot;
	    par.rid = rec_gl_ridzero;
	    par.read = 0;
	    rec_create_record( &par, rcl, han );
	 }
      }

/* If an existing file is required, then open it and generate a handle for  */
/* its top level record.						    */
      else
      {
         rec1_open_file( expand, file, file_len, mode, &slot, &newslot );
         if ( _ok( hds_gl_status ) ) 
         {
             han->slot = slot;
             han->read = ( mode == 'R' );
             han->rid = ridprimary;

/* If the file is being opened for the first time, then locate its Header   */
/* Control Block information.						    */
             if ( newslot )
             {
                 rec1_locate_hcb( slot, 'R', &hcb );
                 if ( _ok( hds_gl_status ) )
                 {

/* Check the container file stamp and report an error if this is not an HDS */
/* file. Close the file again if necessary.				    */
                     if ( hcb->stamp != REC__STAMP )
                     {
                         hds_gl_status = DAT__FILIN;
                         rec1_fmsg( "FILE", slot );
                         emsRep( "REC_ATTACH_FILE_1",
                                    "The file ^FILE is not a valid HDS container \
file.",
                                    &hds_gl_status );
                         rec_close_file( han );
                     }

/* Check the version number of the software that created or last modified   */
/* the file. Report an error if there is a version mis-match. Close the	    */
/* file again if necessary.						    */
                     else if ( hcb->version > REC__VERSION4 )
                     {
                         hds_gl_status = DAT__VERMM;
                         rec1_fmsg( "FILE", slot );
                         emsSeti( "VFILE", hcb->version );
                         emsSeti( "VSOFT", REC__VERSION4 );
                         emsRep( "REC_ATTACH_FILE_2",
                                    "HDS file format version mismatch in file ^FILE \
- file version=^VFILE, software version=^VSOFT (possible re-link needed).",
                                    &hds_gl_status );
                         rec_close_file( han );
                     }
                     else
                     {
                         rec_ga_fcv[ slot ].hds_version = hcb->version;
                         hds_gl_64bit = ( hcb->version > REC__VERSION3 );
                     }
                 }

/* Close the file if its Header Control Block information could not be read. */
                 else
                 {
                    rec_close_file( han );
                 }
             } 
             else        /* !newslot */
             {
                 hds_gl_64bit = ( rec_ga_fcv[ slot ].hds_version > REC__VERSION3 );
             }

/* Read the Record Control Label for the top-level record.		    */
             rec_get_rcl( han, rcl );
         }
      }

/* Exit the routine.							    */
      return;
   }
