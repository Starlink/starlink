/******************************************************************************/
/*
*+
*  Name:
*     exercise.c

*  Purpose:
*     Exercise ALL NBS routines and cause ALL error status to be returned
*     (except ones that indicate system failure or resource exhaustion).

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 1988, 1990, 1993 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (RGO)
*     DJA: D.J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28-MAR-1988 (WFL):
*     07-Feb-1990 (WFL):
*     Update for V2.3
*     31-Mar-1993 (DJA):
*     Update for V2.4. New routines, and test of duff file
*     names removed for UNIX version.
*     4-Nov-1993 (DJA):
*     Update for V2.4.1. Variable length argument stuff
*     done properly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */

/* Include files */

#include <time.h>
#include <stdio.h>
#include <string.h>
#include "ems.h"

/* NBS error codes */

#include "sae_par.h"
#include "nbs_err.h"

/* Utility types */

enum REPORT_LEVEL {report_all,report_ok,report_error,report_unexpected};

enum REPORT_FUNCTION
   {
   report_open	      		= -3,
   report_write			= -2,
   report_close 		= -1,
   expect_ok			= SAI__OK,
   expect_sectionexisted	= NBS__SECTIONEXISTED,
   expect_toomanydims		= NBS__TOOMANYDIMS,
   expect_toomanybytes		= NBS__TOOMANYBYTES,
   expect_badoffset		= NBS__BADOFFSET,
   expect_badoption		= NBS__BADOPTION,
   expect_datanotsaved		= NBS__DATANOTSAVED,
   expect_defining		= NBS__DEFINING,
   expect_notdefining		= NBS__NOTDEFINING,
   expect_nilsid		= NBS__NILSID,
   expect_nilid			= NBS__NILID,
   expect_primitive		= NBS__PRIMITIVE,
   expect_notprimitive		= NBS__NOTPRIMITIVE,
   expect_itemnotfound		= NBS__ITEMNOTFOUND,
   expect_sectionnotfound	= NBS__SECTIONNOTFOUND,
   expect_cantopen		= NBS__CANTOPEN,
   expect_cantwrite		= NBS__CANTWRITE,
   expect_cantread		= NBS__CANTREAD,
   expect_notowner		= NBS__NOTOWNER,
   expect_timeout		= NBS__TIMEOUT,
   expect_datasaved		= NBS__DATASAVED,
   expect_datanotrestored	= NBS__DATANOTRESTORED,
   expect_hasids		= NBS__HASIDS,
   expect_nottoplevel		= NBS__NOTTOPLEVEL,
   expect_toplevel		= NBS__TOPLEVEL,
   expect_neverfound		= NBS__NEVERFOUND,
   expect_initallocfailed	= NBS__INITALLOCFAILED,
   expect_nomoreroom		= NBS__NOMOREROOM,
   expect_badversion		= NBS__BADVERSION,
   expect_impossible		= NBS__IMPOSSIBLE
   };


/* Prototypes */
void report(enum REPORT_FUNCTION function, char * message);
char * errtext( int status );
int trigger( int id, int * status );

/* Static variables */

static enum REPORT_LEVEL report_level = report_all;
static int error_count = 0;
static int status = SAI__OK;

/******************************************************************************/
/******************************************************************************/
/* MAIN                                                                       */
/******************************************************************************/
/******************************************************************************/

int
main ()
{

/* Static IDs and item IDs */

int topsid,sid,strucsid,primsid,dumsid;
int e1,e2d,e3d,e2n,e3n,strucid,primid,dumid;

/* Ordinary variables */

int i,maxdims;
int oldval,dumval;
int dims1[1],dims2[2],dims4[4];
time_t tim;
char msg[79];
char b256[256],d256[256];
int i256[256];

/* Initialise "constants" */

for (i=0; i<256; i++) b256[i] = i;

/* Open the reporting system */

report (report_open,"exercise.lis");
time (&tim);

sprintf( msg, "Noticeboard system exercise program at %s",ctime(&tim) );
report (report_write, msg );
report(report_write,"---------------------------------------------------------------\n");

/******************************************************************************/
/* NBC_TUNE                                                                   */
/******************************************************************************/

report (report_write,"\nNBC_TUNE\n--------\n\n");

/* Tune all possible parameters (set to zero and then back to defaults) */

nbc_tune ("max_defn_size",0,&oldval,&status);
nbc_tune ("max_defn_size",oldval,&dumval,&status);
report (expect_ok,"nbc_tune max_defn_size");
nbc_tune ("timeout_count",0,&oldval,&status);
nbc_tune ("timeout_count",oldval,&dumval,&status);
report (expect_ok,"nbc_tune timeout_count");
nbc_tune ("timeout_interval",0,&oldval,&status);
nbc_tune ("timeout_interval",oldval,&dumval,&status);
report (expect_ok,"nbc_tune timeout_interval");
nbc_tune ("world_write",0,&oldval,&status);
nbc_tune ("world_write",oldval,&dumval,&status);
report (expect_ok,"nbc_tune world_write");
nbc_tune ("increment_modify",0,&oldval,&status);
nbc_tune ("increment_modify",oldval,&dumval,&status);
report (expect_ok,"nbc_tune increment_modify");
nbc_tune ("check_modify",0,&oldval,&status);
nbc_tune ("check_modify",oldval,&dumval,&status);
report (expect_ok,"nbc_tune check_modify");

/* Tune non-existent parameter */

nbc_tune ("doesnt_exist",0,&oldval,&status);
nbc_tune ("doesnt_exist",oldval,&dumval,&status);
report (expect_badoption,"nbc_tune of doesnt_exist");

/******************************************************************************/
/* NBC_DEFINE_STRUCTURE / _PRIMITIVE / _SHAPE / _END_DEFINITION whilst not    */
/* currently defining							      */
/******************************************************************************/

report (report_write,"\nNBC_DEFINE_* errors\n-------------------\n\n");

/* Trigger errors due to calling routines when not defining a noticeboard   */

nbc_define_structure (topsid,"dummy_name","dummy_type",&sid,&status);
report (expect_notdefining,"nbc_define_structure");
nbc_define_primitive (topsid,"dummy_name","dummy_type",0,0,&sid,&status);
report (expect_notdefining,"nbc_define_primitive");
nbc_define_shape (topsid,2,dims2,&status);
report (expect_notdefining,"nbc_define_shape");
nbc_end_definition ("dummy_name","create_noticeboard",&status);
report (expect_notdefining,"nbc_end_definition");

/******************************************************************************/
/* NBC_BEGIN_DEFINITION                                                       */
/******************************************************************************/

report (report_write,"\nNBC_BEGIN_DEFINITION\n--------------------\n\n");

/* Begin defining a noticeboard						    */

nbc_begin_definition (&topsid,&status);
report (expect_ok,"nbc_begin_definition");

/* Try again								    */

nbc_begin_definition (&topsid,&status);
report (expect_defining,"nbc_begin_definition");

/* Create a primitive to permit generating errors when attempt to hang	    */
/* structures off it.							    */

nbc_define_primitive (topsid,"prim_name_1","prim_type_1",2,256,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive at start of definition");

/******************************************************************************/
/* NBC_DEFINE_STRUCTURE                                                       */
/******************************************************************************/

report (report_write,"\nNBC_DEFINE_STRUCTURE\n--------------------\n\n");

/* Create 5 top-level structures					    */

nbc_define_structure (topsid,"struc_name_1","struc_type_1",&strucsid,&status);
report (expect_ok,"nbc_define_structure struc_name_1");
nbc_define_structure (topsid,"struc_name_2","struc_type_2",&strucsid,&status);
report (expect_ok,"nbc_define_structure struc_name_2");
nbc_define_structure (topsid,"struc_name_3","struc_type_3",&strucsid,&status);
report (expect_ok,"nbc_define_structure struc_name_3");
nbc_define_structure (topsid,"struc_name_4","struc_type_4",&strucsid,&status);
report (expect_ok,"nbc_define_structure struc_name_4");
nbc_define_structure (topsid,"struc_name_5","struc_type_5",&strucsid,&status);
report (expect_ok,"nbc_define_structure struc_name_5");

/* Trigger some errors by attempting to define a structure below a nil sid  */
/* and below a primitive.						    */

nbc_define_structure (0,"struc_name_e","struc_type_e",&dumsid,&status);
report (expect_nilsid,"nbc_define_structure");
nbc_define_structure (primsid,"struc_name_e","struc_type_e",&dumsid,&status);
report (expect_primitive,"nbc_define_structure");

/******************************************************************************/
/* NBC_DEFINE_PRIMITIVE                                                       */
/******************************************************************************/

report (report_write,"\nNBC_DEFINE_PRIMITIVE\n--------------------\n\n");

/* Create 5 primitives below struc_name_5				    */

nbc_define_primitive (strucsid,"prim_name_1","prim_type_1",2,4,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive prim_name_1");
nbc_define_primitive (strucsid,"prim_name_2","prim_type_2",2,4,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive prim_name_2");
nbc_define_primitive (strucsid,"prim_name_3","prim_type_3",2,4,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive prim_name_3");
nbc_define_primitive (strucsid,"prim_name_4","prim_type_4",2,4,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive prim_name_4");
nbc_define_primitive (strucsid,"prim_name_5","prim_type_5",2,4,&primsid,
								    &status);
report (expect_ok,"nbc_define_primitive prim_name_5");

/* Trigger some errors by attempting to define a primitive below a nil sid  */
/* and below a primitive.						    */

nbc_define_primitive (0,"prim_name_e","prim_type_e",2,4,&dumsid,&status);
report (expect_nilsid,"nbc_define_primitive");
nbc_define_primitive (primsid,"prim_name_e","prim_type_e",2,4,&dumsid,
								    &status);
report (expect_primitive,"nbc_define_primitive");

/******************************************************************************/
/* NBC_DEFINE_SHAPE                                                           */
/******************************************************************************/

report (report_write,"\nNBC_DEFINE_SHAPE\n----------------\n\n");

/* Define shape for last defined primitive item. First do it legally, then  */
/* try and set too many dimensions.					    */

dims2[0] = 42; dims2[1] = 84;
nbc_define_shape (primsid,2,dims2,&status);
report (expect_ok,"nbc_define_shape");

dims4[0] = 42; dims4[1] = 84; dims4[2] = 168; dims4[3] = 336;
nbc_define_shape (primsid,4,dims4,&status);
report (expect_toomanydims,"nbc_define_shape");

/* Trigger some errors by attempting to define a shapes below a nil sid and */
/* below a structure.							    */

nbc_define_shape (0,2,dims2,&status);
report (expect_nilsid,"nbc_define_shape");
nbc_define_shape (strucsid,2,dims2,&status);
report (expect_notprimitive,"nbc_define_shape");

/******************************************************************************/
/* NBC_END_DEFINITION                                                         */
/******************************************************************************/

report (report_write,"\nNBC_END_DEFINITION\n------------------\n\n");

/* End the definition, creating noticeboard exercise_1			    */

nbc_end_definition ("exercise_1","create_noticeboard",&status);
report (expect_ok,"nbc_end_definition create");

/* Do a couple of quick definitions of noticeboards, saving one definition  */
/* to disc and one definition plus data	to disc.			    */

nbc_begin_definition (&topsid,&status);
nbc_define_structure (topsid,"struc_name_1","struc_type_1",&strucsid,&status);
nbc_define_primitive (strucsid,"prim_name_1","prim_type_1",2,4,&primsid,
								    &status);
nbc_define_primitive (strucsid,"prim_name_2","prim_type_2",2,4,&primsid,
								    &status);
nbc_define_structure (topsid,"struc_name_2","struc_type_2",&strucsid,&status);
nbc_define_primitive (strucsid,"prim_name_1","prim_type_1",2,4,&primsid,
								    &status);
nbc_define_primitive (strucsid,"prim_name_2","prim_type_2",2,4,&primsid,
								    &status);
nbc_define_shape (primsid,2,dims2,&status);
nbc_end_definition ("exercise_2","definition_save",&status);
report (expect_ok,"nbc_define definition_save e2");

nbc_begin_definition (&topsid,&status);
nbc_define_structure (topsid,"struc_name_1","struc_type_1",&strucsid,&status);
nbc_define_primitive (strucsid,"prim_name_1","prim_type_1",2,4,&primsid,
								    &status);
nbc_define_primitive (strucsid,"prim_name_2","prim_type_2",2,4,&primsid,
								    &status);
nbc_define_structure (topsid,"struc_name_2","struc_type_2",&strucsid,&status);
nbc_define_primitive (strucsid,"prim_name_1","prim_type_1",2,4,&primsid,
								    &status);
nbc_define_primitive (strucsid,"prim_name_2","prim_type_2",2,4,&primsid,
								    &status);
nbc_define_shape (primsid,2,dims2,&status);
nbc_end_definition ("exercise_3","noticeboard_save",&status);
report (expect_ok,"nbc_define noticeboard_save e3");

/* Trigger a few errors by using illegal file names and trying to re-create */
/* an already-existing section.						    */

#ifdef vms
  nbc_begin_definition (&topsid,&status);
  nbc_end_definition ("\\**&&;;","noticeboard_save",&status);
  report (expect_cantopen,"nbc_end_definition");
#endif

nbc_begin_definition (&topsid,&status);
report (expect_ok,"nbc_begin_definition");
nbc_end_definition ("exercise_1","create_noticeboard",&status);
report (expect_sectionexisted,"nbc_end_definition");

/******************************************************************************/
/* NBC_RESTORE_DEFINITION / _NOTICEBOARD                                      */
/******************************************************************************/

report (report_write,"\nNBC_RESTORE_*\n-------------\n\n");

/* Try restoring all definitions and noticeboards			    */

nbc_restore_definition ("exercise_2d","exercise_2",&status);
report (expect_ok,"nbc_restore_definition e2d");
nbc_restore_definition ("exercise_3d","exercise_3",&status);
report (expect_datasaved,"nbc_restore_definition e3d");
nbc_restore_noticeboard ("exercise_2n","exercise_2",&status);
report (expect_datanotsaved,"nbc_restore_noticeboard e2n");
nbc_restore_noticeboard ("exercise_3n","exercise_3",&status);
report (expect_ok,"nbc_restore_noticeboard e3n");

/******************************************************************************/
/* NBC_FIND_NOTICEBOARD                                                       */
/******************************************************************************/

report (report_write,"\nNBC_FIND_NOTICEBOARD\n--------------------\n\n");

/* Try finding all noticeboards */

nbc_find_noticeboard ("exercise_1",&e1,&status);
report (expect_ok,"nbc_find_noticeboard e1");
nbc_find_noticeboard ("exercise_2d",&e2d,&status);
report (expect_ok,"nbc_find_noticeboard e2d");
nbc_find_noticeboard ("exercise_3d",&e3d,&status);
report (expect_sectionnotfound,"nbc_find_noticeboard e3d");
nbc_find_noticeboard ("exercise_2n",&e2n,&status);
report (expect_ok,"nbc_find_noticeboard e2n");
nbc_find_noticeboard ("exercise_3n",&e3n,&status);
report (expect_ok,"nbc_find_noticeboard e3n");

/******************************************************************************/
/* NBC_TUNE_NOTICEBOARD                                                       */
/******************************************************************************/

report (report_write,"\nNBC_TUNE_NOTICEBOARD\n--------------------\n\n");

/* Tune all possible parameters (set to zero and then back to defaults) */

/* nbc_tune_noticeboard (e1,"timeout_count",0,&oldval,&status);
nbc_tune_noticeboard (e1,"timeout_count",oldval,&dumval,&status);
report (expect_ok,"nbc_tune_noticeboard timeout_count");
nbc_tune_noticeboard (e1,"timeout_interval",0,&oldval,&status);
nbc_tune_noticeboard (e1,"timeout_interval",oldval,&dumval,&status);
report (expect_ok,"nbc_tune_noticeboard timeout_interval"); */

nbc_tune_noticeboard (e1,"world_write",0,&oldval,&status);
nbc_tune_noticeboard (e1,"world_write",oldval,&dumval,&status);
report (expect_ok,"nbc_tune_noticeboard world_write");
nbc_tune_noticeboard (e1,"increment_modify",0,&oldval,&status);
nbc_tune_noticeboard (e1,"increment_modify",oldval,&dumval,&status);
report (expect_ok,"nbc_tune_noticeboard increment_modify");
nbc_tune_noticeboard (e1,"check_modify",0,&oldval,&status);
nbc_tune_noticeboard (e1,"check_modify",oldval,&dumval,&status);
report (expect_ok,"nbc_tune_noticeboard check_modify");

/* Tune non-existent parameter */

nbc_tune_noticeboard (e1,"doesnt_exist",0,&oldval,&status);
nbc_tune_noticeboard (e1,"doesnt_exist",oldval,&dumval,&status);
report (expect_badoption,"nbc_tune_noticeboard of doesnt_exist");

/******************************************************************************/
/* NBC_FIND_ITEM / NTH_ITEM                                                   */
/******************************************************************************/

report (report_write,"\nNBC_FIND_ITEM\n-------------\n\n");

/* Attempt to find items in all noticeboards				    */

nbc_find_item (e1,"struc_name_1",&strucid,&status);
report (expect_ok,"nbc_find_item e1");
nbc_find_item (e2d,"struc_name_1",&strucid,&status);
report (expect_ok,"nbc_find_item e2d");
nbc_find_item (e3d,"struc_name_1",&strucid,&status);
report (expect_nilid,"nbc_find_item e3d");
nbc_find_item (e2n,"struc_name_1",&strucid,&status);
report (expect_ok,"nbc_find_item e2n");
nbc_find_item (e3n,"struc_name_1",&strucid,&status);
report (expect_ok,"nbc_find_item e3n");

nbc_find_nth_item (e1,1,&strucid,&status);
report (expect_ok,"nbc_find_nth_item e1");
nbc_find_nth_item (e2d,1,&strucid,&status);
report (expect_ok,"nbc_find_nth_item e2d");
nbc_find_nth_item (e3d,1,&strucid,&status);
report (expect_nilid,"nbc_find_nth_item e3d");
nbc_find_nth_item (e2n,1,&strucid,&status);
report (expect_ok,"nbc_find_nth_item e2n");
nbc_find_nth_item (e3n,1,&strucid,&status);
report (expect_ok,"nbc_find_nth_item e3n");

/* Ensure that primid is valid                                              */

nbc_find_nth_item (e1,1,&primid,&status);
report (expect_ok,"nbc_find_nth_item primitive e1");

/* Trigger a few errors by giving bad names and nil and primitive ids	    */

nbc_find_item (e1,"bad_name_1",&dumid,&status);
report (expect_itemnotfound,"nbc_find_item e1");
nbc_find_item (0,"bad_name_1",&dumid,&status);
report (expect_nilid,"nbc_find_item e1");
nbc_find_item (primid,"bad_name_1",&dumid,&status);
report (expect_primitive,"nbc_find_item e1");

/******************************************************************************/
/* NBC_LOSE                                                                   */
/******************************************************************************/

report (report_write,"\nNBC_LOSE\n--------\n\n");

/* Lose and find some noticeboards                                            */

nbc_lose_item (strucid,"check",&status);
report (expect_ok,"nbc_lose_item e3n");
nbc_lose_item (strucid,"check",&status);
report (expect_ok,"nbc_lose_item e3n");
nbc_lose_noticeboard (e3n,"check",&status);
report (expect_ok,"nbc_lose_noticeboard e3n");
nbc_restore_noticeboard ("exercise_3n","exercise_3",&status);
report (expect_ok,"nbc_restore_noticeboard e3n");
nbc_find_noticeboard ("exercise_3n",&e3n,&status);
report (expect_ok,"nbc_find_noticeboard e3n");
nbc_find_nth_item (e3n,1,&strucid,&status);
report (expect_ok,"nbc_find_nth_item e3n");

/* Trigger some errors                                                        */

nbc_lose_item (0,"check",&status);
report (expect_nilid,"nbc_lose_item");
nbc_lose_noticeboard (0,"check",&status);
report (expect_nilid,"nbc_lose_noticeboard");
nbc_lose_noticeboard (e3n,"check",&status);
report (expect_hasids,"nbc_lose_noticeboard e3n");
nbc_lose_item (e3n,"check",&status);
report (expect_toplevel,"nbc_lose_item e3n");
nbc_lose_noticeboard (strucid,"check",&status);
report (expect_nottoplevel,"nbc_lose_noticeboard strucid");
nbc_lose_item (strucid,"check",&status);
report (expect_ok,"nbc_lose_item e3n");
nbc_lose_item (strucid,"check",&status);
report (expect_neverfound,"nbc_lose_item e3n");

/******************************************************************************/
/* NBC_PUT                                                                    */
/******************************************************************************/

report (report_write,"\nNBC_PUT\n-------\n\n");

/* Get primitive and structure ids from noticeboard example_1               */

nbc_find_nth_item (e1,1,&primid,&status);
nbc_find_nth_item (e1,2,&strucid,&status);
report (expect_ok,"nbc_find_nth_item for put");

/* Put values into a primitive item. Use first primitive of noticeboard     */
/* example_1, since it is 256 bytes long.                                   */

nbc_put_value (primid,0,256,b256,&status);
report (expect_ok,"nbc_put_value");

nbc_put_cvalue (primid,0,"test data",&status);
report (expect_ok,"nbc_put_cvalue");

/* Put shape as well.							    */

dims1[0] = 256;
nbc_put_shape (primid,1,dims1,&status);
report (expect_ok,"nbc_put_shape");

/* Put size, clearing it back to zero, then put a slice starting at a	    */
/* non-zero offset							    */

nbc_put_size (primid,0,&status);
report (expect_ok,"nbc_put_size");
nbc_put_value (primid,128,128,b256,&status);
report (expect_ok,"nbc_put_value slice");

/* Increment the board modified count and the item modified count twice.    */

nbc_inc_modified (strucid,&status);
nbc_inc_modified (primid,&status);
nbc_inc_modified (primid,&status);
report (expect_ok,"nbc_inc_modified");

/* Put a trigger routine                                                    */

nbc_put_trigger (primid,trigger,&status);
report (expect_ok,"nbc_put_trigger");

/* Trigger a few errors							    */

nbc_put_value (0,0,0,b256,&status);
report (expect_nilid,"nbc_put_value");
nbc_put_value (strucid,0,0,b256,&status);
report (expect_notprimitive,"nbc_put_value");
nbc_put_value (primid,-1,0,b256,&status);
report (expect_badoffset,"nbc_put_value");
nbc_put_value (primid,128,256,b256,&status);
report (expect_toomanybytes,"nbc_put_value");

nbc_put_cvalue (0,0,"test data",&status);
report (expect_nilid,"nbc_put_cvalue");
nbc_put_cvalue (strucid,0,"test data",&status);
report (expect_notprimitive,"nbc_put_cvalue");
nbc_put_cvalue (primid,-1,"test data",&status);
report (expect_badoffset,"nbc_put_cvalue");
nbc_put_cvalue (primid,250,"test data",&status);
report (expect_toomanybytes,"nbc_put_cvalue");

nbc_put_shape (0,0,dims2,&status);
report (expect_nilid,"nbc_put_shape");
nbc_put_shape (strucid,0,dims2,&status);
report (expect_notprimitive,"nbc_put_shape");
nbc_put_shape (primid,8,dims2,&status);
report (expect_toomanydims,"nbc_put_shape");

nbc_put_size (0,0,&status);
report (expect_nilid,"nbc_put_size");
nbc_put_size (strucid,0,&status);
report (expect_notprimitive,"nbc_put_size");
nbc_put_size (primid,512,&status);
report (expect_toomanybytes,"nbc_put_size");

nbc_inc_modified (0,&status);
report (expect_nilid,"nbc_inc_modified");

nbc_put_trigger (0,trigger,&status);
report (expect_nilid,"nbc_put_trigger");
nbc_put_trigger (strucid,trigger,&status);
report (expect_notprimitive,"nbc_put_trigger");

/******************************************************************************/
/* NBC_SAVE_NOTICEBOARD                                                       */
/******************************************************************************/

report (report_write,"\nNBC_SAVE_NOTICEBOARD\n--------------------\n\n");

/* Attempt to save all noticeboards                                         */

nbc_save_noticeboard (e1,&status);
report (expect_datanotrestored,"nbc_save_noticeboard e1");
nbc_save_noticeboard (e2d,&status);
report (expect_datanotrestored,"nbc_save_noticeboard e2d");
nbc_save_noticeboard (e3d,&status);
report (expect_nilid,"nbc_save_noticeboard e3d");
nbc_save_noticeboard (e2n,&status);
report (expect_datanotrestored,"nbc_save_noticeboard e2n");
nbc_save_noticeboard (e3n,&status);
report (expect_ok,"nbc_save_noticeboard e3n");

/******************************************************************************/
/* NBC_GET                                                                    */
/******************************************************************************/

report (report_write,"\nNBC_GET\n-------\n\n");

/* Get value for same primitive item that was just put			    */

nbc_get_value (primid,0,256,d256,&i,&status);
report (expect_ok,"nbc_get_value");

/* Get its shape							    */

maxdims = 2;
nbc_get_shape (primid,&maxdims,i256,&i,&status);
report (expect_ok,"nbc_get_shape");

/* Call all other get routines						    */

nbc_get_modified (primid,&i,&status);
report (expect_ok,"nbc_get_modified");
nbc_get_modified_pointer (primid,&i,&status);
report (expect_ok,"nbc_get_modified_pointer");
nbc_get_updated (primid,&i,&status);
report (expect_ok,"nbc_get_updated");
nbc_get_pointer (primid,&i,&status);
report (expect_ok,"nbc_get_pointer");
nbc_get_name (primid,d256,&status);
report (expect_ok,"nbc_get_name");
nbc_get_type (primid,d256,&status);
report (expect_ok,"nbc_get_type");
nbc_get_size (primid,&i,&i,&status);
report (expect_ok,"nbc_get_size");
nbc_get_primitive (primid,&i,&status);
report (expect_ok,"nbc_get_primitive");
nbc_get_parent (primid,&i,&status);
report (expect_ok,"nbc_get_parent");
nbc_get_children (strucid,&i,&status);
report (expect_ok,"nbc_get_children");
nbc_get_info (primid,"version",&i,&status);
report (expect_ok,"nbc_get_info");
nbc_get_cinfo (primid,"save_name",d256,&status);
report (expect_ok,"nbc_get_cinfo");

/* Trigger a few errors							    */

nbc_get_value (0,0,0,d256,&i,&status);
report (expect_nilid,"nbc_get_value");
nbc_get_value (strucid,0,0,d256,&i,&status);
report (expect_notprimitive,"nbc_get_value");
nbc_get_value (primid,-1,0,d256,&i,&status);
report (expect_badoffset,"nbc_get_value");
nbc_inc_modified (primid,&status);
nbc_get_value (primid,0,256,d256,&i,&status);
report (expect_timeout,"nbc_get_value");

nbc_get_shape (0,&maxdims,i256,&i,&status);
report (expect_nilid,"nbc_get_shape");
nbc_get_shape (strucid,&maxdims,i256,&i,&status);
report (expect_notprimitive,"nbc_get_shape");
nbc_get_shape (primid,&maxdims,i256,&i,&status);
report (expect_timeout,"nbc_get_shape");
nbc_inc_modified (primid,&status);
report (expect_ok,"nbc_inc_modified after timeout");

nbc_get_modified (0,&i,&status);
report (expect_nilid,"nbc_get_modified");
nbc_get_modified_pointer (0,&i,&status);
report (expect_nilid,"nbc_get_modified_pointer");

nbc_get_pointer (0,&i,&status);
report (expect_nilid,"nbc_get_pointer");
nbc_get_pointer (strucid,&i,&status);
report (expect_notprimitive,"nbc_get_pointer");

nbc_get_name (0,d256,&status);
report (expect_nilid,"nbc_get_name");

nbc_get_type (0,d256,&status);
report (expect_nilid,"nbc_get_type");

nbc_get_size (0,&i,&i,&status);
report (expect_nilid,"nbc_get_size");
nbc_get_size (strucid,&i,&i,&status);
report (expect_notprimitive,"nbc_get_size");

nbc_get_primitive (0,&i,&status);
report (expect_nilid,"nbc_get_primitive");

nbc_get_parent (0,&i,&status);
report (expect_nilid,"nbc_get_parent");

nbc_get_children (0,&i,&status);
report (expect_nilid,"nbc_get_children");
nbc_get_children (primid,&i,&status);
report (expect_primitive,"nbc_get_children");

nbc_get_info (0,"doesnt_exist",&i,&status);
report (expect_nilid,"nbc_get_info");
nbc_get_info (primid,"doesnt_exist",&i,&status);
report (expect_badoption,"nbc_get_info");
nbc_get_cinfo (0,"doesnt_exist",&i,&status);
report (expect_nilid,"nbc_get_cinfo");
nbc_get_cinfo (primid,"doesnt_exist",&i,&status);
report (expect_badoption,"nbc_get_cinfo");

/******************************************************************************/
/* END                                                                        */
/******************************************************************************/

nbc_lose_noticeboard (e1,"FORCE",&status);
report (expect_ok,"nbc_lose_noticeboard e1");
nbc_lose_noticeboard (e2d,"FORCE",&status);
report (expect_ok,"nbc_lose_noticeboard e2d");
nbc_lose_noticeboard (e2n,"FORCE",&status);
report (expect_ok,"nbc_lose_noticeboard e2n");
nbc_lose_noticeboard (e3n,"FORCE",&status);
report (expect_ok,"nbc_lose_noticeboard e3n");

/* Close down the reporting system */

 report (report_close, NULL);

 return EXIT_SUCCESS;
}

/******************************************************************************/
/******************************************************************************/
/* REPORT                                                                     */
/******************************************************************************/
/******************************************************************************/

void
report (function,message)
enum REPORT_FUNCTION function;
char *message;
{
static FILE *file;
int error,output;

/* Handle special functions first */

if (function == report_open)
   {
   if (strcmp (message,"") != 0)
      {
      file = freopen (message,"w",stdout);
      if (!file)
         file = stdout;	/* Questionable?				    */
      }
   else
      file = stdout;
   error_count = 0;
   return;
   }
else if (function == report_write)
   {
   fprintf (file,message);
#ifndef vms
fflush(file);
#endif
   return;
   }
else if (function == report_close)
   {
   fprintf (file,"\nError count = %d\n",error_count);
   fprintf (file,"----------------\n");
   fclose (file);
   return;
   }

/* Now handle "expect" functions, which are all the expected values of status -
   increment error count? */

error = (status != (int)function);
if (error)
   error_count++;

/* Output? */

output = (report_level == report_all) || error;
 if (output) {
   if (error)
      fprintf (file,"*%s: expected %s but got %s\n",message,errtext(function),
							     errtext(status));
   else
      fprintf (file," %s: %s\n",message,errtext(status));
 }
#ifndef vms
fflush(file);
#endif

/* Reset status */

if ( status > (int) expect_ok )
  emsAnnul( &status );
}

/******************************************************************************/
/******************************************************************************/
/* ERRTEXT                                                                    */
/******************************************************************************/
/******************************************************************************/

char *errtext (status)
int status;
{
static int bufptr = 0;
static char buffer[2][257];
bufptr ^= 1;
if (status == SAI__OK)
   strcpy (buffer[bufptr],"ok");
else if (status == NBS__SECTIONEXISTED)
   strcpy (buffer[bufptr],"noticeboard already existed");
else if (status == NBS__TOOMANYDIMS)
   strcpy (buffer[bufptr],"more dimensions than maximum allowed");
else if (status == NBS__TOOMANYBYTES)
   strcpy (buffer[bufptr],"more bytes than maximum allowed");
else if (status == NBS__BADOFFSET)
   strcpy (buffer[bufptr],"offset is less than zero");
else if (status == NBS__BADOPTION)
   strcpy (buffer[bufptr],"illegal parameter / item name");
else if (status == NBS__DATANOTSAVED)
   strcpy (buffer[bufptr],"data part of noticeboard not saved - cannot restore it");
else if (status == NBS__DEFINING)
   strcpy (buffer[bufptr],"currently defining noticeboard contents");
else if (status == NBS__NOTDEFINING)
   strcpy (buffer[bufptr],"not currently defining noticeboard contents");
else if (status == NBS__NILSID	)
   strcpy (buffer[bufptr],"NIL static ID");
else if (status == NBS__NILID	)
   strcpy (buffer[bufptr],"NIL item ID");
else if (status == NBS__PRIMITIVE)
   strcpy (buffer[bufptr],"item is primitive");
else if (status == NBS__NOTPRIMITIVE)
   strcpy (buffer[bufptr],"item is not primitive");
else if (status == NBS__ITEMNOTFOUND)
   strcpy (buffer[bufptr],"item does not exist");
else if (status == NBS__SECTIONNOTFOUND)
   strcpy (buffer[bufptr],"noticeboard does not exist");
else if (status == NBS__CANTOPEN)
   strcpy (buffer[bufptr],"can't open noticeboard definition file");
else if (status == NBS__CANTWRITE)
   strcpy (buffer[bufptr],"can't write noticeboard definition file");
else if (status == NBS__CANTREAD)
   strcpy (buffer[bufptr],"can't read noticeboard definition file");
else if (status == NBS__NOTOWNER)
   strcpy (buffer[bufptr],"non-owner attempted to alter noticeboard");
else if (status == NBS__TIMEOUT	)
   strcpy (buffer[bufptr],"time out getting item value");
else if (status == NBS__DATASAVED)
   strcpy (buffer[bufptr],"data part of noticeboard saved - cannot restore definition");
else if (status == NBS__DATANOTRESTORED)
   strcpy (buffer[bufptr],"data was not restored from noticeboard file - cannot save it");
else if (status == NBS__HASIDS)
   strcpy (buffer[bufptr],"item / noticeboard has items derived from it - cannot lose it");
else if (status == NBS__NOTTOPLEVEL)
   strcpy (buffer[bufptr],"item is not top-level (ie not noticeboard) - cannot lose it");
else if (status == NBS__TOPLEVEL)
   strcpy (buffer[bufptr],"item is top-level (ie noticeboard) - cannot lose it");
else if (status == NBS__NEVERFOUND)
   strcpy (buffer[bufptr],"parent has no items derived from it - cannot lose it");
else if (status == NBS__INITALLOCFAILED)
   strcpy (buffer[bufptr],"couldn't initialise storage allocator");
else if (status == NBS__NOMOREROOM)
   strcpy (buffer[bufptr],"couldn't get memory - increase MAX_DEFN_SIZE if when defining");
else if (status == NBS__BADVERSION)
   strcpy (buffer[bufptr],"noticeboard or definition file had wrong version");
else if (status == NBS__IMPOSSIBLE)
   strcpy (buffer[bufptr],"something impossible happened - system error");
else
   sprintf (buffer[bufptr],"Unrecognised status %d (%x)",status,status);
return buffer[bufptr];
}

/******************************************************************************/
/******************************************************************************/
/* TRIGGER                                                                    */
/******************************************************************************/
/******************************************************************************/

int trigger (id,status)
int id;
int *status;
{
  return 0;
}
