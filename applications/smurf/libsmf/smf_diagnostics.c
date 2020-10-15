/*
*+
*  Name:
*     smf_diagnostics

*  Purpose:
*     Dump diagnostics to disk.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_diagnostics( ThrWorkForce *wf, int where, smfDIMMData *dat,
*		       dim_t chunk, AstKeyMap *keymap, smfArray **allmodel,
*                      smf_modeltype type, int flags, double chunkfactor,
*                      int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     where = int (Given)
*        Indicates where in the algorithm this function is being called,
*        which is used to determine what information to dump to disk.
*        Current options are:
*
*        -1: This function is being called immediately after cleaning the
*        data and before the start of the first iteration.
*
*        0: This function is being called immediately before estimating
*        a new model and subtracting it from the residuals.
*
*        1: This function is being called immediately after estimating
*        a new model and subtracting it from the residuals.
*
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = dim_t (Given)
*        Index of the contiguous chunk of time-series data being processed.
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker. It should
*        contain an entry with key "DIAG" that is a KeyMap with the
*        following entries:
*
*        OUT - The full path/name for the HDS container file in which to
*        store the diagnostic info. This will contain components for each
*        requested model, with names like "COM", "FLT", etc. Each of these
*        components will contain multiple NDFs with names in the following
*        format: "<where>_<chunk>_<what>", where <what> is "power" or "time",
*        <chunk> is the integer chunk index, and <where> is one of:
*           - "bef": the NDF contains the residuals as they were before
*              the model was subtracted.
*           - "mod": the NDF contains the model values themselves.
*           - "aft": the NDF contains the residuals as they were after
*              the model was subtracted.
*        Each NDF will be 2-dimensional, with the first pixel axis
*        representing time or frequency, and the second pixel axis
*        representing iteration number.
*
*        APPEND - If non-zero, it indicates that diagnostic info should be
*        appended to the container file specified by diag.out, which should
*        already exist. If zero, then  any existing container file is first
*        deleted before storing new diagnostics information in it.
*
*        CLEANED - If non-zero, the cleaned data is dumped at the start
*        of the first iteration. Note initial cleaned data is still dumped
*        if CLEANED is non-zero, even if LASTONLY is non-zero.
*
*        CUBE - If non-zero, a full cube containing time-ordered data for
*        all bolometers is created at each iteration, for each required
*        model and set of residuals. These are placed in 3D NDFs with
*        names in the following format: "<where>_<chunk>_cube_<it>", where
*        <chunk> and <where> are described above (under "OUT") and <it>
*        is the iteration number.
*
*        MAP - If non-zero, a 2D map containing the binned time-stream data
*        for all bolometers is created at each iteration, for each required
*        model and set of residuals. These are placed in 2D NDFs with
*        names in the following format: "<where>_<chunk>_map_<it>", where
*        <chunk> and <where> are described above (under "OUT") and <it>
*        is the iteration number. If the MAP value is positive, the map will
*        contain data for just the subarray specified by "ARRAY".If MAP is
*        negative, the map will contain data for all available subarrays.
*
*        MODELS - Indicates the models that are to be written out. It
*        should be a comma separated list of model names (e.g. COM, FLT,
*        AST, RES, etc) contained within parentheses, or a single model
*        name. A model name of RES here refers to the residuals after
*        subtraction of all models in use (typically COM, FLT and AST).
*        The residuals can also be written out at other times - see
*        RES_BEFORE and RES_AFTER.
*
*        POWER - If non-zero, write out the power spectrum for each
*        selected model.
*
*        TIME - If non-zero, write out the time-series for each selected
*        model.
*
*        MINGOOD - The minimum fraction of good values in a time stream
*        for which data should be dumped. An error is reported if the
*        required minimum value is not met.
*
*        BOLO - Indicates the bolometer for which diagnostic information
*        is required (the sub-array is indicated by ARRAY). It can be:
*
*           - A pair of integers, separated by a comma, contained in
*           parentheses, giving the column and row of the bolometer. The
*           first integer should be in the range 1 to 32, and the second
*           should be in the range 1 to 40.
*
*           - A single integer in the range 1 to 1280.
*
*           - The string "MEAN" (case insensitive), in which case all
*           unflagged data from all good bolometers is averaged to form
*           the time-stream to dump.
*
*           - The string "WMEAN" (case insensitive), in which case all
*           unflagged data from all good bolometers is averaged using
*           weights derived from the bolometer noise estimates to form
*           the time-stream to dump.
*
*           - The string "TYPICAL" (case insensitive), in which case a
*           bolometer with typical noise charactersitics is chosen and
*           used. The index of the chosen bolometer is reported, and
*           stored in the dumped NDFs.
*
*        ARRAY - The name of the array (S8A, S*b, etc) containing the
*        data to be written out. If not supplied, the first available
*        array is used.
*
*        MASK - If non-zero, then the AST model will be masked using the
*        current AST mask before being dumped. Otherwise, the AST model
*        will not be masked before being dumped.
*
*        RES_BEFORE - If non-zero, then in addition to writing out the
*        requested models, the residuals are also written out immediately
*        before each requested model is subtracted.
*
*        RES_AFTER - If non-zero, then in addition to writing out the
*        requested models, the residuals are also written out immediately
*        after each requested model is subtracted.
*
*        LASTONLY - If non-zero, then diagnostics are only created for
*        the last iteration. If zero, then diagnostics are created for
*        all iterations. Note initial cleaned data is still dumped if
*        CLEANED is non-zero, even if LASTONLY is non-zero.
*
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays holding the model. Only element zero is used.
*        Should be NULL if the AST model is being dumped or the initial
         cleaned data is being dumped.
*     type = smf_modeltype (Given)
*        Indicates which model is to be dumped.
*     flags = int (Given)
*        Control flags.
*     chunkfactor = double (Given)
*        The scale factor for the current chunk.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Dumps diagnostic information for a single (real or mean) bolometer.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     25-JAN-2013 (DSB):
*        Original version.
*     17-JUL-2014 (DSB):
*        Added option to create diagnostic maps from each model.
*     21-JAN-2015 (DSB):
*        Make the inclusion of data for a specific bolometer optional.
*     10-APR-2018 (DSB):
*        Added parameter "chunkfactor".
*     18-MAR-2021 (DSB):
*        Added parameter "btable".
*     27-MAY-2021 (DSB):
*        Allow initial cleaned data to be dumped.

*  Copyright:
*     Copyright (C) 2018-2021 East Asian Observatory.
*     Copyright (C) 2013-2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/thr.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define SUB_NAMELEN 8
#define MODEL_NAMELEN 4

void smf_diagnostics( ThrWorkForce *wf, int where, smfDIMMData *dat,
                      dim_t chunk, AstKeyMap *keymap, smfArray **allmodel,
                      smf_modeltype type, int flags, double chunkfactor,
                      int *status ){

/* Local Variables: */
   AstKeyMap *kmap;
   AstObject *obj;
   HDSLoc *cloc = NULL;
   HDSLoc *dloc = NULL;
   HDSLoc *mloc = NULL;
   char *btable = NULL;
   char broot[ 200 ];
   char modelnames[ SMF_MODEL_MAX*MODEL_NAMELEN ];
   char root[ 20 ];
   char subarray[ SUB_NAMELEN + 1 ];
   const char *cval;
   const char *modname;
   const char *out;
   const char *table = NULL;
   dim_t ibolo = -4;
   dim_t isub;
   dim_t nsub;
   double mingood;
   int addqual;
   int append;
   int cleaned;
   int cube;
   int history;
   int hits;
   int imodel;
   int irow;
   int ivals[ 2 ];
   int lastonly;
   int map;
   int mask;
   int new;
   int nmodel;
   int nval;
   int power;
   int repbolo;
   int res_after;
   int res_before;
   int rowoffset = 0;
   int there;
   int time;
   smfArray *res;
   smf_modeltype model;
   static smfSampleTable *tabdata = NULL;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Start an AST context to record details of AST Objects created in
   this function. */
   astBegin;

/* Get a pointer to the KeyMap holding parameters controlling the
   diagnostics to be dumped. */
   astMapGet0A( keymap, "DIAG", &obj );
   kmap = (AstKeyMap *) obj;

/* Get pointer to the smfArray containing the current residuals, and the
   number of available subarrays. */
   res = dat->res[0];
   nsub = res->ndat;

/* See if diagnostics are to be created only for the final iteration. */
   astMapGet0I( kmap, "LASTONLY", &lastonly );

/* See if initial cleaned data is to be dumped. */
   astMapGet0I( kmap, "CLEANED", &cleaned );

/* If we are dumping the initial cleaned data, then ignore "lastonly" for
   this call. */
   if( cleaned && where == -1 ) lastonly = 0;

/* See if a table of bolometer values is to be created. */
   astMapGet0C( kmap, "BTABLE", &table );
   if( table ) btable = astStore( NULL, table, strlen(table) + 1 );

/* Get the name of any output HDS container file to create. */
   if( !astMapGet0C( kmap, "OUT", &out ) ) out = NULL;

/* Skip to the end if no dignostics are required, or if diagnostics are
   not needed for this iteration. */
   if( ( out || btable ) && ( !lastonly || (flags & SMF__DIMM_LASTITER) ) ) {

/* See if we should append data for the current run of makemap to NDFs
   created by a previous run (e.g. when running the skyloop script). */
      astMapGet0I( kmap, "APPEND", &append );

/* See if a full cube containing data for all bolometers is required at
   each iteration. */
      astMapGet0I( kmap, "CUBE", &cube );

/* See if a 2D map of each model is required at each iteration. */
      astMapGet0I( kmap, "MAP", &map );

/* See if the AST model is to be dumped as a set of pixel data values or as a
   set of hits values. */
      if( type == SMF__AST ) {
         astMapGet0I( kmap, "ASTHITS", &hits );
      } else {
         hits = 0;
      }

/* See if NDFs are to include Quality arrays. */
      astMapGet0I( kmap, "QUAL", &addqual );

/* See if an ascii table of values falling in a specified map pixel is
   to be produced. If so, create a structure to hold information about
   the table. */
      if( ! tabdata && dat->lut ) {
         astMapGet0C( kmap, "TABLE", &table );
         if( table ) {
            tabdata = astCalloc( 1, sizeof( *tabdata ) );
            tabdata->table = astStore( NULL, table, strlen( table ) + 1 );
            astMapGet0I( kmap, "XPIX", &(tabdata->xpix) );
            astMapGet0I( kmap, "YPIX", &(tabdata->ypix) );
         }
      }

/* If we are appending to previously created NDFs, attempt to open the
   pre-existing container file. */
      new = 0;
      if( out ) {
         if( append ) {
            if( *status == SAI__OK ) {
               hdsOpen( out, "UPDATE", &cloc, status );
               if( *status != SAI__OK ) {
                  errRepf( "", "Failed to append new makemap diagnostic "
                           "info to existing container file \"%s\".", status,
                           out );
               }

/* Get the starting row number from the KeyMap. If it is not in the
   KeyMap, get it form the HDS file and copy it into the KeyMap. */
               if( astMapHasKey( kmap, "ROWOFFSET" ) ) {
                  astMapGet0I( kmap, "ROWOFFSET", &rowoffset );
               } else {
                  datFind( cloc, "NROW", &dloc, status );
                  datGet0I( dloc, &rowoffset, status );
                  datAnnul( &dloc, status );
                  astSetI( kmap, "MapLocked", 0 );
                  astMapPut0I( kmap, "ROWOFFSET", rowoffset, NULL );
                  astSetI( kmap, "MapLocked", 1 );
               }
            }

/* Otherwise, create a new container file, and use zero as the row
   offset. Also indicate that subsequent iteration should append. */
         } else if( *status == SAI__OK ) {

            rowoffset = 0;
            astSetI( kmap, "MapLocked", 0 );
            astMapPut0I( kmap, "ROWOFFSET", rowoffset, NULL );
            astMapPut0I( kmap, "APPEND", 1, NULL );
            astSetI( kmap, "MapLocked", 1 );

            new = 1;
            hdsNew( out, "DIAGNOSTICS", "DIAGNOSTICS", 0, NULL, &cloc,
                    status );
            if( *status != SAI__OK ) {
               errRepf( "", "Failed to write new makemap diagnostic "
                        "info to new container file \"%s\".", status,
                        out );
            }
         }

/* Get the index of the row to add to the container file. */
         irow = dat->iter + rowoffset;
      } else {
         irow = 0;
      }

/* Get the  other required items from the KeyMap. */
      astMapGet1C( kmap, "MODELS", MODEL_NAMELEN, SMF_MODEL_MAX, &nmodel,
                   modelnames );
      astMapGet0I( kmap, "POWER", &power );
      astMapGet0I( kmap, "TIME", &time );
      astMapGet0I( kmap, "RES_BEFORE", &res_before );
      astMapGet0I( kmap, "RES_AFTER", &res_after );
      astMapGet0I( kmap, "MASK", &mask );
      astMapGet0D( kmap, "MINGOOD", &mingood );

      if( *status == SAI__OK ) {
         astMapGet1I( kmap, "BOLO", 2, &nval, ivals );
         if( *status == SAI__OK ) {
            if( nval == 2 ) {
               if( ivals[ 0 ] < 0 || ivals[ 0 ] >= 32 ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Illegal value %d for column number in "
                           "config parameter DIAG.BOLO - must be in "
                           "the range 0 to 31.", status, ivals[ 0 ] );
               } else if( ivals[ 1 ] < 0 || ivals[ 1 ] >= 40 ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Illegal value %d for row number in "
                           "config parameter DIAG.BOLO - must be in "
                           "the range 0 to 39.", status, ivals[ 1 ] );
               } else {
                  ibolo = ivals[ 1 ]*32 + ivals[ 0 ];
               }
            } else {
               if( ivals[ 0 ] < 0 || ivals[ 0 ] >= 1280 ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Illegal value %d for bolometer index in "
                           "config parameter DIAG.BOLO - must be in "
                           "the range 0 to 1279.", status, ivals[ 0 ] );
               } else {
                  ibolo = ivals[ 0 ];
               }
            }

/* If the supplied strings could not be converted to integers, annul the error
   and interpret them as simple strings. */
         } else if( nval == 1 && *status == AST__MPGER ){
            errAnnul( status );

/* Only one string allowed. */
            if( astMapLength( kmap, "BOLO" ) == 1 ) {

/* Get the string, and compare to the allowed values. */
               astMapGet0C( kmap, "BOLO", &cval );
               if( astChrMatch( "MEAN", cval ) ) {
                  ibolo = -1;
               } else if( astChrMatch( "WMEAN", cval ) ) {
                  ibolo = -2;
               } else if( astChrMatch( "TYPICAL", cval ) ) {
                  ibolo = -3;
               } else if( astChrMatch( "NONE", cval ) ) {
                  ibolo = -4;
               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Illegal value %s supplied for config "
                           "parameter DIAG.BOLO.", status, cval );
               }
            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( "", "Illegal value supplied for config "
                        "parameter DIAG.BOLO.", status );
            }
         }
      }

/* Get the index of the required sub-array within the supplied smfArrays. */
      if( astMapGet0C( kmap, "ARRAY", &cval ) ){
         for( isub = 0; isub < nsub; isub++ ) {
            smf_find_subarray( res->sdata[isub]->hdr, subarray,
                               sizeof(subarray), NULL, status );
            if( astChrMatch( subarray, cval ) ) break;
         }

         if( isub == nsub && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "Bad value \"%s\" supplied for config parameter "
                     "DIAG.ARRAY - no data found for array %s.", status,
                     cval, cval );
         }

      } else {
         isub = 0;
         smf_find_subarray( res->sdata[isub]->hdr, subarray,
                            sizeof(subarray), NULL, status );
      }

/* No need for history in the NDFs so switch it off. */
      if( out ) {
         ndfGtune( "AUTO_HISTORY", &history, status );
         ndfTune( 0, "AUTO_HISTORY", status );
      }

/* We report the used bolometer if the initial bolometer setting
   indicates that a typical bolometer is to be chosen and used
   automatically. */
      repbolo = ( ibolo == -3 );

/* If this function has been called immediately before the start of the
   first iteration, then dump the inital cleaned data if requested. */
      if( where == -1 ) {
         if( cleaned ) {
            msgOutf( "", "Diagnostics: Dumping cleaned data at start of first "
                     "iteration.", status );

            if( out ) {
               datThere( cloc, "CLN", &there, status );
               if( !there ) datNew( cloc, "CLN", "DIAGNOSTICS", 0, NULL,
                                    status );
               datFind( cloc, "CLN", &mloc, status );
            }

            if( btable ){
               sprintf( broot, "%s_%d_cln.asc", btable, (int) chunk );
            }

            sprintf( root, "cln_%d", (int) chunk );
            smf_diag( wf, mloc, &ibolo, irow, power, time, isub,
                      dat, SMF__RES, NULL, 1, root, 0, mingood, cube,
                      map, addqual, tabdata, chunkfactor,
                      btable?broot:NULL, keymap, status );
         }

/* Otherwise. */
      } else {

/* See if the current model (indicated by argument "type") is one of the
   ones that are to be written out. */
         for( imodel = 0; imodel < nmodel && *status == SAI__OK; imodel++ ) {
            model = smf_model_gettype( modelnames + imodel*MODEL_NAMELEN, status );
            if( *status == SAI__OK && type == model ) {

/* Ensure we have a component for this model within the container file. */
               modname = smf_model_getname( type, status );
               if( out ) {
                  datThere( cloc, modname, &there, status );
                  if( !there ) datNew( cloc, modname, "DIAGNOSTICS", 0, NULL,
                                       status );
                  datFind( cloc, modname, &mloc, status );
               }

/* If this function has been called immediately before estimating the new
   model, then dump the residuals if requested. */
               if( where == 0 ) {
                  if( res_before && type != SMF__RES ) {
                     msgOutf( "", "Diagnostics: Dumping residuals before subtraction of %s",
                              status, modname );

                     if( btable ){
                        sprintf( broot, "%s_%d_%d_%s_bef.asc", btable, (int) chunk,
                                 dat->iter, modname );
                     }

                     sprintf( root, "bef_%d", (int) chunk );
                     smf_diag( wf, mloc, &ibolo, irow, power, time, isub,
                               dat, type, NULL, 1, root, 0, mingood, cube,
                               map, addqual, tabdata, chunkfactor,
                               btable?broot:NULL, keymap, status );
                  }

/* If this function has been called immediately after estimating the new
   model, then dump the model and also dump the residuals if requested. */
               } else if( where == 1 ) {
                  msgOutf( "", "Diagnostics: Dumping %s model", status, modname );
                  sprintf( root, "mod_%d", (int) chunk );

                  if( btable ){
                     sprintf( broot, "%s_%d_%d_%s_mod.asc", btable, chunk,
                              dat->iter, modname );
                  }
                  smf_diag( wf, mloc, &ibolo, irow, power, time, isub,
                            dat, type, allmodel ? allmodel[ 0 ] : NULL,
                            hits?-999:0, root, mask, mingood, cube, map, addqual,
                            tabdata, chunkfactor, btable?broot:NULL, keymap, status );
                  if( res_after && type != SMF__RES ) {
                     msgOutf( "", "Diagnostics: Dumping residuals after subtraction of %s",
                              status, modname );
                     sprintf( root, "aft_%d", (int) chunk );

                     if( btable ){
                        sprintf( broot, "%s_%d_%d_%s_aft.asc", btable, chunk,
                                 dat->iter, modname );
                     }

                     smf_diag( wf, mloc, &ibolo, irow, power, time, isub,
                               dat, type, NULL, 1, root, 0, mingood, cube,
                               map, addqual, tabdata, chunkfactor,
                               btable?broot:NULL, keymap, status );
                  }

/* Any other "where" value is currently an error. */
               } else if( *status == SAI__OK ){
                  *status = SAI__ERROR;
                  errRepf( "", "smf_diagnostics: Illegal value %d for "
                           "argument \"where\".", status, where );
               }

/* Annul ther locator for the model info in the container file. */
               if( out ) datAnnul( &mloc, status );

/* Report the chosen typical bolometer if required, and store it in the
   KeyMap in place of the original BOLO value, in order to ensure that the
   same bolometer is chosen in future. */
            if( repbolo && ibolo >= 0 ) {
               astMapPut0K( kmap, "BOLO", ibolo, NULL );
               msgOutf( "", "Diagnostics: using \"typical\" bolometer %d.",
                        status, (int) ibolo );
               repbolo = 0;
            }
         }
      }

/* Add extra information to the top level of the container file, if it
   has just been created. */
      if( out ) {
         if( new ) {
            if( ibolo != -4 ) {
               if( ibolo < 0 ) {
                  cval = ( ibolo == -1 ) ? "MEAN" : "WMEAN";
                  datNew0C( cloc, "BOLO", strlen(cval), status );
                  datFind( cloc, "BOLO", &dloc, status );
                  datPut0C( dloc, cval, status );
               } else {
                  datNew0I( cloc, "BOLO", status );
                  datFind( cloc, "BOLO", &dloc, status );
                  datPut0K( dloc, ibolo, status );
               }
               datAnnul( &dloc, status );
            }

            datNew0C( cloc, "ARRAY", strlen(subarray), status );
            datFind( cloc, "ARRAY", &dloc, status );
            datPut0C( dloc, subarray, status );
            datAnnul( &dloc, status );

            datNew0I( cloc, "MASK", status );
            datFind( cloc, "MASK", &dloc, status );
            datPut0I( dloc, mask, status );
            datAnnul( &dloc, status );

            datNew0I( cloc, "NROW", status );
         }

/* Store the number of rows of diagnostics currently stored in the container file. */
         datFind( cloc, "NROW", &dloc, status );
         datPut0I( dloc, irow + 1, status );
         datAnnul( &dloc, status );

/* Re-instate the original NDF history tuning parameter. */
         ndfTune( history, "AUTO_HISTORY", status );

/* Close the container file. */
         datAnnul( &cloc, status );
      }
   }

/* Free resources. */
   btable = astFree( btable );

/* End the AST context, thus deleting any AST objects created in this
   function. */
   astEnd;
}
