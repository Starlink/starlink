/*
 *+
 * Name:
 *    img.h
 *
 * Purpose:
 *    Header file for defining IMG function prototypes and BAD data values.
 *
 * Language:
 *    ANSI C
 *
*  Copyright:
*     Copyright (C) 1996, 2000, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 * Authors:
 *    FCHEAD:  Automated header generator routine (STARLINK)
 *    PDRAPER: P.W. Draper (STARLINK - Durham University)
 *    {enter_new_authors_here}
 *
 * History:
 *    17-MAY-1996 (FCHEAD):
 *       Original version.
 *    17-MAY-1996 (PDRAPER):
 *       Added prologue and updated to final modules.
 *    10-JUN-1996 (PDRAPER):
 *       Added BAD data values.
 *    24-OCT-2000 (PDRAPER):
 *       Added hdrCopy prototype.
 *    08-JUN-2004 (PDRAPER):
 *       Changed BAD values to use  PRIMDAT values.
 *    {enter_further_changes_here}
 *
 *-
 */

/*  Define BAD values. */

#include <float.h>
#include <limits.h>
#include <prm_par.h>

/* PRM defines:
   VAL__BADUB
   VAL__BADB
   VAL__BADUW
   VAL__BADW
   VAL__BADI
   VAL__BADR
   VAL__BADD
*/

/*  IMG historical C-like names, preserve for backwards compatibility */
#define VAL__BADF    VAL__BADR
#define VAL__BADS    VAL__BADW
#define VAL__BADUS   VAL__BADUW


/*  Function prototypes. */

void hdrDelet( char *param,
               char *xname,
               char *item,
               int comp,
               int *status );

void hdrCopy( char *param1,
              char *xname1,
              char *param2,
              char *xname2,
              int *status );

void hdrIn( char *param,
            char *xname,
            char *item,
            int comp,
            char *value,
            int value_length,
            int *status );

void hdrInC( char *param,
             char *xname,
             char *item,
             int comp,
             char *value,
             int value_length,
             int *status );

void hdrInD( char *param,
             char *xname,
             char *item,
             int comp,
             double *value,
             int *status );

void hdrInI( char *param,
             char *xname,
             char *item,
             int comp,
             int *value,
             int *status );

void hdrInL( char *param,
             char *xname,
             char *item,
             int comp,
             int *value,
             int *status );

void hdrInF( char *param,
             char *xname,
             char *item,
             int comp,
             float *value,
             int *status );

void hdrName( char *param,
              char *xname,
              int n,
              char *item,
              int item_length,
              int *status );

void hdrNumb( char *param,
              char *xname,
              char *item,
              int *n,
              int *status );

void hdrOut( char *param,
             char *xname,
             char *item,
             char *commen,
             char *value,
             int value_length,
             int *status );

void hdrOutC( char *param,
              char *xname,
              char *item,
              char *commen,
              char *value,
              int value_length,
              int *status );

void hdrOutD( char *param,
              char *xname,
              char *item,
              char *commen,
              double *value,
              int *status );

void hdrOutI( char *param,
              char *xname,
              char *item,
              char *commen,
              int *value,
              int *status );

void hdrOutL( char *param,
              char *xname,
              char *item,
              char *commen,
              int *value,
              int *status );

void hdrOutF( char *param,
              char *xname,
              char *item,
              char *commen,
              float *value,
              int *status );

void imgCancl( char *param,
               int *status );

void imgDelet( char *param,
               int *status );

void imgFree( char *param,
              int *status );

void imgIn( char *param,
            int *nx,
            int *ny,
            float **ip,
            int *status );

void imgIn1( char *param,
             int *nx,
             float **ip,
             int *status );

void imgIn1UB( char *param,
               int *nx,
               unsigned char **ip,
               int *status );

void imgIn1US( char *param,
               int *nx,
               unsigned short int **ip,
               int *status );

void imgIn1B( char *param,
              int *nx,
              signed char **ip,
              int *status );

void imgIn1D( char *param,
              int *nx,
              double **ip,
              int *status );

void imgIn1I( char *param,
              int *nx,
              int **ip,
              int *status );

void imgIn1F( char *param,
              int *nx,
              float **ip,
              int *status );

void imgIn1S( char *param,
              int *nx,
              short int **ip,
              int *status );

void imgIn2( char *param,
             int *nx,
             int *ny,
             float **ip,
             int *status );

void imgIn2UB( char *param,
               int *nx,
               int *ny,
               unsigned char **ip,
               int *status );

void imgIn2US( char *param,
               int *nx,
               int *ny,
               unsigned short int **ip,
               int *status );

void imgIn2B( char *param,
              int *nx,
              int *ny,
              signed char **ip,
              int *status );

void imgIn2D( char *param,
              int *nx,
              int *ny,
              double **ip,
              int *status );

void imgIn2I( char *param,
              int *nx,
              int *ny,
              int **ip,
              int *status );

void imgIn2F( char *param,
              int *nx,
              int *ny,
              float **ip,
              int *status );

void imgIn2S( char *param,
              int *nx,
              int *ny,
              short int **ip,
              int *status );

void imgIn3( char *param,
             int *nx,
             int *ny,
             int *nz,
             float **ip,
             int *status );

void imgIn3UB( char *param,
               int *nx,
               int *ny,
               int *nz,
               unsigned char **ip,
               int *status );

void imgIn3US( char *param,
               int *nx,
               int *ny,
               int *nz,
               unsigned short int **ip,
               int *status );

void imgIn3B( char *param,
              int *nx,
              int *ny,
              int *nz,
              signed char **ip,
              int *status );

void imgIn3D( char *param,
              int *nx,
              int *ny,
              int *nz,
              double **ip,
              int *status );

void imgIn3I( char *param,
              int *nx,
              int *ny,
              int *nz,
              int **ip,
              int *status );

void imgIn3F( char *param,
              int *nx,
              int *ny,
              int *nz,
              float **ip,
              int *status );

void imgIn3S( char *param,
              int *nx,
              int *ny,
              int *nz,
              short int **ip,
              int *status );

void imgIndf( char *param,
              int *indf,
              int *status );

void imgInUB( char *param,
              int *nx,
              int *ny,
              unsigned char **ip,
              int *status );

void imgInUS( char *param,
              int *nx,
              int *ny,
              unsigned short int **ip,
              int *status );

void imgInB( char *param,
             int *nx,
             int *ny,
             signed char **ip,
             int *status );

void imgInD( char *param,
             int *nx,
             int *ny,
             double **ip,
             int *status );

void imgInI( char *param,
             int *nx,
             int *ny,
             int **ip,
             int *status );

void imgInF( char *param,
             int *nx,
             int *ny,
             float **ip,
             int *status );

void imgInS( char *param,
             int *nx,
             int *ny,
             short int **ip,
             int *status );

void imgMod( char *param,
             int *nx,
             int *ny,
             float **ip,
             int *status );

void imgMod1( char *param,
              int *nx,
              float **ip,
              int *status );

void imgMod1UB( char *param,
                int *nx,
                unsigned char **ip,
                int *status );

void imgMod1US( char *param,
                int *nx,
                unsigned short int **ip,
                int *status );

void imgMod1B( char *param,
               int *nx,
               signed char **ip,
               int *status );

void imgMod1D( char *param,
               int *nx,
               double **ip,
               int *status );

void imgMod1I( char *param,
               int *nx,
               int **ip,
               int *status );

void imgMod1F( char *param,
               int *nx,
               float **ip,
               int *status );

void imgMod1S( char *param,
               int *nx,
               short int **ip,
               int *status );

void imgMod2( char *param,
              int *nx,
              int *ny,
              float **ip,
              int *status );

void imgMod2UB( char *param,
                int *nx,
                int *ny,
                unsigned char **ip,
                int *status );

void imgMod2US( char *param,
                int *nx,
                int *ny,
                unsigned short int **ip,
                int *status );

void imgMod2B( char *param,
               int *nx,
               int *ny,
               signed char **ip,
               int *status );

void imgMod2D( char *param,
               int *nx,
               int *ny,
               double **ip,
               int *status );

void imgMod2I( char *param,
               int *nx,
               int *ny,
               int **ip,
               int *status );

void imgMod2F( char *param,
               int *nx,
               int *ny,
               float **ip,
               int *status );

void imgMod2S( char *param,
               int *nx,
               int *ny,
               short int **ip,
               int *status );

void imgMod3( char *param,
              int *nx,
              int *ny,
              int *nz,
              float **ip,
              int *status );

void imgMod3UB( char *param,
                int *nx,
                int *ny,
                int *nz,
                unsigned char **ip,
                int *status );

void imgMod3US( char *param,
                int *nx,
                int *ny,
                int *nz,
                unsigned short int **ip,
                int *status );

void imgMod3B( char *param,
               int *nx,
               int *ny,
               int *nz,
               signed char **ip,
               int *status );

void imgMod3D( char *param,
               int *nx,
               int *ny,
               int *nz,
               double **ip,
               int *status );

void imgMod3I( char *param,
               int *nx,
               int *ny,
               int *nz,
               int **ip,
               int *status );

void imgMod3F( char *param,
               int *nx,
               int *ny,
               int *nz,
               float **ip,
               int *status );

void imgMod3S( char *param,
               int *nx,
               int *ny,
               int *nz,
               short int **ip,
               int *status );

void imgModUB( char *param,
               int *nx,
               int *ny,
               unsigned char **ip,
               int *status );

void imgModUS( char *param,
               int *nx,
               int *ny,
               unsigned short int **ip,
               int *status );

void imgModB( char *param,
              int *nx,
              int *ny,
              signed char **ip,
              int *status );

void imgModD( char *param,
              int *nx,
              int *ny,
              double **ip,
              int *status );

void imgModI( char *param,
              int *nx,
              int *ny,
              int **ip,
              int *status );

void imgModF( char *param,
              int *nx,
              int *ny,
              float **ip,
              int *status );

void imgModS( char *param,
              int *nx,
              int *ny,
              short int **ip,
              int *status );

void imgName( char *param,
              char *value,
              int value_length,
              int *status );

void imgNew( char *param,
             int nx,
             int ny,
             float **ip,
             int *status );

void imgNew1( char *param,
              int nx,
              float **ip,
              int *status );

void imgNew1UB( char *param,
                int nx,
                unsigned char **ip,
                int *status );

void imgNew1US( char *param,
                int nx,
                unsigned short int **ip,
                int *status );

void imgNew1B( char *param,
               int nx,
               signed char **ip,
               int *status );

void imgNew1D( char *param,
               int nx,
               double **ip,
               int *status );

void imgNew1I( char *param,
               int nx,
               int **ip,
               int *status );

void imgNew1F( char *param,
               int nx,
               float **ip,
               int *status );

void imgNew1S( char *param,
               int nx,
               short int **ip,
               int *status );

void imgNew2( char *param,
              int nx,
              int ny,
              float **ip,
              int *status );

void imgNew2UB( char *param,
                int nx,
                int ny,
                unsigned char **ip,
                int *status );

void imgNew2US( char *param,
                int nx,
                int ny,
                unsigned short int **ip,
                int *status );

void imgNew2B( char *param,
               int nx,
               int ny,
               signed char **ip,
               int *status );

void imgNew2D( char *param,
               int nx,
               int ny,
               double **ip,
               int *status );

void imgNew2I( char *param,
               int nx,
               int ny,
               int **ip,
               int *status );

void imgNew2F( char *param,
               int nx,
               int ny,
               float **ip,
               int *status );

void imgNew2S( char *param,
               int nx,
               int ny,
               short int **ip,
               int *status );

void imgNew3( char *param,
              int nx,
              int ny,
              int nz,
              float **ip,
              int *status );

void imgNew3UB( char *param,
                int nx,
                int ny,
                int nz,
                unsigned char **ip,
                int *status );

void imgNew3US( char *param,
                int nx,
                int ny,
                int nz,
                unsigned short int **ip,
                int *status );

void imgNew3B( char *param,
               int nx,
               int ny,
               int nz,
               signed char **ip,
               int *status );

void imgNew3D( char *param,
               int nx,
               int ny,
               int nz,
               double **ip,
               int *status );

void imgNew3I( char *param,
               int nx,
               int ny,
               int nz,
               int **ip,
               int *status );

void imgNew3F( char *param,
               int nx,
               int ny,
               int nz,
               float **ip,
               int *status );

void imgNew3S( char *param,
               int nx,
               int ny,
               int nz,
               short int **ip,
               int *status );

void imgNewUB( char *param,
               int nx,
               int ny,
               unsigned char **ip,
               int *status );

void imgNewUS( char *param,
               int nx,
               int ny,
               unsigned short int **ip,
               int *status );

void imgNewB( char *param,
              int nx,
              int ny,
              signed char **ip,
              int *status );

void imgNewD( char *param,
              int nx,
              int ny,
              double **ip,
              int *status );

void imgNewI( char *param,
              int nx,
              int ny,
              int **ip,
              int *status );

void imgNewF( char *param,
              int nx,
              int ny,
              float **ip,
              int *status );

void imgNewS( char *param,
              int nx,
              int ny,
              short int **ip,
              int *status );

void imgOut( char *param1,
             char *param2,
             float **ip,
             int *status );

void imgOutUB( char *param1,
               char *param2,
               unsigned char **ip,
               int *status );

void imgOutUS( char *param1,
               char *param2,
               unsigned short int **ip,
               int *status );

void imgOutB( char *param1,
              char *param2,
              signed char **ip,
              int *status );

void imgOutD( char *param1,
              char *param2,
              double **ip,
              int *status );

void imgOutI( char *param1,
              char *param2,
              int **ip,
              int *status );

void imgOutF( char *param1,
              char *param2,
              float **ip,
              int *status );

void imgOutS( char *param1,
              char *param2,
              short int **ip,
              int *status );

void imgTmp( char *param,
             int nx,
             int ny,
             float **ip,
             int *status );

void imgTmp1( char *param,
              int nx,
              float **ip,
              int *status );

void imgTmp1UB( char *param,
                int nx,
                unsigned char **ip,
                int *status );

void imgTmp1US( char *param,
                int nx,
                unsigned short int **ip,
                int *status );

void imgTmp1B( char *param,
               int nx,
               signed char **ip,
               int *status );

void imgTmp1D( char *param,
               int nx,
               double **ip,
               int *status );

void imgTmp1I( char *param,
               int nx,
               int **ip,
               int *status );

void imgTmp1F( char *param,
               int nx,
               float **ip,
               int *status );

void imgTmp1S( char *param,
               int nx,
               short int **ip,
               int *status );

void imgTmp2( char *param,
              int nx,
              int ny,
              float **ip,
              int *status );

void imgTmp2UB( char *param,
                int nx,
                int ny,
                unsigned char **ip,
                int *status );

void imgTmp2US( char *param,
                int nx,
                int ny,
                unsigned short int **ip,
                int *status );

void imgTmp2B( char *param,
               int nx,
               int ny,
               signed char **ip,
               int *status );

void imgTmp2D( char *param,
               int nx,
               int ny,
               double **ip,
               int *status );

void imgTmp2I( char *param,
               int nx,
               int ny,
               int **ip,
               int *status );

void imgTmp2F( char *param,
               int nx,
               int ny,
               float **ip,
               int *status );

void imgTmp2S( char *param,
               int nx,
               int ny,
               short int **ip,
               int *status );

void imgTmp3( char *param,
              int nx,
              int ny,
              int nz,
              float **ip,
              int *status );

void imgTmp3UB( char *param,
                int nx,
                int ny,
                int nz,
                unsigned char **ip,
                int *status );

void imgTmp3US( char *param,
                int nx,
                int ny,
                int nz,
                unsigned short int **ip,
                int *status );

void imgTmp3B( char *param,
               int nx,
               int ny,
               int nz,
               signed char **ip,
               int *status );

void imgTmp3D( char *param,
               int nx,
               int ny,
               int nz,
               double **ip,
               int *status );

void imgTmp3I( char *param,
               int nx,
               int ny,
               int nz,
               int **ip,
               int *status );

void imgTmp3F( char *param,
               int nx,
               int ny,
               int nz,
               float **ip,
               int *status );

void imgTmp3S( char *param,
               int nx,
               int ny,
               int nz,
               short int **ip,
               int *status );

void imgTmpUB( char *param,
               int nx,
               int ny,
               unsigned char **ip,
               int *status );

void imgTmpUS( char *param,
               int nx,
               int ny,
               unsigned short int **ip,
               int *status );

void imgTmpB( char *param,
              int nx,
              int ny,
              signed char **ip,
              int *status );

void imgTmpD( char *param,
              int nx,
              int ny,
              double **ip,
              int *status );

void imgTmpI( char *param,
              int nx,
              int ny,
              int **ip,
              int *status );

void imgTmpF( char *param,
              int nx,
              int ny,
              float **ip,
              int *status );

void imgTmpS( char *param,
              int nx,
              int ny,
              short int **ip,
              int *status );


/* $Id$ */
