#include "sx.h"

Fpair *head=NULL;

/*------------------------------------------------------------------------*/

Error SXSampleD( int nfld, int ndim, int veclen[], int ninpos, float *inpos,
                 double *indata[], int noutpos, float *outpos, double
                 *outdata[], float *lbout, float *ubout, int nclose,
                 float maxrad, float *rscale, int nscale, float power,
                 float coexp, char type, int *outbad ){
/*
*+
*  Name:
*     SXSampleD

*  Purpose:
*     Sample scattered fields at a list of given sample positions
*     using data of type DOUBLE.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXSampleD( int nfld, int ndim, int veclen[], int ninpos,
*                      float *inpos, double *indata[], int noutpos,
*                      float *outpos, double *outdata[], float *lbout,
*                      float *ubout, int nclose, float maxrad, float *rscale,
*                      int nscale, float power, float coexp, char type,
*                      int *outbad );

*  Description:
*     The supplied input data arrays are sampled at the supplied output
*     positions and returned in the output data arrays. The input data
*     must be either scalar or vector (i.e. not tensor for instance). All
*     invalid positions must be removed (culled) from the input
*     positions array before calling this function. Invalid output positions
*     (i.e. positions at which no valid output data values could be created)
*     are flagged by the maximum floating point value (i.e. FLT_MAX defined
*     in float.h) being returned for their output data values. Argument
*     "outbad" is returned holding the number of invalid output positions
*     encountered in each field.
*
*     Each output data value is created by first identifying a set of
*     input positions in the neighbourhood of the output position (see
*     arguments "nclose" and "maxrad"). Each input position is assigned a
*     weight based on its distance from the output position, d:
*
*        exp( coexp*( ( d/scale )**power ) )   : if coexp is not zero
*
*     or
*
*        ( d/scale )**power                    : if coexp is zero
*
*     The squared reciprocal of the scale values are supplied in array
*     "rscale". "rscale[0] is used for the closest point, "rscale[1]" is used
*     for the next closest, etc. If insufficient scale values are supplied,
*     the last scale value (i.e. "rscale[nscale-1]") is used for all remaining
*     positions. The output data value can then be either the weighted mean
*     of the input data values, the weighted sum of the input values, or just
*     the sum of the input wieghts (see argument type).

*  Arguments:
*     nfld = int (Given)
*        The number of matching pairs of input and output data arrays to
*        be processed simultaneously.
*     ndim = int (Given)
*        The number of dimensions describing each position. This should
*        be in the range 1 to 3.
*     veclen[] = int (Given)
*        An array holding the length of the data vectors for each of the
*        "nfld" data arrays.
*     ninpos = int (Given)
*        The number of input positions.
*     inpos = float * (Given)
*        A pointer to the start of the array holding the input positions.
*        This array should hold "ninpos" vectors of dimension "ndim".
*     indata[] = double * (Given)
*        An array of pointers to the start of the "nfld" input data arrays.
*        Each of these input data arrays should hold "ninpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     noutpos = int (Given)
*        The number of output positions.
*     outpos = float * (Given)
*        A pointer to the start of the array holding the output positions.
*        This array should hold "noutpos" vectors of dimension "ndim".
*     outdata[] = double * (Given)
*        An array of pointers to the start of the "nfld" output data arrays.
*        Each of these output data arrays should hold "noutpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     lbout = float * (Given)
*        A pointer to the start of an array in which are stored the
*        lower bounds of the volume of interest. Output positions outside
*        this volume are returned with "invalid" data values. Input
*        positions outside this volume are ignored.
*     ubout = float * (Given)
*        A pointer to the start of an array in which are stored the
*        upper bounds of the volume of interest.
*     nclose = int (Given)
*        The maximum number of input positions to consider when finding
*        each output data value. If this is set to INT_MAX (defined in
*        limits.h) then no limit is placed on the number of input positions
*        which can contribute to each output data value.
*     maxrad = float (Given)
*        The maximum distance which an input position can be away from an
*        output position if it is to contribute to the output data value.
*        If this is set to FLT_MAX (defined in float.h), then no limit is
*        placed on distance.
*     rscale = float * (Given)
*        Pointer to an array holding the squares of the reciprocal of the
*        scale lengths to use when calculating the weight of each input
*        position for a particular output position.
*     nscale = int (Given)
*        The number of scale values supplied. If "nscale" is less than
*        "nclose", then the last scale value is used for all remaining
*        positions.
*     power = float (Given)
*        The power to which the scaled distance is raised when forming the
*        weight of an input position for a particular output position.
*     coexp = float (Given)
*        If this is not zero, then the weight is exponentiated using
*        the supplied value as a multiplicative argument (see the
*        "Description:" section above).
*     type = char (Given)
*        If 0, then the returned output data values are weighted means
*        of the input data values. If 1, then the returned output data
*        values are weighted sums of the input data values. If 2, then
*        the returned output data values are the sum of the weights of
*        the input data values.
*     outbad = int * (Returned)
*        Pointer to an array returned holding the number of invalid
*        positions in each output field.

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      char    bad;       /* Is the sample position invalid? */
      char    badwgt;    /* Has the weight overflowed? */
      float  *disres;    /* Squared distances to close input positions */
      int     fld;       /* Index of current field */
      int     i;         /* Output position index */
      int     gdim[3];   /* Dimensions of tables stored in "poslut" */
      double  hpower;    /* Weight exponent for SQUARED distances */
      double *infld;     /* Pointer to the next input data value */
      int     j;         /* Loop count */
      int     k;         /* Loop count */
      double  k1;        /* Constant used for checking for overflowing weights*/
      double  k2;        /* Constant used for checking for overflowing weights*/
      float   lastrscale;/* Last value supplied in "rscale" */
      float   lbndl[3];  /* Lower bounds of tables stored in "poslut" */
      float   maxd2;     /* Max squared distance to a usable input position */
      double **next;     /* Pointers to the next output data values */
      int     nres;      /* No. of usable input positions */
      double *outfld;    /* Pointer to the next output data value */
      float  *outp;      /* Pointer to the next output position value */
      int    *poslut;    /* Look-up-table identifying near neighbours */
      int    *posres;    /* Indices of close input positions */
      float   rdcell;    /* Reciprocal of cell size in "poslut" */
      double  temp;      /* Temporary staorage */
      double  wgt;       /* Weight for an input position */
      double  wsum;      /* Sum of weights */


/*  Check that a criterion has been given for determining which input
 *  positions to use. */

      if( nclose == INT_MAX && maxrad == FLT_MAX ){
         DXSetError( ERROR_BAD_PARAMETER, "No selection criteria given." );
         return( ERROR );
      }


/*  If needed, store half the exponent for the weights. The exponnent is
 *  halved to take account of the fact that the weights are calcualted from the SQUARED distances. */

      if( power != 0.0 ){
         hpower = (double) 0.5*power;
         k1 = log( (double) 0.5*DBL_MAX )/hpower;
         if( coexp != 0.0 ) k2 = k1*hpower/(double)coexp;
      }


/*  Get work space. */

      next = (double **) DXAllocate( sizeof( double *)*nfld );

      if( nclose < INT_MAX ){

         posres = (int *) DXAllocate( sizeof( int )*nclose );
         disres = (float *) DXAllocate( sizeof( float )*nclose );

         if( posres == NULL || disres == NULL ){
            DXFree( (Pointer) posres );
            DXFree( (Pointer) disres );
            return( ERROR );
         }

      }


/*  Initialise the look-up-tables (etc) needed to find which input
 *  positions are close to each output positions. */

      poslut = SXInitClose( ndim, ninpos, inpos, lbout, ubout, lbndl,
                            gdim, &rdcell );
      if( poslut == NULL ) {
         DXFree( (Pointer) posres );
         DXFree( (Pointer) disres );
         return( ERROR );
      }


/*  Store the maximum squared distance from an output position to a
 *  contributing input position, checking for overflow. */

      if( maxrad < (float) sqrt( (double) FLT_MAX ) ){
         maxd2 = maxrad*maxrad;
      } else {
         maxd2 = FLT_MAX;
      }


/*  Store the last value supplied in the "rscale" array. */

      lastrscale = (double) rscale[nscale-1];


/*  Initialise the array holding pointers to the next output data value
 *  and indicate that no bad output values have yet been created,
 *  for each field. */

      for( fld=0; fld<nfld; fld++ ) {
         next[fld] = outdata[fld];
         outbad[fld] = 0;
      }


/*  Process each output position. */

      for( i=0; i<noutpos; i++ ){


/*  Indicate progress. */

         if( i != 0 && i == 2000*(int)(( (float) i )/2000.0 ) ) {
            DXMessage("SXRegrid: Doing output position %d of %d",i,noutpos );
         }


/*  Return invalid data values if the current output position is
 *  invalid. */

         bad = 0;
         outp = outpos + i*ndim;
         for( j=0; j<ndim; j++ ) {
            if( *(outp++) == FLT_MAX ) bad = 1;
         }

         if( bad ){
            for( fld=0; fld<nfld; fld++ ){
               outfld = next[fld];
               for( k=0; k<veclen[fld]; k++ ) *(outfld++) = DBL_MAX;
               next[fld] = outfld ;
               outbad[fld]++;
            }
            continue;
         }


/*  Find the input positions which contribute to the output position. If
 *  a maximum number of input positions was specified... */

         if( nclose < INT_MAX ){


/*  Find the specified number of input positions which are closest to the
 *  current output position. */

            if( !SXFindClose( ndim, ninpos, inpos, outpos+i*ndim, nclose,
                              rdcell, lbndl, gdim, poslut, posres, disres ) ){
               DXFree( (Pointer) poslut );
               DXFree( (Pointer) posres );
               DXFree( (Pointer) disres );
               return( ERROR );
            }


/*  Count the number of usable input positions (i.e. ones which were found,
 *  and which are not further than the specified maximum distance from the
 *  output position. ) */

            j = nclose - 1;
            while( j>= 0 && ( posres[j] < 0 || disres[j] > maxd2 ) ) j--;
            nres = j + 1;


/*  If no limit was put on the number of input positions, find all input
 *  positions within the specified radius of the current output position. */

         } else {

            if( !SXFindRadius( ndim, ninpos, inpos, outpos+i*ndim, maxrad,
                               rdcell, lbndl, gdim, poslut, &nres, &posres,
                               &disres ) ){
               DXFree( (Pointer) poslut );
               return( ERROR );
            }


/*  If different scale factors have been given for different points (i.e.
 *  if "nscale" was supplied greater than 1), then we need to sort the
 *  positions returned by SXFindRadius into order of increasing distance
 *  from the current sample position so that the correct scale factor is
 *  used for each position. (SXFindClose returns positions already sorted but
 *  SXFindRadius doesn't). */

            if( nscale > 1 ) SXSort( nres, disres, posres );

         }


/*  Initialise the sums for this output position. The sum of the weights
 *  is accumulated in "wsum" and the sum of the weighted input data values
 *  is accumulated in the current position in the output data arrays. */

         wsum = 0.0;

         for( fld=0; fld<nfld; fld++ ){
            outfld = next[fld];
            for( k=0; k<veclen[fld]; k++ ) *(outfld++) = 0.0;
         }


/*  Loop round each of the contributing input positions. */

         for( j=0; j<nres; j++ ){


/*  Calculate the weight for this input position. Exponentiation can easily
 *  cause  overflows. Trap such cases and set a flag (badwgt). */

            badwgt = 0;

            if( power == 0.0 ){
               wgt = 1.0;

            } else {

               if( j < nscale ) {
                  temp = (double) (disres[j]*rscale[j]);
               } else {
                  temp = (double) (disres[j]*lastrscale);
               }

               if( temp > 0.0 ){
                  temp = log( (double) temp );

                  if( ( k1 > 0.0 && temp < k1 ) ||
                      ( k1 < 0.0 && temp > k1 ) ){
                     wgt = exp( (double) hpower*temp );

                     if( coexp != 0.0 ) {
                        if( ( k2 > 0.0 && wgt < k2 ) ||
                            ( k2 < 0.0 && wgt > k2 ) ){
                           wgt = exp( (double) coexp*wgt );
                        } else {
                           badwgt = 1;
                        }
                     }

                  } else {
                     badwgt = 1;
                  }

               } else {

                  if( coexp != 0.0 ) {
                     wgt = 1.0;
                  } else {
                     wgt = 0.0;
                  }
               }

            }


/*  If the weight overflowed, ignore the remaining input positions. */

            if( badwgt ) break;


/*  Increment the sums */

            wsum += wgt;

            for( fld=0; fld<nfld; fld++ ){

               outfld = next[fld];
               infld = indata[fld] + posres[j]*veclen[fld];

               for( k=0; k<veclen[fld]; k++ ) {
                  *(outfld++) += wgt*(*(infld++));
               }

            }

         }


/*  Store the output data values for each field... */

         for( fld=0; fld<nfld; fld++ ){
            outfld = next[fld];


/*  If the output value is not bad, modify the values stored in the output
 *  array (the weighted sum) if required. */

            if( !badwgt ){

               if( type == 0 ){                /*  Weighted mean */

                  if( wsum != 0.0 ){
                     for( k=0; k<veclen[fld]; k++ ) *(outfld++) /= wsum;

                  } else {
                     for( k=0; k<veclen[fld]; k++ ) *(outfld++) = DBL_MAX;
                     outbad[fld]++;
                  }


               } else if( type == 1 ){        /* Weighted sum */

                  for( k=0; k<veclen[fld]; k++ ) *(outfld++);


               } else if( type == 2 ){         /*  Sum of weights */

                  *(outfld++) = wsum;

               }


/*  If the output value is undefined, store "bad" data values (the maximum
 *  floating point value). */

            } else {
               for( k=0; k<veclen[fld]; k++ ) *(outfld++) = DBL_MAX;
               outbad[fld]++;
            }


/*  Store the pointer to the next output value for this field. */

            next[fld] = outfld ;

         }


/*  Free the storage used to hold the list of contributing input positions
 *  if storage is allocated inside SXFindRadius (SXFindClose does not
 *  allocate its own results arrays). */

         if( nclose == INT_MAX ){
            DXFree( (Pointer) posres );
            DXFree( (Pointer) disres );
         }

      }


/*  Finish the progress messages. */

      if( noutpos >= 2000 ) DXMessage("SXRegrid: Done all %d positions",noutpos );


/*  Free the storage used to hold the list of contributing input positions
 *  if storag ewas allocated by this function (i.e. if SXFindClose was
 *  used instead of SXFindRadius). */

      if( nclose < INT_MAX ){
         DXFree( (Pointer) posres );
         DXFree( (Pointer) disres );
      }

      DXFree( (Pointer) next );

      return( OK );

}


/*------------------------------------------------------------------------*/

Error SXSampleF( int nfld, int ndim, int veclen[], int ninpos, float *inpos,
                 float *indata[], int noutpos, float *outpos, float
                 *outdata[], float *lbout, float *ubout, int nclose,
                 float maxrad, float *rscale, int nscale, float power,
                 float coexp, char type, int *outbad ){
/*
*+
*  Name:
*     SXSampleF

*  Purpose:
*     Sample scattered fields at a list of given sample positions
*     using data of type FLOAT.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXSampleF( int nfld, int ndim, int veclen[], int ninpos,
*                      float *inpos, float *indata[], int noutpos,
*                      float *outpos, float *outdata[], float *lbout,
*                      float *ubout, int nclose, float maxrad, float *rscale,
*                      int nscale, float power, float coexp, char type,
*                      int *outbad );

*  Description:
*     The supplied input data arrays are sampled at the supplied output
*     positions and returned in the output data arrays. The input data
*     must be either scalar or vector (i.e. not tensor for instance). All
*     invalid positions must be removed (culled) from the input
*     positions array before calling this function. Invalid output positions
*     (i.e. positions at which no valid output data values could be created)
*     are flagged by the maximum floating point value (i.e. FLT_MAX defined
*     in float.h) being returned for their output data values. Argument
*     "outbad" is returned holding the number of invalid output positions
*     encountered in each field.
*
*     Each output data value is created by first identifying a set of
*     input positions in the neighbourhood of the output position (see
*     arguments "nclose" and "maxrad"). Each input position is assigned a
*     weight based on its distance from the output position, d:
*
*        exp( coexp*( ( d/scale )**power ) )   : if coexp is not zero
*
*     or
*
*        ( d/scale )**power                    : if coexp is zero
*
*     The squared reciprocal of the scale values are supplied in array
*     "rscale". "rscale[0] is used for the closest point, "rscale[1]" is used
*     for the next closest, etc. If insufficient scale values are supplied,
*     the last scale value (i.e. "rscale[nscale-1]") is used for all remaining
*     positions. The output data value can then be either the weighted mean
*     of the input data values, the weighted sum of the input values, or just
*     the sum of the input wieghts (see argument type).

*  Arguments:
*     nfld = int (Given)
*        The number of matching pairs of input and output data arrays to
*        be processed simultaneously.
*     ndim = int (Given)
*        The number of dimensions describing each position. This should
*        be in the range 1 to 3.
*     veclen[] = int (Given)
*        An array holding the length of the data vectors for each of the
*        "nfld" data arrays.
*     ninpos = int (Given)
*        The number of input positions.
*     inpos = float * (Given)
*        A pointer to the start of the array holding the input positions.
*        This array should hold "ninpos" vectors of dimension "ndim".
*     indata[] = float * (Given)
*        An array of pointers to the start of the "nfld" input data arrays.
*        Each of these input data arrays should hold "ninpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     noutpos = int (Given)
*        The number of output positions.
*     outpos = float * (Given)
*        A pointer to the start of the array holding the output positions.
*        This array should hold "noutpos" vectors of dimension "ndim".
*     outdata[] = float * (Given)
*        An array of pointers to the start of the "nfld" output data arrays.
*        Each of these output data arrays should hold "noutpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     lbout = float * (Given)
*        A pointer to the start of an array in which are stored the
*        lower bounds of the volume of interest. Output positions outside
*        this volume are returned with "invalid" data values. Input
*        positions outside this volume are ignored.
*     ubout = float * (Given)
*        A pointer to the start of an array in which are stored the
*        upper bounds of the volume of interest.
*     nclose = int (Given)
*        The maximum number of input positions to consider when finding
*        each output data value. If this is set to INT_MAX (defined in
*        limits.h) then no limit is placed on the number of input positions
*        which can contribute to each output data value.
*     maxrad = float (Given)
*        The maximum distance which an input position can be away from an
*        output position if it is to contribute to the output data value.
*        If this is set to FLT_MAX (defined in float.h), then no limit is
*        placed on distance.
*     rscale = float * (Given)
*        Pointer to an array holding the squares of the reciprocal of the
*        scale lengths to use when calculating the weight of each input
*        position for a particular output position.
*     nscale = int (Given)
*        The number of scale values supplied. If "nscale" is less than
*        "nclose", then the last scale value is used for all remaining
*        positions.
*     power = float (Given)
*        The power to which the scaled distance is raised when forming the
*        weight of an input position for a particular output position.
*     coexp = float (Given)
*        If this is not zero, then the weight is exponentiated using
*        the supplied value as a multiplicative argument (see the
*        "Description:" section above).
*     type = char (Given)
*        If 0, then the returned output data values are weighted means
*        of the input data values. If 1, then the returned output data
*        values are weighted sums of the input data values. If 2, then
*        the returned output data values are the sum of the weights of
*        the input data values.
*     outbad = int * (Returned)
*        Pointer to an array returned holding the number of invalid
*        positions in each output field.

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      char   bad;       /* Is the sample position invalid? */
      char   badwgt;    /* Has the weight overflowed? */
      float *disres;    /* Squared distances to close input positions */
      int    fld;       /* Index of current field */
      int    i;         /* Output position index */
      int    gdim[3];   /* Dimensions of tables stored in "poslut" */
      float  hpower;    /* Weight exponent for SQUARED distances */
      float *infld;     /* Pointer to the next input data value */
      int    j;         /* Loop count */
      int    k;         /* Loop count */
      float  k1;        /* Constant used for checking for overflowing weights*/
      float  k2;        /* Constant used for checking for overflowing weights*/
      float  lastrscale;/* Last value supplied in "rscale" */
      float  lbndl[3];  /* Lower bounds of tables stored in "poslut" */
      float  maxd2;     /* Max squared distance to a usable input position */
      float **next;     /* Pointers to the next output data values */
      int    nres;      /* No. of usable input positions */
      float *outfld;    /* Pointer to the next output data value */
      int   *poslut;    /* Look-up-table identifying near neighbours */
      int   *posres;    /* Indices of close input positions */
      float  rdcell;    /* Reciprocal of cell size in "poslut" */
      float  temp;      /* Temporary staorage */
      float  wgt;       /* Weight for an input position */
      float  wsum;      /* Sum of weights */


/*  Check that a criterion has been given for determining which input
 *  positions to use. */

      if( nclose == INT_MAX && maxrad == FLT_MAX ){
         DXSetError( ERROR_BAD_PARAMETER, "No selection criteria given." );
         return( ERROR );
      }


/*  If needed, store half the exponent for the weights. The exponnent is
 *  halved to take account of the fact that the weights are calcualted from the SQUARED distances. */

      if( power != 0.0 ){
         hpower = 0.5*power;
         k1 = (float) log( (double) 0.5*FLT_MAX )/hpower;
         if( coexp != 0.0 ) k2 = k1*hpower/coexp;
      }


/*  Get work space. */

      next = (float **) DXAllocate( sizeof( float *)*nfld );

      if( nclose < INT_MAX ){

         posres = (int *) DXAllocate( sizeof( int )*nclose );
         disres = (float *) DXAllocate( sizeof( float )*nclose );

         if( posres == NULL || disres == NULL ){
            DXFree( (Pointer) posres );
            DXFree( (Pointer) disres );
            return( ERROR );
         }

      }


/*  Initialise the look-up-tables (etc) needed to find which input
 *  positions are close to each output positions. */

      poslut = SXInitClose( ndim, ninpos, inpos, lbout, ubout, lbndl,
                              gdim, &rdcell );
      if( poslut == NULL ) {
         DXFree( (Pointer) posres );
         DXFree( (Pointer) disres );
         return( ERROR );
      }


/*  Store the maximum squared distance from an output position to a
 *  contributing input position, checking for overflow. */

      if( maxrad < (float) sqrt( (double) FLT_MAX ) ){
         maxd2 = maxrad*maxrad;
      } else {
         maxd2 = FLT_MAX;
      }


/*  Store the last value supplied in the "rscale" array. */

      lastrscale = rscale[nscale-1];


/*  Initialise the array holding pointers to the next output data value
 *  and indicate that no bad output values have yet been created,
 *  for each field. */

      for( fld=0; fld<nfld; fld++ ) {
         next[fld] = outdata[fld];
         outbad[fld] = 0;
      }


/*  Process each output position. */

      for( i=0; i<noutpos; i++ ){


/*  Indicate progress. */

         if( i == 2000*(int)(( (float) i )/2000.0 ) ) {
            DXMessage("SXRegrid: Doing output positions %d of %d",i,noutpos );
         }


/*  Return invalid data values if the current output positrion is
 *  invalid. */

         bad = 0;
         outfld = outpos + i*ndim;
         for( j=0; j<ndim; j++ ) {
            if( *(outfld++) == FLT_MAX ) bad = 1;
         }

         if( bad ){
            for( fld=0; fld<nfld; fld++ ){
               outfld = next[fld];
               for( k=0; k<veclen[fld]; k++ ) *(outfld++) = FLT_MAX;
               next[fld] = outfld ;
               outbad[fld]++;
            }
            continue;
         }


/*  Find the input positions which contribute to the output position. If
 *  a maximum number of input positions was specified... */

         if( nclose < INT_MAX ){


/*  Find the specified number of input positions which are closest to the
 *  current output position. */

            if( !SXFindClose( ndim, ninpos, inpos, outpos+i*ndim, nclose,
                              rdcell, lbndl, gdim, poslut, posres, disres ) ){
               DXFree( (Pointer) poslut );
               DXFree( (Pointer) posres );
               DXFree( (Pointer) disres );
               return( ERROR );
            }


/*  Count the number of usable input positions (i.e. ones which were found,
 *  and which are not further than the specified maximum distance from the
 *  output position. ) */

            j = nclose - 1;
            while( j>= 0 && ( posres[j] < 0 || disres[j] > maxd2 ) ) j--;
            nres = j + 1;


/*  If no limit was put on the number of input positions, find all input
 *  positions within the specified radius of the current output position. */

         } else {

            if( !SXFindRadius( ndim, ninpos, inpos, outpos+i*ndim, maxrad,
                               rdcell, lbndl, gdim, poslut, &nres, &posres,
                               &disres ) ){
               DXFree( (Pointer) poslut );
               return( ERROR );
            }


/*  If different scale factors have been given for different points (i.e.
 *  if "nscale" was supplied greater than 1), then we need to sort the
 *  positions returned by SXFindRadius into order of increasing distance
 *  from the current sample position so that the correct scale factor is
 *  used for each position. (SXFindClose returns positions already sorted but
 *  SXFindRadius doesn't). */

            if( nscale > 1 ) SXSort( nres, disres, posres );

         }


/*  Initialise the sums for this output position. The sum of the weights
 *  is accumulated in "wsum" and the sum of the weighted input data values
 *  is accumulated in the current position in the output data arrays. */

         wsum = 0.0;

         for( fld=0; fld<nfld; fld++ ){
            outfld = next[fld];
            for( k=0; k<veclen[fld]; k++ ) *(outfld++) = 0.0;
         }


/*  Loop round each of the contributing input positions. */

         for( j=0; j<nres; j++ ){


/*  Calculate the weight for this input position. Exponentiation can easily
 *  cause  overflows. Trap such cases and set a flag (badwgt). */

            badwgt = 0;

            if( power == 0.0 ){
               wgt = 1.0;

            } else {

               if( j < nscale ) {
                  temp = disres[j]*rscale[j];
               } else {
                  temp = disres[j]*lastrscale;
               }

               if( temp > 0.0 ){
                  temp = (float) log( (double) temp );

                  if( ( k1 > 0.0 && temp < k1 ) ||
                      ( k1 < 0.0 && temp > k1 ) ){
                     wgt = (float) exp( (double) hpower*temp );

                     if( coexp != 0.0 ) {
                        if( ( k2 > 0.0 && wgt < k2 ) ||
                            ( k2 < 0.0 && wgt > k2 ) ){
                           wgt = (float) exp( (double) coexp*wgt );
                        } else {
                           badwgt = 1;
                        }
                     }

                  } else {
                     badwgt = 1;
                  }

               } else {

                  if( coexp != 0.0 ) {
                     wgt = 1.0;
                  } else {
                     wgt = 0.0;
                  }
               }

            }


/*  If the weight overflowed, ignore the remaining input positions. */

            if( badwgt ) break;


/*  Increment the sums */

            wsum += wgt;

            for( fld=0; fld<nfld; fld++ ){

               outfld = next[fld];
               infld = indata[fld] + posres[j]*veclen[fld];

               for( k=0; k<veclen[fld]; k++ ) {
                  *(outfld++) += wgt*(*(infld++));
               }

            }

         }


/*  Store the output data values for each field... */

         for( fld=0; fld<nfld; fld++ ){
            outfld = next[fld];


/*  If the output value is not bad, modify the values stored in the output
 *  array (the weighted sum) if required. */

            if( !badwgt ){

               if( type == 0 ){                /*  Weighted mean */

                  if( wsum != 0.0 ){
                     for( k=0; k<veclen[fld]; k++ ) *(outfld++) /= wsum;

                  } else {
                     for( k=0; k<veclen[fld]; k++ ) *(outfld++) = FLT_MAX;
                     outbad[fld]++;
                  }


               } else if( type == 1 ){        /* Weighted sum */

                  for( k=0; k<veclen[fld]; k++ ) *(outfld++);


               } else if( type == 2 ){         /*  Sum of weights */

                  *(outfld++) = wsum;

               }


/*  If the output value is undefined, store "bad" data values (the maximum
 *  floating point value). */

            } else {
               for( k=0; k<veclen[fld]; k++ ) *(outfld++) = FLT_MAX;
               outbad[fld]++;
            }


/*  Store the pointer to the next output value for this field. */

            next[fld] = outfld ;

         }


/*  Free the storage used to hold the list of contributing input positions
 *  if storage is allocated inside SXFindRadius (SXFindClose does not
 *  allocate its own results arrays). */

         if( nclose == INT_MAX ){
            DXFree( (Pointer) posres );
            DXFree( (Pointer) disres );
         }

      }


/*  Finish the progress messages. */

      if( noutpos >= 2000 ) DXMessage("SXRegrid: Done all %d positions",noutpos );


/*  Free the storage used to hold the list of contributing input positions
 *  if storag ewas allocated by this function (i.e. if SXFindClose was
 *  used instead of SXFindRadius). */

      if( nclose < INT_MAX ){
         DXFree( (Pointer) posres );
         DXFree( (Pointer) disres );
      }

      DXFree( (Pointer) next );

      return( OK );

}


/*------------------------------------------------------------------------*/

int *SXInitClose( int ndim, int npos, float *pos, float *lbnd,
                  float *ubnd, float *lbndl, int *gdim, float *rdcell ){
/*
*+
*  Name:
*     SXInitClose

*  Purpose:
*     Initialise work arrays needed by StarFindClose

*  Language:
*     ANSI C

*  Prototype:
*     int *SXInitClose( int ndim, int npos, float *pos,
*                         float *lbnd, float *ubnd, float *lbndl, int *gdim,
*                         float *rdcell );

*  Description:
*     This function establishes values needed for subsequent use by
*     SXFindClose and SXFindRadius. It reserves storage for a "work array",
*     fills it with the required values, and returns a pointer to it as the
*     function value. A null pointer is returned and the DX error code is set
*     if an error occurs. Values are also returned in arguments "lbndl", "gdim"
*     and "rdcell". These values should be passed unchanged to
*     SXFindClose or SXFindRadius.
*
*     The volume enclosed by the supplied bounds (arguments "ubnd" and
*     lbnd") is divided up into a regular grid of identical cubic cells.
*     Each cell in this grid is identified by a unique index (in the style
*     of a C array). The number of cells used is such that there is an
*     average of four positions in each cell. The supplied positions are
*     then located within this grid. In elements 0 to npos-1, the returned
*     work array holds the indices of the cells in which each position is
*     located (i.e. element 23 of the work array holds the index of the cell
*     in which position number 23 is located). Positions which are outside
*     the supplied bounds have -1 stored in this part of the work array.
*
*     Elements npos to 2*npos-1 of the returned work array contains the
*     indices of the positions which fall in each cell, while elements
*     2*npos to 2*npos+ngrid-1 (where ngrid is the number of cells in the
*     grid) contain the offset (from element npos) to the start of each
*     cell's position list. Thus element 2*npos+5 gives you the offset to
*     the start of the position list for cell 5. If this offset is "n", the
*     indices of all the supplied positions which fall within cell 5 are
*     listed starting at element npos+n. The last element of the work array
*     holds the offset (from element npos) of the first unused element of
*     the work array.
*
*     With this information, it is possible to quickly get a list of all
*     the supplied positions which fall within any given grid cell.

*  Arguments:
*     ndim = int (Given)
*        The number of dimensions describing each position.
*     npos = int (Given)
*        The number of positions.
*     pos = float * (Given)
*        A pointer to the start of the array holding the positions.
*        This array should hold "npos" vectors of dimension "ndim".
*     lbnd = float * (Given)
*        A pointer to the start of the array holding the lower bounds of
*        the volume to be covered by the grid. This array should have
*        ndim elements.
*     ubnd = float * (Given)
*        A pointer to the start of the array holding the upper bounds of
*        the volume to be covered by the grid. This array should have
*        ndim elements.
*     lbndl = float * (Returned)
*        A pointer to the start of an array in which are returned the
*        actual lower bounds used for the grid.  This array should have
*        ndim elements. It may be necessary to extend the supplied lower
*        bounds to fit a whole number of cells along each axis. The upper
*        bounds actually used can be determined from "lbndl", "gdim" and
*        "rdcell".
*     gdim = int * (Returned)
*        A pointer to the start of an array in which are returned the
*        number of cells on each axis of the grid. This array should have
*        ndim elements.
*     rdcell = float * (Returned)
*        The reciprocal of the length of one side of a grid cell is stored at
*        the location specified by this pointer.

*  Returned Value:
*     A pointer to the start of the work array is returned, or NULL if an
*     error occurs (in which case the DX error code is also set). This
*     array should be freed when it is no longer needed by calling DXFree.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      char   bad;         /* Is position out of bounds? */
      int    cell;        /* Cell index */
      float  dcell;       /* The size of a grid cell */
      int    i;           /* Loop count */
      int    ii[3];       /* 3D cartesian indices */
      int    index;       /* 1D vector index */
      int    j;           /* Loop count */
      int    ngrid;       /* total no. of cells in grid */
      float *next;        /* Pointer to next position value */
      int    nin;         /* Number of positions within bounds */
      char   ok;          /* Is position within bounds? */
      int    temp;        /* Temporary storage */
      int    ul[3];       /* Cartesian upper bounds of current box */
      float  vol;         /* Volume encompassed by supplied bounds. */
      int   *work;        /* Pointer to the start of the work space */
      int   *work1;       /* Pointer to the start of the 1st work array */
      int   *work2;       /* Pointer to the start of the 2nd work array */
      int   *work3;       /* Pointer to the start of the 3rd work array */


/*  Check that the supplied value for "ndim" is OK. */

      if( ndim < 1 || ndim > 3 ) {
         DXSetError( ERROR_INTERNAL, "Bad number of dimensions (%d) supplied to SXInitClosest.", ndim );
         return( NULL );
      }


/*  Check that the supplied grid bounds are OK. */

      for( i=0; i<ndim; i++ ){
         if( lbnd[i] >= ubnd[i] ) {
            DXSetError( ERROR_INTERNAL, "Bad bounds for dimensions %d (%f:%f) supplied to DXInitClosest.", i, lbnd[i], ubnd[i] );
            return( NULL );
         }
      }


/*  Find the number of positions which are within the given bounds. */

      nin = 0;

      for( j=0; j<npos; j++ ){
         next = pos + j*ndim;

         for( i=0; i<ndim; i++ ){
            if( bad = ( (*next) < lbnd[i] || (*next) > ubnd[i] ) ) continue;
            next++;
         }
         if( !bad ) nin++;

      }


/*  Find the size of each grid cell. This is the cell size which gives
 *  (on average) 1 position per cell. */

      vol = 1.0;
      for( i=0; i<ndim; i++ ) vol = vol*( ubnd[i] - lbnd[i] );
      dcell = (float) pow( (double) vol/( (float) nin ), 1.0/( (float) ndim ) );
      *rdcell = 1.0/dcell;


/*  Find the dimensions of the grid, and find new lower bounds such that
 *  the grid encompasses an exact number of grid cells in each dimension.  */

      for( i=0; i<3; i++ ) {
         gdim[i] = 1;
         lbndl[i] = 0;
      }

      for( i=0; i<ndim; i++ ) {
         gdim[i] = 1 + (int) ( ( ubnd[i] - lbnd[i] )*(*rdcell) );
         lbndl[i] = 0.5*( ubnd[i] + lbnd[i] - gdim[i]*dcell );
      }


/*  Find the total number of cells in the grid. */

      ngrid = 1;
      for( i=0; i<ndim; i++) ngrid = ngrid*gdim[i];


/*  Obtain integer work space. Two elements for every supplied position, plus
 *  one element for every grid cell (plus 1), is needed. */

      work = (int *) DXAllocate( sizeof( int )*( 2*npos + ngrid + 1 ) );

      if( work == NULL ) {
         return( NULL );
      }


/*  Divide this work array up into three separate arrays. */

      work1 = work;
      work2 = work1 + npos;
      work3 = work2 + npos;


/*  Initialise work3 to hold zeros. */

      for( i=0; i<ngrid; i++ ) work3[i] = 0;


/*  For each supplied position... */

      for( j=0; j<npos; j++ ){


/*  Find the 3D cartesian indices of the grid cell in which this position is
 *  located. Un-used dimensions are given index zero. If any positions are
 *  outside the supplied bounds, dont use them. */

         for( i=0; i<3; i++ ) ii[i] = 0;

         ok = 1;
         for( i=0; i<ndim; i++ ){
            ii[i] = (int)( ( pos[j*ndim + i] - lbndl[i] )*(*rdcell) ) ;
            if( ii[i] < 0 || ii[i] >= gdim[i] ) ok = 0;
         }


/*  Convert these Cartesian indices to an offset into the 1D cell index
 *  and store this index in the first work array. X ("pos" index 0) varies
 *  fastest in the 1D vector, then Y ("pos" index 1), then Z ("pos" index 2).
 */
         if( ok ) {
            index = ii[0] + gdim[0]*( ii[1] + gdim[1]*ii[2] );
            work1[j] = index;


/*  Increment the number of positions in this grid cell. These population
 *  counts are stored in the 3rd work array. */

            work3[index]++;


/*  Assign a negative cell number to positions which are out of bounds. */

         } else {
            work1[j] = -1;
         }

      }


/*  Convert the grid cell populations in work3 into indices in work2 at
 *  which to start the list of position located within each grid cell. */

      index = 0;
      for( j=0; j<ngrid; j++ ){
         temp = work3[j];
         work3[j] = index;
         index += temp;
      }


/*  Put the index of each position into work2 so that positions in the
 *  same grid cell are grouped together. Ignore out of bounds positions. */

      for( j=0; j<npos; j++ ){
         cell = work1[j];
         if( cell >= 0 ) {
            index = work3[cell]++;
            work2[index] = j;
         }
      }


/*  Restore the previous contents of work3 (i.e. the index in work2 at
 *  which the list of positions in each cell starts). work3[ngrid] is left
 *  holding the index of the first free element in work2. */

      for( j=ngrid; j>0; j-- ) work3[j] = work3[j-1];
      work3[0] = 0;


/*  Return the initialised work array. */

      return( work );

}


/*------------------------------------------------------------------------*/

Error SXFindClose( int ndim, int npos, float *pos, float *samp,
                     int nclose, float rdcell, float *lbndl, int *gdim,
                     int *work, int *posres, float *disres ){
/*
*+
*  Name:
*     SXFindClose

*  Purpose:
*     Find the positions closest to the supplied sample position.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXFindClose( int ndim, int npos, float *pos, float *samp,
*                          int nclose, float rdcell, float *lbndl, int *gdim,
*                          int *work, int *posres, float *disres );

*  Description:
*     This function finds the "nclose" closest positions (within the list
*     supplied in "pos") to the supplied sample position (supplied in
*     "samp"). It does it much more quickly than the obvious "sledgehammer"
*     approach of sorting the distances to every position.
*
*     Before using this function, the function SXInitClose should be
*     called to initialise the required arrays, etc. These arrays define
*     a grid of cubic cells, and contain look-up-tables which provide a
*     fast method for finding all the positions which fall within any
*     given cell. With this information it is possible to restrict the
*     positions which are checked to those in the neighbourhood of the
*     supplied sample position.
*
*     The search for close positions starts in the cell containing the
*     sample position. These positions are sorted explicitly into increasing
*     distance from the sample position. If this does not provide
*     sufficient close neighbours, then the next "layer" of cells is
*     checked (i.e. all the cells which are adjacent to the central cell).
*     This goes on until sufficient neighbours have been found. Extra
*     layers are then checked just in case any of the located neighbours
*     were in the far corners of the previous layer.

*  Arguments:
*     ndim = int (Given)
*        The number of dimensions describing each position. This should
*        be the same as supplied to the previous call of SXInitClose.
*     npos = int (Given)
*        The number of positions. This should be the same as supplied to
*        the previous call of SXInitClose.
*     pos = float * (Given)
*        A pointer to the start of the array holding the positions.
*        This array should hold "npos" vectors of dimension "ndim". They
*        should be the same as supplied to the previous call of SXInitClose.
*     samp = float * (Given)
*        A pointer to the sample position. This array should hold 1 vector of
*        dimension "ndim".
*     nclose = int (Given)
*        The number of nearest neighbours required.
*     rdcell = float (Given)
*        The reciprocal of the length of one side of a grid cell, as returned
*        by SXInitClose.
*     lbndl = float * (Given)
*        A pointer to the start of an array in which are stored the
*        actual lower bounds used for the grid.  This array should have
*        ndim elements, and should be obtained by calling SXInitClose.
*     gdim = int * (Given)
*        A pointer to the start of an array in which are stored the
*        number of cells on each axis of the grid. This array should have
*        ndim elements, and should be obtained by calling SXInitClose.
*     work = int * (Given)
*        A pointer to the work array returned by SXInitClose.
*     posres = int * (Returned)
*        A pointer to the start of an array in which are returned the indices
*        of the "nclose" closest positions, in order of increasing
*        distance from the sample position. It should contain "nclose"
*        elements. The array is padded with the value -1 if insufficient
*        neighbours (i.e. less than "nclose") can be found, and is filled
*        entirely with the value -1 if the supplied sample position is outside
*        the bounds of the grid created by SXInitClose.
*     disres = float * (Returned)
*        A pointer to the start of an array in which are returned the
*        SQUARED distance from the sample position to the corresponding
*        neighbour position. They are stored in the same order as in
*        "posres". This array should contain "nclose" elements.

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      int    boxsize;     /* Half width in cells of box centred on current sample */
      float  d2;          /* Squared distance from current sample to current position */
      float  dd;          /* Increment between current sample to current position */
      char   doit[3];     /* Is the current cell on an edge of the box? */
      int    i;           /* Loop count */
      int    ii0[3];      /* 3D cartesian indices for samples central cell */
      int    ii[3];       /* 3D cartesian indices */
      int    il[3];       /* Cartesian lower bounds of current box */
      int    index;       /* 1D vector index */
      int    ip;          /* Index at which current position is stored in work2 */
      int    ipos;        /* Index of current position */
      int    ix;          /* Cartesian X index of current cell */
      int    iy;          /* Cartesian Y index of current cell */
      int    iz;          /* Cartesian Z index of current cell */
      int    j;           /* Loop count */
      int    k;           /* Loop count */
      int    maxbox;      /* Maximum box size required to catch all positions */
      int    ndone;       /* No. of face cells found within given bounds */
      int    nfound;      /* No. of nearest neighbours found so far */
      char   ok;          /* Is position within bounds? */
      int    temp;        /* Temporary storage */
      int    ul[3];       /* Cartesian upper bounds of current box */
      int   *work1;       /* Pointer to the start of the 1st work array */
      int   *work2;       /* Pointer to the start of the 2nd work array */
      int   *work3;       /* Pointer to the start of the 3rd work array */


/*  Check that neither output array is null. */
      if( posres == NULL || disres == NULL ){
         DXSetError( ERROR_INTERNAL, "Null output array supplied to SXFindClose.c");
         return( ERROR );
      }


/*  Check that the supplied work array is not null. */

      if( work == NULL ){
         DXSetError( ERROR_INTERNAL, "Null work array supplied to SXFindClose.c");
         return( ERROR );
      }


/*  Locate the variousparts of the work array. */

      work1 = work;
      work2 = work1 + npos;
      work3 = work2 + npos;


/*  Initialise the returned array used to store the indices of the
 *  closest positions to negative values. */

      for( i=0; i<nclose; i++ ) posres[i] = -1;


/*  Find the index of the grid cell containing the sample. */

      for( i=0; i<3; i++ ) ii[i] = 0;

      ok = 1;
      for( i=0; i<ndim; i++ ){
         ii0[i] = (int)( ( samp[i] - lbndl[i] )*rdcell ) ;
         if( ii0[i] < 0 || ii0[i] >= gdim[i] ) ok = 0;
      }


/*  Ignore out of bounds samples. */

      if( ok ) {


/*  Loop until the required closest neighbours have been found.
 */
         boxsize = 0;
         nfound = 0;
         maxbox = INT_MAX;
         ndone = 1;

         while( boxsize < maxbox && ndone > 0 ){


/*  Once sufficient neighbours have been found, it is necessary to do
 *  more boxes, because positions in the corners of the current box may
 *  actually be further away than mid-face positions in the next
 *  larger boxes. */

            if( nfound == nclose ) {
               maxbox = 1 + (int) sqrt( (double) disres[nclose-1] )*rdcell;
            }


/*  Go through all the grid cells within a box of the current size, centred
 *  on the cell containing the current sample. First find the Cartesian
 *  bounds of the box.*/

            for( i=0; i<3; i++ ) {
               il[i] = 0;
               ul[i] = 0;
            }

            for( i=0; i<ndim; i++ ) {
               il[i] = ii0[i] - boxsize;
               ul[i] = ii0[i] + boxsize;
            }


/*  Now loop through each cell, ignoring ones that are outside the bounds
 *  of the cell array. A cell is checked only if it is new this time (i.e.
 *  if it is on one of the edges of the box). */

            ndone = 0;

            for( ix=il[0]; ix<=ul[0]; ix++){
               if( ix >= 0 && ix < gdim[0] ){
                  doit[0] = ( ix == il[0] || ix == ul[0] );

                  for( iy=il[1]; iy<=ul[1]; iy++){
                     if( iy >= 0 && iy < gdim[1] ){
                        doit[1] = (ndim>1) && ( iy == il[1] || iy == ul[1] );

                        for( iz=il[2]; iz<=ul[2]; iz++){
                           if( iz >= 0 && iz < gdim[2] ){
                              doit[2] = (ndim>2) && ( iz == il[2] || iz == ul[2] );

                              if( doit[0] || doit[1] || doit[2] ) {
                                 ndone++;


/*  Find the 1d vector index of the current cell. */

                                 index = ix + gdim[0]*( iy + gdim[1]*iz );


/*  Loop round each position in the current cell. */

                                 for( ip=work3[index]; ip<work3[index+1]; ip++ ){
                                    ipos = work2[ip];


/*  Find the squared distance of this position from the current sample. */

                                    d2 = 0;
                                    for( i=0; i<ndim; i++ ){
                                       dd = samp[i] - pos[ipos*ndim + i];
                                       d2 += dd*dd;
                                    }

/*  Find the index at which this position should be stored in the
 *  returned array. */

                                    i = 0;
                                    while( i<nfound && disres[i] < d2 ){
                                       i++;
                                    }


/*  If there is room for it, add this position into the returned array at
 *  the correct positions. */

                                    if( i < nclose ){

                                       if( i < nfound ){


                                          if( nfound < nclose ){
                                             disres[nfound] = disres[nfound-1];
                                             posres[nfound] = posres[nfound-1];
                                          }
                                          for( k=nfound-1; k>i; k--){
                                             disres[k] = disres[k-1];
                                             posres[k] = posres[k-1];
                                          }
                                       }

                                       disres[i] = d2;
                                       posres[i] = ipos;


/*  Increment the number of closest neightbours found (the arrays have no
 *  room to hold more than the number requested). */

                                       if( nfound < nclose ) nfound++;

                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }


/*  Increment the size of the box centred on the current sample. */

            boxsize++;
         }

      }

      return( OK );

}


/*------------------------------------------------------------------------*/

Error SXFindRadius( int ndim, int npos, float *pos, float *samp,
                      float maxrad, float rdcell, float *lbndl, int *gdim,
                      int *work, int *nres, int **posres, float **disres ){
/*
*+
*  Name:
*     SXFindRadius

*  Purpose:
*     Find all positions within a given distance of the supplied sample
*     position.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXFindRadius( int ndim, int npos, float *pos, float *samp,
*                           float maxrad, float rdcell, float *lbndl,
*                           int *gdim, int *work, int *nres, int **posres,
*                           float **disres );

*  Description:
*     This function finds all positions (within the list supplied in "pos")
*     within a sphere of radius "maxrad" centred on the supplied sample
*     position (supplied in "samp"). It does it much more quickly than the
*     obvious "sledgehammer" approach of checking the distances to every
*     position.
*
*     Before using this function, the function SXInitClose should be
*     called to initialise the required arrays, etc. These arrays define
*     a grid of cubic cells, and contain look-up-tables which provide a
*     fast method for finding all the positions which fall within any
*     given cell. With this information it is possible to restrict the
*     positions which are checked to those in the neighbourhood of the
*     supplied sample position.
*
*     The those cells are searched which could concievably hold positions
*     within the required sphere. If the supplied sample position is
*     outside the bounds of the grid, some initial storage is still
*     allocated for the returned arrays ("posres" and "disres") but no
*     values are placed in the arrays ( "nres" is returned holding zero).
*
*  Arguments:
*     ndim = int (Given)
*        The number of dimensions describing each position. This should
*        be the same as supplied to the previous call of SXInitClose.
*     npos = int (Given)
*        The number of positions. This should be the same as supplied to
*        the previous call of SXInitClose.
*     pos = float * (Given)
*        A pointer to the start of the array holding the positions.
*        This array should hold "npos" vectors of dimension "ndim". They
*        should be the same as supplied to the previous call of SXInitClose.
*     samp = float * (Given)
*        A pointer to the sample position. This array should hold 1 vector of
*        dimension "ndim".
*     maxrad = float (Given)
*        The radius of the sphere in which to look for positions.
*     rdcell = float (Given)
*        The reciprocal of the length of one side of a grid cell, as returned
*        by SXInitClose.
*     lbndl = float * (Given)
*        A pointer to the start of an array in which are stored the
*        actual lower bounds used for the grid.  This array should have
*        ndim elements, and should be obtained by calling SXInitClose.
*     gdim = int * (Given)
*        A pointer to the start of an array in which are stored the
*        number of cells on each axis of the grid. This array should have
*        ndim elements, and should be obtained by calling SXInitClose.
*     work = int * (Given)
*        A pointer to the work array returned by SXInitClose.
*     nres = int * (Returned)
*        The number of positions found within the given distance.
*     posres = int ** (Returned)
*        A pointer to a location at which this function can store a
*        pointer to the start of an array of integers. Storage for this
*        array is allocated by this function, and it should be freed when
*        no longer needed by calling DXFree. The array is returned holding
*        the indices of all the positions found within a sphere of radius
*        "maxrad" centred on the supplied smaple position. These indices
*        occupy elements 0 to ("nres"-1), and are stored in no particular
*        order.
*     disres = float ** (Returned)
*        A pointer to a location at which this function can store a
*        pointer to the start of an array of floats. Storage for this
*        array is allocated by this function, and it should be freed when
*        no longer needed by calling DXFree. The array is returned holding
*        the SQUARED distances of the sample position from each position
*        found within a sphere of radius "maxrad" centred on the supplied
*        sample position. These indices occupy elements 0 to ("nres"-1), and
*        are stored in the same order as the position indices returned in
*        "posres".

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      int    cursize;     /* Current size of the returned arrays */
      float  d2;          /* Squared distance from current sample to current position */
      float  dd;          /* Increment between current sample to current position */
      int    i;           /* Loop count */
      int    ii0[3];      /* 3D cartesian indices for samples central cell */
      int    ii[3];       /* 3D cartesian indices */
      int    il[3];       /* Cartesian lower bounds of current box */
      int    incsize;     /* Amount by which to extend the output arrays */
      int    index;       /* 1D vector index */
      float  initsize;    /* Initial size of the output arrays */
      int    ip;          /* Index at which current position is stored in work2 */
      int    ipos;        /* Index of current position */
      int    ix;          /* Cartesian X index of current cell */
      int    iy;          /* Cartesian Y index of current cell */
      int    iz;          /* Cartesian Z index of current cell */
      int    j;           /* Loop count */
      int    k;           /* Loop count */
      int    maxbox;      /* Maximum box size required to catch all positions */
      float  maxd2;       /* Squared radius of sphere */
      char   ok;          /* Is position within bounds? */
      int    ul[3];       /* Cartesian upper bounds of current box */
      int   *work1;       /* Pointer to the start of the 1st work array */
      int   *work2;       /* Pointer to the start of the 2nd work array */
      int   *work3;       /* Pointer to the start of the 3rd work array */
      float  xrr;         /* Squared X distance between cell centres */
      float  yrr;         /* Squared X and Y  distance between cell centres */
      float  zrr;         /* Squared X, Y and Z distance between cell centres */


/*  Check that neither output array is null. */
      if( nres == NULL || posres == NULL || disres == NULL ){
         DXSetError( ERROR_INTERNAL, "Null output location supplied to SXFindRadius.c");
         return( ERROR );
      }


/*  Check that the supplied work array is not null. */

      if( work == NULL ){
         *nres = 0;
         *posres = NULL;
         *disres = NULL;

         DXSetError( ERROR_INTERNAL, "Null work array supplied to SXFindRadius.c");
         return( ERROR );
      }


/*  Initialise the number of positions found within the sphere of radius
 *  "maxrad". */

      *nres = 0;


/*  Reserve storage for the output arrays. These arrays are extended
 *  if required later on in this function. */

      initsize = 30.0;
      for( i=0; i<ndim; i++) {
         initsize *= rdcell*maxrad;
      }
      cursize = (int) initsize;
      if( cursize < 1 ) cursize = 1;

      incsize = (int) initsize*0.2;
      if( incsize < 1 ) incsize = 1;

      *posres = (int *) DXAllocate( sizeof( int )*cursize );
      *disres = (float *) DXAllocate( sizeof( float )*cursize );

      if( *disres == NULL || *posres == NULL ){
         DXFree( (Pointer) *posres );
         DXFree( (Pointer) *disres );
         *posres = NULL;
         *disres = NULL;
          return( ERROR );
      }


/*  Find the index of the grid cell containing the sample. */

      for( i=0; i<3; i++ ) ii[i] = 0;

      ok = 1;
      for( i=0; i<ndim; i++ ){
         ii0[i] = (int)( ( samp[i] - lbndl[i] )*rdcell ) ;
         if( ii0[i] < 0 || ii0[i] >= gdim[i] ) ok = 0;
      }


/*  Ignore out of bounds samples. */

      if( ok ) {


/*  Locate the various parts of the work array. */

         work1 = work;
         work2 = work1 + npos;
         work3 = work2 + npos;


/*  Store the square of the radius of the sphere. */

         maxd2 = maxrad*maxrad;


/*  Store the maximum box size requuired to find all positions within
 *  the given distance. */

         maxbox = 1 + (int) maxrad*rdcell;


/*  Go through all the grid cells within a box of the maximum size, centred
 *  on the cell containing the current sample. First find the Cartesian
 *  bounds of the box.*/

         for( i=0; i<3; i++ ) {
            il[i] = 0;
            ul[i] = 0;
         }

         for( i=0; i<ndim; i++ ) {
            il[i] = ii0[i] - maxbox;
            ul[i] = ii0[i] + maxbox;
         }


/*  Now loop through each cell, ignoring ones that are outside the bounds
 *  of the cell array. */


         for( ix=il[0]; ix<=ul[0]; ix++){
            if( ix >= 0 && ix < gdim[0] ){

               for( iy=il[1]; iy<=ul[1]; iy++){
                  if( iy >= 0 && iy < gdim[1] ){

                     for( iz=il[2]; iz<=ul[2]; iz++){
                        if( iz >= 0 && iz < gdim[2] ){


/*  Find the 1d vector index of the current cell. */

                              index = ix + gdim[0]*( iy + gdim[1]*iz );


/*  Loop round each position in the current cell. */

                              for( ip=work3[index]; ip<work3[index+1]; ip++ ){
                                 ipos = work2[ip];


/*  Find the squared distance of this position from the current sample. */

                                 d2 = 0;
                                 for( i=0; i<ndim; i++ ){
                                    dd = samp[i] - pos[ipos*ndim + i];
                                    d2 += dd*dd;
                                 }


/*  If this position is within the given radius... */

                                 if( d2 <= maxd2 ){


/*  If the returned arrays are full, extend them. */

                                    if( (*nres) == cursize ){
                                       cursize += incsize;

                                       *posres = (int *) DXReAllocate( (Pointer)
                                                      *posres, sizeof( int )*cursize );
                                       *disres = (float *) DXReAllocate( (Pointer)
                                                      *disres, sizeof( float )*cursize );

                                       if( *disres == NULL || *posres == NULL ){
                                          DXFree( (Pointer) *posres );
                                          DXFree( (Pointer) *disres );

                                          *nres = 0;
                                          *posres = NULL;
                                          *disres = NULL;

                                          return( ERROR );
                                       }
                                    }


/*  Add this position into the returned array at the correct positions. */

                                    (*disres)[(*nres)] = d2;
                                    (*posres)[(*nres)] = ipos;


/*  Increment the number of positions found. */

                                    (*nres)++;


                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }

      return( OK );

}


/*------------------------------------------------------------------------*/

void SXSort( int n, float *d, int *m ){
/*
*+
*  Name:
*     SXSort

*  Purpose:
*     Sorts the supplied arrays into increasing order.

*  Language:
*     ANSI C

*  Prototype:
*     void SXSort( int n, float *d, float *m );

*  Description:
*     A simple bubblesort is used to order the values in the "d" array
*     into increasing order. The values in the "m" array are shuffled to
*     maintain the correspondance between values in the two arrays.

*  Arguments:
*     n = int (Given)
*        The number of values in each array.
*     d = float * (Given and Returned)
*        A pointer to the start of the array holding the values to be
*        sorted.
*     m = int * (Given and Returned)
*        A pointer to the start of the array holding integer identifiers
*        associated with each value in "d".

*  Returned Value:
*     void

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      int i,top,itmp;
      float tmp;
      char more;

      more = 1;
      top = n;

      while( more ){
         more = 0;

         for( i=1; i<top; i++ ){
            if( d[i] < d[i-1] ){

               more = 1;

               tmp = d[i];
               d[i] = d[i-1];
               d[i-1] = tmp;

               itmp = m[i];
               m[i] = m[i-1];
               m[i-1] = itmp;

            }
         }

         top--;

      }

}



/*------------------------------------------------------------------------*/

float *SXGet1r( char *name, Object in, int *n ){
/*
*+
*  Name:
*     SXGet1r

*  Purpose:
*     Obtains a list of floating point values from a DX parameter.

*  Language:
*     ANSI C

*  Prototype:
*     float *SXGet1r( char *name, Object in, int *n );

*  Description:
*     Checks the supplied object to see that it contains an array of
*     real scalar values, converts them to type float and returns a pointer
*     to the data.

*  Arguments:
*     name = char * (Given)
*        The name of the parameter to refer to in the text of any error
*        messages which are generated by this function.
*     in= Object (Given)
*        The DX object associated with the parameter.
*     n = int * (Returned)
*        A pointer to the place at which the number of values obtained is
*        writtten.

*  Returned Value:
*     A pointer to the an array holding the supplied values, or NULL if
*     an error occurs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      Category categ;
      int rank,shape[4];
      float *data;
      Array temp;


/*  Abort if the supplied object is not an array. */

      if( DXGetObjectClass( in ) != CLASS_ARRAY ){
         DXSetError( ERROR_BAD_CLASS,
                     "parameter \"%s\" should be a list of scalar values.",
                     name );
         return( NULL );
      }


/*  Get information about the array. */

      if( !DXGetArrayInfo( (Array) in, n, NULL, &categ, &rank, shape ) ){
         DXSetError( ERROR_UNEXPECTED,
                     "Unable to get information about parameter \"%s\".",
                     name );
         return( NULL );
      }


/*  Check that it holds REAL values */

      if( categ != CATEGORY_REAL ) {
         DXSetError( ERROR_DATA_INVALID,
                     "REAL values required for parameter \"%s\".",
                     name );
         return( NULL );
      }


/*  Convert the array type to FLOAT */

      temp = DXArrayConvertV( (Array) in, TYPE_FLOAT, CATEGORY_REAL,
                              rank, shape );


/*  A single vector is equivalent to a list of scalars */

      if( rank == 1 && *n == 1 ){
         rank = 0;
         *n = shape[0];
      }


/*  Check that it is now a scalar list */

      if( rank > 0 ) {
         DXSetError( ERROR_DATA_INVALID,
                     "parameter \"%s\" should be a 1-D list of scalar values.",
                     name );
         return( NULL );
      }


/*  Return a pointer to the data . */

      data = (float *) DXGetArrayData( (Array) temp );

      return( data );

}

/*------------------------------------------------------------------------*/

int *SXGet1i( char *name, Object in, int *n ){
/*
*+
*  Name:
*     SXGet1i

*  Purpose:
*     Obtains a list of integer values from a DX parameter.

*  Language:
*     ANSI C

*  Prototype:
*     int *SXGet1i( char *name, Object in, int *n );

*  Description:
*     Checks the supplied object to see that it contains an array of
*     real integer values, converts them to type int and returns a pointer
*     to the data.

*  Arguments:
*     name = char * (Given)
*        The name of the parameter to refer to in the text of any error
*        messages which are generated by this function.
*     in= Object (Given)
*        The DX object associated with the parameter.
*     n = int * (Returned)
*        A pointer to the place at which the number of values obtained is
*        writtten.

*  Returned Value:
*     A pointer to the an array holding the supplied values, or NULL if
*     an error occurs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      Category categ;
      int rank,shape[4];
      int *data;
      Array temp;


/*  Abort if the supplied object is not an array. */

      if( DXGetObjectClass( in ) != CLASS_ARRAY ){
         DXSetError( ERROR_BAD_CLASS,
                     "parameter \"%s\" should be a list of scalar values.",
                     name );
         return( NULL );
      }


/*  Get information about the array. */

      if( !DXGetArrayInfo( (Array) in, n, NULL, &categ, &rank, shape ) ){
         DXSetError( ERROR_UNEXPECTED,
                     "Unable to get information about parameter \"%s\".",
                     name );
         return( NULL );
      }


/*  Check that it holds REAL values */

      if( categ != CATEGORY_REAL ) {
         DXSetError( ERROR_DATA_INVALID,
                     "REAL values required for parameter \"%s\".",
                     name );
         return( NULL );
      }


/*  Convert the array type to INT */

      temp = DXArrayConvertV( (Array) in, TYPE_INT, CATEGORY_REAL,
                              rank, shape );


/*  A single vector is equivalent to a list of scalars */

      if( rank == 1 && *n == 1 ){
         rank = 0;
         *n = shape[0];
      }


/*  Check that it is now a scalar list */

      if( rank > 0 ) {
         DXSetError( ERROR_DATA_INVALID,
                     "parameter \"%s\" should be a 1-D list of scalar values.",
                     name );
         return( NULL );
      }


/*  Return a pointer to the data . */

      data = (int *) DXGetArrayData( (Array) temp );

      return( data );

}


/*------------------------------------------------------------------------*/

Error  SXGet0rs( char *param, Object in, float hi, float lo, int nopt,
                 char *opt[], float *rval, int *sval ){
/*
*+
*  Name:
*     SXGet0rs

*  Purpose:
*     Obtains an float or textual value from a given object

*  Language:
*     ANSI C

*  Prototype:
*     Error  SXGet0rs( char *param, Object in, float hi, float lo, int nopt,
*                      char *opt[], float *rval, int *sval );

*  Description:
*     If the supplied object contains a float value it is checked to see if it
*     is within the range specified by "hi" and "lo" (or outside this
*     range if "lo" is greater than or equal to "hi), and an error is
*     reported if it is not. If it is, the float value is returned in
*     "*rval" and "*sval" is set to -1 to indicate that a float value
*     was supplied.
*
*     If the supplied object contains a string, it is compared with the
*     strings supplied in "opt", and the index of the matching string (if
*     any) is returned in "*sval". An error is reported if no string is
*     matched. The comparison is case sensitive and no abreviations are
*     allowed.

*  Arguments:
*     name = char * (Given)
*        The name of the parameter to refer to in the text of any error
*        messages which are generated by this function.
*     in= Object (Given)
*        The DX object associated with the parameter.
*     hi = float (Given)
*        The highest acceptable float value.
*     lo = float (Given)
*        The lowest acceptable float value.
*     nopt = int (Given)
*        The number of textual options supplied in "opt".
*     opt[] = char * (Given)
*        An array of pointers to a set of character strings. These are the
*        allowed textual values for the parameter.
*     rval = float * (Returned)
*        A pointer to the place at which any supplied float value will
*        be stored. Supplied value is unchanged if a textual option was
*        supplied.
*     sval = int * (Returned)
*        A pointer to the place at which the index (within "opt") of the any
*        supplied textual value will be stored. Returned equal to -1 if a
*        numeric value was supplied. No value is returned if NULL is
*        supplied.

*  Returned Value:
*     Returns OK if no error occurs, and ERROR otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      char *text;        /* Pointer to supplied string value */


/*  Check the type of the supplied object. */

      switch( DXGetObjectClass( in ) ){


/*  If it is a numeric array, extract the value from it. */

      case CLASS_ARRAY:

         if( !DXExtractFloat( in, rval ) ){
            DXSetError( ERROR_BAD_PARAMETER, "cannot get numeric value for parameter \"%s\".", param );
            return( ERROR );
         }


/*  Check it is within the supplied bounds */

         if( hi > lo ){
            if( (*rval) < lo || (*rval) > hi ){
               DXSetError( ERROR_BAD_PARAMETER, "value given for parameter \"%s\" (%f) is outside allowed range [%f,%f].", param, *rval, lo, hi );
               return( ERROR );
            }

         } else {
            if( (*rval) < lo && (*rval) > hi ){
               DXSetError( ERROR_BAD_PARAMETER, "value given for parameter \"%s\" (%f) is inside disallowed range [%f,%f].", param, *rval, hi, lo );
               return( ERROR );
            }

         }


/*  Indicate that a numeric value has been supplied. */

         if( sval ) *sval = -1;

         break;


/*  If a string object has been supplied, get a pointer to it. */

      case CLASS_STRING:

         if( !DXExtractString( in, &text ) ){
            DXSetError( ERROR_BAD_PARAMETER, "cannot get string value for parameter \"%s\".", param );
            return( ERROR );
         }


/* Compare it against each supplied value. */

         if( sval ){

            for( (*sval)=0; (*sval)<nopt; (*sval)++ ){
               if( !strcmp( text, opt[ (*sval) ] ) ) break;
            }

            if( (*sval) >= nopt ){
               DXSetError( ERROR_BAD_PARAMETER, "bad value (\"%s\") obtained for parameter \"%s\".", text, param );
               return( ERROR );
            }

         } else {
            DXSetError( ERROR_BAD_PARAMETER, "bad value (\"%s\") obtained for parameter \"%s\".", text, param );
            return( ERROR );
         }

         break;


/*  Report an error if an unexpected object type has been supplied. */

      default:
         DXSetError( ERROR_UNEXPECTED, "bad object type supplied for parameter \"%s\".", param );
         return( ERROR );

      }

      return( OK );

}



/*------------------------------------------------------------------------*/

Error  SXGet0is( char *param, Object in, int hi, int lo, int nopt,
                 char *opt[], int *ival, int *sval ){
/*
*+
*  Name:
*     SXGet0is

*  Purpose:
*     Obtains an integer or textual value from a given object

*  Language:
*     ANSI C

*  Prototype:
*     Error  SXGet0is( char *param, Object in, int hi, int lo, int nopt,
*                      char *opt[], int *ival, int *sval );

*  Description:
*     If the supplied object contains an integer it is checked to see if it
*     is within the range specified by "hi" and "lo" (or outside this
*     range if "lo" is greater than or equal to "hi), and an error is
*     reported if it is not. If it is, the integer value is returned in
*     "*ival" and "*sval" is set to -1 to indicate that an integer value
*     was supplied.
*
*     If the supplied object contains a string, it is compared with the
*     strings supplied in "opt", and the index of the matching string (if
*     any) is returned in "*sval". An error is reported if no string is
*     matched. The comparison is case sensitive and no abreviations are
*     allowed.

*  Arguments:
*     name = char * (Given)
*        The name of the parameter to refer to in the text of any error
*        messages which are generated by this function.
*     in= Object (Given)
*        The DX object associated with the parameter.
*     hi = int (Given)
*        The highest acceptable integer value.
*     lo = int (Given)
*        The lowest acceptable integer value.
*     nopt = int (Given)
*        The number of textual options supplied in "opt".
*     opt[] = char * (Given)
*        An array of pointers to a set of character strings. These are the
*        allowed textual values for the parameter.
*     ival = int * (Returned)
*        A pointer to the place at which any supplied integer value will
*        be stored. Supplied value is unchanged if a textual option was
*        supplied.
*     sval = int * (Returned)
*        A pointer to the place at which the index (within "opt") of the any
*        supplied textual value will be stored. Returned equal to -1 if a
*        numeric value was supplied. No value is returned if supplied
*        equal to NULL.

*  Returned Value:
*     Returns OK if no error occurs, and ERROR otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      char *text;        /* Pointer to supplied string value */


/*  Check the type of the supplied object. */

      switch( DXGetObjectClass( in ) ){


/*  If it is a numeric array, extract the value from it. */

      case CLASS_ARRAY:

         if( !DXExtractInteger( in, ival ) ){
            DXSetError( ERROR_BAD_PARAMETER, "cannot get numeric value for parameter \"%s\".", param );
            return( ERROR );
         }


/*  Check it is within the supplied bounds */

         if( hi > lo ){
            if( (*ival) < lo || (*ival) > hi ){
               DXSetError( ERROR_BAD_PARAMETER, "value given for parameter \"%s\" (%d) is outside allowed range [%d,%d].", param, *ival, lo, hi );
               return( ERROR );
            }

         } else {
            if( (*ival) < lo && (*ival) > hi ){
               DXSetError( ERROR_BAD_PARAMETER, "value given for parameter \"%s\" (%d) is inside disallowed range [%d,%d].", param, *ival, hi, lo );
               return( ERROR );
            }

         }


/*  Indicate that a numeric value has been supplied. */

         if( sval ) *sval = -1;

         break;


/*  If a string object has been supplied, get a pointer to it. */

      case CLASS_STRING:

         if( !DXExtractString( in, &text ) ){
            DXSetError( ERROR_BAD_PARAMETER, "cannot get string value for parameter \"%s\".", param );
            return( ERROR );
         }


/* Compare it against each supplied value. */

         if( sval ){
            for( (*sval)=0; (*sval)<nopt; (*sval)++ ){
               if( !strcmp( text, opt[ (*sval) ] ) ) break;
            }

            if( (*sval) >= nopt ){
               DXSetError( ERROR_BAD_PARAMETER, "bad value (\"%s\") obtained for parameter \"%s\".", text, param );
               return( ERROR );
            }

         } else {
            DXSetError( ERROR_BAD_PARAMETER, "bad value (\"%s\") obtained for parameter \"%s\".", text, param );
            return( ERROR );
         }

         break;


/*  Report an error if an unexpected object type has been supplied. */

      default:
         DXSetError( ERROR_UNEXPECTED, "bad object type supplied for parameter \"%s\".", param );
         return( ERROR );

      }

      return( OK );

}



/*------------------------------------------------------------------------*/

Object SXMakeOut( Object in, Field template, int typecheck, int maxrank,
                  int maxshape, char *dep ){
/*
*+
*  Name:
*     SXMakeOut

*  Purpose:
*     Create an object matching the input object in which all fields are
*     replaced by the supplied template.

*  Language:
*     ANSI C

*  Prototype:
*     Object SXMakeOut( Object in, Field template, int typecheck, int maxrank,
*                       int maxshape, char *dep );

*  Description:
*     A new object is created which has the structure of the supplied "in"
*     object, but in which each field is replaced by a copy of the "template"
*     field. The output fields retain their input names, but all other
*     attributes and components are inherited from "template".
*
*     If any input field component does not satisfy the requirements
*     specified by the arguments (or if a non-REAL input data component is
*     found), then an error is reported.
*
*     Information describing each input/output field pair is stored in a
*     linked list of "Fpair" structures (each structure describes a single
*     field pair), which should be freed when no longer needed. The external
*     variable "head" points to the most recently created "Fpair"
*     structure. The "next" item within each structure points to the
*     previous "Fpair" structure (the "next" item in the first structure
*     is NULL). Each structure contains:
*
*        Field  infld    =  An input field.
*        Field  outfld   =  The corresponding output field.
*        Object pos      =  The "positions" component of the input field.
*        int    postag   =  The "object tag" (a unique integer identifer) for
*                           the input "positions" component.
*        Object data     =  The "data" component of the input field.
*        Type   datatype =  The data type of the input "data" component.
*        int    datalen  =  The length of each input data vector
*        Fpair *next     =  Pointer to the previous Fpair structure.

*  Arguments:
*     in = Object (Given)
*        The input object.
*     template = Field (Given)
*        The template field for the output object.
*     typecheck = int (Given)
*        If 1, then all input field data components must have type
*        TYPE_FLOAT or TYPE_DOUBLE. If 0, no check on data type is performed.
*     maxrank = int (Given)
*        The maximum rank allowed for an input field data component.
*     maxshape = int (Given)
*        The maximum size of the input field data for rank 1 (i.e. the
*        maximum dimensionality of the data if its rank is 1).
*     dep = char * (Given)
*        The dependancy required for the input "data" components. Should
*        be NULL, "positions" or "connections". If NULL, no check is
*        performed.

*  Returned Value:
*     A pointer to the output object, or NULL is an error has occurred.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      Category cat;
      Fpair   *fp;
      int      i;
      Object   in_mem;
      char    *mname;
      Object   name;
      Object   out;
      Object   out_mem;
      int      rank;
      char    *text;         /* Pointer to attribute text */


/*  If the supplied input object is a field, create a modifiable copy of
 *  the template field with which to replace the input field. */

      switch( DXGetObjectClass( in ) ){
      case ( CLASS_FIELD ):

         out = DXCopy( (Object) template, COPY_STRUCTURE );
         if( !out ) return( NULL );


/*  Copy the name attribute from the input field to the output field. */

         name = DXGetAttribute( in, "name" );
         DXSetAttribute( out, "name", name );


/*  If the supplied input object is a field, create an "Fpair" structure
 *  describing the field, and put a pointer to this new Fpair structure
 *  into the previous Fpair structure. */

         if( !( fp = (Fpair *) DXAllocate ( sizeof( Fpair ) ) ) ){
            DXDelete( out );
            return( NULL );
         }

         if( head ) {
            fp->next = head;
         } else {
            fp->next = NULL;
         }

         head = fp;


/*  Store the input and output fields. */

         fp->infld = (Field) in;
         fp->outfld = (Field) out;


/*  Store the input positions object and tag value. */

         if( !(fp->pos = DXGetComponentValue( (Field) in, "positions" ) ) ){
            DXSetError( ERROR_DATA_INVALID, "missing \"positions\" in input." );
            goto error;
         }

         if( !( fp->postag = DXGetObjectTag( fp->pos ) ) ) goto error;


/*  Store the input data array. */

         fp->data = DXGetComponentValue( (Field) in, "data" );
         if( !fp->data ){
            DXSetError( ERROR_DATA_INVALID, "missing \"data\" in input." );
            goto error;
         }


/*  Get information about the input data array and check it can be used. */

         if( !DXGetArrayInfo( (Array) fp->data, NULL, &(fp->datatype), &cat, &rank, &(fp->datalen) ) ) goto error;

         if( typecheck == 1 ){
            if( fp->datatype != TYPE_FLOAT && fp->datatype != TYPE_DOUBLE ){
               DXSetError( ERROR_DATA_INVALID, "non-floating point data component found in \"input\"." );
               goto error;
            }
         }

         if( cat != CATEGORY_REAL ){
            DXSetError( ERROR_DATA_INVALID, "data component in \"input\" is not of category REAL." );
            goto error;
         }

         if( rank == 0 ){   /* Scalar data is equivalent to 1-d vector data */
            fp->datalen = 1;

         } else if( rank == 1 && fp->datalen == 1 ){
            rank = 0;

         }

         if( rank > maxrank ){
            DXSetError( ERROR_DATA_INVALID, "rank %d data component found in \"input\".", rank );
            goto error;
         }

         if( fp->datalen > maxshape ){
            DXSetError( ERROR_DATA_INVALID, "%s dimensional data component found in \"input\".", fp->datalen );
            goto error;
         }


/*  Check that the data values are "dependant" on the correct component. */

         if( dep ){
            if( !DXGetStringAttribute( fp->data, "dep", &text ) ){
               DXSetError( ERROR_MISSING_DATA, "data dependancy attribute missing from \"input\".");
               goto error;
            }

            if( strcmp( text, dep ) ){
               DXSetError( ERROR_DATA_INVALID, "\"input\" data components must be dependant on %s.",dep);
               goto error;
            }

         }

/*  Return the new output field */

         return( out );


error:
         DXFree( (Pointer) fp );
         DXDelete( out );
         return( NULL );

         break;


/*  If the supplied input object is a group, create a new output group. */

      case ( CLASS_GROUP ):
         out = (Object) DXNewGroup();
         if( !out ) return( NULL );


/*  Recursively copy each member of the input group to the output. */

         for( i=0; in_mem=DXGetEnumeratedMember( (Group) in, i, &mname ); i++ ){

            out_mem = SXMakeOut( in_mem, template, typecheck, maxrank, maxshape,
                                 dep );
            if( !out_mem ){
               DXDelete( out );
               return( NULL );
            }

            if( mname ) {
               if( !DXSetMember( (Group) out, mname, out_mem ) ) {
                  DXDelete( out_mem );
                  DXDelete( out );
                  return( NULL );
               }
            } else {
               if( !DXSetEnumeratedMember( (Group) out, i, out_mem ) ) {
                  DXDelete( out_mem );
                  DXDelete( out );
                  return( NULL );
               }
            }

         }

         return( out );

         break;

      }

}


/*------------------------------------------------------------------------*/

Error SXGetInvPos( Object field, int nel, int ndim, float *data, char *dep ){
/*
*+
*  Name:
*     SXGetInvPos

*  Purpose:
*     Find all inalid positions or connections.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXGetInvPos( Object field, int nel, int ndim, float *data,
*                        char *dep );

*  Description:
*     For each invalid connection or position (as selected by "dep") in
*     the supplied field, the corresponding "ndim" values supplied in array
*     data[] are replaced by the value FLT_MAX (defined in float.h).

*  Arguments:
*     field = Object (Given)
*        The field from which the "invalid connections/positions" component
*        is to be obtained.
*     nel = int (Given)
*        The number of vector elements in the "data[]" array.
*     ndim = int (Given)
*        The length of each vector element in the "data[]" array.
*     data = float * (Given and Returned)
*        A pointer to an array of "nel" elements, each having "ndim"
*        components. All components of the elements corresponding to
*        invalid positions or connection in "field" are returned set to
*        FLT_MAX.
*     dep = char * (Given)
*        The required invalid component; "positions" or "connections".

*  Returned Value:
*     Returns OK if no error occurs, and ERROR otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      float *d;
      InvalidComponentHandle handle;
      int i;


/*  Return if there is no "invalid positions/connections" component in the
 *  given field. */

      if( !strcmp( dep, "positions" ) ){
         if( !DXGetComponentValue( (Field) field, "invalid positions" ) ){
            return( OK );
         }

      } else {
         if( !DXGetComponentValue( (Field) field, "invalid connections" ) ){
            return( OK );
         }

      }


/*  Get a handle for the invalid positions/connections information. */

      handle = DXCreateInvalidComponentHandle( field, NULL, dep );
      if( !handle ) return( ERROR );


/*  Check each position/connection in the field. */

      for( i=0; i<nel; i++ ){
         if( DXIsElementInvalid( handle, i ) ) {
            for( d=data+i*ndim; d<data+(i+1)*ndim; d++ ) *d = FLT_MAX;
         }
      }


/*  Delete the handle. */

      if( !DXFreeInvalidComponentHandle( handle ) ) return( ERROR );

      return( OK );
}


/*------------------------------------------------------------------------*/

Error SXSetInvPosD( Object field, int nel, int veclen, double *data,
                    char *dep ){
/*
*+
*  Name:
*     SXSetInvPosD

*  Purpose:
*     Sets invalid those positions or connections specified in a supplied
*     DOUBLE array

*  Language:
*     ANSI C

*  Prototype:
*     Error SXSetInvPosD( Object field, int nel, int veclen, double *data,
*                         char *dep );

*  Description:
*     The first component of each element of the supplied "data[]" array is
*     compared with the constant DBL_MAX (defined in float.h). Any
*     matching elements are set invalid in the positions or connections
*     component (specified by "dep") of the supplied field. Any
*     connections or positions which are already invalid in the supplied
*     field, remain invalid.

*  Arguments:
*     field = Object (Given)
*        The field in which the "invalid connections/positions" component
*        is to be stored.
*     nel = int (Given)
*        The number of vector elements in the "data[]" array.
*     veclen = int (Given)
*        The length of each vector element in the "data[]" array.
*     data = double * (Given)
*        A pointer to an array of "nel" elements, each having "veclen"
*        components.
*     dep = char * (Given)
*        The required invalid component; "positions" or "connections".

*  Returned Value:
*     Returns OK if no error occurs, and ERROR otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      double *d;
      InvalidComponentHandle handle;
      int i,j;


/*  Get a handle for the invalid positions/connections information. */

      handle = DXCreateInvalidComponentHandle( field, NULL, dep );
      if( !handle ) return( ERROR );


/*  Flag each invalid element in the field. */

      for( i=0; i<nel; i++ ){
         if( *(data+i*veclen) == DBL_MAX ) {
            for( j=0; j<veclen; j++) *(data+i*veclen+j) = 0.0;
            DXSetElementInvalid( handle, i );
         }
      }


/*  Save the invalid element information in the output field. */

      if( !DXSaveInvalidComponent( (Field) field, handle ) ) return( ERROR );


/*  Ensure that any "invalid connections" component is consistent with the
 *  new "invalid positions" component, and remove unreferenced positions. */

      if( !strcmp( dep, "positions" ) ) DXInvalidateConnections( field );
      DXInvalidateUnreferencedPositions( field );


/*  Delete the handle. */

      if( !DXFreeInvalidComponentHandle( handle ) ) return( ERROR );

      return( OK );
}



/*------------------------------------------------------------------------*/

Error SXSetInvPosF( Object field, int nel, int veclen, float *data,
                    char *dep ){
/*
*+
*  Name:
*     SXSetInvPosF

*  Purpose:
*     Sets invalid those positions or connections specified in a supplied
*     FLOAT array

*  Language:
*     ANSI C

*  Prototype:
*     Error SXSetInvPosF( Object field, int nel, int veclen, float *data,
*                         char *dep );

*  Description:
*     The first component of each element of the supplied "data[]" array is
*     compared with the constant FLT_MAX (defined in float.h). Any
*     matching elements are set invalid in the positions or connections
*     component (specified by "dep") of the supplied field. Any
*     connections or positions which are already invalid in the supplied
*     field, remain invalid.

*  Arguments:
*     field = Object (Given)
*        The field in which the "invalid connections/positions" component
*        is to be stored.
*     nel = int (Given)
*        The number of vector elements in the "data[]" array.
*     veclen = int (Given)
*        The length of each vector element in the "data[]" array.
*     data = float * (Given)
*        A pointer to an array of "nel" elements, each having "veclen"
*        components.
*     dep = char * (Given)
*        The required invalid component; "positions" or "connections".

*  Returned Value:
*     Returns OK if no error occurs, and ERROR otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      float *d;
      InvalidComponentHandle handle;
      int i,j;


/*  Get a handle for the new invalid positions/connections information. */

      handle = DXCreateInvalidComponentHandle( field, NULL, dep );
      if( !handle ) return( ERROR );


/*  Flag each invalid position/connection in the field. */

      for( i=0; i<nel; i++ ){
         if( *(data+i*veclen) == FLT_MAX ) {
            for( j=0; j<veclen; j++) *(data+i*veclen+j) = 0.0;
            DXSetElementInvalid( handle, i );
         }
      }


/*  Save the invalid positions/connections information in the output field. */

      if( !DXSaveInvalidComponent( (Field) field, handle ) ) return( ERROR );


/*  Ensure that any "invalid connections" component is consistent with the
 *  new "invalid positions" component, and remove unreferenced positions. */

      if( !strcmp( dep, "positions" ) ) DXInvalidateConnections( field );
      DXInvalidateUnreferencedPositions( field );


/*  Delete the handle. */

      if( !DXFreeInvalidComponentHandle( handle ) ) return( ERROR );

      return( OK );
}



/*------------------------------------------------------------------------*/

float *SXGetGrid( Object in, int *nsamp, int *ndim, float *lbnd, float
                  *ubnd, Object *grid ){
/*
*+
*  Name:
*     SXGetGrid

*  Purpose:
*     Get the positions, size and shape from a field, and flag any
*     invalid positions.

*  Language:
*     ANSI C

*  Prototype:
*     float *SXGetGrid( Object in, int *nsamp, int *ndim, float *lbnd,
*                       float *ubnd, Object *grid );

*  Description:
*     An error is reported if the supplied object ("in") is not a field.
*     Otherwise, a copy of the field is made and returned in "*grid". The
*     positions component of the field is checked to make sure that it is
*     of type FLOAT and category REAL, and that each positions is a 1-d
*     vector of length less than 4. An error is reported if any of these
*     conditions are not met. The number of positions, dimensionality of
*     each position and the bounding box of the supplied field are returned.
*     A pointer to the positions array is obtained and returned. Any
*     invalid positions are flagged by filling the "ndim" corresponding values
*     in the returned array with the value FLT_MAX (defined in float.h).

*  Arguments:
*     in= Object (Given)
*        The DX object.
*     nsamp = int * (Returned)
*        A pointer to the location at which to returned the number of
*        positions in the positions component.
*     ndim = int * (Returned)
*        A pointer to the location at which to returned the dimensionality
*        of each position.
*     lbnd = float * (Returned)
*        A pointer to an array of at least "ndim" elements. The first
*        "ndim" elements are returned holding the lower bounds of the
*        field.
*     ubnd = float * (Returned)
*        A pointer to an array of at least "ndim" elements. The first
*        "ndim" elements are returned holding the upper bounds of the
*        field.
*     grid = Object * (Returned)
*        A Pointer to a location at which to return the copy of the
*        supplied field.

*  Returned Value:
*     Returns a pointer to the positions array if no error occurs, and
*     NULL otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      float    bnd;          /* Current bound value */
      Point    box[8];       /* Bounding box */
      Category cat;          /* Grid category */
      Array    gridbox_array;/* The grid box array */
      float   *gridbox;      /* Pointer to grid box array */
      Array    gridpos_array;/* The grid positions array */
      float   *gridpos;      /* Pointer to grid positions array */
      int      i;            /* Loop count */
      int      j;            /* Loop count */
      int      ncorn;        /* No. of corners in the grid bounding box */
      int      rank;         /* Array rank */
      Type     type;         /* Array numeric type */


/*  Check that the object supplied is a field. */

      if( DXGetObjectClass( in ) != CLASS_FIELD ){
         DXSetError( ERROR_DATA_INVALID, "the \"grid\" object is not a field." );
         return( NULL );
      }


/*  Create a modifiable copy of the grid field */

      *grid = DXCopy( in, COPY_STRUCTURE );
      if( !grid ) return( NULL );


/*  Find the positions component of the "grid" field. */

      gridpos_array = (Array) DXGetComponentValue( (Field) *grid, "positions" );
      if( !gridpos_array ){
         DXSetError( ERROR_MISSING_DATA, "the \"grid\" field has no positions." );
         return( NULL );
      }


/*  Get its shape, size and type. Check it is usable. */

      if( !DXGetArrayInfo( gridpos_array, nsamp, &type, &cat, &rank, ndim ) ) return( NULL );

      if( type != TYPE_FLOAT ){
         DXSetError( ERROR_DATA_INVALID, "positions component in \"grid\" is not of type FLOAT." );
         return( NULL );
      }

      if( cat != CATEGORY_REAL ){
         DXSetError( ERROR_DATA_INVALID, "positions component in \"grid\" is not of category REAL." );
         return( NULL );
      }

      if( rank > 1 ){
         DXSetError( ERROR_DATA_INVALID, "rank %d positions component found in \"grid\".", rank );
         return( NULL );
      }

      if( rank == 0 ){   /* Scalar data is equivalent to 1-d vector data */
         rank = 1;
         *ndim = 1;
      }

      if( *ndim > 3 ){
         DXSetError( ERROR_DATA_INVALID, "%d-dimensional positions component found in \"grid\".", *ndim );
         return( NULL );
      }


/*  Get the bounding box component from the grid */

      gridbox_array = (Array) DXGetComponentValue( (Field) *grid, "box" );


/*  If no bounding box was found, create one. */

      if( !gridbox_array ){
         if( !DXBoundingBox( (Object) *grid, box ) ){
            DXSetError( ERROR_UNEXPECTED, "cannot create a bounding box." );
            return( NULL );
         }
         gridbox_array = (Array) DXGetComponentValue( (Field) *grid, "box" );
      }


/*  Get a pointer to the bounding box array and the number of items in
 *  the array. */

      gridbox = (float *) DXGetArrayData( gridbox_array );
      DXGetArrayInfo( gridbox_array, &ncorn, NULL, NULL, NULL, NULL );


/*  Store the bounds for each dimension. */

      for( j=0; j<(*ndim); j++){
         lbnd[j] = FLT_MAX;
         ubnd[j] = FLT_MIN;
      }

      for( i=0; i<ncorn; i++ ){
         for( j=0; j<(*ndim); j++ ){
            bnd = *(gridbox++);
            if( bnd < lbnd[j] ) lbnd[j]=bnd;
            if( bnd > ubnd[j] ) ubnd[j]=bnd;
         }
      }



/*  Get a pointer to the positions values. */

      gridpos = (float *) DXGetArrayData( gridpos_array );


/*  Replace any invalid positions with the value FLT_MAX (defined in
 *  float.h). */

      if( !SXGetInvPos( *grid, *nsamp, *ndim, gridpos, "positions" ) ) return( NULL );


/*  Return the pointer */

      return( gridpos );

}


/*------------------------------------------------------------------------*/



float *SXPut1r( char *name, int n, Object *out ){
/*
*+
*  Name:
*     SXPut1r

*  Purpose:
*     Creates an object to hold a list of floating point values

*  Language:
*     ANSI C

*  Prototype:
*     float *SXPut1r( char *name, int n, Object *out );

*  Description:
*     Creates a new array of type FLOAT with n scalar elements to hold the
*     values of the specified output parameter. Returns a pointer to the
*     array, and the new Array object.

*  Arguments:
*     name = char * (Given)
*        The name of the parameter to refer to in the text of any error
*        messages which are generated by this function.
*     n = int (Given)
*        The number of scalar values associated with the output parameter.
*     out = *Object (Return)
*        A pointer to the DX object to associate with the parameter.

*  Returned Value:
*     A pointer to an un-initialised array of the requested size, or NULL
*     if an error occurs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      float  *data;     /* Pointer to the array data */
      Array   array;    /* The new array */


/*  Make a new scalar array structure to hold the output data values. This
 *  call just describes the structure of the array, it doesn't actually
 *  allocate any space for the array. */

      array = DXNewArray( TYPE_FLOAT, CATEGORY_REAL, 0 );
      if( !array ) goto error;


/*  Allocate space in the array for the output values. */

      if( !DXAddArrayData( array, 0, n, NULL ) ) goto error;


/*  Get a pointer to the array storage. */

      data = (float *)DXGetArrayData( array );
      if( data == NULL ) goto error;


/*  Return the pointer to the array storage, and the data structure. */

      *out = (Object) array;
      return( data );


/*  Arrive here if an error occurred. Report a message and return a NULL. */

error:
      DXSetError( ERROR_UNEXPECTED,
                  "Unable to create output parameter \"%s\".",
                  name );
      *out = NULL;
      return( NULL );

}


/*------------------------------------------------------------------------*/

Error SXBinF( int nfld, int veclen[], int ninpos, float *indata[], int nbin,
              float *outdata[], int *map, int *counts, char type,
              int *outbad ){
/*
*+
*  Name:
*     SXBinF

*  Purpose:
*     Bin scattered fields into a grid of connections using data of type
*     float.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXBinF( int nfld, int veclen[], int ninpos, float *indata[],
*                   int nbin, float *outdata[], int *map, int *counts,
*                   char type, int *outbad );

*  Description:
*     All input data values falling within each output bin are found and
*     summed. The mean, sum or count is returned. Undefined output values
*     are flagged with the value FLT_MAX (defined in float.h).

*  Arguments:
*     nfld = int (Given)
*        The number of matching pairs of input and output data arrays to
*        be processed simultaneously.
*     veclen[] = int (Given)
*        An array holding the length of the output data vectors for each of
*        the "nfld" data arrays. If type is 2 (i.e. counts are to be
*        returned), then veclen should be supplied equal to 1 (the input
*        data values are ignored).
*     ninpos = int (Given)
*        The number of input positions.
*     indata[] = float * (Given)
*        An array of pointers to the start of the "nfld" input data arrays.
*        Each of these input data arrays should hold "ninpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     nbin = int (Given)
*        The number of output bins.
*     outdata[] = float * (Given)
*        An array of pointers to the start of the "nfld" output data arrays.
*        Each of these output data arrays should hold "nbin" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     map = int * (Given)
*        A pointer to the start of an array holding the bin number (plus 1)
*        for each input position. It shouyld contain ninpos elements. A
*        value of zero should be stored for any input positions which are
*        not contained in any valid output bin.
*     counts = int * (Given)
*        A pointer to an integer work array containing nbin elements.
*     type = char (Given)
*        If 0, then the returned output data values are weighted means
*        of the input data values. If 1, then the returned output data
*        values are weighted sums of the input data values. If 2, then
*        the returned output data values are the sum of the weights of
*        the input data values.
*     outbad = int * (Returned)
*        Pointer to an array returned holding the number of invalid
*        positions in each output field.

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      int     cnt;       /* Bin population */
      int     fld;       /* Index of current field */
      int     i;         /* Input position index */
      int     ibin;      /* Bin index */
      float *infld;     /* Pointer to the next input data value */
      int     j;         /* Loop count */
      float *outfld;    /* Pointer to the next output data value */


/*  Initialise all the output values to zero, and indicate that no bad
 *  output values have been found. */

      for( fld=0; fld<nfld; fld++ ) {
         outfld = outdata[fld];
         for( i=0; i<nbin*veclen[fld]; i++ ) *(outfld++) = 0.0;
         for( i=0; i<nbin; i++ ) counts[i] = 0;
         outbad[fld] = 0;
      }


/*  Process each input position. */

      for( i=0; i<ninpos; i++ ){


/*  Get the index of the output bin containing this input position. */

         ibin = map[i] - 1;


/*  Skip input positions which are not contained within valid output
 *  bins. */

         if( ibin >= 0 && ibin < nbin ){


/*  Increment the required output sums. */

            if( type == 0 ) {          /*  output = mean */
               counts[ibin]++;

               for( fld=0; fld<nfld; fld++ ){
                  infld = indata[fld] + i*veclen[fld];
                  outfld = outdata[fld] + ibin*veclen[fld];
                  for( j=0; j<veclen[fld]; j++ ) outfld[j] += infld[j];
               }

            } else if( type == 1 ){    /*  output = sum */
               for( fld=0; fld<nfld; fld++ ){
                  infld = indata[fld] + i*veclen[fld];
                  outfld = outdata[fld] + ibin*veclen[fld];
                  for( j=0; j<veclen[fld]; j++ ) outfld[j] += infld[j];
               }

            } else {                      /*  output = count */
               counts[ibin]++;

            }

         }

      }


/*  If required, form the mean output values. */

      if( type == 0 ){


/*  Find the start of each fields data array in turn. */

         for( fld=0; fld<nfld; fld++ ) {
            outfld = outdata[fld];


/*  Do each bin. */

            for( ibin=0; ibin<nbin; ibin++ ) {


/*  If this bin has some members, divide the stored data values by the
 *  number of members. */

               cnt = counts[ibin];
               if( cnt > 0 ){
                  for( j=0; j<veclen[fld]; j++ ) *(outfld++) /= (float) cnt;


/*  If this bin has no members, store bad output values and increment the
 *  number of bad output bins in this field. */

               } else {
                  for( j=0; j<veclen[fld]; j++ ) *(outfld++) = FLT_MAX;
                  outbad[fld]++;
               }

            }

         }


/*  If required, copy the bin counts to the output arrays. */

      } else if( type == 2 ){

         for( fld=0; fld<nfld; fld++ ) {
            outfld = outdata[fld];
            for( ibin=0; ibin<nbin; ibin++ ) *(outfld++) = (float) counts[ibin];
         }

      }


      return( OK );

}



/*------------------------------------------------------------------------*/
Error SXBinD( int nfld, int veclen[], int ninpos, double *indata[], int nbin,
              double *outdata[], int *map, int *counts, char type,
              int *outbad ){
/*
*+
*  Name:
*     SXBinD

*  Purpose:
*     Bin scattered fields into a grid of connections using data of type
*     DOUBLE.

*  Language:
*     ANSI C

*  Prototype:
*     Error SXBinD( int nfld, int veclen[], int ninpos, double *indata[],
*                   int nbin, double *outdata[], int *map, int *counts,
*                   char type, int *outbad );

*  Description:
*     All input data values falling within each output bin are found and
*     summed. The mean, sum or count is returned. Undefined output values
*     are flagged with the value DBL_MAX (defined in float.h).

*  Arguments:
*     nfld = int (Given)
*        The number of matching pairs of input and output data arrays to
*        be processed simultaneously.
*     veclen[] = int (Given)
*        An array holding the length of the output data vectors for each of
*        the "nfld" data arrays. If type is 2 (i.e. counts are to be
*        returned), then veclen should be supplied equal to 1 (the input
*        data values are ignored).
*     ninpos = int (Given)
*        The number of input positions.
*     indata[] = double * (Given)
*        An array of pointers to the start of the "nfld" input data arrays.
*        Each of these input data arrays should hold "ninpos" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     nbin = int (Given)
*        The number of output bins.
*     outdata[] = double * (Given)
*        An array of pointers to the start of the "nfld" output data arrays.
*        Each of these output data arrays should hold "nbin" vectors with
*        dimensions given by the corresponding element of array "veclen".
*     map = int * (Given)
*        A pointer to the start of an array holding the bin number (plus 1)
*        for each input position. It shouyld contain ninpos elements. A
*        value of zero should be stored for any input positions which are
*        not contained in any valid output bin.
*     counts = int * (Given)
*        A pointer to an integer work array containing nbin elements.
*     type = char (Given)
*        If 0, then the returned output data values are weighted means
*        of the input data values. If 1, then the returned output data
*        values are weighted sums of the input data values. If 2, then
*        the returned output data values are the sum of the weights of
*        the input data values.
*     outbad = int * (Returned)
*        Pointer to an array returned holding the number of invalid
*        positions in each output field.

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      int     cnt;       /* Bin population */
      int     fld;       /* Index of current field */
      int     i;         /* Input position index */
      int     ibin;      /* Bin index */
      double *infld;     /* Pointer to the next input data value */
      int     j;         /* Loop count */
      double *outfld;    /* Pointer to the next output data value */


/*  Initialise all the output values to zero, and indicate that no bad
 *  output values have been found. */

      for( fld=0; fld<nfld; fld++ ) {
         outfld = outdata[fld];
         for( i=0; i<nbin*veclen[fld]; i++ ) *(outfld++) = 0.0;
         outbad[fld] = 0;
      }


/*  Process each input position. */

      for( i=0; i<ninpos; i++ ){


/*  Get the index of the output bin containing this input position. */

         ibin = map[i] - 1;


/*  Skip input positions which are not contained within valid output
 *  bins. */

         if( ibin >= 0 && ibin < nbin ){


/*  Increment the required output sums. */

            if( type == 0 ) {          /*  output = mean */
               counts[ibin]++;

               for( fld=0; fld<nfld; fld++ ){
                  infld = indata[fld] + i*veclen[fld];
                  outfld = outdata[fld] + ibin*veclen[fld];
                  for( j=0; j<veclen[fld]; j++ ) outfld[j] += infld[j];
               }

            } else if( type == 1 ){    /*  output = sum */
               for( fld=0; fld<nfld; fld++ ){
                  infld = indata[fld] + i*veclen[fld];
                  outfld = outdata[fld] + ibin*veclen[fld];
                  for( j=0; j<veclen[fld]; j++ ) outfld[j] += infld[j];
               }

            } else {                      /*  output = count */
               counts[ibin]++;

            }

         }

      }


/*  If required, form the mean output values. */

      if( type == 0 ){


/*  Find the start of each fields data array in turn. */

         for( fld=0; fld<nfld; fld++ ) {
            outfld = outdata[fld];


/*  Do each bin. */

            for( ibin=0; ibin<nbin; ibin++ ) {


/*  If this bin has some members, divide the stored data values by the
 *  number of members. */

               cnt = counts[ibin];
               if( cnt > 0 ){
                  for( j=0; j<veclen[fld]; j++ ) *(outfld++) /= (double) cnt;


/*  If this bin has no members, store bad output values and increment the
 *  number of bad output bins in this field. */

               } else {
                  for( j=0; j<veclen[fld]; j++ ) *(outfld++) = DBL_MAX;
                  outbad[fld]++;
               }

            }

         }


/*  If required, copy the bin counts to the output arrays. */

      } else if( type == 2 ){

         for( fld=0; fld<nfld; fld++ ) {
            outfld = outdata[fld];
            for( ibin=0; ibin<nbin; ibin++ ) *(outfld++) = (double) counts[ibin];
         }

      }


      return( OK );

}


/*------------------------------------------------------------------------*/

Interpolator SXGetIntp( Object in, int *ncon, int *ndim, Object *grid ){
/*
*+
*  Name:
*     SXGetIntp

*  Purpose:
*     Get an interpolator for the given object

*  Language:
*     ANSI C

*  Prototype:
*     Interpolator SXGetIntp( Object in, int *ncon, int *ndim, Object *grid );

*  Description:
*     An error is reported if the supplied object ("in") is not a field.
*     Otherwise, a copy of the field is made and returned in "*grid". The
*     positions component of the field is checked to make sure that it is
*     of type FLOAT and category REAL, and that each positions is a 1-d
*     vector of length less than 4. An error is reported if any of these
*     conditions are not met. The dimensionality of each position and the
*     number of connections of the supplied field are returned. A new
*     connection-dependant integer "data" component is added to grid
*     containing the index of the corresponding connection (starting at 1).
*     An interpolator for the modified grid is obtained and returned.

*  Arguments:
*     in= Object (Given)
*        The DX object.
*     ncon = int * (Returned)
*        A pointer to the location at which to returned the number of
*        connections in the connections component.
*     ndim = int * (Returned)
*        A pointer to the location at which to returned the dimensionality
*        of each position.
*     grid = Object * (Returned)
*        A Pointer to a location at which to return the copy of the
*        supplied field.

*  Returned Value:
*     Returns the interpolator if no error occurs, and NULL otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      Category     cat;           /* Grid category */
      int         *data;          /* Pointer to new data array */
      Array        data_array;    /* New data array */
      Array        gridcon_array; /* The grid connections array */
      Array        gridpos_array; /* The grid positions array */
      int          i;             /* Loop count */
      Interpolator interp;        /* The interpolator */
      int          rank;          /* Array rank */
      Type         type;          /* Array numeric type */


/*  Check that the object supplied is a field. */

      if( DXGetObjectClass( in ) != CLASS_FIELD ){
         DXSetError( ERROR_DATA_INVALID, "the \"grid\" object is not a field." );
         return( NULL );
      }


/*  Create a modifiable copy of the grid field */

      *grid = DXCopy( in, COPY_STRUCTURE );
      if( !grid ) return( NULL );


/*  Find the positions component of the "grid" field. */

      gridpos_array = (Array) DXGetComponentValue( (Field) *grid, "positions" );
      if( !gridpos_array ) {
         DXSetError( ERROR_MISSING_DATA, "the \"grid\" field has no positions" );
         return( NULL );
      }


/*  Get its shape, size and type. Check it is usable. */

      if( !DXGetArrayInfo( gridpos_array, NULL, &type, &cat, &rank, ndim ) ) return( NULL );

      if( type != TYPE_FLOAT ){
         DXSetError( ERROR_DATA_INVALID, "positions component in \"grid\" is not of type FLOAT." );
         return( NULL );
      }

      if( cat != CATEGORY_REAL ){
         DXSetError( ERROR_DATA_INVALID, "positions component in \"grid\" is not of category REAL." );
         return( NULL );
      }

      if( rank > 1 ){
         DXSetError( ERROR_DATA_INVALID, "rank %d positions component found in \"grid\".", rank );
         return( NULL );
      }

      if( rank == 0 ){   /* Scalar data is equivalent to 1-d vector data */
         rank = 1;
         *ndim = 1;
      }

      if( *ndim > 3 ){
         DXSetError( ERROR_DATA_INVALID, "%d-dimensional positions component found in \"grid\".", *ndim );
         return( NULL );
      }


/*  Find the connections component of the "grid" field. */

      gridcon_array = (Array) DXGetComponentValue( (Field) *grid, "connections" );
      if( !gridcon_array ) {
         DXSetError( ERROR_MISSING_DATA, "the \"grid\" field has no connections." );
         return( NULL );
      }


/*  Get its size. */

      if( !DXGetArrayInfo( gridcon_array, ncon, NULL, NULL, NULL, NULL ) ) return( NULL );


/*  Assign a new data component to the grid, which enumerates the
 *  connections, starting at 1 .*/

      data_array = DXNewArray( TYPE_INT, CATEGORY_REAL, 0 );
      if( !data_array ) return( NULL );
      DXSetStringAttribute( (Object) data_array, "dep", "connections" );

      if( !DXAddArrayData( data_array, 0, *ncon, NULL ) ) {
         DXDelete( (Object) data_array );
         return( NULL );
      }

      data = (int *) DXGetArrayData( data_array );

      for( i=1; i<=(*ncon); i++ ) *(data++) = i;

      if( !DXSetComponentValue( (Field) *grid, "data", (Object) data_array ) ){
         DXDelete( (Object) data_array );
         return( NULL );
      }

      if( !DXChangedComponentValues( (Field) *grid, "data" ) ) return( NULL );


/*  Create and return the interpolator. */

      interp = DXNewInterpolator( *grid, INTERP_INIT_IMMEDIATE, -1.0 );
      return( interp );

}
/*------------------------------------------------------------------------*/
