/************************************************************************/
/*                                                                      */
/*                       CFITSIO Lexical Parser                         */
/*                                                                      */
/* This file is one of 3 files containing code which parses an          */
/* arithmetic expression and evaluates it in the context of an input    */
/* FITS file table extension.  The CFITSIO lexical parser is divided    */
/* into the following 3 parts/files: the CFITSIO "front-end",           */
/* eval_f.c, contains the interface between the user/CFITSIO and the    */
/* real core of the parser; the FLEX interpreter, eval_l.c, takes the   */
/* input string and parses it into tokens and identifies the FITS       */
/* information required to evaluate the expression (ie, keywords and    */
/* columns); and, the BISON grammar and evaluation routines, eval_y.c,  */
/* receives the FLEX output and determines and performs the actual      */
/* operations.  The files eval_l.c and eval_y.c are produced from       */
/* running flex and bison on the files eval.l and eval.y, respectively. */
/* (flex and bison are available from any GNU archive: see www.gnu.org) */
/*                                                                      */
/* The grammar rules, rather than evaluating the expression in situ,    */
/* builds a tree, or Nodal, structure mapping out the order of          */
/* operations and expression dependencies.  This "compilation" process  */
/* allows for much faster processing of multiple rows.  This technique  */
/* was developed by Uwe Lammers of the XMM Science Analysis System,     */
/* although the CFITSIO implementation is entirely code original.       */
/*                                                                      */
/*                                                                      */
/* Modification History:                                                */
/*                                                                      */
/*   Kent Blackburn      c1992  Original parser code developed for the  */
/*                              FTOOLS software package, in particular, */
/*                              the fselect task.                       */
/*   Kent Blackburn      c1995  BIT column support added                */
/*   Peter D Wilson   Feb 1998  Vector column support added             */
/*   Peter D Wilson   May 1998  Ported to CFITSIO library.  User        */
/*                              interface routines written, in essence  */
/*                              making fselect, fcalc, and maketime     */
/*                              capabilities available to all tools     */
/*                              via single function calls.              */
/*   Peter D Wilson   Jun 1998  Major rewrite of parser core, so as to  */
/*                              create a run-time evaluation tree,      */
/*                              inspired by the work of Uwe Lammers,    */
/*                              resulting in a speed increase of        */
/*                              10-100 times.                           */
/*   Peter D Wilson   Jul 1998  gtifilter(a,b,c,d) function added       */
/*   Peter D Wilson   Aug 1998  regfilter(a,b,c,d) function added       */
/*   Peter D Wilson   Jul 1999  Make parser fitsfile-independent,       */
/*                              allowing a purely vector-based usage    */
/*   Peter D Wilson   Aug 1999  Add row-offset capability               */
/*   Peter D Wilson   Sep 1999  Add row-range capability to ffcalc_rng  */
/*                                                                      */
/************************************************************************/

#include <limits.h>
#include <ctype.h>
#include "eval_defs.h"
#include "region.h"

typedef struct {
     int  datatype;   /* Data type to cast parse results into for user       */
     void *dataPtr;   /* Pointer to array of results, NULL if to use iterCol */
     void *nullPtr;   /* Pointer to nulval, use zero if NULL                 */
     long maxRows;    /* Max No. of rows to process, -1=all, 0=1 iteration   */
     int  anyNull;    /* Flag indicating at least 1 undef value encountered  */
} parseInfo;

/*  Internal routines needed to allow the evaluator to operate on FITS data  */

static void Setup_DataArrays( int nCols, iteratorCol *cols,
			      long fRow, long nRows );
static int  find_column( char *colName, void *itslval );
static int  find_keywd ( char *key,     void *itslval );
static int  allocateCol( int nCol, int *status );
static int  load_column( int varNum, long fRow, long nRows,
			 void *data, char *undef );

/*---------------------------------------------------------------------------*/
int fffrow( fitsfile *fptr,         /* I - Input FITS file                   */
            char     *expr,         /* I - Boolean expression                */
            long     firstrow,      /* I - First row of table to eval        */
            long     nrows,         /* I - Number of rows to evaluate        */
            long     *n_good_rows,  /* O - Number of rows eval to True       */
            char     *row_status,   /* O - Array of boolean results          */
            int      *status )      /* O - Error status                      */
/*                                                                           */
/* Evaluate a boolean expression using the indicated rows, returning an      */
/* array of flags indicating which rows evaluated to TRUE/FALSE              */
/*---------------------------------------------------------------------------*/
{
   parseInfo Info;
   int naxis, constant;
   long nelem, naxes[MAXDIMS], elem;
   char result;

   if( *status ) return( *status );

   if( ffiprs( fptr, 0, expr, MAXDIMS, &Info.datatype, &nelem, &naxis,
               naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem<0 ) {
      constant = 1;
      nelem = -nelem;
   } else
      constant = 0;

   if( Info.datatype!=TLOGICAL || nelem!=1 ) {
      ffcprs();
      ffpmsg("Expression does not evaluate to a logical scalar.");
      return( *status = PARSE_BAD_TYPE );
   }

   if( constant ) { /* No need to call parser... have result from ffiprs */
      result = gParse.Nodes[gParse.resultNode].value.data.log;
      *n_good_rows = nrows;
      for( elem=0; elem<nrows; elem++ )
	 row_status[elem] = result;
   } else {
      firstrow     = (firstrow>1 ? firstrow : 1);
      Info.dataPtr = row_status;
      Info.nullPtr = NULL;
      Info.maxRows = nrows;
   
      if( ffiter( gParse.nCols, gParse.colData, firstrow-1, 0,
		  parse_data, (void*)&Info, status ) == -1 )
	 *status = 0;  /* -1 indicates exitted without error before end... OK */

      if( *status ) {

	 /***********************/
	 /* Error... Do nothing */
	 /***********************/

      } else {

	 /***********************************/
	 /* Count number of good rows found */
	 /***********************************/

	 *n_good_rows = 0L;
	 for( elem=0; elem<Info.maxRows; elem++ ) {
	    if( row_status[elem]==1 ) ++*n_good_rows;
	 }
      }
   }

   ffcprs();
   return(*status);
}

/*--------------------------------------------------------------------------*/
int ffsrow( fitsfile *infptr,   /* I - Input FITS file                      */
            fitsfile *outfptr,  /* I - Output FITS file                     */
            char     *expr,     /* I - Boolean expression                   */
            int      *status )  /* O - Error status                         */
/*                                                                          */
/* Evaluate an expression on all rows of a table.  If the input and output  */
/* files are not the same, copy the TRUE rows to the output file.  If the   */
/* files are the same, delete the FALSE rows (preserve the TRUE rows).      */
/* Can copy rows between extensions of the same file, *BUT* if output       */
/* extension is before the input extension, the second extension *MUST* be  */
/* opened using ffreopen, so that CFITSIO can handle changing file lengths. */
/*--------------------------------------------------------------------------*/
{
   parseInfo Info;
   int naxis, constant;
   long nelem, rdlen, naxes[MAXDIMS], maxrows, nbuff, nGood, inloc, outloc;
   OFF_T inbyteloc, outbyteloc, hsize;
   long ntodo, freespace;
   unsigned char *buffer, result;
   struct {
      long rowLength, numRows, heapSize;
      OFF_T dataStart, heapStart;
   } inExt, outExt;

   if( *status ) return( *status );

   if( ffiprs( infptr, 0, expr, MAXDIMS, &Info.datatype, &nelem, &naxis,
               naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem<0 ) {
      constant = 1;
      nelem = -nelem;
   } else
      constant = 0;

   /**********************************************************************/
   /* Make sure expression evaluates to the right type... logical scalar */
   /**********************************************************************/

   if( Info.datatype!=TLOGICAL || nelem!=1 ) {
      ffcprs();
      ffpmsg("Expression does not evaluate to a logical scalar.");
      return( *status = PARSE_BAD_TYPE );
   }

   /***********************************************************/
   /*  Extract various table information from each extension  */
   /***********************************************************/

   if( infptr->HDUposition != (infptr->Fptr)->curhdu )
      ffmahd( infptr, (infptr->HDUposition) + 1, NULL, status );
   if( *status ) {
      ffcprs();
      return( *status );
   }
   inExt.rowLength = (long) (infptr->Fptr)->rowlength;
   inExt.numRows   = (infptr->Fptr)->numrows;
   inExt.heapSize  = (infptr->Fptr)->heapsize;
   if( inExt.numRows == 0 ) { /* Nothing to copy */
      ffcprs();
      return( *status );
   }

   if( outfptr->HDUposition != (outfptr->Fptr)->curhdu )
      ffmahd( outfptr, (outfptr->HDUposition) + 1, NULL, status );
   if( (outfptr->Fptr)->datastart < 0 )
      ffrdef( outfptr, status );
   if( *status ) {
      ffcprs();
      return( *status );
   }
   outExt.rowLength = (long) (outfptr->Fptr)->rowlength;
   outExt.numRows   = (outfptr->Fptr)->numrows;
   if( !outExt.numRows )
      (outfptr->Fptr)->heapsize = 0L;
   outExt.heapSize  = (outfptr->Fptr)->heapsize;

   if( inExt.rowLength != outExt.rowLength ) {
      ffpmsg("Output table has different row length from input");
      ffcprs();
      return( *status = PARSE_BAD_OUTPUT );
   }

   /***********************************/
   /*  Fill out Info data for parser  */
   /***********************************/

   Info.dataPtr = (char *)malloc( inExt.numRows * sizeof(char) );
   Info.nullPtr = NULL;
   Info.maxRows = inExt.numRows;
   if( !Info.dataPtr ) {
      ffpmsg("Unable to allocate memory for row selection");
      ffcprs();
      return( *status = MEMORY_ALLOCATION );
   }
   
   if( constant ) { /*  Set all rows to the same value from constant result  */

      result = gParse.Nodes[gParse.resultNode].value.data.log;
      for( ntodo = 0; ntodo<inExt.numRows; ntodo++ )
	 ((char*)Info.dataPtr)[ntodo] = result;
      nGood = (result ? inExt.numRows : 0);

   } else {

      ffiter( gParse.nCols, gParse.colData, 0L, 0L,
	      parse_data, (void*)&Info, status );

      nGood = 0;
      for( ntodo = 0; ntodo<inExt.numRows; ntodo++ )
	 if( ((char*)Info.dataPtr)[ntodo] ) nGood++;
   }

   if( *status ) {
      /* Error... Do nothing */
   } else {
      rdlen  = inExt.rowLength;
      buffer = (unsigned char *)malloc(maxvalue(500000,rdlen) * sizeof(char) );
      if( buffer==NULL ) {
         ffcprs();
         return( *status=MEMORY_ALLOCATION );
      }
      maxrows = 500000L/rdlen;
      nbuff = 0;
      inloc = 1;
      if( infptr==outfptr ) { /* Skip initial good rows if input==output file */
         while( ((char*)Info.dataPtr)[inloc-1] ) inloc++;
	 outloc = inloc;
      } else {
	 outloc = outExt.numRows + 1;
	 if (outloc > 1) 
            ffirow( outfptr, outExt.numRows, nGood, status );
      }

      do {
         if( ((char*)Info.dataPtr)[inloc-1] ) {
            ffgtbb( infptr, inloc, 1L, rdlen, buffer+rdlen*nbuff, status );
            nbuff++;
            if( nbuff==maxrows ) {
               ffptbb( outfptr, outloc, 1L, rdlen*nbuff, buffer,  status );
               outloc += nbuff;
               nbuff = 0;
            }
         }
         inloc++;
      } while( !*status && inloc<=inExt.numRows );

      if( nbuff ) {
	 ffptbb( outfptr, outloc, 1L, rdlen*nbuff, buffer,  status );
         outloc += nbuff;
      }

      if( infptr==outfptr ) {

         if( outloc<=inExt.numRows )
	    ffdrow( infptr, outloc, inExt.numRows-outloc+1, status );

      } else if( inExt.heapSize && nGood ) {

         /* Copy heap, if it exists and at least one row copied */

         /********************************************************/
         /*  Get location information from the output extension  */
         /********************************************************/

         if( outfptr->HDUposition != (outfptr->Fptr)->curhdu )
            ffmahd( outfptr, (outfptr->HDUposition) + 1, NULL, status );
         outExt.dataStart = (outfptr->Fptr)->datastart;
         outExt.heapStart = (outfptr->Fptr)->heapstart;

         /*************************************************/
         /*  Insert more space into outfptr if necessary  */
         /*************************************************/

         hsize     = outExt.heapStart + outExt.heapSize;
         freespace = ( ( (hsize + 2879) / 2880) * 2880) - hsize;
         ntodo     = inExt.heapSize;

         if ( (freespace - ntodo) < 0) {       /* not enough existing space? */
            ntodo = (ntodo - freespace + 2879) / 2880;  /* number of blocks  */
            ffiblk(outfptr, ntodo, 1, status);          /* insert the blocks */
         }
         ffukyj( outfptr, "PCOUNT", inExt.heapSize+outExt.heapSize,
                 NULL, status );

         /*******************************************************/
         /*  Get location information from the input extension  */
         /*******************************************************/

         if( infptr->HDUposition != (infptr->Fptr)->curhdu )
            ffmahd( infptr, (infptr->HDUposition) + 1, NULL, status );
         inExt.dataStart = (infptr->Fptr)->datastart;
         inExt.heapStart = (infptr->Fptr)->heapstart;

         /**********************************/
         /*  Finally copy heap to outfptr  */
         /**********************************/

         ntodo  =  inExt.heapSize;
         inbyteloc  =  inExt.heapStart +  inExt.dataStart;
         outbyteloc = outExt.heapStart + outExt.dataStart + outExt.heapSize;

         while ( ntodo && !*status ) {
            rdlen = minvalue(ntodo,500000);
            ffmbyt( infptr,  inbyteloc,  REPORT_EOF, status );
            ffgbyt( infptr,  rdlen,  buffer,     status );
            ffmbyt( outfptr, outbyteloc, IGNORE_EOF, status );
            ffpbyt( outfptr, rdlen,  buffer,     status );
            inbyteloc  += rdlen;
            outbyteloc += rdlen;
            ntodo  -= rdlen;
         }

         /***********************************************************/
         /*  But must update DES if data is being appended to a     */
         /*  pre-existing heap space.  Edit each new entry in file  */
         /***********************************************************/

         if( outExt.heapSize ) {
            long repeat, offset, j;
            int i;
            for( i=1; i<=(outfptr->Fptr)->tfield; i++ ) {
               if( (outfptr->Fptr)->tableptr[i-1].tdatatype<0 ) {
                  for( j=outExt.numRows+1; j<=outExt.numRows+nGood; j++ ) {
                     ffgdes( outfptr, i, j, &repeat, &offset, status );
                     offset += outExt.heapSize;
                     ffpdes( outfptr, i, j, repeat, offset, status );
                  }
               }
            }
         }

      } /*  End of HEAP copy  */

      free(buffer);
   }

   free(Info.dataPtr);
   ffcprs();

   ffcmph(outfptr, status);  /* compress heap, deleting any orphaned data */
   return(*status);
}

/*---------------------------------------------------------------------------*/
int ffcrow( fitsfile *fptr,      /* I - Input FITS file                      */
            int      datatype,   /* I - Datatype to return results as        */
            char     *expr,      /* I - Arithmetic expression                */
            long     firstrow,   /* I - First row to evaluate                */
            long     nelements,  /* I - Number of elements to return         */
            void     *nulval,    /* I - Ptr to value to use as UNDEF         */
            void     *array,     /* O - Array of results                     */
            int      *anynul,    /* O - Were any UNDEFs encountered?         */
            int      *status )   /* O - Error status                         */
/*                                                                           */
/* Calculate an expression for the indicated rows of a table, returning      */
/* the results, cast as datatype (TSHORT, TDOUBLE, etc), in array.  If       */
/* nulval==NULL, UNDEFs will be zeroed out.  For vector results, the number  */
/* of elements returned may be less than nelements if nelements is not an    */
/* even multiple of the result dimension.  Call fftexp to obtain the         */
/* dimensions of the results.                                                */
/*---------------------------------------------------------------------------*/
{
   parseInfo Info;
   int naxis;
   long nelem1, naxes[MAXDIMS];

   if( *status ) return( *status );

   if( ffiprs( fptr, 0, expr, MAXDIMS, &Info.datatype, &nelem1, &naxis,
               naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem1<0 ) nelem1 = - nelem1;

   if( nelements<nelem1 ) {
      ffcprs();
      ffpmsg("Array not large enough to hold at least one row of data.");
      return( *status = PARSE_LRG_VECTOR );
   }

   firstrow = (firstrow>1 ? firstrow : 1);

   if( datatype ) Info.datatype = datatype;

   Info.dataPtr = array;
   Info.nullPtr = nulval;
   Info.maxRows = nelements / nelem1;
   
   if( ffiter( gParse.nCols, gParse.colData, firstrow-1, 0,
               parse_data, (void*)&Info, status ) == -1 )
      *status=0;  /* -1 indicates exitted without error before end... OK */

   *anynul = Info.anyNull;
   ffcprs();
   return( *status );
}

/*--------------------------------------------------------------------------*/
int ffcalc( fitsfile *infptr,   /* I - Input FITS file                      */
            char     *expr,     /* I - Arithmetic expression                */
            fitsfile *outfptr,  /* I - Output fits file                     */
            char     *parName,  /* I - Name of output parameter             */
            char     *parInfo,  /* I - Extra information on parameter       */
            int      *status )  /* O - Error status                         */
/*                                                                          */
/* Evaluate an expression for all rows of a table.  Call ffcalc_rng with    */
/* a row range of 1-MAX.                                                    */
{
   long start=1, end=LONG_MAX;

   return ffcalc_rng( infptr, expr, outfptr, parName, parInfo,
                      1, &start, &end, status );
}

/*--------------------------------------------------------------------------*/
int ffcalc_rng( fitsfile *infptr,   /* I - Input FITS file                  */
                char     *expr,     /* I - Arithmetic expression            */
                fitsfile *outfptr,  /* I - Output fits file                 */
                char     *parName,  /* I - Name of output parameter         */
                char     *parInfo,  /* I - Extra information on parameter   */
                int      nRngs,     /* I - Row range info                   */
                long     *start,    /* I - Row range info                   */
                long     *end,      /* I - Row range info                   */
                int      *status )  /* O - Error status                     */
/*                                                                          */
/* Evaluate an expression using the data in the input FITS file and place   */
/* the results into either a column or keyword in the output fits file,     */
/* depending on the value of parName (keywords normally prefixed with '#')  */
/* and whether the expression evaluates to a constant or a table column.    */
/* The logic is as follows:                                                 */
/*    (1) If a column exists with name, parName, put results there.         */
/*    (2) If parName starts with '#', as in #NAXIS, put result there,       */
/*        with parInfo used as the comment. If expression does not evaluate */
/*        to a constant, flag an error.                                     */
/*    (3) If a keyword exists with name, parName, and expression is a       */
/*        constant, put result there, using parInfo as the new comment.     */
/*    (4) Else, create a new column with name parName and TFORM parInfo.    */
/*        If parInfo is NULL, use a default data type for the column.       */
/*--------------------------------------------------------------------------*/
{
   parseInfo Info;
   int naxis, constant, typecode, newNullKwd=0;
   long nelem, naxes[MAXDIMS], repeat, width;
   int col_cnt, colNo;
   Node *result;
   char card[81], tform[16], nullKwd[9], tdimKwd[9];
   int hdutype;

   if( *status ) return( *status );
   
   if( ffiprs( infptr, 0, expr, MAXDIMS, &Info.datatype, &nelem, &naxis,
               naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem<0 ) {
      constant = 1;
      nelem = -nelem;
   } else
      constant = 0;

   /*  Case (1): If column exists put it there  */

   colNo = 0;
   if( ffgcno( outfptr, CASEINSEN, parName, &colNo, status )==COL_NOT_FOUND ) {

      /*  Output column doesn't exist.  Test for keyword. */

      /* Case (2): Does parName indicate result should be put into keyword */

      *status = 0;
      if( parName[0]=='#' ) {
	 if( ! constant ) {
	    ffcprs();
	    ffpmsg( "Cannot put tabular result into keyword (ffcalc)" );
	    return( *status = PARSE_BAD_TYPE );
	 }
	 parName++;

      } else if( constant ) {

	 /* Case (3): Does a keyword named parName already exist */

	 if( ffgcrd( outfptr, parName, card, status )==KEY_NO_EXIST ) {
	    colNo = -1;
	 } else if( *status ) {
	    ffcprs();
	    return( *status );
	 }

      } else
	 colNo = -1;

      if( colNo<0 ) {

	 /* Case (4): Create new column */

	 *status = 0;
	 ffgncl( outfptr, &colNo, status );
	 colNo++;
	 ffghdt( outfptr, &hdutype, status );
	 if( parInfo==NULL || *parInfo=='\0' ) {
	    /*  Figure out best default column type  */
	    if( hdutype==BINARY_TBL ) {
	       sprintf(tform,"%ld",nelem);
	       switch( Info.datatype ) {
	       case TLOGICAL:  strcat(tform,"L");  break;
	       case TLONG:     strcat(tform,"J");  break;
	       case TDOUBLE:   strcat(tform,"D");  break;
	       case TSTRING:   strcat(tform,"A");  break;
	       case TBIT:      strcat(tform,"X");  break;
	       }
	    } else {
	       switch( Info.datatype ) {
	       case TLOGICAL:
		  ffcprs();
		  ffpmsg("Cannot create LOGICAL column in ASCII table");
		  return( *status = NOT_BTABLE );
		  break;
	       case TLONG:     strcpy(tform,"I11");     break;
	       case TDOUBLE:   strcpy(tform,"D23.15");  break;
	       case TSTRING:   
	       case TBIT:      sprintf(tform,"A%ld",nelem);  break;
	       }
	    }
	    parInfo = tform;
	 } else if( !(isdigit((int) *parInfo)) && hdutype==BINARY_TBL ) {
	    if( Info.datatype==TBIT && *parInfo=='B' )
	       nelem = (nelem+7)/8;
	    sprintf(tform,"%ld%s",nelem,parInfo);
	    parInfo = tform;
	 }
	 fficol( outfptr, colNo, parName, parInfo, status );
	 if( naxis>1 )
	    ffptdm( outfptr, colNo, naxis, naxes, status );

	 /*  Setup TNULLn keyword in case NULLs are encountered  */

	 ffkeyn("TNULL", colNo, nullKwd, status);
	 if( ffgcrd( outfptr, nullKwd, card, status )==KEY_NO_EXIST ) {
	    *status = 0;
	    if( hdutype==BINARY_TBL ) {
	       long nullVal=0;
	       fits_binary_tform( parInfo, &typecode, &repeat, &width, status );
	       if( typecode==TBYTE )
		  nullVal = UCHAR_MAX;
	       else if( typecode==TSHORT )
		  nullVal = SHRT_MIN;
	       else if( typecode==TINT )
		  nullVal = INT_MIN;
	       else if( typecode==TLONG )
		  nullVal = LONG_MIN;
	       if( nullVal ) {
		  ffpkyj( outfptr, nullKwd, nullVal, "Null value", status );
		  fits_set_btblnull( outfptr, colNo, nullVal, status );
		  newNullKwd = 1;
	       }
	    } else if( hdutype==ASCII_TBL ) {
	       ffpkys( outfptr, nullKwd, "NULL", "Null value string", status );
	       fits_set_atblnull( outfptr, colNo, "NULL", status );
	       newNullKwd = 1;
	    }
	 }

      }

   } else if( *status ) {
      ffcprs();
      return( *status );
   } else {

      /********************************************************/
      /*  Check if a TDIM keyword should be written/updated.  */
      /********************************************************/

      ffkeyn("TDIM", colNo, tdimKwd, status);
      ffgcrd( outfptr, tdimKwd, card, status );
      if( *status==0 ) {
         /*  TDIM exists, so update it with result's dimension  */
         ffptdm( outfptr, colNo, naxis, naxes, status );
      } else if( *status==KEY_NO_EXIST ) {
         /*  TDIM does not exist, so clear error stack and     */
         /*  write a TDIM only if result is multi-dimensional  */
         *status = 0;
         ffcmsg();
         if( naxis>1 )
            ffptdm( outfptr, colNo, naxis, naxes, status );
      }
      if( *status ) {
         /*  Either some other error happened in ffgcrd   */
         /*  or one happened in ffptdm                    */
         ffcprs();
         return( *status );
      }

   }

   if( colNo>0 ) {

      /*  Output column exists (now)... put results into it  */

      int anyNull = 0;
      int nPerLp, i;

      /*************************************/
      /* Create new iterator Output Column */
      /*************************************/

      col_cnt = gParse.nCols;
      if( allocateCol( col_cnt, status ) ) {
	 ffcprs();
	 return( *status );
      }

      fits_iter_set_by_num( gParse.colData+col_cnt, outfptr,
			    colNo, 0, OutputCol );
      gParse.nCols++;

      for( i=0; i<nRngs; i++ ) {
         Info.dataPtr = NULL;
         Info.maxRows = end[i]-start[i]+1;
         if( Info.maxRows < 10 )
            nPerLp = Info.maxRows;
         else
            nPerLp = 0;

         if( ffiter( gParse.nCols, gParse.colData, start[i]-1,
                     nPerLp, parse_data, (void*)&Info, status ) == -1 )
            *status = 0;
         else if( *status ) {
            ffcprs();
            return( *status );
         }
         if( Info.anyNull ) anyNull = 1;
      }

      if( newNullKwd && !anyNull ) {
         ffdkey( outfptr, nullKwd, status );
      }

   } else {

      /* Put constant result into keyword */

      result  = gParse.Nodes + gParse.resultNode;
      switch( Info.datatype ) {
      case TDOUBLE:
	 ffukyd( outfptr, parName, result->value.data.dbl, 15,
		 parInfo, status );
	 break;
      case TLONG:
	 ffukyj( outfptr, parName, result->value.data.lng, parInfo, status );
	 break;
      case TLOGICAL:
	 ffukyl( outfptr, parName, result->value.data.log, parInfo, status );
	 break;
      case TBIT:
      case TSTRING:
	 ffukys( outfptr, parName, result->value.data.str, parInfo, status );
	 break;
      }
   }

   ffcprs();
   return( *status );
}

/*--------------------------------------------------------------------------*/
int fftexp( fitsfile *fptr,      /* I - Input FITS file                     */
            char     *expr,      /* I - Arithmetic expression               */
            int      maxdim,     /* I - Max Dimension of naxes              */
            int      *datatype,  /* O - Data type of result                 */
            long     *nelem,     /* O - Vector length of result             */
            int      *naxis,     /* O - # of dimensions of result           */
            long     *naxes,     /* O - Size of each dimension              */
            int      *status )   /* O - Error status                        */
/*                                                                          */
/* Evaluate the given expression and return information on the result.      */
/*--------------------------------------------------------------------------*/
{
   ffiprs( fptr, 0, expr, maxdim, datatype, nelem, naxis, naxes, status );
   ffcprs();
   return( *status );
}

/*--------------------------------------------------------------------------*/
int ffiprs( fitsfile *fptr,      /* I - Input FITS file                     */
            int      compressed, /* I - Is FITS file hkunexpanded?          */
            char     *expr,      /* I - Arithmetic expression               */
            int      maxdim,     /* I - Max Dimension of naxes              */
            int      *datatype,  /* O - Data type of result                 */
            long     *nelem,     /* O - Vector length of result             */
            int      *naxis,     /* O - # of dimensions of result           */
            long     *naxes,     /* O - Size of each dimension              */
            int      *status )   /* O - Error status                        */
/*                                                                          */
/* Initialize the parser and determine what type of result the expression   */
/* produces.                                                                */
/*--------------------------------------------------------------------------*/
{
   Node *result;
   int  i,lexpr, tstatus = 0;
   static iteratorCol dmyCol;

   if( *status ) return( *status );

   /*  Initialize the Parser structure  */

   gParse.def_fptr   = fptr;
   gParse.compressed = compressed;
   gParse.nCols      = 0;
   gParse.colData    = NULL;
   gParse.varData    = NULL;
   gParse.getData    = find_column;
   gParse.loadData   = load_column;
   gParse.Nodes      = NULL;
   gParse.nNodesAlloc= 0;
   gParse.nNodes     = 0;
   gParse.status     = 0;

   if( ffgkyj(fptr, "NAXIS2", &gParse.totalRows, 0, &tstatus) )
   {
      /* this might be a 1D or null image with no NAXIS2 keyword */
      gParse.totalRows = 0;
   }   
   /*  Copy expression into parser... read from file if necessary  */

   if( expr[0]=='@' ) {
      if( ffimport_file( expr+1, &gParse.expr, status ) ) return( *status );
      lexpr = strlen(gParse.expr);
   } else {
      lexpr = strlen(expr);
      gParse.expr = (char*)malloc( (2+lexpr)*sizeof(char));
      strcpy(gParse.expr,expr);
   }
   strcat(gParse.expr + lexpr,"\n");
   gParse.index    = 0;
   gParse.is_eobuf = 0;

   /*  Parse the expression, building the Nodes and determing  */
   /*  which columns are neded and what data type is returned  */

   ffrestart(NULL);
   if( ffparse() )
      return( *status = PARSE_SYNTAX_ERR );

   /*  Check results  */

   *status = gParse.status;
   if( *status ) return(*status);

   if( !gParse.nNodes ) {
      ffpmsg("Blank expression");
      return( *status = PARSE_SYNTAX_ERR );
   }
   if( !gParse.nCols ) {
      dmyCol.fptr = fptr;         /* This allows iterator to know value of */
      gParse.colData = &dmyCol;   /* fptr when no columns are referenced   */
   }

   result = gParse.Nodes + gParse.resultNode;

   *naxis = result->value.naxis;
   *nelem = result->value.nelem;
   for( i=0; i<*naxis && i<maxdim; i++ )
      naxes[i] = result->value.naxes[i];

   switch( result->type ) {
   case BOOLEAN:
      *datatype = TLOGICAL;
      break;
   case LONG:
      *datatype = TLONG;
      break;
   case DOUBLE:
      *datatype = TDOUBLE;
      break;
   case BITSTR:
      *datatype = TBIT;
      break;
   case STRING:
      *datatype = TSTRING;
      break;
   default:
      *datatype = 0;
      ffpmsg("Bad return data type");
      *status = gParse.status = PARSE_BAD_TYPE;
      break;
   }
   gParse.datatype = *datatype;
   free(gParse.expr);

   if( result->operation==CONST_OP ) *nelem = - *nelem;
   return(*status);
}

/*--------------------------------------------------------------------------*/
void ffcprs( void )  /*  No parameters                                      */
/*                                                                          */
/* Clear the parser, making it ready to accept a new expression.            */
/*--------------------------------------------------------------------------*/
{
   int col, node, i;

   if( gParse.nCols > 0 ) {
      free( gParse.colData  );
      for( col=0; col<gParse.nCols; col++ ) {
	 if( gParse.varData[col].undef == NULL ) continue;
	 if( gParse.varData[col].type  == BITSTR )
	   free( ((char**)gParse.varData[col].data)[0] );
	 free( gParse.varData[col].undef );
      }
      free( gParse.varData );
      gParse.nCols = 0;
   }

   if( gParse.nNodes > 0 ) {
      node = gParse.nNodes;
      while( node-- ) {
	 i = gParse.Nodes[node].SubNodes[0];
	 if( gParse.Nodes[node].operation==gtifilt_fct ) {
	    free( gParse.Nodes[ i ].value.data.ptr );
	 }
	 else if( gParse.Nodes[node].operation==regfilt_fct ) {
	    fits_free_region( (SAORegion *)gParse.Nodes[ i ].value.data.ptr );
	 }
      }
      gParse.nNodes = 0;
   }
   if( gParse.Nodes ) free( gParse.Nodes );
   gParse.Nodes = NULL;
}

/*---------------------------------------------------------------------------*/
int parse_data( long        totalrows, /* I - Total rows to be processed     */
                long        offset,    /* I - Number of rows skipped at start*/
                long        firstrow,  /* I - First row of this iteration    */
                long        nrows,     /* I - Number of rows in this iter    */
                int         nCols,     /* I - Number of columns in use       */
                iteratorCol *colData,  /* IO- Column information/data        */
                void        *userPtr ) /* I - Data handling instructions     */
/*                                                                           */
/* Iterator work function which calls the parser and copies the results      */
/* into either an OutputCol or a data pointer supplied in the userPtr        */
/* structure.                                                                */
/*---------------------------------------------------------------------------*/
{
    int status, constant=0, anyNullThisTime=0;
    long jj, kk, idx, remain, ntodo;
    Node *result;

    /* declare variables static to preserve their values between calls */
    static void *Data, *Null;
    static int  datasize;
    static long lastRow, jnull, repeat, resDataSize;
    static parseInfo *userInfo;
    static long zeros[4] = {0,0,0,0};

    /*--------------------------------------------------------*/
    /*  Initialization procedures: execute on the first call  */
    /*--------------------------------------------------------*/
    if (firstrow == offset+1)
    {
       userInfo = (parseInfo*)userPtr;
       userInfo->anyNull = 0;

       if( userInfo->maxRows>0 )
          userInfo->maxRows = minvalue(totalrows,userInfo->maxRows);
       else if( userInfo->maxRows<0 )
          userInfo->maxRows = totalrows;
       else
          userInfo->maxRows = nrows;

       lastRow = firstrow + userInfo->maxRows - 1;

       if( userInfo->dataPtr==NULL ) {

          if( colData[nCols-1].iotype == InputCol ) {
             ffpmsg("Output column for parser results not found!");
             return( PARSE_NO_OUTPUT );
          }
          /* Data gets set later */
          Null = colData[nCols-1].array;
          userInfo->datatype = colData[nCols-1].datatype;

	  /* Check for a TNULL keyword for output column */

	  status = 0;
	  jnull = 0L;
	  ffgknj( colData[nCols-1].fptr, "TNULL", colData[nCols-1].colnum,
		  1, &jnull, (int*)&jj, &status );
	  if( status==BAD_INTKEY ) {
	     /*  Probably ASCII table with text TNULL keyword  */
	     switch( userInfo->datatype ) {
	     case TSHORT:  jnull = SHRT_MIN;      break;
	     case TINT:    jnull = INT_MIN;       break;
	     case TLONG:   jnull = LONG_MIN;      break;
	     }
	  }
	  repeat = colData[nCols-1].repeat;

       } else {

          Data = userInfo->dataPtr;
          Null = (userInfo->nullPtr ? userInfo->nullPtr : zeros);
	  repeat = gParse.Nodes[gParse.resultNode].value.nelem;

       }

       /* Determine the size of each element of the returned result */

       switch( userInfo->datatype ) {
       case TBIT:       /*  Fall through to TBYTE  */
       case TLOGICAL:   /*  Fall through to TBYTE  */
       case TBYTE:     datasize = sizeof(char);     break;
       case TSHORT:    datasize = sizeof(short);    break;
       case TINT:      datasize = sizeof(int);      break;
       case TLONG:     datasize = sizeof(long);     break;
       case TFLOAT:    datasize = sizeof(float);    break;
       case TDOUBLE:   datasize = sizeof(double);   break;
       case TSTRING:   datasize = sizeof(char*);    break;
       }

       /* Determine the size of each element of the calculated result */
       /*   (only matters for numeric/logical data)                   */

       switch( gParse.Nodes[gParse.resultNode].type ) {
       case BOOLEAN:   resDataSize = sizeof(char);    break;
       case LONG:      resDataSize = sizeof(long);    break;
       case DOUBLE:    resDataSize = sizeof(double);  break;
       }
    }

    /*-------------------------------------------*/
    /*  Main loop: process all the rows of data  */
    /*-------------------------------------------*/

    /*  If writing to output column, set first element to appropriate  */
    /*  null value.  If no NULLs encounter, zero out before returning. */

    if( userInfo->dataPtr == NULL ) {
       /* First, reset Data pointer to start of output array */
       Data = (char*)colData[nCols-1].array + datasize;

       switch( userInfo->datatype ) {
       case TLOGICAL: *(char  *)Null = 'U';             break;
       case TBYTE:    *(char  *)Null = (char )jnull;    break;
       case TSHORT:   *(short *)Null = (short)jnull;    break;
       case TINT:     *(int   *)Null = (int  )jnull;    break;
       case TLONG:    *(long  *)Null = (long )jnull;    break;
       case TFLOAT:   *(float *)Null = FLOATNULLVALUE;  break;
       case TDOUBLE:  *(double*)Null = DOUBLENULLVALUE; break;
       case TSTRING: (*(char **)Null)[0] = '\1';
                     (*(char **)Null)[1] = '\0';        break;
       }
    }

    /* Alter nrows in case calling routine didn't want to do all rows */

    nrows = minvalue(nrows,lastRow-firstrow+1);

    Setup_DataArrays( nCols, colData, firstrow, nrows );

    /* Parser allocates arrays for each column and calculation it performs. */
    /* Limit number of rows processed during each pass to reduce memory     */
    /* requirements... In most cases, iterator will limit rows to less      */
    /* than 2500 rows per iteration, so this is really only relevant for    */
    /* hk-compressed files which must be decompressed in memory and sent    */
    /* whole to parse_data in a single iteration.                           */

    remain = nrows;
    while( remain ) {

       ntodo = minvalue(remain,2500);
       Evaluate_Parser ( firstrow, ntodo );
       if( gParse.status ) break;

       firstrow += ntodo;
       remain   -= ntodo;

       /*  Copy results into data array  */

       result = gParse.Nodes + gParse.resultNode;
       if( result->operation==CONST_OP ) constant = 1;

       switch( result->type ) {

       case BOOLEAN:
       case LONG:
       case DOUBLE:
	  if( constant ) {
	     char undef=0;
	     for( kk=0; kk<ntodo; kk++ )
		for( jj=0; jj<repeat; jj++ )
		   ffcvtn( gParse.datatype,
			   &(result->value.data),
			   &undef, result->value.nelem /* 1 */,
			   userInfo->datatype, Null,
			   (char*)Data + (kk*repeat+jj)*datasize,
			   &anyNullThisTime, &gParse.status );
	  } else {
	     if ( repeat == result->value.nelem ) {
		ffcvtn( gParse.datatype,
			result->value.data.ptr,
			result->value.undef,
			result->value.nelem*ntodo,
			userInfo->datatype, Null, Data,
			&anyNullThisTime, &gParse.status );
	     } else if( result->value.nelem == 1 ) {
		for( kk=0; kk<ntodo; kk++ )
		   for( jj=0; jj<repeat; jj++ ) {
		      ffcvtn( gParse.datatype,
			      (char*)result->value.data.ptr + kk*resDataSize,
			      (char*)result->value.undef + kk,
			      1, userInfo->datatype, Null,
			      (char*)Data + (kk*repeat+jj)*datasize,
			      &anyNullThisTime, &gParse.status );
		   }
	     } else {
		int nCopy;
		nCopy = minvalue( repeat, result->value.nelem );
		for( kk=0; kk<ntodo; kk++ ) {
		   ffcvtn( gParse.datatype,
			   (char*)result->value.data.ptr
                                  + kk*result->value.nelem*resDataSize,
			   (char*)result->value.undef
                                  + kk*result->value.nelem,
			   nCopy, userInfo->datatype, Null,
			   (char*)Data + (kk*repeat)*datasize,
			   &anyNullThisTime, &gParse.status );
		   if( nCopy < repeat ) {
		      memset( (char*)Data + (kk*repeat+nCopy)*datasize,
			      0, (repeat-nCopy)*datasize);
		   }
		}

	     }
	     if( result->operation>0 ) {
		free( result->value.data.ptr );
	     }
	  }
          if( gParse.status==OVERFLOW_ERR ) {
             gParse.status = NUM_OVERFLOW;
             ffpmsg("Numerical overflow while converting expression to necessary datatype");
          }
          break;

       case BITSTR:
          switch( userInfo->datatype ) {
          case TBYTE:
	     idx = -1;
             for( kk=0; kk<ntodo; kk++ ) {
                for( jj=0; jj<result->value.nelem; jj++ ) {
                   if( jj%8 == 0 )
                      ((char*)Data)[++idx] = 0;
		   if( constant ) {
		      if( result->value.data.str[jj]=='1' )
			 ((char*)Data)[idx] |= 128>>(jj%8);
		   } else {
		      if( result->value.data.strptr[kk][jj]=='1' )
			 ((char*)Data)[idx] |= 128>>(jj%8);
		   }
                }
	     }
             break;
          case TBIT:
          case TLOGICAL:
	     if( constant ) {
		for( kk=0; kk<ntodo; kk++ )
		   for( jj=0; jj<result->value.nelem; jj++ ) {
		      ((char*)Data)[ jj+kk*result->value.nelem ] =
			 ( result->value.data.str[jj]=='1' );
		   }
	     } else {
		for( kk=0; kk<ntodo; kk++ )
		   for( jj=0; jj<result->value.nelem; jj++ ) {
		      ((char*)Data)[ jj+kk*result->value.nelem ] =
			 ( result->value.data.strptr[kk][jj]=='1' );
		   }
	     }
             break; 
	  case TSTRING:
	     if( constant ) {
		for( jj=0; jj<ntodo; jj++ ) {
		   strcpy( ((char**)Data)[jj], result->value.data.str );
		}
	     } else {
		for( jj=0; jj<ntodo; jj++ ) {
		   strcpy( ((char**)Data)[jj], result->value.data.strptr[jj] );
		}
	     }
	     break;
          default:
             ffpmsg("Cannot convert bit expression to desired type.");
             gParse.status = PARSE_BAD_TYPE;
             break;
          }
          if( result->operation>0 ) {
             free( result->value.data.strptr[0] );
             free( result->value.data.strptr );
          }
          break;

       case STRING:
          if( userInfo->datatype==TSTRING ) {
	     if( constant ) {
		for( jj=0; jj<ntodo; jj++ )
		   strcpy( ((char**)Data)[jj], result->value.data.str );
	     } else {
		for( jj=0; jj<ntodo; jj++ )
		   if( result->value.undef[jj] ) {
		      anyNullThisTime = 1;
		      strcpy( ((char**)Data)[jj],
			      *(char **)Null );
		   } else {
		      strcpy( ((char**)Data)[jj],
			      result->value.data.strptr[jj] );
		   }
	     }
          } else {
             ffpmsg("Cannot convert string expression to desired type.");
             gParse.status = PARSE_BAD_TYPE;
          }
          if( result->operation>0 ) {
             free( result->value.data.strptr[0] );
             free( result->value.data.strptr );
          }
          break;
       }

       if( gParse.status ) break;

       /*  Increment Data to point to where the next block should go  */

       if( result->type==BITSTR && userInfo->datatype==TBYTE )
          Data = (char*)Data
                    + datasize * ( (result->value.nelem+7)/8 ) * ntodo;
       else if( result->type==STRING )
          Data = (char*)Data + datasize * ntodo;
       else
          Data = (char*)Data + datasize * ntodo * repeat;
    }

    /* If no NULLs encountered during this pass, set Null value to */
    /* zero to make the writing of the output column data faster   */

    if( anyNullThisTime )
       userInfo->anyNull = 1;
    else if( userInfo->dataPtr == NULL ) {
       if( userInfo->datatype == TSTRING )
          memcpy( *(char **)Null, zeros, 2 );
       else 
          memcpy( Null, zeros, datasize );
    }

    /*-------------------------------------------------------*/
    /*  Clean up procedures:  after processing all the rows  */
    /*-------------------------------------------------------*/

    if( firstrow - 1 == lastRow ) {
       if( !gParse.status && userInfo->maxRows<totalrows ) return (-1);
    }

    return(gParse.status);  /* return successful status */
}

static void Setup_DataArrays( int nCols, iteratorCol *cols,
			      long fRow, long nRows )
    /***********************************************************************/
    /*  Setup the varData array in gParse to contain the fits column data. */
    /*  Then, allocate and initialize the necessary UNDEF arrays for each  */
    /*  column used by the parser.                                         */
    /***********************************************************************/
{
   int     i;
   long    nelem, len, row, idx;
   char  **bitStrs;
   char  **sptr;
   char   *barray;
   long   *iarray;
   double *rarray;

   gParse.firstDataRow = fRow;
   gParse.nDataRows    = nRows;

   /*  Resize and fill in UNDEF arrays for each column  */

   for( i=0; i<nCols; i++ ) {
      if( cols[i].iotype == OutputCol ) continue;

      nelem  = gParse.varData[i].nelem;
      len    = nelem * nRows;

      switch ( gParse.varData[i].type ) {

      case BITSTR:
      /* No need for UNDEF array, but must make string DATA array */
	 len = (nelem+1)*nRows;   /* Count '\0' */
	 bitStrs = (char**)gParse.varData[i].data;
	 if( bitStrs ) free( bitStrs[0] );
	 free( bitStrs );
	 bitStrs = (char**)malloc( nRows*sizeof(char*) );
	 if( bitStrs==NULL ) {
	    gParse.varData[i].data = gParse.varData[i].undef = NULL;
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 bitStrs[0] = (char*)malloc( len*sizeof(char) );
	 if( bitStrs[0]==NULL ) {
	    free( bitStrs );
	    gParse.varData[i].data = gParse.varData[i].undef = NULL;
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }

	 for( row=0; row<nRows; row++ ) {
	    bitStrs[row] = bitStrs[0] + row*(nelem+1);
	    idx = (row)*( (nelem+7)/8 ) + 1;
	    for(len=0; len<nelem; len++) {
	       if( ((char*)cols[i].array)[idx] & (1<<(7-len%8)) )
		  bitStrs[row][len] = '1';
	       else
		  bitStrs[row][len] = '0';
	       if( len%8==7 ) idx++;
	    }
	    bitStrs[row][len] = '\0';
	 }
	 gParse.varData[i].undef = (char*)bitStrs;
	 gParse.varData[i].data  = (char*)bitStrs;
	 break;

      case STRING:
	 sptr = (char**)cols[i].array;
	 free( gParse.varData[i].undef );
	 gParse.varData[i].undef = (char*)malloc( nRows*sizeof(char) );
	 if( gParse.varData[i].undef==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 row = nRows;
	 while( row-- )
	    gParse.varData[i].undef[row] =
	       ( **sptr != '\0' && FSTRCMP( sptr[0], sptr[row+1] )==0 );
	 gParse.varData[i].data  = sptr + 1;
	 break;

      case BOOLEAN:
	 barray = (char*)cols[i].array;
	 free( gParse.varData[i].undef );
	 gParse.varData[i].undef = (char*)malloc( len*sizeof(char) );
	 if( gParse.varData[i].undef==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.varData[i].undef[len] = 
	       ( barray[0]!=0 && barray[0]==barray[len+1] );
	 }
	 gParse.varData[i].data  = barray + 1;
	 break;

      case LONG:
	 iarray = (long*)cols[i].array;
	 free( gParse.varData[i].undef );
	 gParse.varData[i].undef = (char*)malloc( len*sizeof(char) );
	 if( gParse.varData[i].undef==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.varData[i].undef[len] = 
	       ( iarray[0]!=0L && iarray[0]==iarray[len+1] );
	 }
	 gParse.varData[i].data  = iarray + 1;
	 break;

      case DOUBLE:
	 rarray = (double*)cols[i].array;
	 free( gParse.varData[i].undef );
	 gParse.varData[i].undef = (char*)malloc( len*sizeof(char) );
	 if( gParse.varData[i].undef==NULL ) {
	    gParse.status = MEMORY_ALLOCATION;
	    break;
	 }
	 while( len-- ) {
	    gParse.varData[i].undef[len] = 
	       ( rarray[0]!=0.0 && rarray[0]==rarray[len+1]);
	 }
	 gParse.varData[i].data  = rarray + 1;
	 break;
      }

      if( gParse.status ) {  /*  Deallocate NULL arrays of previous columns */
	 while( i-- ) {
	    if( gParse.varData[i].type==BITSTR )
	       free( ((char**)gParse.varData[i].data)[0] );
	    free( gParse.varData[i].undef );
	    gParse.varData[i].undef = NULL;
	 }
	 return;
      }
   }
}

/*--------------------------------------------------------------------------*/
int ffcvtn( int   inputType,  /* I - Data type of input array               */
            void  *input,     /* I - Input array of type inputType          */
            char  *undef,     /* I - Array of flags indicating UNDEF elems  */
            long  ntodo,      /* I - Number of elements to process          */
            int   outputType, /* I - Data type of output array              */
            void  *nulval,    /* I - Ptr to value to use for UNDEF elements */
            void  *output,    /* O - Output array of type outputType        */
            int   *anynull,   /* O - Any nulls flagged?                     */
            int   *status )   /* O - Error status                           */
/*                                                                          */
/* Convert an array of any input data type to an array of any output        */
/* data type, using an array of UNDEF flags to assign nulvals to            */
/*--------------------------------------------------------------------------*/
{
   long i;

   switch( outputType ) {

   case TLOGICAL:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            if( ((unsigned char*)input)[i] )
                ((unsigned char*)output)[i] = 1;
            else
                ((unsigned char*)output)[i] = 0;
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            if( ((short*)input)[i] )
                ((unsigned char*)output)[i] = 1;
            else
                ((unsigned char*)output)[i] = 0;
         break;
      case TLONG:
         for( i=0; i<ntodo; i++ )
            if( ((long*)input)[i] )
                ((unsigned char*)output)[i] = 1;
            else
                ((unsigned char*)output)[i] = 0;
         break;
      case TFLOAT:
         for( i=0; i<ntodo; i++ )
            if( ((float*)input)[i] )
                ((unsigned char*)output)[i] = 1;
            else
                ((unsigned char*)output)[i] = 0;
         break;
      case TDOUBLE:
         for( i=0; i<ntodo; i++ )
            if( ((double*)input)[i] )
                ((unsigned char*)output)[i] = 1;
            else
                ((unsigned char*)output)[i] = 0;
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((unsigned char*)output)[i] = *(unsigned char*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TBYTE:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((unsigned char*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         fffi2i1((short*)input,ntodo,1.,0.,0,0,0,NULL,NULL,(unsigned char*)output,status);
         break;
      case TLONG:
         for (i = 0; i < ntodo; i++) {
	    if( undef[i] ) {
	       ((unsigned char*)output)[i] = *(unsigned char*)nulval;
	       *anynull = 1;
	    } else {
	       if( ((long*)input)[i] < 0 ) {
		  *status = OVERFLOW_ERR;
		  ((unsigned char*)output)[i] = 0;
	       } else if( ((long*)input)[i] > UCHAR_MAX ) {
		  *status = OVERFLOW_ERR;
		  ((unsigned char*)output)[i] = UCHAR_MAX;
	       } else
		  ((unsigned char*)output)[i] = 
		     (unsigned char) ((long*)input)[i];
	    }
         }
	 return( *status );
         break;
      case TFLOAT:
         fffr4i1((float*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (unsigned char*)output,status);
         break;
      case TDOUBLE:
         fffr8i1((double*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (unsigned char*)output,status);
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((unsigned char*)output)[i] = *(unsigned char*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TSHORT:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((short*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            ((short*)output)[i] = ((short*)input)[i];
         break;
      case TLONG:
         for (i = 0; i < ntodo; i++) {
	    if( undef[i] ) {
	       ((short*)output)[i] = *(short*)nulval;
	       *anynull = 1;
	    } else {
	       if( ((long*)input)[i] < SHRT_MIN ) {
		  *status = OVERFLOW_ERR;
		  ((short*)output)[i] = SHRT_MIN;
	       } else if ( ((long*)input)[i] > SHRT_MAX ) {
		  *status = OVERFLOW_ERR;
		  ((short*)output)[i] = SHRT_MAX;
	       } else
		  ((short*)output)[i] = (short) ((long*)input)[i];
	    }
         }
	 return( *status );
         break;
      case TFLOAT:
         fffr4i2((float*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (short*)output,status);
         break;
      case TDOUBLE:
         fffr8i2((double*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (short*)output,status);
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((short*)output)[i] = *(short*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TINT:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((int*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            ((int*)output)[i] = ((short*)input)[i];
         break;
      case TLONG:
         for( i=0; i<ntodo; i++ )
            ((int*)output)[i] = ((long*)input)[i];
         break;
      case TFLOAT:
         fffr4int((float*)input,ntodo,1.,0.,0,0,NULL,NULL,
		  (int*)output,status);
         break;
      case TDOUBLE:
         fffr8int((double*)input,ntodo,1.,0.,0,0,NULL,NULL,
		  (int*)output,status);
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((int*)output)[i] = *(int*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TLONG:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((long*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            ((long*)output)[i] = ((short*)input)[i];
         break;
      case TLONG:
         for( i=0; i<ntodo; i++ )
            ((long*)output)[i] = ((long*)input)[i];
         break;
      case TFLOAT:
         fffr4i4((float*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (long*)output,status);
         break;
      case TDOUBLE:
         fffr8i4((double*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (long*)output,status);
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((long*)output)[i] = *(long*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TFLOAT:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((float*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            ((float*)output)[i] = ((short*)input)[i];
         break;
      case TLONG:
         for( i=0; i<ntodo; i++ )
            ((float*)output)[i] = ((long*)input)[i];
         break;
      case TFLOAT:
         for( i=0; i<ntodo; i++ )
            ((float*)output)[i] = ((float*)input)[i];
         break;
      case TDOUBLE:
         fffr8r4((double*)input,ntodo,1.,0.,0,0,NULL,NULL,
		 (float*)output,status);
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((float*)output)[i] = *(float*)nulval;
            *anynull = 1;
         }
      }
      break;

   case TDOUBLE:
      switch( inputType ) {
      case TLOGICAL:
      case TBYTE:
         for( i=0; i<ntodo; i++ )
            ((double*)output)[i] = ((unsigned char*)input)[i];
         break;
      case TSHORT:
         for( i=0; i<ntodo; i++ )
            ((double*)output)[i] = ((short*)input)[i];
         break;
      case TLONG:
         for( i=0; i<ntodo; i++ )
            ((double*)output)[i] = ((long*)input)[i];
         break;
      case TFLOAT:
         for( i=0; i<ntodo; i++ )
            ((double*)output)[i] = ((float*)input)[i];
         break;
      case TDOUBLE:
         for( i=0; i<ntodo; i++ )
            ((double*)output)[i] = ((double*)input)[i];
         break;
      default:
         *status = BAD_DATATYPE;
         break;
      }
      for(i=0;i<ntodo;i++) {
         if( undef[i] ) {
            ((double*)output)[i] = *(double*)nulval;
            *anynull = 1;
         }
      }
      break;

   default:
      *status = BAD_DATATYPE;
      break;
   }
   return ( *status );
}

/*---------------------------------------------------------------------------*/
int fffrwc( fitsfile *fptr,        /* I - Input FITS file                    */
            char     *expr,        /* I - Boolean expression                 */
            char     *timeCol,     /* I - Name of time column                */
            char     *parCol,      /* I - Name of parameter column           */
            char     *valCol,      /* I - Name of value column               */
            long     ntimes,       /* I - Number of distinct times in file   */
            double   *times,       /* O - Array of times in file             */
            char     *time_status, /* O - Array of boolean results           */
            int      *status )     /* O - Error status                       */
/*                                                                           */
/* Evaluate a boolean expression for each time in a compressed file,         */
/* returning an array of flags indicating which times evaluated to TRUE/FALSE*/
/*---------------------------------------------------------------------------*/
{
   parseInfo Info;
   long alen, width;
   int parNo, typecode;
   int naxis, constant, nCol=0;
   long nelem, naxes[MAXDIMS], elem;
   char result;

   if( *status ) return( *status );

   fits_get_colnum( fptr, CASEINSEN, timeCol, &gParse.timeCol, status );
   fits_get_colnum( fptr, CASEINSEN, parCol,  &gParse.parCol , status );
   fits_get_colnum( fptr, CASEINSEN, valCol,  &gParse.valCol, status );
   if( *status ) return( *status );
   
   if( ffiprs( fptr, 1, expr, MAXDIMS, &Info.datatype, &nelem,
               &naxis, naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem<0 ) {
      constant = 1;
      nelem = -nelem;
      nCol = gParse.nCols;
      gParse.nCols = 0;    /*  Ignore all column references  */
   } else
      constant = 0;

   if( Info.datatype!=TLOGICAL || nelem!=1 ) {
      ffcprs();
      ffpmsg("Expression does not evaluate to a logical scalar.");
      return( *status = PARSE_BAD_TYPE );
   }

   /*******************************************/
   /* Allocate data arrays for each parameter */
   /*******************************************/
   
   parNo = gParse.nCols;
   while( parNo-- ) {
      switch( gParse.colData[parNo].datatype ) {
      case TLONG:
	 if( (gParse.colData[parNo].array =
	      (long *)malloc( (ntimes+1)*sizeof(long) )) )
	    ((long*)gParse.colData[parNo].array)[0] = 1234554321;
	 else
	    *status = MEMORY_ALLOCATION;
	 break;
      case TDOUBLE:
	 if( (gParse.colData[parNo].array =
	      (double *)malloc( (ntimes+1)*sizeof(double) )) )
	    ((double*)gParse.colData[parNo].array)[0] = DOUBLENULLVALUE;
	 else
	    *status = MEMORY_ALLOCATION;
	 break;
      case TSTRING:
	 if( !fits_get_coltype( fptr, gParse.valCol, &typecode,
				&alen, &width, status ) ) {
	    alen++;
	    if( (gParse.colData[parNo].array =
		 (char **)malloc( (ntimes+1)*sizeof(char*) )) ) {
	       if( (((char **)gParse.colData[parNo].array)[0] =
		    (char *)malloc( (ntimes+1)*sizeof(char)*alen )) ) {
		  for( elem=1; elem<=ntimes; elem++ )
		     ((char **)gParse.colData[parNo].array)[elem] =
			((char **)gParse.colData[parNo].array)[elem-1]+alen;
		  ((char **)gParse.colData[parNo].array)[0][0] = '\0';
	       } else {
		  free( gParse.colData[parNo].array );
		  *status = MEMORY_ALLOCATION;
	       }
	    } else {
	       *status = MEMORY_ALLOCATION;
	    }
	 }
	 break;
      }
      if( *status ) {
	 while( parNo-- ) {
	    if( gParse.colData[parNo].datatype==TSTRING )
	       free( ((char **)gParse.colData[parNo].array)[0] );
	    free( gParse.colData[parNo].array );
	 }
	 return( *status );
      }
   }
   
   /**********************************************************************/
   /* Read data from columns needed for the expression and then parse it */
   /**********************************************************************/
   
   if( !uncompress_hkdata( fptr, ntimes, times, status ) ) {
      if( constant ) {
	 result = gParse.Nodes[gParse.resultNode].value.data.log;
	 elem = ntimes;
	 while( elem-- ) time_status[elem] = result;
      } else {
	 Info.dataPtr  = time_status;
	 Info.nullPtr  = NULL;
	 Info.maxRows  = ntimes;
	 *status       = parse_data( ntimes, 0, 1, ntimes, gParse.nCols,
				     gParse.colData, (void*)&Info );
      }
   }
   
   /************/
   /* Clean up */
   /************/
   
   parNo = gParse.nCols;
   while ( parNo-- ) {
      if( gParse.colData[parNo].datatype==TSTRING )
	 free( ((char **)gParse.colData[parNo].array)[0] );
      free( gParse.colData[parNo].array );
   }
   
   if( constant ) gParse.nCols = nCol;

   ffcprs();
   return(*status);
}

/*---------------------------------------------------------------------------*/
int uncompress_hkdata( fitsfile *fptr,
                       long     ntimes,
                       double   *times,
                       int      *status )
/*                                                                           */
/* description                                                               */
/*---------------------------------------------------------------------------*/
{
   char parName[256], *sPtr[1], found[1000];
   int parNo, anynul;
   long naxis2, row, currelem;
   double currtime, newtime;

   sPtr[0] = parName;
   currelem = 0;
   currtime = -1e38;

   parNo=gParse.nCols;
   while( parNo-- ) found[parNo] = 0;

   if( ffgkyj( fptr, "NAXIS2", &naxis2, NULL, status ) ) return( *status );

   for( row=1; row<=naxis2; row++ ) {
      if( ffgcvd( fptr, gParse.timeCol, row, 1L, 1L, 0.0,
                  &newtime, &anynul, status ) ) return( *status );
      if( newtime != currtime ) {
         /*  New time encountered... propogate parameters to next row  */
         if( currelem==ntimes ) {
            ffpmsg("Found more unique time stamps than caller indicated");
            return( *status = PARSE_BAD_COL );
         }
         times[currelem++] = currtime = newtime;
	 parNo = gParse.nCols;
         while( parNo-- ) {
            switch( gParse.colData[parNo].datatype ) {
            case TLONG:
               ((long*)gParse.colData[parNo].array)[currelem] =
                  ((long*)gParse.colData[parNo].array)[currelem-1];
               break;
            case TDOUBLE:
               ((double*)gParse.colData[parNo].array)[currelem] =
                  ((double*)gParse.colData[parNo].array)[currelem-1];
               break;
            case TSTRING:
               strcpy( ((char **)gParse.colData[parNo].array)[currelem],
                       ((char **)gParse.colData[parNo].array)[currelem-1] );
               break;
            }
         }
      }

      if( ffgcvs( fptr, gParse.parCol, row, 1L, 1L, "",
                  sPtr, &anynul, status ) ) return( *status );
      parNo = gParse.nCols;
      while( parNo-- )
	 if( !strcasecmp( parName, gParse.varData[parNo].name ) ) break;

      if( parNo>=0 ) {
	 found[parNo] = 1; /* Flag this parameter as found */
         switch( gParse.colData[parNo].datatype ) {
         case TLONG:
            ffgcvj( fptr, gParse.valCol, row, 1L, 1L,
                    ((long*)gParse.colData[parNo].array)[0],
                    ((long*)gParse.colData[parNo].array)+currelem,
                    &anynul, status );
            break;
         case TDOUBLE:
            ffgcvd( fptr, gParse.valCol, row, 1L, 1L,
                    ((double*)gParse.colData[parNo].array)[0],
                    ((double*)gParse.colData[parNo].array)+currelem,
                    &anynul, status );
            break;
         case TSTRING:
            ffgcvs( fptr, gParse.valCol, row, 1L, 1L,
                    ((char**)gParse.colData[parNo].array)[0],
                    ((char**)gParse.colData[parNo].array)+currelem,
                    &anynul, status );
            break;
         }
         if( *status ) return( *status );
      }
   }

   if( currelem<ntimes ) {
      ffpmsg("Found fewer unique time stamps than caller indicated");
      return( *status = PARSE_BAD_COL );
   }

   /*  Check for any parameters which were not located in the table  */
   parNo = gParse.nCols;
   while( parNo-- )
      if( !found[parNo] ) {
	 sprintf( parName, "Parameter not found: %-30s", 
		  gParse.varData[parNo].name );
	 ffpmsg( parName );
	 *status = PARSE_SYNTAX_ERR;
      }
   return( *status );
}

/*---------------------------------------------------------------------------*/
int ffffrw( fitsfile *fptr,         /* I - Input FITS file                   */
            char     *expr,         /* I - Boolean expression                */
            long     *rownum,       /* O - First row of table to eval to T   */
            int      *status )      /* O - Error status                      */
/*                                                                           */
/* Evaluate a boolean expression, returning the row number of the first      */
/* row which evaluates to TRUE                                               */
/*---------------------------------------------------------------------------*/
{
   int naxis, constant, dtype;
   long nelem, naxes[MAXDIMS];
   char result;

   if( *status ) return( *status );

   if( ffiprs( fptr, 0, expr, MAXDIMS, &dtype, &nelem, &naxis,
               naxes, status ) ) {
      ffcprs();
      return( *status );
   }
   if( nelem<0 ) {
      constant = 1;
      nelem = -nelem;
   } else
      constant = 0;

   if( dtype!=TLOGICAL || nelem!=1 ) {
      ffcprs();
      ffpmsg("Expression does not evaluate to a logical scalar.");
      return( *status = PARSE_BAD_TYPE );
   }

   *rownum = 0;
   if( constant ) { /* No need to call parser... have result from ffiprs */
      result = gParse.Nodes[gParse.resultNode].value.data.log;
      if( result ) {
	 /*  Make sure there is at least 1 row in table  */
	 ffgnrw( fptr, &nelem, status );
	 if( nelem )
	    *rownum = 1;
      }
   } else {
      if( ffiter( gParse.nCols, gParse.colData, 0, 0,
		  ffffrw_work, (void*)rownum, status ) == -1 )
	 *status = 0;  /* -1 indicates exitted without error before end... OK */
   }

   ffcprs();
   return(*status);
}

/*---------------------------------------------------------------------------*/
int ffffrw_work(long        totalrows, /* I - Total rows to be processed     */
                long        offset,    /* I - Number of rows skipped at start*/
                long        firstrow,  /* I - First row of this iteration    */
                long        nrows,     /* I - Number of rows in this iter    */
                int         nCols,     /* I - Number of columns in use       */
                iteratorCol *colData,  /* IO- Column information/data        */
                void        *userPtr ) /* I - Data handling instructions     */
/*                                                                           */
/* Iterator work function which calls the parser and searches for the        */
/* first row which evaluates to TRUE.                                        */
/*---------------------------------------------------------------------------*/
{
    long idx;
    Node *result;

    Evaluate_Parser( firstrow, nrows );

    if( !gParse.status ) {

       result = gParse.Nodes + gParse.resultNode;
       if( result->operation==CONST_OP ) {

	  if( result->value.data.log ) {
	     *(long*)userPtr = firstrow;
	     return( -1 );
	  }

       } else {

	  for( idx=0; idx<nrows; idx++ )
	     if( result->value.data.logptr[idx] && !result->value.undef[idx] ) {
		*(long*)userPtr = firstrow + idx;
		return( -1 );
	     }
       }
    }

    return( gParse.status );
}


/*************************************************************************

        Functions used by the evaluator to access FITS data
            (find_column, find_keywd, allocateCol, load_column)

 *************************************************************************/


static int find_column( char *colName, void *itslval )
{
   FFSTYPE *thelval = (FFSTYPE*)itslval;
   int col_cnt, status;
   int colnum, typecode, type, hdutype;
   long repeat, width;
   fitsfile *fptr;
   char temp[80];
   double tzero,tscale;
   int istatus;

   if( *colName == '#' )
      return( find_keywd( colName + 1, itslval ) );

   fptr = gParse.def_fptr;
   status = 0;
   if( gParse.compressed )
      colnum = gParse.valCol;
   else
      if( fits_get_colnum( fptr, CASEINSEN, colName, &colnum, &status ) ) {
	 if( status == COL_NOT_FOUND ) {
	    type = find_keywd( colName, itslval );
	    if( type != pERROR ) ffcmsg();
	    return( type );
	 }
	 gParse.status = status;
	 return pERROR;
      }
   
   if( fits_get_coltype( fptr, colnum, &typecode,
			 &repeat, &width, &status ) ) {
      gParse.status = status;
      return pERROR;
   }

   col_cnt = gParse.nCols;
   if( allocateCol( col_cnt, &gParse.status ) ) return pERROR;
   fits_iter_set_by_num( gParse.colData+col_cnt, fptr, colnum, 0, InputCol );

   /*  Make sure we don't overflow variable name array  */
   strncpy(gParse.varData[col_cnt].name,colName,MAXVARNAME);
   gParse.varData[col_cnt].name[MAXVARNAME] = '\0';

   switch( typecode ) {
   case TBIT:
      gParse.varData[col_cnt].type     = BITSTR;
      gParse.colData[col_cnt].datatype = TBYTE;
      type = BITCOL;
      break;
   case TBYTE:
   case TSHORT:
   case TLONG:
      /* The datatype of column with TZERO and TSCALE keywords might be 
         float or double. 
      */
      sprintf(temp,"TZERO%d",colnum);
      istatus = 0;
      if(fits_read_key(fptr,TDOUBLE,temp,&tzero,NULL,&istatus)) {
          tzero = 0.0;
      } 
      sprintf(temp,"TSCAL%d",colnum);
      istatus = 0;
      if(fits_read_key(fptr,TDOUBLE,temp,&tscale,NULL,&istatus)) {
          tscale = 1.0;
      } 
      if (tscale == 1.0 && (tzero == 0.0 || tzero == 32768.0 )) {
          gParse.varData[col_cnt].type     = LONG;
          gParse.colData[col_cnt].datatype = TLONG;
      } else if (tscale == 1.0 &&  tzero == 2147483648.0 ) {
          gParse.varData[col_cnt].type     = LONG;
          gParse.colData[col_cnt].datatype = TULONG;
      } else {
          gParse.varData[col_cnt].type     = DOUBLE;
          gParse.colData[col_cnt].datatype = TDOUBLE;
      }
      type = COLUMN;
      break;
   case TFLOAT:
   case TDOUBLE:
      gParse.varData[col_cnt].type     = DOUBLE;
      gParse.colData[col_cnt].datatype = TDOUBLE;
      type = COLUMN;
      break;
   case TLOGICAL:
      gParse.varData[col_cnt].type     = BOOLEAN;
      gParse.colData[col_cnt].datatype = TLOGICAL;
      type = BCOLUMN;
      break;
   case TSTRING:
      gParse.varData[col_cnt].type     = STRING;
      gParse.colData[col_cnt].datatype = TSTRING;
      type = SCOLUMN;
      fits_get_hdu_type( fptr, &hdutype, &status );
      if( hdutype == ASCII_TBL ) repeat = width;
      break;
   default:
      gParse.status = PARSE_BAD_TYPE;
      return pERROR;
   }
   gParse.varData[col_cnt].nelem = repeat;
   if( repeat>1 && typecode!=TSTRING ) {
      if( fits_read_tdim( fptr, colnum, MAXDIMS,
			  &gParse.varData[col_cnt].naxis,
			  &gParse.varData[col_cnt].naxes[0], &status )
	  ) {
	 gParse.status = status;
	 return pERROR;
      }
   } else {
      gParse.varData[col_cnt].naxis = 1;
      gParse.varData[col_cnt].naxes[0] = 1;
   }
   gParse.nCols++;
   thelval->lng = col_cnt;

   return( type );
}

static int find_keywd(char *keyname, void *itslval )
{
   FFSTYPE *thelval = (FFSTYPE*)itslval;
   int status, type;
   char keyvalue[FLEN_VALUE], dtype;
   fitsfile *fptr;
   double rval;
   int bval;
   long ival;

   status = 0;
   fptr = gParse.def_fptr;
   if( fits_read_keyword( fptr, keyname, keyvalue, NULL, &status ) ) {
      if( status == KEY_NO_EXIST ) {
	 /*  Do this since ffgkey doesn't put an error message on stack  */
	 sprintf(keyvalue, "ffgkey could not find keyword: %s",keyname);
	 ffpmsg(keyvalue);
      }
      gParse.status = status;
      return( pERROR );
   }
      
   if( fits_get_keytype( keyvalue, &dtype, &status ) ) {
      gParse.status = status;
      return( pERROR );
   }
      
   switch( dtype ) {
   case 'C':
      fits_read_key_str( fptr, keyname, keyvalue, NULL, &status );
      type = STRING;
      strcpy( thelval->str , keyvalue );
      break;
   case 'L':
      fits_read_key_log( fptr, keyname, &bval, NULL, &status );
      type = BOOLEAN;
      thelval->log = bval;
      break;
   case 'I':
      fits_read_key_lng( fptr, keyname, &ival, NULL, &status );
      type = LONG;
      thelval->lng = ival;
      break;
   case 'F':
      fits_read_key_dbl( fptr, keyname, &rval, NULL, &status );
      type = DOUBLE;
      thelval->dbl = rval;
      break;
   default:
      type = pERROR;
      break;
   }

   if( status ) {
      gParse.status=status;
      return pERROR;
   }

   return( type );
}

static int allocateCol( int nCol, int *status )
{
   if( (nCol%25)==0 ) {
      if( nCol ) {
	 gParse.colData  = (iteratorCol*) realloc( gParse.colData,
                                              (nCol+25)*sizeof(iteratorCol) );
	 gParse.varData  = (DataInfo   *) realloc( gParse.varData,
                                              (nCol+25)*sizeof(DataInfo)    );
      } else {
	 gParse.colData  = (iteratorCol*) malloc( 25*sizeof(iteratorCol) );
	 gParse.varData  = (DataInfo   *) malloc( 25*sizeof(DataInfo)    );
      }
      if(    gParse.colData  == NULL
          || gParse.varData  == NULL    ) {
	 if( gParse.colData  ) free(gParse.colData);
	 if( gParse.varData  ) free(gParse.varData);
	 gParse.colData = NULL;
	 gParse.varData = NULL;
	 return( *status = MEMORY_ALLOCATION );
      }
   }
   gParse.varData[nCol].data  = NULL;
   gParse.varData[nCol].undef = NULL;
   return 0;
}

static int load_column( int varNum, long fRow, long nRows,
			void *data, char *undef )
{
   iteratorCol *var = gParse.colData+varNum;
   long nelem,nbytes,row,len,idx;
   char **bitStrs;
   unsigned char *bytes;
   int status = 0, anynul;

   nelem = nRows * var->repeat;

   switch( var->datatype ) {
   case TBYTE:
      nbytes = ((var->repeat+7)/8) * nRows;
      bytes = (unsigned char *)malloc( nbytes * sizeof(char) );

      ffgcvb(var->fptr, var->colnum, fRow, 1L, nbytes,
	     0, bytes, &anynul, &status);

      nelem = var->repeat;
      bitStrs = (char **)data;
      for( row=0; row<nRows; row++ ) {
	 idx = (row)*( (nelem+7)/8 ) + 1;
	 for(len=0; len<nelem; len++) {
	    if( bytes[idx] & (1<<(7-len%8)) )
	       bitStrs[row][len] = '1';
	    else
	       bitStrs[row][len] = '0';
	    if( len%8==7 ) idx++;
	 }
	 bitStrs[row][len] = '\0';
      }

      free( (char *)bytes );
      break;
   case TSTRING:
      ffgcfs(var->fptr, var->colnum, fRow, 1L, nRows,
	     (char **)data, undef, &anynul, &status);
      break;
   case TLOGICAL:
      ffgcfl(var->fptr, var->colnum, fRow, 1L, nelem,
	     (char *)data, undef, &anynul, &status);
      break;
   case TLONG:
      ffgcfj(var->fptr, var->colnum, fRow, 1L, nelem,
	     (long *)data, undef, &anynul, &status);
      break;
   case TDOUBLE:
      ffgcfd(var->fptr, var->colnum, fRow, 1L, nelem,
	     (double *)data, undef, &anynul, &status);
      break;
   }

   if( status ) {
      gParse.status = status;
      return pERROR;
   }

   return 0;
}
