#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
#include "kaplibs.h"
#include "grp.h"
#include "par.h"
#include "cupid.h"

void clumps() {
/*
*+
*  Name:
*     CLUMPS

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void clumps();

*  Description:
*     This application identifies clumps within a 2 or 3 dimensional NDF,
*     and creates an output catalogue containing the clump parameters.
*     The algorithm used to identify the clumps can be chosen from a
*     list of supported algorithms.

*  Usage:
*     clumps in outcat method mask

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*        Specifies values for the configuration parameters used by the
*        clump finding algorithms. If a null (!) value is supplied, a set
*        of default configuration parameter values will be used.
*
*        A comma-separated list of strings should be given in which each
*        string is either a "keyword=value" setting, or the name of a text 
*        file preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner (any blank lines or lines beginning 
*        with "#"). Settings are applied in the order in which they occur 
*        within the list, with later settings over-riding any earlier 
*        settings given for the same keyword.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*        
*        where <keyword> has the form "algorithm.param"; that is, the name
*        of the algorithm, followed by a dot, followed by the name of the
*        parameter to be set. The parameters available for each algorithm
*        are listed in the "Configuration Parameters" section below. Default 
*        values will be used for any unspecified parameters. [current value]
*     IN = NDF (Update)
*        The 2 or 3 dimensional NDF to be analysed. The Quality component
*        of this NDF will be updated if the parameter MASK is set true.
*        In this case the NDF must not be write-protected. If MASK is set
*        false, then read access only is required, and so the NDF may be
*        write protected.
*     MASK = _LOGICAL (Read)
*        If true, then a mask identifying the pixels within each clump 
*        is written to the Quality array of the supplied NDF, replacing
*        any existing Quality array. For this purpose, the Quality array
*        is considered to be an array of unsigned bytes, and a value
*        between zero and 511 will be written to each element. This is
*        the row number within the output catalogue for the clump 
*        containing the corresponding data pixel. If a pixel contributes
*        to more than one clump, then the corresponding element of the 
*        Quality array will hold the row number of the last clump to be 
*        found at that pixel. [current value]
*     METHOD = LITERAL (Read)
*        The algorithm to use. Each algorithm is described in more detail
*        in the "Algorithms:" section below. Can be one of:
*
*        - GaussClumps
*        - ClumpFind
*
*        Each algorithm has a collection of extra tuning values which are
*        set via the CONFIG parameter.   [current value]
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the clump parameters.

*  Algorithms:
*     - GaussClumps: Described by Stutski & Gusten (1990, ApJ 356, 513).
*     This algorithm proceeds by fitting a Gaussian profile to the 
*     brightest peak in the data. It then subtracts the fit from the data 
*     and iterates, fitting a new ellipse to the brightest peak in the 
*     residuals. This continues until the total value in the fitted ellipses 
*     equals the total value in the original data. Each fitted ellipse is 
*     taken to be a single clump and is added to the output catalogue. In
*     this algorithm, clumps may overlap. 
*     - ClumpFind: Described by Williams et al (1994, ApJ 428, 693). This 
*     algorithm works by first contouring the data at a multiple of the
*     noise, then searches for peaks of emission which locate the clumps,
*     and then follows them down to lower intensities. No a priori clump
*     profile is assumed. In this algorithm, clumps never overlap.

*  Configuration Parameters:
*     Each supported algorithm uses several configuration parameters,
*     values for which can be specified using the CONFIG parameter. These
*     are listed below. Each parameter is specified by the algorithm
*     name, followed by a dot, followed by the parameter name. Default
*     values are shown in square brackets:
*
*     - GaussClumps.MaxClumps: Specifies a termination criterion for
*     the GaussClumps algorithm. The algorithm will terminate when
*     "MaxClumps" clumps have been identified, or when one of the other 
*     termination criteria is met. [unlimited]
*     - GaussClumps.MinIntegral: Specifies a termination criterion 
*     for the GaussClumps algorithm. The algorithm will terminate when the 
*     difference between the integrated intensity of all subtracted clumps 
*     and the integrated intensity in the supplied data array is less than
*     or equal to "MinIntegral" times the integrated intensity in the
*     supplied data array, or when one of the other termination criteria 
*     is met. [0.01]
*     - GaussClumps.PsfPix: Specifies the width of the Point Spread Function
*     within the input data, in pixels. A box filter of this size is used
*     to smooth each row of data when determining the default value for
*     the RMS noise in the data. [3.0]
*     - GaussClumps.RMS: The RMS noise in the data. The default value is
*     determined by smoothing each row with a box filter of width given by
*     "PsfPix" and finding the residuals between the original and
*     smoothed data. []
*     - GaussClumps.Threshold: Specifies a termination criterion for
*     the GaussClumps algorithm. The algorithm will terminate when the peak
*     value in the residuals left after subtraction of the fitted clumps 
*     is less than "Threshold" times the RMS noise in the original data,
*     or when one of the other termination criteria is met. [2.0]

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Variables: */
   AstFrameSet *iwcs;           /* Pointer to the WCS FrameSet */
   AstKeyMap *keymap;           /* Pointer to KeyMap holding config settings */
   char dtype[ 20 ];            /* NDF data type */
   char itype[ 20 ];            /* NDF data type */
   char method[ 15 ];           /* Algorithm string supplied by user */
   int dim[ NDF__MXDIM ];       /* Pixel axis dimensions */
   int el;                      /* Number of array elements mapped */
   int i;                       /* Loop count */
   int igrp;                    /* GRP identifier for configuration settings */
   int indf;                    /* Identifier for input NDF */
   int mask;                    /* Write a mask to the supplied NDF? */
   int ndim;                    /* Total number of pixel axes */
   int nsig;                    /* Number of significant pixel axes */
   int sdim[ NDF__MXDIM ];      /* The indices of the significant pixel axes */
   int size;                    /* Size of the "igrp" group */
   int slbnd[ NDF__MXDIM ];     /* The lower bounds of the significant pixel axes */
   int subnd[ NDF__MXDIM ];     /* The upper bounds of the significant pixel axes */
   int type;                    /* Integer identifier for data type */
   void *ipd;                   /* Pointer to Data array */
   void *ipq;                   /* Pointer to Quality array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();

/* See if a mask is to be written to the Quality component of the NDF. */
   parGet0l( "MASK", &mask, status );

/* Get an identifier for the input NDF, opening it with the minimum
   required access. If required map the quality array. */
   if( mask ) {
      ndfAssoc( "IN", "UPDATE", &indf, status );
      ndfMap( indf, "Quality", "_UBYTE", "WRITE/ZERO", &ipq, &el, 
              status );
   } else {
      ndfAssoc( "IN", "READ", &indf, status );
      ipq = NULL;
   }

/* Get the dimensions of the NDF, and count the significant ones. */
   ndfDim( indf, NDF__MXDIM, dim, &ndim, status );
   nsig = 0;
   for( i = 0; i < ndim; i++ ) {
      if( dim[ i ] > 1 ) nsig++;
   }

/* Abort if the NDF is not 2- or 3- dimensional. */
   if( nsig < 2 || nsig > 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "NDF", indf );
         if( nsig != 1 ) {
            msgSeti( "NSIG", nsig );
            errRep( "CLUMPS_ERR2", "\"^NDF\" has ^NSIG significant "
                    "pixel axes", status );
         } else {
            errRep( "CLUMPS_ERR3", "\"^NDF\" has 1 significant pixel "
                    "axis", status );
         }
         errRep( "CLUMPS_ERR4", "This application requires 2 or 3 "
                 "significant pixel axes", status );
      }
      goto L999;
   }          

/* Get the WCS FrameSet and the significant axis bounds. */
   kpg1Asget( indf, nsig, 1, 0, 0, sdim, slbnd, subnd, &iwcs, status );

/* Choose the data type to use when mapping the NDF Data array. */
   ndfMtype( "_REAL,_DOUBLE", indf, indf, "DATA", itype, 20, dtype, 20,
             status );
   if( !strcmp( itype, "_DOUBLE" ) ) {
      type = CUPID__DOUBLE;
   } else {
      type = CUPID__FLOAT;
   }

/* Map the Data array. */
   ndfMap( indf, "DATA", itype, "READ", &ipd, &el, status );

/* Determine which algorithm to use. */
   parChoic( "METHOD", "GAUSSCLUMPS", "GAUSSCLUMPS,CLUMPFIND", 1, method,
             15,  status );

/* Read a group of configuration setting. */
   igrp = GRP__NOID;
   kpg1Gtgrp( "CONFIG", &igrp, &size, status );

/* If no group was supplied, annul the error and create an empty KeyMap. */
   if( *status == PAR__NULL ) {
      errAnnul( status );
      keymap = astKeyMap( "" );

/* If a group was supplied, create an AST KeyMap holding the value for each 
   configuration setting, indexed using its name, then delete the GRP group. */
   } else {
      kpg1Kymap( igrp, &keymap, status );
      grpDelet( igrp, status );      
   }

/* Switch for each method */
   if( !strcmp( method, "GAUSSCLUMPS" ) ) {
      cupidGaussClumps( type, nsig, slbnd, subnd, ipd, (unsigned char *) ipq, 
                       keymap ); 

   } else if( !strcmp( method, "CLUMPFIND" ) ) {
      cupidClumpFind( type, nsig, slbnd, subnd, ipd, (unsigned char *) ipq,
                     keymap ); 

   } else if( *status == SAI__OK ) {
      msgSetc( "METH", method );
      errRep( "CLUMPS_ERR1", "Requested Method ^METH has not yet been "
              "implemented.", status );
   }

/* Tidy up */
L999:

/* End the NDF context */
   ndfEnd( status );

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "CLUMPS_ERR", "CLUMPS: Failed to identify clumps of emission "
              "within a 2- or 3-D NDF.", status );
   }
}

