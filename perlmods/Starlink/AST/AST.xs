/*

  AST.xs

  Copyright (C) 2004 Tim Jenness. All Rights Reserved.

*/

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#ifdef __cplusplus
}
#endif


#include "ast.h"

/* Helper functions */
#include "arrays.h"
#include "arrays.c"

char ** pack1Dchar( AV * avref ) {
  int i;
  SV ** elem;
  char ** outarr;
  int len;

  /* number of elements */
  len  = av_len( avref ) + 1;
  /* Temporary storage */
  outarr = get_mortalspace( len,'v');

  for (i=0; i<len; i++) {
    elem = av_fetch( avref, i, 0);
    if (elem == NULL ) {
      /* undef */
    } else {
      outarr[i] = SvPV( *elem, PL_na );
    }
  }
  return outarr;
}

MODULE = Starlink::AST     PACKAGE = Starlink::AST


void
astBegin()
 CODE:
   astBegin;


void
astEnd()
 CODE:
   astEnd;

int
astVersion()
 CODE:
  RETVAL = astVersion;
 OUTPUT:
  RETVAL

void
astIntraReg()
 CODE:
   Perl_croak(aTHX_ "astIntraReg Not yet implemented\n");

MODULE = Starlink::AST  PACKAGE = Starlink::AST        PREFIX = ast

int
astVersion( class )
  char * class
 CODE:
  RETVAL = astVersion;
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::Frame

AstFrame *
new( class, naxes, options )
  char * class
  int naxes
  char * options
 CODE:
  RETVAL = astFrame( naxes, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::FrameSet

AstFrameSet *
new( class, frame, options )
  char * class
  AstFrame * frame
  char * options
 CODE:
  RETVAL = astFrameSet( frame, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::CmpFrame

AstCmpFrame *
new( class, frame1, frame2, options )
  char * class
  AstFrame * frame1
  AstFrame * frame2
  char * options
 CODE:
  RETVAL = astCmpFrame( frame1, frame2, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::CmpMap

AstCmpMap *
new( class, map1, map2, series, options )
  char * class
  AstMapping * map1
  AstMapping * map2
  int series
  char * options
 CODE:
  RETVAL = astCmpMap( map1, map2, series, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::FitsChan

# Note that FitsChan inherits from AstChannelPtr

# Need to add proper support for the callbacks

AstFitsChan *
new( ... )
 CODE:
  RETVAL = astFitsChan( NULL, NULL, "");
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::XmlChan

# Need to add proper support for the callbacks

AstXmlChan *
new( ... )
 CODE:
  RETVAL = astXmlChan( NULL, NULL, "");
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::GrismMap

AstGrismMap *
new( class, options )
  char * class
  char * options
 CODE:
  RETVAL = astGrismMap( options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::IntraMap

AstIntraMap *
new( class, name, nin, nout, options )
  char * class
  char * name
  int nin
  int nout
  char * options
 CODE:
  RETVAL = astIntraMap( name, nin, nout, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::LutMap

AstLutMap *
new( class, lut, start, inc, options )
  char * class
  AV* lut
  double start
  double inc
  char * options
 PREINIT:
  int nlut;
  double * clut;
 CODE:
  nlut = av_len( lut );
  clut = pack1D( (SV*)lut, 'd' );
  RETVAL = astLutMap( nlut, clut, start, inc, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::MathMap

AstMathMap *
new( class, nin, nout, fwd, inv, options )
  char * class
  int nin
  int nout
  AV* fwd
  AV* inv
  char * options
 PREINIT:
  int nfwd;
  int ninv;
  SV** elem;
  int i;
  char ** cfwd;
  char ** cinv;
 CODE:
  nfwd = av_len( fwd ) + 1;
  ninv = av_len( inv ) + 1;
  cfwd = pack1Dchar( fwd );
  cinv = pack1Dchar( inv );
  RETVAL = astMathMap( nin, nout, nfwd, (const char **)cfwd, 
                       ninv, (const char**)cinv, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST  PACKAGE = Starlink::AST::MatrixMap

# Note that form is derived from the size of matrix

AstMatrixMap *
new( class, nin, nout, matrix, options )
  char * class
  int nin
  int nout
  AV* matrix
  char * options
 PREINIT:
  int len;
  int form;
  double * cmatrix;
 CODE:
  len = av_len( matrix );
  /* determine form from number of elements */
  if (len == -1) {
    form = 2;
  } else if (len == nin || len == nout ) {
    form = 1;
  } else if ( len == (nin * nout ) ) {
    form = 0;
  } else {
    Perl_croak(aTHX_ "MatrixMap: matrix len not consistent with nout/nin");
  }
  cmatrix = pack1D((SV*)matrix, 'd');
  RETVAL = astMatrixMap( nin, nout, form, cmatrix, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::PcdMap

AstPcdMap *
new( class, disco, pcdcen, options )
  char * class
  double disco
  AV* pcdcen
  char * options
 PREINIT:
  int len;
  double * cpcdcen;
 CODE:
  len = av_len( pcdcen );
  if (len != 2 ) {
    Perl_croak(aTHX_ "Must supply two values to PcdCen");
  }
  cpcdcen = pack1D((SV*)pcdcen, 'd');
  RETVAL = astPcdMap( disco, cpcdcen, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::PermMap

AstPermMap *
new( class, inperm, outperm, constant, options )
  char * class
  AV* inperm
  AV* outperm
  AV* constant
  char * options
 PREINIT:
  int len;
  int * coutperm;
  int * cinperm;
  double * cconstant;
  int nin;
  int nout;
 CODE:
  nin = av_len( inperm );
  if (nin == -1 ) {
    /* no values */
    cinperm = NULL;
  } else {
    cinperm = pack1D((SV*)inperm, 'i' );
  }
  nout = av_len( outperm );
  if (nout == -1 ) {
    /* no values */
    coutperm = NULL;
  } else {
    coutperm = pack1D((SV*)outperm, 'i' );
  }
  len = av_len( constant );
  if (len == -1 ) {
    /* no values */
    cconstant = NULL;
  } else {
    cconstant = pack1D((SV*)constant, 'd' );
  }
  RETVAL = astPermMap(nin, cinperm, nout, coutperm, cconstant, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::PolyMap

AstPolyMap *
new( class )
 CODE:
  Perl_croak(aTHX_ "PolyMap not yet implemented");

MODULE = Starlink::AST   PACKAGE = Starlink::AST::ShiftMap

AstShiftMap *
new( class, shift, options )
  char * class
  AV* shift
  char * options
 PREINIT:
  int ncoord;
  double * cshift;
 CODE:
  ncoord = av_len( shift );
  cshift = pack1D((SV*)shift, 'd');
  RETVAL = astShiftMap( ncoord, cshift, options);
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::SkyFrame

AstSkyFrame *
new( class, options )
  char * class
  char * options
 CODE:
  RETVAL = astSkyFrame( options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::SpecFrame

AstSpecFrame *
new( class, options )
  char * class
  char * options
 CODE:
  RETVAL = astSpecFrame( options );
 OUTPUT:
  RETVAL


MODULE = Starlink::AST   PACKAGE = Starlink::AST::SlaMap

AstSlaMap *
new( class, flags, options )
  char * class
  int flags
  char * options
 CODE:
  RETVAL = astSlaMap( flags, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::SpecMap

AstSpecMap *
new( class, nin, flags, options )
  char * class
  int nin
  int flags
  char * options
 CODE:
  RETVAL = astSpecMap( nin, flags, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::UnitMap

AstUnitMap *
new( class, ncoord, options )
  char * class
  int ncoord
  char * options
 CODE:
  RETVAL = astUnitMap( ncoord, options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::WcsMap

AstWcsMap *
new( class, ncoord, type, lonax, latax, options )
  char * class
  int ncoord
  int type
  int lonax
  int latax
  char * options
 CODE:
  RETVAL = astWcsMap( ncoord, type, lonax, latax,options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::WinMap

# we derive ncoord from the input array dimensions

AstWinMap *
new( class, ina, inb, outa, outb, options )
  char * class
  AV* ina
  AV* inb
  AV* outa
  AV* outb
  char * options
 CODE:
  /* minimal arg checking - lazy XXXX */
  RETVAL = astWinMap( av_len(ina), pack1D((SV*)ina,'d'), pack1D((SV*)inb,'d'),
                      pack1D((SV*)outa,'d'), pack1D((SV*)outb,'d'),options );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = Starlink::AST::ZoomMap

AstZoomMap *
new( class, ncoord, zoom, options )
  char * class
  int ncoord
  double zoom
  char * options
 CODE:
  RETVAL = astZoomMap( ncoord, zoom, options );
 OUTPUT:
  RETVAL


MODULE = Starlink::AST   PACKAGE = AstObjectPtr PREFIX = ast

void
astClear( this, attrib )
  AstObject * this
  char * attrib
 CODE:
   astClear( this, attrib );

void
astAnnul( this )
  AstObject * this
 CODE:
  astAnnul( this );

AstObject *
astClone( this )
  AstObject * this
 CODE:
  RETVAL = astClone( this );
 OUTPUT:
  RETVAL

AstObject *
astCopy( this )
  AstObject * this
 CODE:
  RETVAL = astCopy( this );
 OUTPUT:
  RETVAL

void
astDelete( this )
  AstObject * this
 CODE:
  astDelete( this );

void
astExempt( this )
  AstObject * this
 CODE:
  astExempt( this );

void
astExport( this )
  AstObject * this
 CODE:
  astExport( this );

const char *
astGetC( this, attrib )
  AstObject * this
  char * attrib
 CODE:
  RETVAL = astGetC( this, attrib );
 OUTPUT:
  RETVAL

# Float is just an alias for double

double
astGetD( this, attrib )
  AstObject * this
  char * attrib
 ALIAS:
  astGetF = 1 
 CODE:
  RETVAL = astGetD( this, attrib );
 OUTPUT:
  RETVAL

int
astGetI( this, attrib )
  AstObject * this
  char * attrib
 CODE:
  RETVAL = astGetI( this, attrib );
 OUTPUT:
  RETVAL

long
astGetL( this, attrib )
  AstObject * this
  char * attrib
 CODE:
  RETVAL = astGetL( this, attrib );
 OUTPUT:
  RETVAL

# Need to decide later whether the astIsA functions need to be
# implemented



# sprintf behaviour is left to the enclosing perl layer

void
ast_Set(this, settings )
  AstObject * this
  char * settings
 CODE:
  astSet(this, settings );

void
astSetC( this, attrib, value )
  AstObject * this
  char * attrib
  char * value
 CODE:
  astSetC( this, attrib, value );

# Float is just an alias for double

void
astSetD( this, attrib, value )
  AstObject * this
  char * attrib
  double value
 ALIAS:
  astSetF = 1 
 CODE:
  astSetD( this, attrib, value );


void
astSetI( this, attrib, value )
  AstObject * this
  char * attrib
  int value
 CODE:
  astSetI( this, attrib, value );


void
astSetL( this, attrib, value )
  AstObject * this
  char * attrib
  long value
 CODE:
  astSetL( this, attrib, value );

void
astShow( this )
  AstObject * this
 CODE:
  astShow( this );

bool
astTest( this, attrib )
  AstObject * this
  char * attrib
 CODE:
  RETVAL = astTest( this, attrib );
 OUTPUT:
  RETVAL

# This duplicates the Starlink::AST->Version method

int
astVersion(class)
  AstObject * class
 CODE:
  RETVAL = astVersion;
 OUTPUT:
  RETVAL

# Use annul as automatic destructor

void
ast_DESTROY( this )
  AstObject * this
 CODE:
  astAnnul( this );

MODULE = Starlink::AST   PACKAGE = AstFramePtr PREFIX = ast


double
astAngle( this, a, b, c )
  AstFrame * this
  AV* a
  AV* b
  AV* c
 PREINIT:
  double * aa;
  double * bb;
  double * cc;
  int naxes;
 CODE:
  /* Create C arrays of the correct dimensions */
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(a) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  if (av_len(b) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in second coord array must be %d",
                naxes);
  if (av_len(c) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in third coord array must be %d",
                naxes);

  aa = pack1D( (SV*)a, 'd');
  bb = pack1D( (SV*)b, 'd');
  cc = pack1D( (SV*)c, 'd');

  /* Call the ast function */
  RETVAL = astAngle( this, aa, bb, cc);
 OUTPUT:
  RETVAL

double
astAxAngle( this, a, b, axis )
  AstFrame * this
  AV* a
  AV* b
  int axis
 PREINIT:
  double * aa;
  double * bb;
  int naxes;
 CODE:
  /* Create C arrays of the correct dimensions */
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(a) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  if (av_len(b) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in second coord array must be %d",
                naxes);

  aa = pack1D( (SV*)a, 'd');
  bb = pack1D( (SV*)b, 'd');
  RETVAL = astAxAngle( this, aa, bb, axis);
 OUTPUT:
  RETVAL

double
astAxDistance( this, axis, v1, v2)
  AstFrame * this
  int axis
  double v1
  double v2
 CODE:
  RETVAL = astAxDistance( this, axis, v1, v2);
 OUTPUT:
  RETVAL

double
astAxOffset( this, axis, v1, dist)
  AstFrame * this
  int axis
  double v1
  double dist
 CODE:
  RETVAL = astAxOffset( this, axis, v1, dist);
 OUTPUT:
  RETVAL

AstFrameSet *
astConvert( from, to, domainlist )
  AstFrame * from
  AstFrame * to
  char * domainlist
 CODE:
  RETVAL = astConvert( from, to, domainlist );
 OUTPUT:
  RETVAL

double
astDistance( this, point1, point2 )
  AstFrame * this
  AV* point1
  AV* point2
 PREINIT:
  double * aa;
  double * bb;
  int naxes;
 CODE:
  /* Create C arrays of the correct dimensions */
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(point1) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  if (av_len(point2) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in second coord array must be %d",
                naxes);

  aa = pack1D( (SV*)point1, 'd');
  bb = pack1D( (SV*)point2, 'd');
  RETVAL = astDistance( this, aa, bb);
 OUTPUT:
  RETVAL

AstFrameSet *
astFindFrame( this, template, domainlist )
  AstFrame * this
  AstFrame * template
  char * domainlist
 CODE:
  RETVAL = astFindFrame( this, template, domainlist );
 OUTPUT:
  RETVAL

const char *
astFormat( this, axis, value )
  AstFrame * this
  int axis
  double value
 CODE:
  RETVAL = astFormat( this, axis, value );
 OUTPUT:
  RETVAL

int
astGetActiveUnit( this )
  AstFrame * this
 CODE:
  RETVAL = astGetActiveUnit( this );
 OUTPUT:
  RETVAL

void
astNorm( this, value )
  AstFrame * this
  AV* value
 PREINIT:
  int naxes;
  double * aa;
 CODE:
  /* Create C arrays of the correct dimensions */
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(value) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  aa = pack1D( (SV*)value, 'd');
  astNorm( this, aa );

# Return list

void
astOffset( this, point1, point2, offset )
  AstFrame * this
  AV* point1
  AV* point2
  double offset
 PREINIT:
  int naxes;
  double * aa;
  double * bb;
  double * point3;
  int i;
 PPCODE:
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(point1) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  aa = pack1D( (SV*)point1, 'd');
  if (av_len(point2) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in second coord array must be %d",
                naxes);
  bb = pack1D( (SV*)point2, 'd');


  /* Somewhere to put the return values */
  point3 = get_mortalspace( naxes, 'd' );

  astOffset( this, aa, bb, offset, point3 );

  /* now need to push the resulting values onto the return stack */
  for (i =0; i < naxes; i++ ) {
    XPUSHs(sv_2mortal(newSVnv(point3[i])));
  }

# This method technically returns an angle and a pair of coordinates
# We could return this as a list of the angle and ref to array.
# Currently return 3 numbers

void
astOffset2( this, point1, angle, offset )
  AstFrame * this
  AV* point1
  double angle
  double offset
 PREINIT:
  int naxes;
  double * aa;
  double * point2;
  int i;
  double RETVAL;
 PPCODE:
  naxes = astGetI( this, "Naxes" );

  /* Copy from the perl array to the C array */
  if (av_len(point1) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in first coord array must be %d",
                naxes);
  aa = pack1D( (SV*)point1, 'd');

  /* Somewhere to put the return values */
  point2 = get_mortalspace( naxes, 'd' );

  RETVAL = astOffset2( this, aa, angle, offset, point2 );

  /* Push the angle on to the stack */
  XPUSHs(sv_2mortal(newSVnv(RETVAL)));

  /* now need to push the resulting values onto the return stack */
  for (i =0; i < naxes; i++ ) {
    XPUSHs(sv_2mortal(newSVnv(point2[i])));
  }

void
astPermAxes( this, perm )
  AstFrame * this
  AV* perm
 PREINIT:
  int * aa;
  int naxes;
 CODE:
  naxes = astGetI(this, "Naxes");
  /* Copy from the perl array to the C array */
  if (av_len(perm) != naxes-1)
     Perl_croak(aTHX_ "Number of elements in perm array must be %d",
                naxes);
  aa = pack1D( (SV*)perm, 'i');
  astPermAxes( this, aa );

# Returns a new frame and an optional mapping
# Use a boolean to control whether a mapping is returned
# Ignore for now

# Also note that we count axes ourselfs

AstFrame *
astPickAxes( this, axes )
  AstFrame * this;
  AV* axes
 PREINIT:
  int maxaxes;
  int naxes;
  int * aa;
 CODE:
  maxaxes = astGetI(this, "Naxes");
  naxes = av_len(axes) + 1;
  if ( naxes > maxaxes )
    Perl_croak(aTHX_ "Number of axes selected must be less than number of axes in frame");
  aa = pack1D( (SV*)axes, 'i');
  RETVAL = astPickAxes( this, naxes, aa, NULL);
 OUTPUT:
  RETVAL

# Need to think about astResolve  XXXXX

void
astSetActiveUnit( this, value )
  AstFrame * this
  int value
 CODE:
  astSetActiveUnit( this, value );

# astUnformat currently returns the value not the number of
# characters read. Returns undef if no character read
#  XXXXX

double
astUnformat( this, axis, string )
  AstFrame * this
  int axis
  char * string
 PREINIT:
  int nread;
 CODE:
  nread = astUnformat( this, axis, string, &RETVAL );
  if (nread == 0 ) XSRETURN_UNDEF;
 OUTPUT:
  RETVAL


MODULE = Starlink::AST   PACKAGE = AstFrameSetPtr PREFIX = ast

void
astAddFrame( this, iframe, map, frame)
  AstFrameSet * this
  int iframe
  AstMapping * map
  AstFrame * frame
 CODE:
  astAddFrame( this, iframe, map, frame );


AstFrame *
astGetFrame( this, iframe )
  AstFrameSet * this
  int iframe
 CODE:
  RETVAL = astGetFrame( this, iframe );
 OUTPUT:
  RETVAL

AstMapping *
astGetMapping( this, iframe1, iframe2 )
  AstFrameSet * this
  int iframe1
  int iframe2
 CODE:
  RETVAL = astGetMapping( this, iframe1, iframe2 );
 OUTPUT:
  RETVAL

void
astRemapFrame( this, iframe, map )
  AstFrameSet * this
  int iframe
  AstMapping * map
 CODE:
  astRemapFrame( this, iframe, map );

void
astRemoveFrame( this, iframe )
  AstFrameSet * this
  int iframe
 CODE:
  astRemoveFrame( this, iframe );

MODULE = Starlink::AST   PACKAGE = AstMappingPtr PREFIX = ast

# Return the new mappings and booleans as a list
# Do this later since it requires subversion of the typemap
# system

# XXXXX

void
astDecompose( this )
  AstMapping * this
 PREINIT:
  AstMapping * map1;
  AstMapping * map2;
  int series;
  int invert1;
  int invert2;
 PPCODE:
  Perl_croak("astDecompose not yet implemented\n");
  astDecompose(this, &map1, &map2, &series, &invert1, &invert2);

void
astInvert( this )
  AstMapping * this
 CODE:
  astInvert( this );

# astMapBox  XXXX


# astRate    XXXXX


# astResample XXXX


AstMapping *
astSimplify( this )
  AstMapping * this
 CODE:
  RETVAL = astSimplify( this );
 OUTPUT:
  RETVAL

# astTran1  XXXX

# astTran2  XXXX

# astTranN  XXXX

# astTranP  XXXX



MODULE = Starlink::AST   PACKAGE = AstChannelPtr PREFIX = ast

AstObject *
astRead( channel )
  AstChannel * channel
 CODE:
  RETVAL = astRead( channel );
 OUTPUT:
  RETVAL

int
astWrite( channel, object )
  AstChannel * channel
  AstObject * object
 CODE:
  RETVAL = astWrite( channel, object );
 OUTPUT:
  RETVAL

MODULE = Starlink::AST   PACKAGE = AstFitsChanPtr PREFIX = ast

void
astPutFits( this, card, overwrite )
  AstFitsChan * this
  char * card
  int overwrite
 CODE:
  astPutFits(this, card, overwrite);

void
astDelFits( this )
  AstFitsChan * this
 CODE:
  astDelFits( this );

# Need to handle a NULL card  - XXXXX

int
astFindFits( this, name, card, inc )
  AstFitsChan * this
  char * name
  char * card = NO_INIT
  int inc
 PREINIT:
  char buff[81];
 CODE:
  card = buff;
  RETVAL = astFindFits( this, name, card, inc );
 OUTPUT:
  RETVAL 
  card

MODULE = Starlink::AST   PACKAGE = AstSpecFramePtr PREFIX = ast

void
astSetRefPos( this, frm, lon, lat)
  AstSpecFrame * this
  AstSkyFrame * frm
  double lon
  double lat
 CODE:
  astSetRefPos( this, frm, lon, lat );

# XXX frm is allowed to be null here

void
astGetRefPos( this, frm )
  AstSpecFrame * this
  AstSkyFrame * frm
 PREINIT:
  double lon;
  double lat;
 PPCODE:
  astGetRefPos( this, frm, &lon, &lat );
  XPUSHs(sv_2mortal(newSVnv(lon)));
  XPUSHs(sv_2mortal(newSVnv(lat)));
 

MODULE = Starlink::AST   PACKAGE = AstSlaMapPtr PREFIX = astSla

void
astSlaAdd( this, cvt, args )
  AstSlaMap * this
  char * cvt
  AV* args
 PREINIT:
  double * cargs;
 CODE:
  cargs = pack1D((SV*)args, 'd');
  astSlaAdd( this, cvt, cargs );

MODULE = Starlink::AST   PACKAGE = AstSpecMapPtr PREFIX = astSpec

void
astSpecAdd( this, cvt, args )
  AstSpecMap * this
  char * cvt
  AV* args
 PREINIT:
  double * cargs;
 CODE:
  cargs = pack1D((SV*)args, 'd');
  astSpecAdd( this, cvt, cargs );
