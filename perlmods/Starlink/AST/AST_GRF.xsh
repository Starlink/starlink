MODULE = Starlink::AST  PACKAGE = Starlink::AST::Grf

int
GRF__STYLE()
 CODE:
#ifdef GRF__STYLE
    RETVAL = GRF__STYLE;
#else
    Perl_croak(aTHX_ "Constant GRF__STYLE not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__WIDTH()
 CODE:
#ifdef GRF__WIDTH
    RETVAL = GRF__WIDTH;
#else
    Perl_croak(aTHX_ "Constant GRF__WIDTH not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__SIZE()
 CODE:
#ifdef GRF__SIZE
    RETVAL = GRF__SIZE;
#else
    Perl_croak(aTHX_ "Constant GRF__SIZE not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__FONT()
 CODE:
#ifdef GRF__FONT
    RETVAL = GRF__FONT;
#else
    Perl_croak(aTHX_ "Constant GRF__FONT not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__COLOUR()
 CODE:
#ifdef GRF__COLOUR
    RETVAL = GRF__COLOUR;
#else
    Perl_croak(aTHX_ "Constant GRF__COLOUR not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__TEXT()
 CODE:
#ifdef GRF__TEXT
    RETVAL = GRF__TEXT;
#else
    Perl_croak(aTHX_ "Constant GRF__TEXT not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__LINE()
 CODE:
#ifdef GRF__LINE
    RETVAL = GRF__LINE;
#else
    Perl_croak(aTHX_ "Constant GRF__LINE not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__MARK()
 CODE:
#ifdef GRF__MARK
    RETVAL = GRF__MARK;
#else
    Perl_croak(aTHX_ "Constant GRF__MARK not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__NATTR()
 CODE:
#ifdef GRF__NATTR
    RETVAL = GRF__NATTR;
#else
    Perl_croak(aTHX_ "Constant GRF__NATTR not defined\n");
#endif
 OUTPUT:
  RETVAL

