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

### Capabilities

int
GRF__ESC()
 CODE:
#ifdef GRF__ESC
    RETVAL = GRF__ESC;
#else
    Perl_croak(aTHX_ "Constant GRF__ESC not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__MJUST()
 CODE:
#ifdef GRF__MJUST
    RETVAL = GRF__MJUST;
#else
    Perl_croak(aTHX_ "Constant GRF__MJUST not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__SCALES()
 CODE:
#ifdef GRF__SCALES
    RETVAL = GRF__SCALES;
#else
    Perl_croak(aTHX_ "Constant GRF__SCALES not defined\n");
#endif
 OUTPUT:
  RETVAL

### Escape sequences

int
GRF__ESPER()
 CODE:
#ifdef GRF__ESPER
    RETVAL = GRF__ESPER;
#else
    Perl_croak(aTHX_ "Constant GRF__ESPER not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESSUP()
 CODE:
#ifdef GRF__ESSUP
    RETVAL = GRF__ESSUP;
#else
    Perl_croak(aTHX_ "Constant GRF__ESSUP not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESSUB()
 CODE:
#ifdef GRF__ESSUB
    RETVAL = GRF__ESSUB;
#else
    Perl_croak(aTHX_ "Constant GRF__ESSUB not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESGAP()
 CODE:
#ifdef GRF__ESGAP
    RETVAL = GRF__ESGAP;
#else
    Perl_croak(aTHX_ "Constant GRF__ESGAP not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESBAC()
 CODE:
#ifdef GRF__ESBAC
    RETVAL = GRF__ESBAC;
#else
    Perl_croak(aTHX_ "Constant GRF__ESBAC not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESSIZ()
 CODE:
#ifdef GRF__ESSIZ
    RETVAL = GRF__ESSIZ;
#else
    Perl_croak(aTHX_ "Constant GRF__ESSIZ not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESWID()
 CODE:
#ifdef GRF__ESWID
    RETVAL = GRF__ESWID;
#else
    Perl_croak(aTHX_ "Constant GRF__ESWID not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESFON()
 CODE:
#ifdef GRF__ESFON
    RETVAL = GRF__ESFON;
#else
    Perl_croak(aTHX_ "Constant GRF__ESFON not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESCOL()
 CODE:
#ifdef GRF__ESCOL
    RETVAL = GRF__ESCOL;
#else
    Perl_croak(aTHX_ "Constant GRF__ESCOL not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESSTY()
 CODE:
#ifdef GRF__ESSTY
    RETVAL = GRF__ESSTY;
#else
    Perl_croak(aTHX_ "Constant GRF__ESSTY not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESPOP()
 CODE:
#ifdef GRF__ESPOP
    RETVAL = GRF__ESPOP;
#else
    Perl_croak(aTHX_ "Constant GRF__ESPOP not defined\n");
#endif
 OUTPUT:
  RETVAL

int
GRF__ESPSH()
 CODE:
#ifdef GRF__ESPSH
    RETVAL = GRF__ESPSH;
#else
    Perl_croak(aTHX_ "Constant GRF__ESPSH not defined\n");
#endif
 OUTPUT:
  RETVAL

