/*
 * Already done these definitions?
 */
#if !defined(_ADI_PARSE_H_)
#define _ADI_PARSE_H_ 1

#define ADI_FILENAME_BUF	200

typedef
  enum
    {
    TOK__NULL,
    TOK__CONST,
    TOK__SYM,           TOK__COMMA,		TOK__AT,
    TOK__ATAT,		TOK__SLASHAT,
    TOK__EQ,            TOK__NE,                TOK__LT,
    TOK__GT,            TOK__LE,                TOK__GE,
    TOK__PLUSEQ,        TOK__MINUSEQ,           TOK__MULEQ,
    TOK__DIVEQ,         TOK__CONC,		TOK__BSLASH,
    TOK__PLUS,          TOK__MINUS,             TOK__MUL,
    TOK__DIV,           TOK__CARAT,             TOK__ASSIGN,
    TOK__OR,            TOK__AND,               TOK__NOT,
    TOK__LPAREN,        TOK__RPAREN,		TOK__CARAT2,
    TOK__LBRACE,        TOK__RBRACE,		TOK__COLONEQ,
    TOK__UND1,		TOK__UND2,		TOK__UND3,
    TOK__LBRAK,         TOK__RBRAK,		TOK__PERIOD,
    TOK__SEMICOLON,     TOK__COLON,             TOK__SCOPE,
    TOK__QUERY,		TOK__QUERY2,		TOK__BAR,
    TOK__COND,          TOK__RARROW,
    TOK__LCHEV,         TOK__RCHEV,             TOK__RCHEV3,
    TOK__DOLLAR,        TOK__INCR,              TOK__DECR,
    TOK__END,
    TOK__TRAP,          TOK__NOTATOK
    }
  ADItokenType;


typedef
  struct {
    ADItokenType        t;
    char                dat[200];
    int                 nc;
    char		*ptr0;
    ADIobj              dt;
    ADIobj		cs;
    }
  ADItoken;

#define ADI_STREAM__MAXCPB 3
#define ADI_STREAM__MAXTPB 1

typedef
  struct ADIdeviceTag *ADIdevicePtr;

typedef
  enum {
    ADIdevFile, ADIdevCstring
    }
  ADIdeviceType;

typedef
  struct ADIdeviceTag {
    FILE                *f;
    ADIdevicePtr        last;
    ADIdeviceType       type;
    char                *buf;
    int                 bufsiz;
    int			bnc;
    int			line,col;
    char                *ptr;
    unsigned		pstatic:1;	/* 'f' or 'ptr' is static */
    unsigned		dstatic:1;	/* Static data? */
    unsigned		isatty:1;	/* Interactive device? */
    char		cmode;		/* String copy mode */
    ADIobj		name;
    }
  ADIdevice;


#define ADI_STREAM__DEFAULT 0x0000      /* Reset all bits */
#define ADI_STREAM__IN      0x0001      /* Input stream */
#define ADI_STREAM__OUT	    0x0002	/* Output stream */
#define ADI_STREAM__EOLISP  0x0004	/* End of line is white space */
#define ADI_STREAM__MODEBITS (ADI_STREAM__IN|ADI_STREAM__OUT)

typedef
  struct {
    int                 nc_pb;
    char                c_pb[ADI_STREAM__MAXCPB];
    int                 nt_pb;
    ADItoken            tok_pb[ADI_STREAM__MAXTPB];
    ADItoken            ctok;
    ADIdevice		basedev;
    ADIdevicePtr        dev;
    int			flags;
    }
  ADIstream;

#define         _strm_q(_x)     (_DTDEF(_x)->selfid==UT_cid_strm)

#define         _strm_data(_x)   ((ADIstream *) _DTDAT(_x))


typedef
  enum {
    None, LtoR, RtoL
    }
  Associativity;

typedef
  struct {
    ADItokenType        tok;            /* Lexical token */
    int                 priority;       /* Priority */
    Associativity       assoc;          /* Associativity */
    ADIobj		*key;	   	/* Head keyword code */
    }
  ADIoperatorDescrip;

#define DEFINE_OP_TABLE(_name) \
	static ADIoperatorDescrip _name[] = {

#define DEFINE_OP_TENTRY(_tok,_pri,_ass,_kvar) \
	{_tok,_pri,_ass,&_kvar}

#define END_OP_TABLE	{TOK__TRAP,0,None,NULL}}

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Sub-package initialiser
 */
void   	prsx_init( ADIstatus status );
ADIobj  prsx_symname( ADIobj stream, ADIstatus status );
ADIobj	prsx_cvalue( ADIobj stream, ADIstatus status );
void	prsx_namvalcmp( ADIobj stream, ADIobj id, ADIstatus status );

/*
 * ADI internal routines
 */
ADIobj	adix_istrm_i( ADIstream *stream, ADIstatus status );
ADIinteger 	adix_ostrm_i( ADIstream *stream, ADIobj id, int cmode,
			ADIstatus status );

ADIobj 		ADIparseBlankExp( ADIobj str, ADIobj grammar, ADIstatus status );
ADIobj 		ADIparseExpInt( ADIobj str, ADIobj grammar, int priority, ADIstatus status );
void		ADIparseClassSupers( ADIobj str, ADIobj *slist,
			ADIobj *mlist, ADIstatus status );
void		ADIparseClassMembers( ADIobj str, ADIobj *mlist, ADIstatus status );
ADIobj 		ADIparseComDelList( ADIobj pstream, ADIobj grammar, ADItokenType endtok,
			ADIlogical mend, ADIstatus status );
void 		ADIparseError( ADIobj stream, ADIstatype code, char *ctx,
			ADIstatus status, ... );
ADIinteger 	ADIparseScanInt( ADIobj stream, char *data, int base,
			ADIstatus status );
ADIlogical 	ADIparseScanLog( ADIobj stream, ADIlogical *lval,
			ADIstatus status );
ADItokenType	ADIcurrentToken( ADIobj stream, ADIstatus status );

ADIobj 		ADIstrmDel( int narg, ADIobj args[], ADIstatus status );

void 		ADIstrmGetTokenData( ADIobj stream, char **data, int *len,
			ADIstatus status );
ADIobj		ADIstrmNew( char *mode, ADIstatus status );
ADIobj    	ADIstrmExtendC( ADIobj str, char *buf, int blen,
			ADIstatus status );
ADIobj  	ADIstrmExtendCst( ADIobj str, char *buf, int blen,
			ADIstatus status );
ADIobj  	ADIstrmExtendFile( ADIobj str, ADIobj name, FILE *f, ADIstatus status );
void 		ADIstrmFflush( ADIobj stream, ADIstatus status );
void 		ADIstrmFlush( ADIstatus status );
void		ADIstrmPutCh( ADIobj str, char ch, ADIstatus status );
void 		ADIstrmCBprintf( char *buf, int blen, char *fmt, va_list ap,
			int *used, ADIstatus status );
void 		ADIstrmFprintf( ADIobj stream, char *format,
			ADIstatus status, ... );
void 		ADIstrmPrintf( char *format, ADIstatus status, ... );
int		ADIstrmPutInt( ADIdevice *dev, char *data, int n, int check,
			ADIstatus status );
int 		ADIstrmVprintf( ADIobj stream, char *format,
			va_list ap, ADIstatus status );

void    	ADIaddDeviceToStream( ADIobj str, ADIdeviceType type,
			ADIstatus status );
void    	ADIclearStream( ADIobj str, ADIstatus status );
void    	ADIdescribeToken( ADItokenType tok, char **str, int *len );
void    	ADIdropDevice( ADIobj str, ADIstatus status );
void    	ADIdropStream( ADIobj str, ADIstatus status );
ADItokenType 	ADIisTokenInSet( ADIobj str, ADItokenType tlist[],
			ADIstatus status );
ADIlogical	ADIisTokenCstring( ADIobj str, char *string,
			ADIstatus status );
void      	ADImatchToken( ADIobj str, ADItokenType t,
			ADIstatus status );
ADIlogical      ADIifMatchToken( ADIobj str, ADItokenType t,
			ADIstatus status );
ADItokenType 	ADInextToken( ADIobj str, ADIstatus status );
char         	ADIreadCharFromStream( ADIstream *str, ADIstatus status );
void		ADIreadCharsFromStream( ADIstream *str, int n, char *data,
			ADIstatus status );
void    	ADIresetStream( ADIobj str, ADIstatus status );
void    	ADIreturnCharToStream( ADIobj str, char ch,
			ADIstatus status );

int 		ADIgetStreamAttrs( ADIobj str, ADIstatus status );
void 		ADIputStreamAttrs( ADIobj str, int flag, ADIstatus status );
int 		ADIsetStreamAttr( ADIobj str, int flag, ADIstatus status );

#ifdef __cplusplus
}
#endif

extern ADIobj  EXC_ArrayBound;
extern ADIobj  EXC_BoolExp;
extern ADIobj  EXC_ControlC;
extern ADIobj  EXC_Error;
extern ADIobj  EXC_ExceedMaxRecurse;
extern ADIobj  EXC_InvalidArg;
extern ADIobj  EXC_MathErr;
extern ADIobj  EXC_NoSuchField;
extern ADIobj  EXC_ReturnValue;
extern ADIobj  EXC_ScopeBreak;
extern ADIobj  EXC_SyntaxError;

extern ADIobj  K_AddTo;
extern ADIobj  K_Alternatives;
extern ADIobj  K_And;
extern ADIobj  K_Apply;
extern ADIobj  K_Array;
extern ADIobj  K_ArrayRef;

extern ADIobj  K_Binding;
extern ADIobj  K_Blank;
extern ADIobj  K_BlankSeq;
extern ADIobj  K_BlankNullSeq;
extern ADIobj  K_Break;

extern ADIobj  K_Catch;
extern ADIobj  K_Concat;
extern ADIobj  K_Condition;

extern ADIobj  K_DefClass;
extern ADIobj  K_DefEnum;
extern ADIobj  K_DefProc;
extern ADIobj  K_DefRep;
extern ADIobj  K_Divide;
extern ADIobj  K_DivideBy;
extern ADIobj  K_Dot;
extern ADIobj  K_DoWhile;

extern ADIobj  K_Echo;
extern ADIobj  K_Equal;

extern ADIobj  K_Factorial;
extern ADIobj  K_Finally;
extern ADIobj  K_Foreach;

extern ADIobj  K_Get;
extern ADIobj  K_GE;
extern ADIobj  K_GT;
extern ADIobj  K_Global;

extern ADIobj  K_HoldAll;
extern ADIobj  K_HoldFirst;
extern ADIobj  K_HoldRest;

extern ADIobj  K_If;

extern ADIobj  K_List;
extern ADIobj  K_Listable;
extern ADIobj  K_LE;
extern ADIobj  K_LT;
extern ADIobj  K_Local;

extern ADIobj  K_Map;
extern ADIobj  K_Multiply;
extern ADIobj  K_MultiplyBy;

extern ADIobj  K_Negate;
extern ADIobj  K_Not;
extern ADIobj  K_NotEqual;

extern ADIobj  K_Or;

extern ADIobj  K_Pattern;
extern ADIobj  K_PatternTest;
extern ADIobj  K_Plus;
extern ADIobj  K_PostDec;
extern ADIobj  K_PostInc;
extern ADIobj  K_Power;
extern ADIobj  K_PreDec;
extern ADIobj  K_PreInc;
extern ADIobj  K_Print;
extern ADIobj  K_Put;

extern ADIobj  K_Query;

extern ADIobj  K_Raise;
extern ADIobj  K_Range;
extern ADIobj  K_Require;
extern ADIobj  K_ReRaise;
extern ADIobj  K_Return;

extern ADIobj  K_Set;
extern ADIobj  K_SetDelayed;
extern ADIobj  K_Subtract;
extern ADIobj  K_SubtractFrom;
extern ADIobj  K_Switch;
extern ADIobj  K_Symbol;

extern ADIobj  K_Try;

extern ADIobj  K_While;
extern ADIobj  K_WildCard;

/* Structure written to start of every object stream
 */
typedef
  struct {
    char	magic[4];
    char	major;
    char 	minor;
    char	spare[2];
    }
  ADIstrmHdr;

/*
 * Write primitives
 */
#define _OS_STRM_write(_s,_n,_d) ADIstrmPutInt((_s)->dev,(char *) (_d),(_n),0,status)

#define _OS_STRM_id(_s,_d)	_OS_STRM_write(_s,sizeof(ADIobj),_d)
#define _OS_STRM_b(_s,_d)	_OS_STRM_write(_s,sizeof(ADIbyte),_d)
#define _OS_STRM_w(_s,_d)	_OS_STRM_write(_s,sizeof(ADIword),_d)
#define _OS_STRM_i(_s,_d)	_OS_STRM_write(_s,sizeof(ADIinteger),_d)
#define _OS_STRM_r(_s,_d)	_OS_STRM_write(_s,sizeof(ADIreal),_d)
#define _OS_STRM_d(_s,_d)	_OS_STRM_write(_s,sizeof(ADIdouble),_d)
#define _OS_STRM_l(_s,_d)	_OS_STRM_write(_s,sizeof(ADIlogical),_d)
#define _OS_STRM_tm(_s,_d)	_OS_STRM_write(_s,1,_d)

typedef
  ADIinteger (*ADIcOstreamer)(ADIstream *,ADIobj,char *,int,ADIstatus);

/*
 * Read primitives
 */
typedef
  ADIobj (*ADIcIstreamer)(ADIstream *,ADIstatus);

#define _IS_STRM_read(_s,_n,_d) ADIreadCharsFromStream((_s),(_n), (char *) (_d),status)

#define _IS_STRM_id(_s,_d)	_IS_STRM_read(_s,sizeof(ADIobj),_d)
#define _IS_STRM_b(_s,_d)	_IS_STRM_read(_s,sizeof(ADIbyte),_d)
#define _IS_STRM_w(_s,_d)	_IS_STRM_read(_s,sizeof(ADIword),_d)
#define _IS_STRM_i(_s,_d)	_IS_STRM_read(_s,sizeof(ADIinteger),_d)
#define _IS_STRM_r(_s,_d)	_IS_STRM_read(_s,sizeof(ADIreal),_d)
#define _IS_STRM_d(_s,_d)	_IS_STRM_read(_s,sizeof(ADIdouble),_d)
#define _IS_STRM_l(_s,_d)	_IS_STRM_read(_s,sizeof(ADIlogical),_d)
#define _IS_STRM_tm(_s,_d)	_IS_STRM_read(_s,1,_d)

#define _S_MARK_nul	0x00
#define _S_MARK_obj	0x01
#define _S_MARK_han	0x02
#define _S_MARK_ary	0x03

#define _S_MARK_objmask 0x0F

#define _S_MARK_cflag   0x10

#endif

