typedef union {
    int    Node;        /* Index of Node */
    double dbl;         /* real value    */
    long   lng;         /* integer value */
    char   log;         /* logical value */
    char   str[256];    /* string value  */
} FFSTYPE;
#define	BOOLEAN	258
#define	LONG	259
#define	DOUBLE	260
#define	STRING	261
#define	BITSTR	262
#define	FUNCTION	263
#define	BFUNCTION	264
#define	GTIFILTER	265
#define	REGFILTER	266
#define	COLUMN	267
#define	BCOLUMN	268
#define	SCOLUMN	269
#define	BITCOL	270
#define	ROWREF	271
#define	OR	272
#define	AND	273
#define	EQ	274
#define	NE	275
#define	GT	276
#define	LT	277
#define	LTE	278
#define	GTE	279
#define	POWER	280
#define	NOT	281
#define	INTCAST	282
#define	FLTCAST	283
#define	UMINUS	284


extern FFSTYPE fflval;
