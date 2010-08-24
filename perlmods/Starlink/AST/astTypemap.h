/* Prototypes for astTypemap.c helper functions */

SV* createPerlObject( const char *, AstObject * );
IV extractAstIntPointer( SV * );
char * ntypeToClass( const char * );
SV* getPerlObjectAttr( SV *, const char * );
void setPerlObjectAttr( SV *, const char *, SV * );
int ReportPerlError ( int );
void setPerlAstObject( SV *, AstObject * );
