/* Prototypes for astTypemap.c helper functions */

SV* createPerlObject( char *, AstObject * );
IV extractAstIntPointer( SV * );
char * ntypeToClass( char * );
SV* getPerlObjectAttr( SV *, char * );
void setPerlObjectAttr( SV *, char *, SV * );
int ReportPerlError ( int );
void setPerlAstObject( SV *, AstObject * );
