/* Prototypes for astTypemap.c helper functions */

SV* createPerlObject( char *, AstObject * );
IV extractAstIntPointer( SV * );
char * ntypeToClass( char * );
SV* getPerlObjectAttr( SV *, char * );
