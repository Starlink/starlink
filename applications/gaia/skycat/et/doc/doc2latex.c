/*
** The program coded by this file reads a single HTML file and
** converts it to the LaTeX markup language.
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#define stricmp strcasecmp

/****************** Begin Escape Sequence Translator *************/
/*
** The next section of code implements routines used to translate
** the '&' escape sequences of SGML to individual characters.
** Examples:
**
**         &amp;          &
**         &lt;           <
**         &gt;           >
**         &nbsp;         nonbreakable space
*/

/* Each escape sequence is recorded as an instance of the following
** structure
*/
typedef struct sgEsc Esc;
struct sgEsc {
  char *zName;     /* The name of this escape sequence.  ex:  "amp" */
  int value;       /* The value for this sequence.       ex:  '&' */
  Esc *pNext;      /* Next sequence with the same hash on zName */
};

/* The size of the handler hash table.  For best results this should
** be a prime number which is about the same size as the number of
** escape sequences known to the system. */
#define ESC_HASH_SIZE 7

/* The following flag is TRUE if the escape sequence has table needs
** to be initialized.
*/
static int bEscNeedsInit = 1;

/* The hash table 
**
** If the name of an escape sequences hashes to the value H, then
** apEscHash[H] will point to a linked list of Esc structures, one of
** which will be the Esc structure for that escape sequence.
*/
static Esc *apEscHash[ESC_HASH_SIZE];

/* Hash a escape sequence name.  The value returned is an integer
** between 0 and ESC_HASH_SIZE-1, inclusive.
*/
static int EscHash(const char *zName){
  int h = 0;      /* The hash value to be returned */
  char c;         /* The next character in the name being hashed */

  while( (c=*zName)!=0 ){
    if( isupper(c) ) c = tolower(c);
    h = h<<5 ^ h ^ c;
    zName++;
  }
  if( h<0 ) h = -h;
  return h % ESC_HASH_SIZE;
}

/* The following is a table of all escape sequences.  Add new sequences
** by adding entries to this table.
*/
static Esc esc_sequences[] = {
  { "amp",       '&',        0 },
  { "lt",        '<',        0 },
  { "gt",        '>',        0 },
  { "nbsp",      '~',        0 },
};

/* Initialize the escape sequence hash table
*/
static void EscInit(void){
  int i;  /* For looping thru the list of escape sequences */
  int h;  /* The hash on a sequence */

  for(i=0; i<sizeof(esc_sequences)/sizeof(esc_sequences[0]); i++){
    h = EscHash(esc_sequences[i].zName);
    esc_sequences[i].pNext = apEscHash[h];
    apEscHash[h] = &esc_sequences[i];
  }
}

/* Translate escape sequences in the string "z".  "z" is overwritten
** with the translated sequence.
**
** Unrecognized escape sequences are unaltered.
**
** Example:
**
**      input =    "AT&amp;T &gt MCI"
**      output =   "AT&T > MCI"
*/
void TranslateEscapes(char *z){
  int from;   /* Read characters from this position in z[] */
  int to;     /* Write characters into this position in z[] */
  int h;      /* A hash on the escape sequence */
  Esc *p;     /* For looping down the escape sequence collision chain */

  from = to = 0;
  if( bEscNeedsInit ){
    EscInit();
    bEscNeedsInit = 0;
  }
  while( z[from] ){
    if( z[from]=='&' ){
      int i = from+1;
      int c;
      while( z[i] && isalpha(z[i]) ){ i++; }
      c = z[i];
      z[i] = 0;
      h = EscHash(&z[from+1]);
      p = apEscHash[h];
      while( p && strcmp(p->zName,&z[from+1])!=0 ){ p = p->pNext; }
      z[i] = c;
      if( p ){
        z[to++] = p->value;
        from = i;
        if( c==';' ) from++;
      }else{
        z[to++] = z[from++];
      }
    }else{
      z[to++] = z[from++];
    }
  }
  z[to] = 0;
}
/******************* End Escape Sequence Translator ***************/

/******************* Begin SGML parser code *******************/
/*
** The following code implements several subroutines that are
** used to parse HTML.
*/
/* These three pointers define certain special handlers.  xSpaceHandler
** is called to deal with all white space.  xWordHandler is called to
** deal with every sequence of characters that is not white-space.
** xDefaultMarkupHandler is called for every SGML directive that
** doesn't have its own handler.
*/
static void (*xSpaceHandler)(const char*,void*);
static void (*xWordHandler)(const char*,void*);
static void (*xDefaultMarkupHandler)(int, const char**, void*);

/* Each specific SGML markup handler is stored in a hash table as an 
** instance of the following structure.
*/
typedef struct sgHandler Handler;
struct sgHandler {
  char *zName;                                /* Name of markup to handle */
  void (*xHandler)(int, const char**, void*); /* Routine to do the work */
  Handler *pCollide;                          /* Next handler with same hash */
};

/* The size of the handler hash table.  For best results, this should
** be a prime number which is at least as large as the number of
** handlers registered with the system 
*/
#define SGML_HASH_SIZE 203

/* The handler hash table 
**
** If an SGML markup name hashes to H, then apHandler[H] will point to
** a linked list of Handler structure, one of which will describe the
** handler for that particular markup (if it exists.)
*/
static Handler *apHandler[SGML_HASH_SIZE];

/* Hash a handler name
**
** SGML markup is case insensitive, so this function will give the
** same hash regardless of the case of the markup name.
**
** The value returned is an integer between 0 and SGML_HASH_SIZE-1,
** inclusive.
*/
static int SgmlHash(const char *zName){
  int h = 0;
  char c;
  while( (c=*zName)!=0 ){
    if( isupper(c) ) c = tolower(c);
    h = h<<5 ^ h ^ c;
    zName++;
  }
  if( h<0 ) h = -h;
  return h % SGML_HASH_SIZE;
}

/* Record the current line number
*/
int linenum = 1;

/* Return a single character from the given input file.  Return
** EOF at end-of-file.  Keep the linenum variable up-to-date.
*/
static int OneChar(FILE *in){
  int c = getc(in);
  if( c=='\n' ) linenum++;
  return c;
}

/* Given a pointer to an input file, read the and parse that file
** as if it were SGML.
**
** This is not a true SGML parser, in that it handles some unusual
** cases differently, and ignores the & operator completely.
*/
void SgmlParse(
  FILE *in,            /* Read input from this stream */
  void *pArg           /* Give this value as the 3rd argument to each handler */
){
  int c;               /* The next character of input */
  int i, j;            /* Loop counters */
  int argc;            /* The number of arguments on a markup */
  Handler *pHandler;   /* The handler used for the markup */
  char *argv[100];     /* Pointers to each markup argument. */
  char zBuf[1000];     /* Space to hold the markup.  (No markup can
                       ** be longer than 1000 characters, therefore.) */

  c = OneChar(in);
  while( c!=EOF ){
    if( isspace(c) ){
      /* Case 1: spaces 
      */
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=OneChar(in))!=EOF && isspace(c) ){
        zBuf[i++] = c;
      }
      zBuf[i] = 0;
      if( xSpaceHandler ){
        (*xSpaceHandler)(zBuf,pArg);
      }
    }else if( c=='<' ){
      /* Case 2: markup
      **
      ** Begin by copying the complete markup into zBuf[]
      */
      int cQuote = 0;
      i = 0;
      while( (c=OneChar(in))!=EOF && (cQuote || c!='>') ){
        if( i<sizeof(zBuf)-2 ) zBuf[i++] = c;
        if( cQuote ){
          if( cQuote==c ) cQuote = 0;
        }else if( c=='"' || c=='\'' ){
          cQuote = c;
        }
      }
      if( c=='>' ) c = OneChar(in);
      zBuf[i] = 0;

      /* Now that the markup is completely in zBuf[], parse it up.
      **
      ** argv[0] will be a pointer to the name.  argv[1] will point to
      ** the first argument and argv[2] will point to the first argument's
      ** value.  argv[N/2+1] will point to the N-th argument and
      ** argv[N/2+2] will point to the N-th argument's value.
      **
      ** For example, in:      <img src="image.gif" alt="figure 1">
      ** we will get:
      **                   argv[0] = "img"
      **                   argv[1] = "src"
      **                   argv[2] = "image.gif"
      **                   argv[3] = "alt"
      **                   argv[4] = "figure 1"
      **                   argv[5] = NULL
      **
      ** Note that the argv[] list is NULL terminated.  Also note that
      ** argument values are stripped of quotation marks.
      */
      argc = 0;
      argv[0] = &zBuf[0];
      TranslateEscapes(argv[0]);
      for(j=0; zBuf[j] && !isspace(zBuf[j]); j++){}
      if( zBuf[j] ){
        zBuf[j++] = 0;
        while( argc<(sizeof(argv)/sizeof(argv[0])) - 4 && zBuf[j] ){
          while( isspace(zBuf[j]) ) j++;
          argv[++argc] = &zBuf[j];
          while( zBuf[j] && !isspace(zBuf[j]) && zBuf[j]!='=' ) j++;
          if( zBuf[j]!='=' ){
            argv[argc+1] = argv[argc];
            argc++;
            if( zBuf[j] ) zBuf[j++] = 0;
            continue;
          }
          zBuf[j++] = 0;
          if( zBuf[j]=='"' || zBuf[j]=='\'' ){
            cQuote = zBuf[j++];
          }else{
            cQuote = 0;
          }
          argv[++argc] = &zBuf[j];
          if( cQuote ){
            while( zBuf[j] && zBuf[j]!=cQuote ) j++;
          }else{
            while( zBuf[j] && !isspace(zBuf[j]) ) j++;
          }
          if( zBuf[j] ) zBuf[j++] = 0;
          TranslateEscapes(argv[argc]);
        }
      }
      argv[++argc] = 0;

      /* Now process the markup.  If there is a specific handler for
      ** this markup, use it.  Otherwise, call the default markup handler
      */
      pHandler = apHandler[SgmlHash(argv[0])];
      while( pHandler && stricmp(pHandler->zName,argv[0])!=0 ){
        pHandler = pHandler->pCollide;
      }
      if( pHandler ){
        if( pHandler->xHandler ){
          (*pHandler->xHandler)(argc,(const char**)argv,pArg);
        }
      }else if( xDefaultMarkupHandler ){
        (*xDefaultMarkupHandler)(argc,(const char**)argv,pArg);
      }
    }else{
      /* Ordinary text.  This will get passed to the word handler.
      */
      zBuf[0] = c;
      i = 1;
      while( i<sizeof(zBuf)-2 && (c=OneChar(in))!=EOF && c!='<' && !isspace(c)){
        zBuf[i++] = c;
      }
      zBuf[i] = 0;
      /* Dispose of a word */
      TranslateEscapes(zBuf);
      if( xWordHandler ){
        (*xWordHandler)(zBuf,pArg);
      }
    }
  }
}

/* Clear out the handler hash table.  Call this to erase all handlers other
** than the space, word and default markup handlers.
*/
void SgmlHandlerReset(void){
  Handler *pHandler;
  int i;

  for(i=0; i<SGML_HASH_SIZE; i++){
    Handler *pNext;
    for(pHandler=apHandler[i]; pHandler; pHandler=pNext){
      pNext = pHandler->pCollide;
      free(pHandler);
    }
    apHandler[i] = 0;
  }
}

/* Install a new handler for specific markup.  zName is the name of the
** markup that is to be handled.  xFunc is a pointer to the routine to
** call whenever a markup named zName appears in the input.
*/
extern void *malloc();
void SgmlHandler(const char *zName, void (*xFunc)(int,const char**,void*)){
  int h = SgmlHash(zName);
  Handler *pNew = malloc( sizeof(Handler) + strlen(zName) + 1 );
  if( pNew==0 ) return;
  pNew->zName = (char*)&pNew[1];
  strcpy(pNew->zName,zName);
  pNew->pCollide = apHandler[h];
  pNew->xHandler = xFunc;
  apHandler[h] = pNew;  
}

/* Install the default handlers
*/
void SgmlWordHandler(void (*xWord)(const char*,void*)){
  xWordHandler = xWord;
}
void SgmlSpaceHandler(void (*xSpace)(const char*,void*)){
  xSpaceHandler = xSpace;
}
void SgmlDefaultMarkupHandler(void (*xMarkup)(int,const char**,void*)){
  xDefaultMarkupHandler = xMarkup;
}
/****************** End of SGML Parser Code **********************/

/* The number of errors seen
*/
int errcnt = 0;

/* The current state of the translation from DOC format into
** LaTeX is contained in the following static variables
*/
int column = 0;        /* How many characters are already on the current line */
int saw_space = 1;     /* True if the previous character is a space */
int in_preformated = 0;  /* True when processing preformatted text */
int ignore = 0;          /* True to ignore all text and markup */

/* The number of nested lists.  This information is used only for
** error checking -- to make sure things like <h3> don't occur
** inside of a list.
*/
int list_depth = 0;   /* Nesting depth of <ol>...</ol> or <ul>...</ul> */
int in_deflist = 0;   /* Nesting depth of <dl>...</dl> */

/* An instance of the following structure records the innermost
** context.  This information is used for error checking -- to make
** sure every <ol> has a matching </ol> and every <h3> has a matching
** </h3> and so forth.
*/
struct sgContext {
  char zLabel[12];
  int linenum;
};

/* Here is a stack of contexts.  The stack can be finite, because if
** we overflow the program still works, we just lose some error checking
*/
struct sgContext contextStack[100];

/* The maximum depth of the context stack
*/
#define MaxContextStackDepth (sizeof(contextStack)/sizeof(contextStack[0]))

/* The current depth of the context stack
*/
int contextStackDepth = 0;

/* Push a new context onto the stack
*/
void PushContext(const char *zContext){
  int i = contextStackDepth++;
  if( i < MaxContextStackDepth ){
    sprintf(contextStack[i].zLabel,"%.11s",zContext);
    contextStack[i].linenum = linenum;
  }
}

/* Pop an element of the context stack.  If the element to be popped does
** not match the top of the stack, issue an error message.
*/
void PopContext(const char *zContext){
  int i = --contextStackDepth;
  if( i<0 ){
    fprintf(stderr,"line %d: </%s> has no corresponding <%s>\n",
      linenum, zContext, zContext);
    errcnt++;
    contextStackDepth = 0;
  }
  if( i >= MaxContextStackDepth ) return;
  if( stricmp(contextStack[i].zLabel,zContext)!=0 ){
    /* Doesn't match.  Must be some kind of error. */
    fprintf(stderr,"line %d: <%s> has no matching </%s>\n",
      contextStack[i].linenum,
      contextStack[i].zLabel,
      contextStack[i].zLabel);
    errcnt++;
    if( i>0 && stricmp(contextStack[i-1].zLabel,zContext)==0 ){
      contextStackDepth--;
    }
  }
}

/* This routine is called to handle all white-space
*/
void HandleSpace(const char *zSpace, void *pDummy){
  if( in_preformated ){
    /* When processing preformatted text, white space is copied verbatim */
    printf("%s",zSpace);
    saw_space = 1;
  }else if( ignore || saw_space ){
    /* When ignoring input, or the last character was a space, do nothing */
  }else if( column>60 ){
    /* Create a new line if the current line is filling up */
    printf("\n");
    column = 0;
    saw_space = 1;
  }else{
    /* All white-space is reduced to a single space character */
    printf(" ");
    column++;
    saw_space = 1;
  }
}

/* This routine is called to handle all non-whitespace in the original
** DOC file.
*/
void HandleWord(const char *zWord, void *pDummy){
  if( in_preformated ){
    /* Copy the text verbatim when processing preformatted text */
    printf("%s",zWord);
    saw_space = 0;
    return;
  }else if( ignore ){
    /* Do nothing if we are ignoring text */
    return;
  }

  /* Control reaches here on the usual case when we want to copy the
  ** text to output, but we are NOT processing preformatted text.
  **
  ** We have to insert a backslash before certain characters that
  ** have special meaning to LaTeX.
  */
  while( *zWord ){
    switch( *zWord ){
      case '_':
      case '&':
      case '%':
      case '$':
      case '#':
      case '{':
      case '}':
        putchar('\\');
        putchar(*zWord);
        column += 2;
        break;

      case '>':
      case '<':
        printf("$%c$",*zWord);
        column += 3;
        break;

      default:
        putchar(*zWord);
        column++;
        break;
    }
    zWord++;
  }
  saw_space = 0;
}

/* If we are within a <hN>..</hN>, then heading_level is set to N.
** If we aren't in a heading, heading_level is set ot 0.  This
** is used for error checking only.
*/
int heading_level = 0;

/* If the previous heading was tagged (if it had a tag=TAG argument)
** then the value of the tag is stored in the following variable.
*/
char heading_tag[100];

/* Processing all <h1>, <h2>, ..., <h6> markups.
*/
void Heading(int argc, const char **argv, void *pDummy){
  int level;
  int i;
  char *headings[] = {
     "section", "subsection", "subsubsection", "paragraph", 
     "subparagraph", "subparagraph"
  };

  if( ignore ) return;
  if( heading_level ){
    fprintf(stderr,"line %d: <%s> within <h%d>\n",
      linenum,argv[0],heading_level);
    errcnt++;
  }
  if( list_depth || in_deflist ){
    fprintf(stderr,"line %d: <%s> within a list\n",
      linenum, argv[0]);
    errcnt++;
  }
  PushContext(argv[0]);
  level = argv[0][1] - '0';
  printf("\n\n\\%s{",headings[level-1]);
  column = strlen(headings[level-1])+2;
  saw_space = 0;
  heading_level = level;
  heading_tag[0] = 0;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      sprintf(heading_tag,"%.*s",(int)sizeof(heading_tag)-1,argv[i+1]);
    }else{
      fprintf(stderr,"line %d: Unknown argument to <h%d>: %s=%s\n",
        linenum, level+1, argv[i], argv[i+1]);
    }
  }
}

/* Process all </h1>, </h2>, ..., </h6> markups.
*/
void EndHeading(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext(&argv[0][1]);
  printf("}\n");
  if( heading_tag[0] ){
    printf("\\label{%s}\n",heading_tag);
    heading_tag[0] = 0;
  }
  column = 0;
  saw_space = 1;
  heading_level = 0;
}

/* Process the <br> markup
*/
void Break(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  printf(" \\\\\n");
  column = 0;
  saw_space = 1;
}

/* Process the <p> markup
*/
void Paragraph(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  printf("\n\n");
  column = 0;
  saw_space = 1;
}
void EndParagraph(int argc, const char **argv, void *pDummy){
}

/* Start a new line of output, if we aren't already on a new
** line.
*/
void newline(void){
  if( column>0 ){ 
    printf("\n");
    column = 0;
    saw_space = 1;
  }
}

/* Process the <pre> markup
*/
void Preformat(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  PushContext("pre");
  printf("\\begin{verbatim}");
  in_preformated = 1;
}

/* Process the </pre> markup
*/
void EndPreformat(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  printf("\\end{verbatim}\n");
  saw_space = 1;
  PopContext("pre");
  column = 0;
  in_preformated = 0;
}

/* The <latex> directive works a lot like <pre>.  Between <latex> and
** </latex> everything is copied verbatim.  But, no code is issued
** to turn on \begin{verbatim}...\end{verbatim} in the output.
*/
void Latex(int argc, const char **argv, void *pDummy){
  in_preformated = 1;
  PushContext("latex");
}
void EndLatex(int argc, const char **argv, void *pDummy){
  in_preformated = 0;
  PopContext("latex");
}


/* Process the <ol> markup
*/
void OrderedList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  PushContext("ol");
  list_depth++;
  printf("\\begin{enumerate}\n");
}

/* Process the </ol> markup
*/
void EndOrderedList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  PopContext("ol");
  list_depth--;
  if( list_depth<0 ) list_depth = 0;
  printf("\\end{enumerate}\n");
}

/* Process the <ul> markup
*/
void BulletList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  PushContext("ul");
  list_depth++;
  printf("\\begin{itemize}\n");
}

/* Process the </ul> markup
*/
void EndBulletList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  PopContext("ul");
  list_depth--;
  if( list_depth<0 ) list_depth = 0;
  printf("\\end{itemize}\n");
}

/* Process the <li> markup
*/
void ListItem(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  if( list_depth==0 ){
    fprintf(stderr,"line %d: <li> not within a list.\n",linenum);
    errcnt++;
  }
  printf("\\item\n");
}

/* Ignore all text until we call On().  (This is actually used
** in order to ignore the text contained in <title>...</title>)
*/
void Off(int argc, const char **argv, void *pDummy){
  ignore = 1;
}

/* Stop ignoring text
*/
void On(int argc, const char **argv, void *pDummy){
  ignore = 0;
}

/* The <dl>..</dl> structure makes use of \begin{quote}...\end{quote}
** of LaTeX.  The following variable remembers whether or not we have
** a \begin{quote} without a matching \end{quote}
*/
static in_quote = 0;

/* Process the <dl> markup
*/
void DefList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  printf("\n\noindent\n");
  if( in_deflist ){
    fprintf(stderr,"line %d: Can't nest <dl>...</dl>\n",linenum);
    errcnt++;
  }
  in_deflist++;
  PushContext("dl");
  in_quote = 0;
}

/* Process the <dt> markup
*/
void DefTag(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  if( in_quote ){
    printf("\\end{quote}\n");
  }
  printf("{\\em ");
  column = 5;
  saw_space = 1;
  in_quote = 0;
}

/* Process the <dd> markup
*/
void DefData(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  printf("}\n");
  if( !in_quote ){
    printf("\\begin{quote}\n");
  }
  column = 0;
  saw_space = 1;
  in_quote = 1;
}

/* Process the </dl> markup
*/
void EndDefList(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  newline();
  if( in_quote ){
    printf("\\end{quote}\n");
  }
  in_deflist = 0;
  PopContext("dl");
  in_quote = 0;
}

/* Process the <em> markup
*/
void Emphasize(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PushContext("em");
  printf("{\\em ");
  column += 5;
  saw_space = 1;
}

/* Process the </em> markup
*/
void EndEmphasize(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext("em");
  printf("}");
  column += 1;
  saw_space = 0;
}

/* Process the <strong> markup
*/
void Strong(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PushContext("strong");
  printf("{\\bf ");
  column += 5;
  saw_space = 1;
}

/* Process the </strong> markup
*/
void EndStrong(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext("strong");
  printf("}");
  column += 1;
  saw_space = 0;
}

/* Process the <code> and </code> markup
*/
void Code(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PushContext("code");
  printf("{\\tt ");
  column += 5;
  saw_space = 1;
}
void EndCode(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext("code");
  printf("}");
  column += 1;
  saw_space = 0;
}

/* Process the <func> and </func> markup
*/
void Func(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PushContext("code");
  printf("{\\tt ");
  column += 5;
  saw_space = 1;
}
void EndFunc(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext("code");
  printf("()}");
  column += 3;
  saw_space = 0;
}

/* Process the <blockquote> markup
*/
void BlockQuote(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PushContext("blockquote");
  newline();
  printf("\\begin{quote}\n");
}

/* Process the </blockquote> markup
*/
void EndBlockQuote(int argc, const char **argv, void *pDummy){
  if( ignore ) return;
  PopContext("blockquote");
  newline();
  printf("\\end{quote}\n");
}

/* Process the <image eps=FILE width=WIDTH tag=TAG caption=TEXT> markup.
**
** The eps= field is required.  It specifies the name of a file containing
** the postscript image to be imported.  The width= field is optional.
** if present, it specifies that the image should be scaled to the
** given width (the height is change proporationally.)  The WIDTH should
** be an integer or floating-point value followed by a unit.  Units can
** be  "cm" (centimeters), "in" (inches), "em" (ems -- the with of an M 
** character), or "pt" (printer's points).
*/
void Image(int argc, const char **argv, void *pDummy){
  int i;
  const char *zImage = 0;
  const char *zWidth = 0;
  const char *zTag = 0;
  const char *zCaption = 0;

  if( ignore ) return;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"eps")==0 ){
      zImage = argv[i+1];
    }else if( stricmp(argv[i],"width")==0 ){
      zWidth = argv[i+1];
    }else if( stricmp(argv[i],"tag")==0 ){
      zTag = argv[i+1];
    }else if( stricmp(argv[i],"caption")==0 ){
      zCaption = argv[i+1];
    }
  }
  if( zImage==0 ){
    fprintf(stderr,"line %d: No \"eps\" flag on <image>\n",linenum);
    errcnt++;
    return;
  }
  newline();
  if( zCaption ){
    printf("\\begin{figure}\n");
  }
  if( zWidth==0 ){
    printf("\\begin{center}\\mbox{\\psbox{%s}}\n",zImage);
  }else{
    printf("\\begin{center}\\mbox{\\psboxto(%s;0pt){%s}}\n",
    zWidth,zImage);
  }
  if( zCaption ){
    printf("\\caption{");
    HandleWord(zCaption,0);
    printf("}\n");
    column = 0;
    saw_space = 1;
    if( zTag ){
      printf("\\label{%s}\n",zTag);
    }
    printf("\\end{center}\\end{figure}\n");
  }else{
    printf("\\end{center}\n");
  }
}

/* Process the <index text=TEXT> markup
*/
void Index(int argc, const char **argv, void *pDummy){
  int i;
  if( ignore ) return;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"text")==0 ){
      if( saw_space ) newline();
      printf("\\index{%s}",argv[i+1]);
      column = 7 + strlen(argv[i+1]);
      saw_space = 0;
      break;
    }
  }
}

/* Process the <ref tag=TAG> markup
*/
void Ref(int argc, const char **argv, void *pDummy){
  int i;
  if( ignore ) return;
  for(i=1; i<argc; i+=2){
    if( stricmp(argv[i],"tag")==0 ){
      printf("\\ref{%s}",argv[i+1]);
      break;
    }
  }
  saw_space = 0;
}

/* Process the <quote> and </quote> markups
*/
void Quote(int argc, const char **argv, void *NotUsed){
  printf("``");
}
void EndQuote(int argc, const char **argv, void *NotUsed){
  printf("''");
}

/* A table of markups and the routines to handle them
*/
static struct {
  char *zMark;
  void (*xHandler)(int, const char**, void*);
} markups[] = {
  { "h1",      Heading },
  { "h2",      Heading },
  { "h3",      Heading },
  { "h4",      Heading },
  { "h5",      Heading },
  { "h6",      Heading },
  { "/h1",     EndHeading },
  { "/h2",     EndHeading },
  { "/h3",     EndHeading },
  { "/h4",     EndHeading },
  { "/h5",     EndHeading },
  { "/h6",     EndHeading },
  { "br",      Break },
  { "p",       Paragraph },
  { "/p",      EndParagraph },
  { "pre",     Preformat },
  { "/pre",    EndPreformat },
  { "ol",      OrderedList },
  { "ul",      BulletList },
  { "/ol",     EndOrderedList },
  { "/ul",     EndBulletList },
  { "li",      ListItem },
  { "title",   Off },
  { "/title",  On },
  { "html",    Off },
  { "/html",   On },
  { "latex",   Latex },
  { "/latex",  EndLatex },
  { "dl",      DefList },
  { "/dl",     EndDefList },
  { "dt",      DefTag },
  { "dd",      DefData },
  { "em",      Emphasize },
  { "/em",     EndEmphasize },
  { "image",   Image },
  { "index",   Index },
  { "ref",     Ref },
  { "blockquote",  BlockQuote },
  { "/blockquote", EndBlockQuote },
  { "strong",  Strong },
  { "/strong", EndStrong },
  { "quote",   Quote },
  { "/quote",  EndQuote },
  { "code",    Code },
  { "/code",   EndCode },
  { "cmd",     Code },
  { "/cmd",    EndCode },
  { "filename",    Code },
  { "/filename",   EndCode },
  { "func",    Func },
  { "/func",   EndFunc },
  
};

/* Call this function to parse the complete SGML file whose
** input stream is pIn
*/
static void ParseWholeFile(FILE *pIn){
  static int sgmlNeedsInit = 1;
  if( sgmlNeedsInit ){
    int i;
    SgmlWordHandler(HandleWord);
    SgmlSpaceHandler(HandleSpace);
    SgmlDefaultMarkupHandler(0);
    for(i=0; i<sizeof(markups)/sizeof(markups[0]); i++){
      SgmlHandler(markups[i].zMark,markups[i].xHandler);
    }
    sgmlNeedsInit = 0;
  }
  printf("\\input{psbox.tex}\n");
  SgmlParse(pIn,0);
}

int main(int argc, char **argv){
  if( argc==1 ){
    ParseWholeFile(stdin);
  }else if( argc==2 ){
    FILE *pIn = fopen(argv[1],"r");
    if( pIn==0 ){
      char zErrMsg[1000];
      sprintf(zErrMsg,"Can't open \"%.*s\"",(int)sizeof(zErrMsg)-30,argv[1]);
      perror(zErrMsg);
      exit(1);
    }
    ParseWholeFile(pIn);
    fclose(pIn);
  }else{
    fprintf(stderr,"Usage: %s [FILENAME]\n",argv[0]);
    exit(1);
  }
  return errcnt;
}
