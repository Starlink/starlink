static char const rcsid[] = "@(#) $Id$";
/*
** Copyright (c) 1998, 1999 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
** 
** You should have received a copy of the GNU General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
**
** Author contact information:
**   drh@acm.org
**   http://www.hwaci.com/drh/
*/
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#if defined(_WIN32) || defined(WIN32)
# include <windows.h>
# if !defined(R_OK)
#   define R_OK 4
# endif
#else
# include <unistd.h>
#endif
#include <time.h>
#include <assert.h>

/*
** Version information for this program
*/
static char zVersion[] = "mktclapp version 3.10.  February 22, 2000";

/*
** Each new TCL commands discovered while scanning C/C++ source code is
** stored in an instance of the following structure.
*/
typedef struct EtCmd EtCmd;
struct EtCmd {
  char *zIf;         /* Surrounding #if statement */
  char *zName;       /* Name of the command */
  int isObj;         /* True if this is a Tcl_Obj command */
  EtCmd *pNext;      /* Next command on a list of them all */
};

/*
** This is a list of all TCL commands in the scanned source
*/
static EtCmd *cmdList = 0;

/*
** Number of commands and object commands.
*/
static int nCmd = 0;
static int nObjCmd = 0;

/*
** Each nested "#if" statement is stored as an instance of the
** following structure.
*/
typedef struct IfStmt IfStmt;
struct IfStmt {
  char *zArg;        /* Argument to the #if.  Ex:  "defined(DEBUG)" */
  int invert;        /* True to put a "!" in front */
  int line;          /* Line number of the original #if */
  IfStmt *pNext;     /* Next #if statement down on the stack */
};

/*
** The nested #if statements
*/
static IfStmt *ifStack = 0;

/*
** Name of this program.
*/
static char *Argv0 = "mktclapp";

/*
** Number of errors
*/
static int nError = 0;

/*
** Surround the call to Et_AppInit() with this #if
*/
static char *seenEtAppInit = "0";

/*
** Surround the call to Et_PreInit() with this #if
*/
static char *seenEtPreInit = "0";

/*
** Surround the implmentation of main() with the inverse of this #if
*/
static char *seenMain = "0";

/*
** Surround the call to Et_CustomMainLoop() with the inverse of this #if
*/
static char *seenEtCustomMainLoop = "0";

/*
** Allocate memory.  Never fail.  If not enough memory is available,
** print an error message and abort.
*/
void *SafeMalloc(int nByte){
  void *p = malloc(nByte);
  if( p==0 ){
    fprintf(stderr,"Out of memory.  Can't allocate %d bytes\n", nByte);
    exit(1);
  }
  memset(p, 0, nByte);
  return p;
}
void *SafeRealloc(void *old, int nByte){
  void *p;
  if( old==0 ) return SafeMalloc(nByte);
  p = realloc(old, nByte);
  if( p==0 ){
    fprintf(stderr,"Out of memory.  Can't allocate %d bytes\n", nByte);
    exit(1);
  }
  return p;
}

/*
** The opposite of SafeMalloc().  Free memory previously obtained.
*/
void SafeFree(void *pMem){
  if( pMem ) free(pMem);
}

/*
** Return TRUE if the given character can be part of a C identifier.
*/
static int IsIdent(int c){
  return isalnum(c) || c=='_';
}

/*
** Create an "#if" argument that captures the state of all nested
** "#if" statements, ORed with "zExtra".  Space to hold
** the returned string is obtained from SafeMalloc and must be
** freed by the calling function.
**
** If the conditional is always TRUE, then NULL is returned.
*/
static char *IfString(char *zExtra){
  int len = 0;
  IfStmt *p;
  char *z;
  int i;
  int isStackTrue = 1;
  int isStackFalse = 0;
  int isExtraFalse = 0;
  char *zSep;
  IfStmt *altStack;

  if( zExtra && *zExtra ){
    if( zExtra[1]==0 && zExtra[0]=='0' ){
      isExtraFalse = 1;
    }else if( zExtra[1]==0 && zExtra[0]=='1' ){
      return 0;
    }
    len = strlen(zExtra) + 10;
  }else{
    len = 1;
    isExtraFalse = 1;
  }
  for(p=ifStack; p; p=p->pNext){
    len += strlen(p->zArg) + 6;
    if( p->zArg[0]=='0' && p->zArg[1]==0 ){
      if( !p->invert ){ 
        isStackFalse = 1;
        isStackTrue = 0;
        break;
      }
    }else if( p->zArg[0]=='1' && p->zArg[1]==0 ){
      if( p->invert ){ 
        isStackFalse = 1;
        isStackTrue = 0;
        break;
      }
    }else{
      isStackTrue = 0;
    }
  }
  if( isStackTrue ){
    return 0;
  }else if( isStackFalse && isExtraFalse ){
    z = SafeMalloc( 2 );
    strcpy(z,"0");
    return z;
  }
  z = SafeMalloc( len );
  if( !isExtraFalse ){
    sprintf(z,"(%s) || (",zExtra);
    i = strlen(z);
  }else{
    i = 0;
  }
  zSep = "";
  altStack = 0;
  while( ifStack ){
    p = ifStack;
    ifStack = p->pNext;
    p->pNext = altStack;
    altStack = p;
  }
  for(p=altStack; p; p=p->pNext){
    if( p->zArg[0]=='0' && p->zArg[1]==0 && p->invert ) continue;
    if( p->zArg[0]=='1' && p->zArg[1]==0 && !p->invert ) continue;
    if( p->invert ){
      sprintf(&z[i],"%s!%s",zSep,p->zArg);
    }else{
      sprintf(&z[i],"%s%s",zSep,p->zArg);
    }
    i += strlen(&z[i]);
    zSep = " && ";
  }
  while( altStack ){
    p = altStack;
    altStack = p->pNext;
    p->pNext = ifStack;
    ifStack = p;
  }
  if( !isExtraFalse ){
    sprintf(&z[i],")");
  }
  return z;
}

/*
** Push a new "#if" onto the if stack.
*/
static void PushIf(char *zArg, int line, int isNegated, int isDefined){
  char *z;
  IfStmt *p;
  if( !isDefined ){
    int i;
    z = SafeMalloc( strlen(zArg) + 3 );
    for(i=0; zArg[i] && IsIdent(zArg[i]); i++){}
    if( zArg[i]==0 ){
      sprintf(z,"%s",zArg);
    }else{
      sprintf(z,"(%s)",zArg);
    }
  }else{
    z = SafeMalloc( strlen(zArg) + 10 );
    sprintf(z,"defined(%s)",zArg);
  }
  p = SafeMalloc( sizeof(IfStmt) );
  p->zArg = z;
  p->line = line;
  p->invert = isNegated;
  p->pNext = ifStack;
  ifStack = p;
}

/*
** Extract the argument to an #if.  Remove all leading and trailing
** space.  
*/
static char *GetArg(const char *fileName, char *z, int *pI, int *pLine){
  int i = *pI;
  int line = *pLine;
  int start;
  char *zResult;
  int j, k;

  while( isspace(z[i]) && z[i]!='\n' ){ i++; }
  start = i;
  if( z[i]=='\n' || z[i]==0 ){
    fprintf(stderr,"%s: Missing argument to \"#if\" on line %d\n",
      fileName, *pLine);
    nError++;
    line++;
  }else{
    while( z[i] && z[i]!='\n' ){
      if( z[i]=='\\' && z[i+1]!=0 ){
        i++;
      }
      if( z[i]=='\n' ){
        line++;
      }
      i++;
    }
  }
  zResult = SafeMalloc( i + 1 - start );
  for(j=0, k=start; k<i; k++){
    if( isspace(z[k]) && j>0 && isspace(zResult[j-1]) ){
      /* Do nothing */
    }else if( z[k]=='\\' && z[k+1]=='\n' ){
      if( j>0 && !isspace(zResult[j-1]) ){
        zResult[j++] = ' ';
      }
      k++;
    }else if( z[k]=='\\' ){
      zResult[j++] = z[k++];
    }
    zResult[j++] = z[k];
  }
  zResult[j] = 0;
  while( j>0 && isspace(zResult[j-1]) ){
    j--;
    zResult[j] = 0;
  }
  *pI = i;
  *pLine = line;
  return zResult;
}


/*
** Read the complete text of a file into memory.  Return 0 if there
** is any kind of error.
*/
char *ReadFileIntoMemory(const char *fileName, int *pLength){
  FILE *in;             /* Input file stream */
  char *textBuf;        /* A buffer in which to put entire text of input */
  int toRead;           /* Amount of input file read to read */
  int got;              /* Amount read so far */
  struct stat statBuf;  /* Status buffer for the file */

  if( stat(fileName,&statBuf)!=0 ){
    fprintf(stderr,"%s: no such file: %s\n", Argv0, fileName);
    return 0;
  }
  textBuf = SafeMalloc( statBuf.st_size + 1 );
  in = fopen(fileName,"rb");
  if( in==0 ){
    fprintf(stderr,"%s: can't open for reading: %s\n", Argv0, fileName);
    SafeFree(textBuf);
    return 0;
  }
  textBuf[statBuf.st_size] = 0;
  toRead = statBuf.st_size;
  got = 0;
  while( toRead ){
    int n = fread(&textBuf[got],1,toRead,in);
    if( n<=0 ) break;
    toRead -= n;
    got += n;
  }
  fclose(in);
  textBuf[got] = 0;
  if( pLength ) *pLength = got;
  return textBuf;
}

/*
** Given the "aaaa" part of the name of an ET_COMMAND_aaaa function,
** compute the name of the corresponding Tcl command.
**
** The name is usually the same, except if there are two underscores
** in the middle of the command, they are changed to colons.  This
** feature allows namespaces to be used.  Example:  The function
** named
**
**       ET_COMMAND_space1__proc1(ET_TCLARGS){...}
**
** will generate a TCL command called
**
**       space1::proc1
**
** Space to hold the TCL command name is obtained from malloc().
*/
static char *FuncToProc(char *zFunc){
  char *zProc;
  int i;

  zProc = SafeMalloc( strlen(zFunc) + 1 );
  strcpy(zProc, zFunc);
  for(i=0; zProc[i]; i++){
    if( i>0 && zProc[i]=='_' && zProc[i+1]=='_' && 
        isalnum(zProc[i-1]) && isalnum(zProc[i+2]) ){
      zProc[i] = ':';
      zProc[i+1] = ':';
    }
  }
  return zProc;
}

/*
** Scan a source file looking for new TCL commands and/or the Et_AppInit()
** or Et_PreInit() functions.
**
** Skip all comments, and any text contained within "#if 0".."#endif"
*/
void ScanFile(const char *fileName){
  char *z;     /* Complete text of the file, NULL terminated. */
  int i, j;
  int inBrace = 0;
  int line = 1;

  z = ReadFileIntoMemory(fileName, 0);
  if( z==0 ){
    nError++;
    return;
  }
  for(i=0; z[i]; i++){
    switch( z[i] ){
      case '\n':
        line++;
        break;
      case '/':
        /* This might be a comment.  If it is, skip it. */
        if( z[i+1]=='*' ){
          int start = line;
          i += 2;
          while( z[i] && (z[i]!='*' || z[i+1]!='/') ){
            if( z[i]=='\n' ) line++;
            i++;
          }
          if( z[i]==0 ){
            fprintf(stderr,"%s: Unterminated comment beginning on line %d\n",
              fileName, start);
            nError++;
          }else{
            i++;
          }
        }else if( z[i+1]=='/' ){
          while( z[i] && z[i]!='\n' ){ i++; }
          if( z[i] ){ line++; };
        }
        break;
      case '\'': {
        /* Skip character literals */
        int start = line;
        for(i++; z[i] && z[i]!='\''; i++){
          if( z[i]=='\n' ){
            fprintf(stderr,"%s: Newline in character literal on line %d\n",
              fileName, start);
            line++;
          }
          if( z[i]=='\\' ) i++;
        }
        if( z[i]==0 ){
          fprintf(stderr,"%s: unterminate character literal on line %d\n",
            fileName, start);
          nError++;
        }
        break;
      }
      case '"': {
        /* Skip over a string */
        int start = line;
        for(i++; z[i] && z[i]!='"'; i++){
          if( z[i]=='\n' ){
            fprintf(stderr,"%s: Newline in string literal on line %d\n",
              fileName, start);
            line++;
          }
          if( z[i]=='\\' ) i++;
        }
        if( z[i]==0 ){
          fprintf(stderr,"%s: unterminate string literal on line %d\n",
            fileName, start);
          nError++;
        }
        break;
      }
      case '#':
        /* This might be a preprocessor macro such as #if 0 or #endif */
        if( i>0 && z[i-1]!='\n' ) break;
        for(j=i+1; isspace(z[j]); j++){}
        if( strncmp(&z[j],"endif",5)==0 ){
          if( ifStack==0 ){
            fprintf(stderr,"%s: Unmatched \"#endif\" on line %d\n",
              fileName, line);
            nError++;
          }else{
            IfStmt *p = ifStack;
            ifStack = p->pNext;
            SafeFree(p->zArg);
            SafeFree(p);
          }
          break;
        }
        if( strncmp(&z[j],"else",4)==0 ){
          if( ifStack==0 ){
            fprintf(stderr,"%s: No \"#if\" to pair with \"#else\" on line %d\n",
              fileName, line);
            nError++;
          }else{
            ifStack->invert = !ifStack->invert;
          }
          break;
        }
        if( z[j]!='i' || z[j+1]!='f' ) break;
        if( strncmp(&z[j+2],"ndef",4)==0 ){
          char *zArg;
          int start = line;
          i = j+6;
          zArg = GetArg(fileName, z,&i,&line);
          PushIf(zArg,start,1,1);
          SafeFree(zArg);
        }else if( strncmp(&z[j+2],"def",3)==0 ){
          char *zArg;
          int start = line;
          i = j+5;
          zArg = GetArg(fileName,z,&i,&line);
          PushIf(zArg,start,0,1);
          SafeFree(zArg);
        }else{
          char *zArg;
          int start = line;
          i = j+2;
          zArg = GetArg(fileName,z,&i,&line);
          PushIf(zArg,start,0,0);
          SafeFree(zArg);
        }
        break;
      case '{':
        inBrace++;
        break;
      case '}':
        inBrace--;
        break;
      case 'm':
        /* Check main() */ 
        if( inBrace>0 ) break;
        if( i>0 && IsIdent(z[i-1]) ) break;
        if( strncmp(&z[i],"main",4)==0 && !IsIdent(z[i+4]) ){
          seenMain = IfString(seenMain);
        }
      case 'E':
        /* Check ET_COMMAND_... or Et_AppInit or Et_PreInit */
        if( inBrace>0 ) break;
        if( i>0 && IsIdent(z[i-1]) ) break;
        if( z[i+1]=='T' && strncmp(&z[i],"ET_COMMAND_",11)==0 ){
          EtCmd *p;
          for(j=i+11; IsIdent(z[j]); j++){}
          p = SafeMalloc( sizeof(EtCmd) );
          p->zIf = IfString(0);
          p->zName = SafeMalloc( j-(i+9) );
          sprintf(p->zName,"%.*s",j-(i+11),&z[i+11]);
          p->pNext = cmdList;
          cmdList = p;
          nCmd++;
        }else if( z[i+1]=='T' && strncmp(&z[i],"ET_OBJCOMMAND_",14)==0 ){
          EtCmd *p;
          for(j=i+14; IsIdent(z[j]); j++){}
          p = SafeMalloc( sizeof(EtCmd) );
          p->zIf = IfString(0);
          p->zName = SafeMalloc( j-(i+9) );
          p->isObj = 1;
          sprintf(p->zName,"%.*s",j-(i+14),&z[i+14]);
          p->pNext = cmdList;
          cmdList = p;
          nObjCmd++;
        }else if( z[i+1]=='t' ){
          if( strncmp(&z[i],"Et_AppInit",10)==0 && !IsIdent(z[i+10]) ){
            seenEtAppInit = IfString(seenEtAppInit);
          }
          if( strncmp(&z[i],"Et_PreInit",10)==0 && !IsIdent(z[i+10]) ){
            seenEtPreInit = IfString(seenEtPreInit);
          }
          if( strncmp(&z[i],"Et_CustomMainLoop",17)==0 && !IsIdent(z[i+17]) ){
            seenEtCustomMainLoop = IfString(seenEtCustomMainLoop);
          }
        }
        break;
      default:
        /* Do nothing.  Continue to the next character */
        break;
    }
  }
  SafeFree(z);
  while( ifStack ){
    IfStmt *p = ifStack;
    fprintf(stderr,"%s: unterminated \"#if\" on line %d\n", fileName, p->line);
    nError++;
    ifStack = p->pNext;
    SafeFree(p->zArg);
    SafeFree(p);
  }
}

/*
** Set a macro according to the value of an #if argument.
*/
static void SetMacro(char *zMacroName, char *zIf){
  if( zIf==0 || *zIf==0 ){
    printf("#define %s 1\n",zMacroName);
  }else if( zIf[0]=='0' && zIf[1]==0 ){
    printf("#define %s 0\n",zMacroName);
  }else{
    printf(
      "#if %s\n"
      "# define %s 1\n"
      "#else\n"
      "# define %s 0\n"
      "#endif\n",
      zIf, zMacroName, zMacroName
    );
  }
}

/* Forward declaration...*/
static void WriteAsString(char*,int);

/*
** Set a string macro to the value given, if that value is not NULL.
*/
static void SetStringMacro(char *zMacroName, char *z){
  if( z==0 || *z==0 ) return;
  printf("#define %s ", zMacroName);
  WriteAsString(z,0);
}

/*
** Look at the name of the file given and see if it is a Tcl file
** or a C or C++ source file.  Return TRUE for TCL and FALSE for
** C or C++.
*/
static int IsTclFile(char *zFile){
  static char *azCSuffix[] = {
    ".c", ".cc", ".C", ".cpp", ".CPP", ".cxx", ".CXX"
  };
  int len = strlen(zFile);
  int i;
  for(i=0; i<sizeof(azCSuffix)/sizeof(azCSuffix[0]); i++){
    int len2 = strlen(azCSuffix[i]);
    if( len>len2 && strcmp(&zFile[len-len2],azCSuffix[i])==0 ){
      return 0;
    }
  }
  return 1;
}

/*
** Compress a TCL script by removing comments and excess white-space
*/
static void CompressTcl(char *z){
  int i, j, c;
  int atLineStart = 1;
  for(i=j=0; (c=z[i])!=0; i++){
    switch( c ){
      case ' ':
      case '\t':
      case '\r':
        if( atLineStart ){
          c = 0;
        }
        break;
      case '#':
        if( atLineStart && !isalpha(z[i+1]) && strncmp(z,"# @(#)",6)!=0 ){
          while( z[i] && z[i]!='\n' ){ 
            if( z[i]=='\\' ){
              i++;
              if( z[i]=='\r' && z[i+1]=='\n' ){ i++; }
            }
            i++; 
          }
          c = 0;
          if( z[i]==0 ){ i--; }
        }else{
          atLineStart = 0;
        }
        break;
      case '\n':
        if( atLineStart ){
          c = 0;
        }else if( (i>0 && z[i-1]=='\\') 
               || (i>1 && z[i-1]=='\r' && z[i-2]=='\\') ){
          /* The line continues.  Do not compress.
          ** Compressing here breaks BWidgets... */
        }else{
          atLineStart = 1;
        }
        break;
      default:
        atLineStart = 0;
        break;
    }
    if( c!=0 ){
      z[j++] = c;
    }
  }
  z[j] = 0;
}

/*
** Write the text of the given file as a string.  Tcl-style comments
** are removed if the doCompress flag is true.
*/
static void WriteAsString(char *z, int shroud){
  int c;
  int priorc = 0;
  int xor;
  int atLineStart = 1;
  if( shroud>0 ){
    xor = shroud;
  }
  putchar('"');
  atLineStart = 0;
  while( (c=*z)!=0 ){
    z++;
    if( c=='\r' && *z=='\n' ) continue;
    if( shroud>0 && c>=0x20 ){ c ^= xor; xor = (xor+1)&0x1f; }
    if( atLineStart ){
      putchar('"');
      atLineStart = 0;
    }
    switch( c ){
      case '?':
        /* Prevent two "?" characters in a row, as this causes problems
        ** for compilers that interpret trigraphs */
        if( c==priorc ){
          putchar('\\');
          putchar( ((c>>6)&3) + '0' );
          putchar( ((c>>3)&7) + '0' );
          putchar( (c&7) + '0' );
          c = 0;
        }else{
          putchar(c);
        }
        break;         
      case '"':
      case '\\':
        putchar('\\');
        putchar(c);
        break;
      case '\n':
        putchar('\\');
        putchar('n');
        putchar('"');
        putchar('\n');
        atLineStart = 1;
        break;
      default:
        if( c<' ' || c>'~' ){
          putchar('\\');
          putchar( ((c>>6)&3) + '0' );
          putchar( ((c>>3)&7) + '0' );
          putchar( (c&7) + '0' );
        }else{
          putchar(c);
        }
        break;
    }
    priorc = c;
  }
  if( !atLineStart ){
    putchar('"');
    putchar('\n');
  }
}

/*
** The header string.
*/
static char zHeader[] = 
"/* Automatically generated code */\n"
"/* DO NOT EDIT */\n"
"#ifndef ET_TCLARGS\n"
"#include <tcl.h>\n"
"#ifdef __cplusplus\n"
"# define ET_EXTERN extern \"C\"\n"
"#else\n"
"# define ET_EXTERN extern\n"
"#endif\n"
"ET_EXTERN char *mprintf(const char*,...);\n"
"ET_EXTERN char *vmprintf(const char*,...);\n"
"ET_EXTERN int Et_EvalF(Tcl_Interp*,const char *,...);\n"
"ET_EXTERN int Et_GlobalEvalF(Tcl_Interp*,const char *,...);\n"
"ET_EXTERN int Et_DStringAppendF(Tcl_DString*,const char*,...);\n"
"ET_EXTERN int Et_ResultF(Tcl_Interp*,const char*,...);\n"
"ET_EXTERN int Et_Init(int,char**);\n"
"ET_EXTERN Tcl_Interp *Et_Interp;\n"
"#if TCL_RELEASE_VERSION>=8\n"
"ET_EXTERN int Et_AppendObjF(Tcl_Obj*,const char*,...);\n"
"#endif\n"
"#define ET_TCLARGS "
     "ClientData clientData,Tcl_Interp*interp,int argc,char**argv\n"
"#define ET_OBJARGS "
     "ClientData clientData,Tcl_Interp*interp,int objc,Tcl_Obj *CONST objv[]\n"
"#endif\n"
;

/*
** Print a usage comment and die
*/
static void Usage(char *argv0){
  fprintf(stderr,"Usage: %s arguments...\n", argv0);
  fprintf(stderr,
     "  -version           print the version number of mktclapp and exit\n"
     "  -header            print a header file and exit\n"
     "  -srcdir DIR        Prepend DIR to all relative pathnames\n"
     "  -notk              built a Tcl-only program.  No GUI\n"
     "  -extension NAME    build a Tcl/Tk extension with the given name\n"
     "  -autofork          automatically fork the program into the background\n"
     "  -strip-tcl         remove comments and extra white-space from\n"
     "                     subsequent TCL files\n"
     "  -dont-strip-tcl    stop stripping TCL files\n"
     "  -tcl-library DIR   directory holding the TCL script library\n"
     "  -tk-library DIR    directory holding the TK script library\n"
     "  -main-script FILE  run the script FILE after initialization\n"
     "  -read-stdin        read standard input\n"
     "  -console           create a console window\n"
     "  -shroud            hide compile-in TCL from view\n"
     "  -enable-obj        use TCL Obj commands where possible\n"
     "  -standalone        make the \"source\" TCL command only work\n"
     "                     for builtin scripts\n"
     "  -f FILE            read more command-line parameters from FILE\n"
     "  -i FILE            make the binary file FILE part of the C code\n"
     "  *.c                scan this file for new TCL commands\n"
     "  *.tcl              compile this file into the generated C code\n"
  );
  exit(1);
}

/*
** Read one or more characters form "in" that follow a \ and
** interpret them appropriately.  Return the character that
** results from this interpretation.
*/
static int EscapeChar(FILE *in){
  int c, d;
  c = getc(in);
  switch( c ){
    case 'n':
      c = '\n';
      break;
    case 'r':
      c = '\r';
      break;
    case 'f':
      c = '\f';
      break;
    case 't':
      c = '\t';
      break;
    case 'b':
      c = '\b';
      break;
    case 'a':
      c = '\a';
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      c -= '0';
      d = getc(in);
      if( d<'0' || d>'7' ){
        ungetc(d,in);
        break;
      }
      c = (c<<3) + (d - '0');
      if( d<'0' || d>'7' ){
        ungetc(d,in);
        break;
      }
      c = (c<<3) + (d - '0');
      break;
    default:
      break;
  }
  return c;
}

/* MS-Windows and MS-DOS both have the following serious OS bug:  the
** length of a command line is severely restricted.  But this program
** occasionally requires long command lines.  Hence the following
** work around.
**
** If the parameters "-f FILENAME" appear anywhere on the command line,
** then the named file is scanned for additional command line arguments.
** These arguments are substituted in place of the "FILENAME" argument
** in the original argument list.
**
** This first parameter to this routine is the index of the "-f"
** parameter in the argv[] array.  The argc and argv are passed by
** pointer so that they can be changed.
**
** Parsing of the parameters in the file is very simple.  Parameters
** can be separated by any amount of white-space (including newlines
** and carriage returns.)  " and ' can be used for quoting strings
** with embedded spaces.  The \ character escapes the following character.
** The length of a token is limited to about 1000 characters.
*/
static void AddParameters(int index, int *pArgc, char ***pArgv){
  int argc = *pArgc;      /* The original argc value */
  char **argv = *pArgv;   /* The original argv value */
  int newArgc;            /* Value for argc after inserting new arguments */
  char **zNew;            /* The new argv after this routine is done */
  char *zFile;            /* Name of the input file */
  int nNew = 0;           /* Number of new entries in the argv[] file */
  int nAlloc = 0;         /* Space allocated for zNew[] */
  int i;                  /* Loop counter */
  int n;                  /* Number of characters in a new argument */
  int c;                  /* Next character of input */
  int startOfLine = 1;    /* True if we are where '#' can start a comment */
  FILE *in;               /* The input file */
  char zBuf[1000];        /* A single argument is accumulated here */

  if( index+1==argc ) return;
  zFile = argv[index+1];
  in = fopen(zFile,"r");
  if( in==0 ){
    fprintf(stderr,"Can't open input file \"%s\"\n",zFile);
    exit(1);
  }
  c = ' ';
  while( c!=EOF ){
    while( c!=EOF && isspace(c) ){
      if( c=='\n' ){
        startOfLine = 1;
      }
      c = getc(in); 
      if( startOfLine && c=='#' ){
        while( c!=EOF && c!='\n' ){
          c = getc(in);
        }
      }
    }
    n = 0;
    if( c=='\'' || c=='"' ){
      int quote = c;
      c = getc(in);
      startOfLine = 0;
      while( c!=EOF && c!=quote ){
        if( c=='\\' ) c = EscapeChar(in);
        if( n<sizeof(zBuf)-1 ){ zBuf[n++] = c; }
        c = getc(in);
      }
      if( c!=EOF ) c = getc(in);
    }else{
      while( c!=EOF && !isspace(c) ){
        if( c=='\\' ) c = EscapeChar(in);
        if( n<sizeof(zBuf)-1 ){ zBuf[n++] = c; }
        startOfLine = 0;
        c = getc(in);
      }
    }
    zBuf[n] = 0;
    if( n>0 ){
      nNew++;
      if( nNew + argc >= nAlloc ){
        if( nAlloc==0 ){
          nAlloc = 100 + argc;
          zNew = malloc( sizeof(char*) * nAlloc );
        }else{
          nAlloc *= 2;
          zNew = realloc( zNew, sizeof(char*) * nAlloc );  
        }
      }
      if( zNew ){
        int j = nNew + index;
        zNew[j] = malloc( n + 1 );
        if( zNew[j] ){
          strcpy( zNew[j], zBuf );
        }
      }
    }
  }
  if( nNew>0 ){
    newArgc = argc + nNew - 1;
    for(i=0; i<=index; i++){
      zNew[i] = argv[i];
    }
  }else{
    zNew = argv;
  }
  for(i=nNew + index + 1; i<newArgc; i++){
    zNew[i] = argv[i + 1 - nNew];
  }
  zNew[newArgc] = 0;
  *pArgc = newArgc;
  *pArgv = zNew;
}

int main(int argc, char **argv){
  int i;                  /* Loop counter */
  EtCmd *pCmd;            /* A new TCL command found in C code */
  int useTk = 1;          /* False if the -notk flag is used */
  int autoFork = 0;       /* True if the -autofork flag is used */
  int nTcl = 0;           /* Number of TCL scripts */
  char **azTcl;           /* Name of all TCL scripts */
  int *aDoCompress;       /* Whether or not to compress each TCL script */
  int nData = 0;          /* Number of data files */
  char **azData;          /* Names of all data files */
  int doCompress = 1;     /* Current state of the compression flag */
  char *zTclLib = 0;      /* Name of the TCL library */
  char *zTkLib = 0;       /* Name of the TK library */
  char *zMainScript = 0;  /* Name of a script to run first */
  int shroud = 0;         /* True to encrypt the compiled-in TCL */
  int readStdin = 0;      /* True to read TCL commands from STDIN */
  int enableObj = 0;      /* Enable the use of object commands */
  int standalone = 0;     /* True to disable the "source" command */
  int stringify = 0;      /* True to output only strings of the scripts */
  int console = 0;        /* True to put up a debugging console */
  char *zExtension = 0;   /* Name of the extension.  NULL if a complete app */
  int nHash;              /* Number of entries in hash table */
  extern char const zTail[];   /* Neither array can exceed 64K or VC++ */
  extern char const zTail2[];  /* will break.  So we have to split in two */

  if( argc>=2 && strcmp(argv[1],"-header")==0 ){
    printf("%s",zHeader);
    return 0;
  }
  if( argc>=2 && strcmp(argv[1],"-version")==0 ){
    printf("%s\n",zVersion);
    return 0;
  }
  azTcl = SafeMalloc( sizeof(char*)*(argc + 100) );
  azData = SafeMalloc( sizeof(char*)*(argc + 100) );
  aDoCompress = SafeMalloc( sizeof(int)*(argc + 100) );
  for(i=1; i<argc; i++){
    if( argv[i][0]=='-' ){
      if( strcmp(argv[i],"-header")==0 ){
        printf("%s",zHeader);
        return 0;
      }else if( strcmp(argv[i],"-notk")==0 ){
        useTk = 0;
      }else if( i<argc-1 && strcmp(argv[i],"-extension")==0 ){
        zExtension = argv[++i];
      }else if( strcmp(argv[i],"-autofork")==0 ){
        autoFork = 1;
      }else if( strcmp(argv[i],"-read-stdin")==0 ){
        readStdin = 1;
      }else if( strcmp(argv[i],"-console")==0 ){
        console = 1;
      }else if( strcmp(argv[i],"-shroud")==0 ){
        shroud = 1;
      }else if( strcmp(argv[i],"-strip-tcl")==0 ){
        doCompress = 1;
      }else if( strcmp(argv[i],"-dont-strip-tcl")==0 ){
        doCompress = 0;
      }else if( strcmp(argv[i],"-enable-obj")==0 ){
        enableObj = 1;
      }else if( strcmp(argv[i],"-standalone")==0 ){
        standalone = 1;
      }else if( strcmp(argv[i],"-stringify")==0 ){
        stringify = 1;
      }else if( i<argc-1 && strcmp(argv[i],"-srcdir")==0 ){
        chdir(argv[++i]);
      }else if( i<argc-1 && strcmp(argv[i],"-main-script")==0 ){
        zMainScript = argv[++i];
      }else if( i<argc-1 && strcmp(argv[i],"-tcl-library")==0 ){
        zTclLib = argv[++i];
      }else if( i<argc-1 && strcmp(argv[i],"-tk-library")==0 ){
        zTkLib = argv[++i];
      }else if( i<argc-1 && strcmp(argv[i],"-i")==0 ){
        i++;
        if( access(argv[i],R_OK) ){
          fprintf(stderr,"%s: can't open \"%s\" for reading\n",Argv0,argv[i]);
          nError++;
        }else{
          azData[nData] = argv[i];
        }
        nData++;
      }else if( strcmp(argv[i],"-f")==0 ){
        AddParameters(i, &argc, &argv);
        azTcl = SafeRealloc(azTcl, sizeof(char*)*(argc + 100) );
        azData = SafeRealloc(azData, sizeof(char*)*(argc + 100) );
        aDoCompress = SafeRealloc(aDoCompress, sizeof(int)*(argc + 100) );
#ifdef TEST
      }else if( strcmp(argv[i],"-sizes")==0 ){
        fprintf(stderr,"zTail size: %d  zTail2 size: %d\n",
                strlen(zTail)+1, strlen(zTail2)+1);
#endif
      }else{
        Usage(argv[0]);
      }
    }else if( IsTclFile(argv[i]) ){
      if( access(argv[i],R_OK) ){
        fprintf(stderr,"%s: can't open \"%s\" for reading\n", Argv0, argv[i]);
        nError++;
      }else{
        int len = strlen(argv[i]);
        azTcl[nTcl] = argv[i];
        if( len>=9 && strcmp(&argv[i][len-9],"/tclIndex")==0 ){
          aDoCompress[nTcl] = 0;
        }else{
          aDoCompress[nTcl] = doCompress;
        }
        nTcl++;
      }
    }else{
      ScanFile(argv[i]);
    }
  }
  if( nError>0 ) return nError;
  if( shroud>0 ){
    shroud = time(0) % 31 + 1;
  }
  if( stringify ){
    for(i=0; i<nTcl; i++){
      char *z;
      z = ReadFileIntoMemory(azTcl[i], 0);
      if( z==0 ) continue;
      if( aDoCompress[i] ) CompressTcl(z);
      WriteAsString(z,shroud);
      printf(";\n");
      SafeFree(z);
    }
    return 0;
  }
  if( nObjCmd>0 ) enableObj = 1;
  printf(
    "/* This code is automatically generated by \"mktclapp\""
       " version 3.10 */\n"
    "/* DO NOT EDIT */\n"
    "#include <tcl.h>\n"
    "#define INTERFACE 1\n"
    "#if INTERFACE\n"
    "#define ET_TCLARGS "
       "ClientData clientData,Tcl_Interp*interp,int argc,char**argv\n"
    "#define ET_OBJARGS "
       "ClientData clientData,Tcl_Interp*interp,int objc,Tcl_Obj*CONST objv[]\n"
    "#endif\n"   
  );
  printf("#define ET_ENABLE_OBJ %d\n", enableObj);
  printf("#define ET_ENABLE_TK %d\n", useTk!=0);
  printf("#define ET_AUTO_FORK %d\n", autoFork!=0);
  printf("#define ET_STANDALONE %d\n", standalone!=0);
  printf("#define ET_N_BUILTIN_SCRIPT %d\n", nTcl);
  printf("#define ET_VERSION \"3.10\"\n");
  SetMacro("ET_HAVE_APPINIT",seenEtAppInit);
  SetMacro("ET_HAVE_PREINIT",seenEtPreInit);
  SetMacro("ET_HAVE_MAIN",seenMain);
  SetMacro("ET_HAVE_CUSTOM_MAINLOOP",seenEtCustomMainLoop);
  SetStringMacro("ET_TCL_LIBRARY", zTclLib);
  SetStringMacro("ET_TK_LIBRARY", zTkLib);
  SetStringMacro("ET_MAIN_SCRIPT", zMainScript);
  if( zExtension ){
    int i;
    if( islower(zExtension[0]) ){
      zExtension[0] = toupper(zExtension[0]);
    }
    for(i=1; zExtension[i]; i++){
      if( isupper(zExtension[i]) ){
        zExtension[i] = tolower(zExtension[i]);
      }
    }
    printf("#define ET_EXTENSION_NAME %s_Init\n", zExtension);
    printf("#define ET_SAFE_EXTENSION_NAME %s_SafeInit\n", zExtension);
    printf("#define ET_EXTENSION 1\n");
  }else{
    printf("#define ET_EXTENSION 0\n");
  }
  printf("#define ET_SHROUD_KEY %d\n",shroud);
  printf("#define ET_READ_STDIN %d\n",readStdin);
  printf("#define ET_CONSOLE %d\n",console);
  for(pCmd=cmdList; pCmd; pCmd=pCmd->pNext){
    if( pCmd->zIf && pCmd->zIf[0]=='0' && pCmd->zIf[1]==0 ) continue;
    if( pCmd->isObj ){
      printf("extern int ET_OBJCOMMAND_%s(ET_OBJARGS);\n", pCmd->zName);
    }else{
      printf("extern int ET_COMMAND_%s(ET_TCLARGS);\n", pCmd->zName);
    }
  }
  printf(
    "static struct {\n"
    "  char *zName;\n"
    "  int (*xProc)(ET_TCLARGS);\n"
    "} Et_CmdSet[] = {\n"
  );
  for(pCmd=cmdList; pCmd; pCmd=pCmd->pNext){
    char *zProc;
    if( pCmd->isObj ) continue;
    if( pCmd->zIf ){
      if( pCmd->zIf[0]=='0' && pCmd->zIf[1]==0 ) continue;
      printf("#if %s\n",pCmd->zIf);
    }
    zProc = FuncToProc(pCmd->zName);
    printf(" { \"%s\", ET_COMMAND_%s },\n", zProc, pCmd->zName);
    SafeFree(zProc);
    if( pCmd->zIf ){
      printf("#endif\n");
    }
  }
  printf("{0, 0}};\n");
  if( enableObj ){
    char *zProc;
    printf(
      "static struct {\n"
      "  char *zName;\n"
      "  int (*xProc)(ET_OBJARGS);\n"
      "} Et_ObjSet[] = {\n"
    );
    for(pCmd=cmdList; pCmd; pCmd=pCmd->pNext){
      if( !pCmd->isObj ) continue;
      if( pCmd->zIf ){
        if( pCmd->zIf[0]=='0' && pCmd->zIf[1]==0 ) continue;
        printf("#if %s\n",pCmd->zIf);
      }
      zProc = FuncToProc(pCmd->zName);
      printf(" { \"%s\", ET_OBJCOMMAND_%s },\n", zProc, pCmd->zName);
      SafeFree(zProc);
      if( pCmd->zIf ){
        printf("#endif\n");
      }
    }
    printf("{0, 0}};\n");
  }
  for(i=0; i<nTcl; i++){
    char *z;
    printf("static char Et_zFile%d[] = \n",i);
    z = ReadFileIntoMemory(azTcl[i], 0);
    if( z==0 ) continue;
    if( aDoCompress[i] ) CompressTcl(z);
    WriteAsString(z,shroud);
    printf(";\n");
    SafeFree(z);
  }
  for(i=0; i<nData; i++){
    char *z;
    int len, j, col;
    printf("static unsigned char Et_acData%d[] = {\n",i);
    z = ReadFileIntoMemory(azData[i], &len);
    if( z==0 ) continue;
    for(j=col=0; j<len; j++){
      printf(" 0x%02x,", z[j]&0xff);
      if( ++col >= 12 ){
        printf("\n");
        col = 0;
      }
    }
    if( col>0 ) printf("\n");
    printf("};\n");
    SafeFree(z);
  }
  printf(
    "struct EtFile {\n"
    "  char *zName;\n"
    "  char *zData;\n"
    "  int nData;\n"
    "  int shrouded;\n"
    "  struct EtFile *pNext;\n"
    "};\n"
    "static struct EtFile Et_FileSet[] = {\n"
  );
  for(i=0; i<nTcl; i++){
    printf("  { \"%s\", Et_zFile%d, sizeof(Et_zFile%d)-1, %d, 0 },\n", 
      azTcl[i], i, i, shroud);
  }
  for(i=0; i<nData; i++){
    printf("  { \"%s\", Et_acData%d, sizeof(Et_acData%d), 0, 0 },\n", 
      azData[i], i, i);
  }
  fflush(stdout);
  nHash = nTcl*2 + 1;
  if( nHash<71 ){
    nHash = 71;
  }
  printf(
    "{0, 0}};\n"
    "static struct EtFile *Et_FileHashTable[%d];\n"
    "%s%s",
    nHash, zTail, zTail2
  );
  return nError;
}

char const zTail[] =
"/* The following copyright notice applies to code generated by\n"
"** \"mktclapp\".  The \"mktclapp\" program itself is covered by the\n"
"** GNU Public License.\n"
"**\n"
"** Copyright (c) 1998 D. Richard Hipp\n"
"**\n"
"** The author hereby grants permission to use, copy, modify, distribute,\n"
"** and license this software and its documentation for any purpose, provided\n"
"** that existing copyright notices are retained in all copies and that this\n"
"** notice is included verbatim in any distributions. No written agreement,\n"
"** license, or royalty fee is required for any of the authorized uses.\n"
"** Modifications to this software may be copyrighted by their authors\n"
"** and need not follow the licensing terms described here, provided that\n"
"** the new terms are clearly indicated on the first page of each file where\n"
"** they apply.\n"
"**\n"
"** In no event shall the author or the distributors be liable to any party\n"
"** for direct, indirect, special, incidental, or consequential damages\n"
"** arising out of the use of this software, its documentation, or any\n"
"** derivatives thereof, even if the author has been advised of the \n"
"** possibility of such damage.  The author and distributors specifically\n"
"** disclaim any warranties, including but not limited to the implied\n"
"** warranties of merchantability, fitness for a particular purpose, and\n"
"** non-infringment.  This software is provided at no fee on an\n"
"** \"as is\" basis.  The author and/or distritutors have no obligation\n"
"** to provide maintenance, support, updates, enhancements and/or\n"
"** modifications.\n"
"**\n"
"** GOVERNMENT USE: If you are acquiring this software on behalf of the\n"
"** U.S. government, the Government shall have only \"Restricted Rights\"\n"
"** in the software and related documentation as defined in the Federal \n"
"** Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you\n"
"** are acquiring the software on behalf of the Department of Defense, the\n"
"** software shall be classified as \"Commercial Computer Software\" and the\n"
"** Government shall have only \"Restricted Rights\" as defined in Clause\n"
"** 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the\n"
"** author grants the U.S. Government and others acting in its behalf\n"
"** permission to use and distribute the software in accordance with the\n"
"** terms specified in this license. \n"
"*/\n"
"#include <ctype.h>\n"
"#include <string.h>\n"
"#include <stdarg.h>\n"
"#include <stdio.h>\n"
"#include <stdlib.h>\n"
"#include <sys/types.h>\n"
"#include <sys/stat.h>\n"
"#include <fcntl.h>\n"
"\n"
"/* Include either the Tcl or the Tk header file.  Use the \"Internal\"\n"
"** version of the header file if and only if we are generating an\n"
"** extension  that is linking against the Stub library.\n"
"** Many installations do not have the internal header files\n"
"** available, so using the internal headers only when absolutely\n"
"** necessary will help to reduce compilation problems.\n"
"*/\n"
"#if ET_EXTENSION && defined(TCL_USE_STUBS)\n"
"# if ET_ENABLE_TK\n"
"#   include <tkInt.h>\n"
"# else\n"
"#   include <tclInt.h>\n"
"# endif\n"
"#else\n"
"# if ET_ENABLE_TK\n"
"#   include <tk.h>\n"
"# else\n"
"#   include <tcl.h>\n"
"# endif\n"
"#endif\n"
"\n"
"/*\n"
"** ET_WIN32 is true if we are running Tk under windows.  The\n"
"** <tcl.h> module will define __WIN32__ for us if we are compiling\n"
"** for windows.\n"
"*/\n"
"#if defined(__WIN32__) && ET_ENABLE_TK\n"
"# define ET_WIN32 1\n"
"# include <windows.h>\n"
"#else\n"
"# define ET_WIN32 0\n"
"#endif\n"
"\n"
"/*\n"
"** Always disable ET_AUTO_FORK under windows.  Windows doesn't\n"
"** fork well.\n"
"*/\n"
"#if defined(__WIN32__)\n"
"# undef ET_AUTO_FORK\n"
"# define ET_AUTO_FORK 0\n"
"#endif\n"
"\n"
"/*\n"
"** Omit <unistd.h> under windows.  But we need it for Unix.\n"
"*/\n"
"#if !defined(__WIN32__)\n"
"# include <unistd.h>\n"
"#endif\n"
"\n"
"/*\n"
"** The Tcl*InsertProc functions allow the system calls \"stat\",\n"
"** \"access\" and \"open\" to be overloaded.  This in turns allows us\n"
"** to substituted compiled-in strings for files in the filesystem.\n"
"** But the Tcl*InsertProc functions are only available in Tcl8.0.3\n"
"** and later.\n"
"**\n"
"** Define the ET_HAVE_INSERTPROC macro if and only if we are dealing\n"
"** with Tcl8.0.3 or later.\n"
"*/\n"
"#if TCL_MAJOR_VERSION==8 && (TCL_MINOR_VERSION>0 || TCL_RELEASE_SERIAL>=3)\n"
"# define ET_HAVE_INSERTPROC\n"
"#endif\n"
"\n"
"/*\n"
"** If we are using the Tcl*InsertProc() functions, we should provide\n"
"** prototypes for them.  But the prototypes are in the tclInt.h include\n"
"** file, which we don't want to require the user to have on hand.  So\n"
"** we provide our own prototypes here.\n"
"**\n"
"** Note that if TCL_USE_STUBS is defined, then the tclInt.h is required\n"
"** anyway, so these prototypes are not included if TCL_USE_STUBS is\n"
"** defined.  \n"
"*/\n"
"#if defined(ET_HAVE_INSERTPROC) && !defined(TCL_USE_STUBS)\n"
"#ifdef __cplusplus\n"
"  extern \"C\" int TclStatInsertProc(int (*)(char*, struct stat *));\n"
"  extern \"C\" int TclAccessInsertProc(int (*)(char*, int));\n"
"  extern \"C\" int TclOpenFileChannelInsertProc(Tcl_Channel (*)(Tcl_Interp*,char*,\n"
"                                                          char*,int));\n"
"#else\n"
"  extern int TclStatInsertProc(int (*)(char*, struct stat *));\n"
"  extern int TclAccessInsertProc(int (*)(char*, int));\n"
"  extern int TclOpenFileChannelInsertProc(Tcl_Channel (*)(Tcl_Interp*,char*,\n"
"                                                          char*,int));\n"
"#endif\n"
"#endif\n"
"\n"
"\n"
"/*\n"
"** Don't allow Win32 applications to read from stdin.  Nor\n"
"** programs that automatically go into the background.  Force\n"
"** the use of a console in these cases.\n"
"*/\n"
"#if (ET_WIN32 || ET_AUTO_FORK) && ET_READ_STDIN\n"
"# undef ET_READ_STDIN\n"
"# undef ET_CONSOLE\n"
"# define ET_READ_STDIN 0\n"
"# define ET_CONSOLE 1\n"
"#endif\n"
"\n"
"/*\n"
"** The console won't work without Tk.\n"
"*/\n"
"#if ET_ENABLE_TK==0 && ET_CONSOLE\n"
"# undef ET_CONSOLE\n"
"# define ET_CONSOLE 0\n"
"# undef ET_READ_STDIN\n"
"# define ET_READ_STDIN 1\n"
"#endif\n"
"\n"
"/*\n"
"** We MUST start using Tcl_GetStringResult() in Tcl8.3\n"
"** But these functions didn't exists in Tcl 7.6.  So make\n"
"** them macros.\n"
"*/\n"
"#if TCL_MAJOR_VERSION<8\n"
"# define Tcl_GetStringResult(I)  ((I)->result)\n"
"#endif\n"
"\n"
"/*\n"
"** Set ET_HAVE_OBJ to true if we are able to link against the\n"
"** new Tcl_Obj interface.  This is only the case for Tcl version\n"
"** 8.0 and later.\n"
"*/\n"
"#if ET_ENABLE_OBJ || TCL_MAJOR_VERSION>=8\n"
"# define ET_HAVE_OBJ 1\n"
"#else\n"
"# define ET_HAVE_OBJ 0\n"
"#endif\n"
"\n"
"/*\n"
"** The Tcl_GetByteArrayFromObj() only appears in Tcl version 8.1\n"
"** and later.  Substitute Tcl_GetStringFromObj() in Tcl version 8.0.X\n"
"*/\n"
"#if ET_HAVE_OBJ && TCL_MINOR_VERSION==0\n"
"# define Tcl_GetByteArrayFromObj Tcl_GetStringFromObj\n"
"#endif\n"
"\n"
"/*\n"
"** Tcl code to implement the console.\n"
"**\n"
"** This code is written and tested separately, then run through\n"
"** \"mktclapp -stringify\" and then pasted in here.\n"
"*/\n"
"#if ET_ENABLE_TK && !ET_EXTENSION\n"
"static char zEtConsole[] =\n"
"\"proc console:create {w prompt title} {\\n\"\n"
"\"upvar #0 $w.t v\\n\"\n"
"\"if {[winfo exists $w]} {destroy $w}\\n\"\n"
"\"if {[info exists v]} {unset v}\\n\"\n"
"\"toplevel $w\\n\"\n"
"\"wm title $w $title\\n\"\n"
"\"wm iconname $w $title\\n\"\n"
"\"frame $w.mb -bd 2 -relief raised\\n\"\n"
"\"pack $w.mb -side top -fill x\\n\"\n"
"\"menubutton $w.mb.file -text File -menu $w.mb.file.m\\n\"\n"
"\"menubutton $w.mb.edit -text Edit -menu $w.mb.edit.m\\n\"\n"
"\"pack $w.mb.file $w.mb.edit -side left -padx 8 -pady 1\\n\"\n"
"\"set m [menu $w.mb.file.m]\\n\"\n"
"\"$m add command -label {Source...} -command \\\"console:SourceFile $w.t\\\"\\n\"\n"
"\"$m add command -label {Save As...} -command \\\"console:SaveFile $w.t\\\"\\n\"\n"
"\"$m add separator\\n\"\n"
"\"$m add command -label {Close} -command \\\"destroy $w\\\"\\n\"\n"
"\"$m add command -label {Exit} -command exit\\n\"\n"
"\"set m [menu $w.mb.edit.m]\\n\"\n"
"\"$m add command -label Cut -command \\\"console:Cut $w.t\\\"\\n\"\n"
"\"$m add command -label Copy -command \\\"console:Copy $w.t\\\"\\n\"\n"
"\"$m add command -label Paste -command \\\"console:Paste $w.t\\\"\\n\"\n"
"\"$m add command -label {Clear Screen} -command \\\"console:Clear $w.t\\\"\\n\"\n"
"\"catch {$m config -postcommand \\\"console:EnableEditMenu $w\\\"}\\n\"\n"
"\"scrollbar $w.sb -orient vertical -command \\\"$w.t yview\\\"\\n\"\n"
"\"pack $w.sb -side right -fill y\\n\"\n"
"\"text $w.t -font fixed -yscrollcommand \\\"$w.sb set\\\"\\n\"\n"
"\"pack $w.t -side right -fill both -expand 1\\n\"\n"
"\"bindtags $w.t Console\\n\"\n"
"\"set v(text) $w.t\\n\"\n"
"\"set v(history) 0\\n\"\n"
"\"set v(historycnt) 0\\n\"\n"
"\"set v(current) -1\\n\"\n"
"\"set v(prompt) $prompt\\n\"\n"
"\"set v(prior) {}\\n\"\n"
"\"set v(plength) [string length $v(prompt)]\\n\"\n"
"\"set v(x) 0\\n\"\n"
"\"set v(y) 0\\n\"\n"
"\"$w.t mark set insert end\\n\"\n"
"\"$w.t tag config ok -foreground blue\\n\"\n"
"\"$w.t tag config err -foreground red\\n\"\n"
"\"$w.t insert end $v(prompt)\\n\"\n"
"\"$w.t mark set out 1.0\\n\"\n"
"\"catch {rename puts console:oldputs$w}\\n\"\n"
"\"proc puts args [format {\\n\"\n"
"\"if {![winfo exists %s]} {\\n\"\n"
"\"rename puts {}\\n\"\n"
"\"rename console:oldputs%s puts\\n\"\n"
"\"return [uplevel #0 puts $args]\\n\"\n"
"\"}\\n\"\n"
"\"switch -glob -- \\\"[llength $args] $args\\\" {\\n\"\n"
"\"{1 *} {\\n\"\n"
"\"set msg [lindex $args 0]\\\\n\\n\"\n"
"\"set tag ok\\n\"\n"
"\"}\\n\"\n"
"\"{2 stdout *} {\\n\"\n"
"\"set msg [lindex $args 1]\\\\n\\n\"\n"
"\"set tag ok\\n\"\n"
"\"}\\n\"\n"
"\"{2 stderr *} {\\n\"\n"
"\"set msg [lindex $args 1]\\\\n\\n\"\n"
"\"set tag err\\n\"\n"
"\"}\\n\"\n"
"\"{2 -nonewline *} {\\n\"\n"
"\"set msg [lindex $args 1]\\n\"\n"
"\"set tag ok\\n\"\n"
"\"}\\n\"\n"
"\"{3 -nonewline stdout *} {\\n\"\n"
"\"set msg [lindex $args 2]\\n\"\n"
"\"set tag ok\\n\"\n"
"\"}\\n\"\n"
"\"{3 -nonewline stderr *} {\\n\"\n"
"\"set msg [lindex $args 2]\\n\"\n"
"\"set tag err\\n\"\n"
"\"}\\n\"\n"
"\"default {\\n\"\n"
"\"uplevel #0 console:oldputs%s $args\\n\"\n"
"\"return\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"console:Puts %s $msg $tag\\n\"\n"
"\"} $w $w $w $w.t]\\n\"\n"
"\"after idle \\\"focus $w.t\\\"\\n\"\n"
"\"}\\n\"\n"
"\"bind Console <1> {console:Button1 %W %x %y}\\n\"\n"
"\"bind Console <B1-Motion> {console:B1Motion %W %x %y}\\n\"\n"
"\"bind Console <B1-Leave> {console:B1Leave %W %x %y}\\n\"\n"
"\"bind Console <B1-Enter> {console:cancelMotor %W}\\n\"\n"
"\"bind Console <ButtonRelease-1> {console:cancelMotor %W}\\n\"\n"
"\"bind Console <KeyPress> {console:Insert %W %A}\\n\"\n"
"\"bind Console <Left> {console:Left %W}\\n\"\n"
"\"bind Console <Control-b> {console:Left %W}\\n\"\n"
"\"bind Console <Right> {console:Right %W}\\n\"\n"
"\"bind Console <Control-f> {console:Right %W}\\n\"\n"
"\"bind Console <BackSpace> {console:Backspace %W}\\n\"\n"
"\"bind Console <Control-h> {console:Backspace %W}\\n\"\n"
"\"bind Console <Delete> {console:Delete %W}\\n\"\n"
"\"bind Console <Control-d> {console:Delete %W}\\n\"\n"
"\"bind Console <Home> {console:Home %W}\\n\"\n"
"\"bind Console <Control-a> {console:Home %W}\\n\"\n"
"\"bind Console <End> {console:End %W}\\n\"\n"
"\"bind Console <Control-e> {console:End %W}\\n\"\n"
"\"bind Console <Return> {console:Enter %W}\\n\"\n"
"\"bind Console <KP_Enter> {console:Enter %W}\\n\"\n"
"\"bind Console <Up> {console:Prior %W}\\n\"\n"
"\"bind Console <Control-p> {console:Prior %W}\\n\"\n"
"\"bind Console <Down> {console:Next %W}\\n\"\n"
"\"bind Console <Control-n> {console:Next %W}\\n\"\n"
"\"bind Console <Control-k> {console:EraseEOL %W}\\n\"\n"
"\"bind Console <<Cut>> {console:Cut %W}\\n\"\n"
"\"bind Console <<Copy>> {console:Copy %W}\\n\"\n"
"\"bind Console <<Paste>> {console:Paste %W}\\n\"\n"
"\"bind Console <<Clear>> {console:Clear %W}\\n\"\n"
"\"proc console:Puts {w t tag} {\\n\"\n"
"\"set nc [string length $t]\\n\"\n"
"\"set endc [string index $t [expr $nc-1]]\\n\"\n"
"\"if {$endc==\\\"\\\\n\\\"} {\\n\"\n"
"\"if {[$w index out]<[$w index {insert linestart}]} {\\n\"\n"
"\"$w insert out [string range $t 0 [expr $nc-2]] $tag\\n\"\n"
"\"$w mark set out {out linestart +1 lines}\\n\"\n"
"\"} else {\\n\"\n"
"\"$w insert out $t $tag\\n\"\n"
"\"}\\n\"\n"
"\"} else {\\n\"\n"
"\"if {[$w index out]<[$w index {insert linestart}]} {\\n\"\n"
"\"$w insert out $t $tag\\n\"\n"
"\"} else {\\n\"\n"
"\"$w insert out $t\\\\n $tag\\n\"\n"
"\"$w mark set out {out -1 char}\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"$w yview insert\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Insert {w a} {\\n\"\n"
"\"$w insert insert $a\\n\"\n"
"\"$w yview insert\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Left {w} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"if {$col>$v(plength)} {\\n\"\n"
"\"$w mark set insert \\\"insert -1c\\\"\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Backspace {w} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"if {$col>$v(plength)} {\\n\"\n"
"\"$w delete {insert -1c}\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:EraseEOL {w} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"if {$col>=$v(plength)} {\\n\"\n"
"\"$w delete insert {insert lineend}\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Right {w} {\\n\"\n"
"\"$w mark set insert \\\"insert +1c\\\"\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Delete w {\\n\"\n"
"\"$w delete insert\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Home w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"$w mark set insert $row.$v(plength)\\n\"\n"
"\"}\\n\"\n"
"\"proc console:End w {\\n\"\n"
"\"$w mark set insert {insert lineend}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Enter w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"set start $row.$v(plength)\\n\"\n"
"\"set line [$w get $start \\\"$start lineend\\\"]\\n\"\n"
"\"if {$v(historycnt)>0} {\\n\"\n"
"\"set last [lindex $v(history) [expr $v(historycnt)-1]]\\n\"\n"
"\"if {[string compare $last $line]} {\\n\"\n"
"\"lappend v(history) $line\\n\"\n"
"\"incr v(historycnt)\\n\"\n"
"\"}\\n\"\n"
"\"} else {\\n\"\n"
"\"set v(history) [list $line]\\n\"\n"
"\"set v(historycnt) 1\\n\"\n"
"\"}\\n\"\n"
"\"set v(current) $v(historycnt)\\n\"\n"
"\"$w insert end \\\\n\\n\"\n"
"\"$w mark set out end\\n\"\n"
"\"if {$v(prior)==\\\"\\\"} {\\n\"\n"
"\"set cmd $line\\n\"\n"
"\"} else {\\n\"\n"
"\"set cmd $v(prior)\\\\n$line\\n\"\n"
"\"}\\n\"\n"
"\"if {[info complete $cmd]} {\\n\"\n"
"\"set rc [catch {uplevel #0 $cmd} res]\\n\"\n"
"\"if {![winfo exists $w]} return\\n\"\n"
"\"if {$rc} {\\n\"\n"
"\"$w insert end $res\\\\n err\\n\"\n"
"\"} elseif {[string length $res]>0} {\\n\"\n"
"\"$w insert end $res\\\\n ok\\n\"\n"
"\"}\\n\"\n"
"\"set v(prior) {}\\n\"\n"
"\"$w insert end $v(prompt)\\n\"\n"
"\"} else {\\n\"\n"
"\"set v(prior) $cmd\\n\"\n"
"\"regsub -all {[^ ]} $v(prompt) . x\\n\"\n"
"\"$w insert end $x\\n\"\n"
"\"}\\n\"\n"
"\"$w mark set insert end\\n\"\n"
"\"$w mark set out {insert linestart}\\n\"\n"
"\"$w yview insert\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Prior w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"if {$v(current)<=0} return\\n\"\n"
"\"incr v(current) -1\\n\"\n"
"\"set line [lindex $v(history) $v(current)]\\n\"\n"
"\"console:SetLine $w $line\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Next w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"if {$v(current)>=$v(historycnt)} return\\n\"\n"
"\"incr v(current) 1\\n\"\n"
"\"set line [lindex $v(history) $v(current)]\\n\"\n"
"\"console:SetLine $w $line\\n\"\n"
"\"}\\n\"\n"
"\"proc console:SetLine {w line} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"scan [$w index insert] %d.%d row col\\n\"\n"
"\"set start $row.$v(plength)\\n\"\n"
"\"$w delete $start end\\n\"\n"
"\"$w insert end $line\\n\"\n"
"\"$w mark set insert end\\n\"\n"
"\"$w yview insert\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Button1 {w x y} {\\n\"\n"
"\"global tkPriv\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"set v(mouseMoved) 0\\n\"\n"
"\"set v(pressX) $x\\n\"\n"
"\"set p [console:nearestBoundry $w $x $y]\\n\"\n"
"\"scan [$w index insert] %d.%d ix iy\\n\"\n"
"\"scan $p %d.%d px py\\n\"\n"
"\"if {$px==$ix} {\\n\"\n"
"\"$w mark set insert $p\\n\"\n"
"\"}\\n\"\n"
"\"$w mark set anchor $p\\n\"\n"
"\"focus $w\\n\"\n"
"\"}\\n\"\n"
"\"proc console:nearestBoundry {w x y} {\\n\"\n"
"\"set p [$w index @$x,$y]\\n\"\n"
"\"set bb [$w bbox $p]\\n\"\n"
"\"if {![string compare $bb \\\"\\\"]} {return $p}\\n\"\n"
"\"if {($x-[lindex $bb 0])<([lindex $bb 2]/2)} {return $p}\\n\"\n"
"\"$w index \\\"$p + 1 char\\\"\\n\"\n"
"\"}\\n\"\n"
"\"proc console:SelectTo {w x y} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"set cur [console:nearestBoundry $w $x $y]\\n\"\n"
"\"if {[catch {$w index anchor}]} {\\n\"\n"
"\"$w mark set anchor $cur\\n\"\n"
"\"}\\n\"\n"
"\"set anchor [$w index anchor]\\n\"\n"
"\"if {[$w compare $cur != $anchor] || (abs($v(pressX) - $x) >= 3)} {\\n\"\n"
"\"if {$v(mouseMoved)==0} {\\n\"\n"
"\"$w tag remove sel 0.0 end\\n\"\n"
"\"}\\n\"\n"
"\"set v(mouseMoved) 1\\n\"\n"
"\"}\\n\"\n"
"\"if {[$w compare $cur < anchor]} {\\n\"\n"
"\"set first $cur\\n\"\n"
"\"set last anchor\\n\"\n"
"\"} else {\\n\"\n"
"\"set first anchor\\n\"\n"
"\"set last $cur\\n\"\n"
"\"}\\n\"\n"
"\"if {$v(mouseMoved)} {\\n\"\n"
"\"$w tag remove sel 0.0 $first\\n\"\n"
"\"$w tag add sel $first $last\\n\"\n"
"\"$w tag remove sel $last end\\n\"\n"
"\"update idletasks\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:B1Motion {w x y} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"set v(y) $y\\n\"\n"
"\"set v(x) $x\\n\"\n"
"\"console:SelectTo $w $x $y\\n\"\n"
"\"}\\n\"\n"
"\"proc console:B1Leave {w x y} {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"set v(y) $y\\n\"\n"
"\"set v(x) $x\\n\"\n"
"\"console:motor $w\\n\"\n"
"\"}\\n\"\n"
"\"proc console:motor w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"if {![winfo exists $w]} return\\n\"\n"
"\"if {$v(y)>=[winfo height $w]} {\\n\"\n"
"\"$w yview scroll 1 units\\n\"\n"
"\"} elseif {$v(y)<0} {\\n\"\n"
"\"$w yview scroll -1 units\\n\"\n"
"\"} else {\\n\"\n"
"\"return\\n\"\n"
"\"}\\n\"\n"
"\"console:SelectTo $w $v(x) $v(y)\\n\"\n"
"\"set v(timer) [after 50 console:motor $w]\\n\"\n"
"\"}\\n\"\n"
"\"proc console:cancelMotor w {\\n\"\n"
"\"upvar #0 $w v\\n\"\n"
"\"catch {after cancel $v(timer)}\\n\"\n"
"\"catch {unset v(timer)}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Copy w {\\n\"\n"
"\"if {![catch {set text [$w get sel.first sel.last]}]} {\\n\"\n"
"\"clipboard clear -displayof $w\\n\"\n"
"\"clipboard append -displayof $w $text\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:canCut w {\\n\"\n"
"\"set r [catch {\\n\"\n"
"\"scan [$w index sel.first] %d.%d s1x s1y\\n\"\n"
"\"scan [$w index sel.last] %d.%d s2x s2y\\n\"\n"
"\"scan [$w index insert] %d.%d ix iy\\n\"\n"
"\"}]\\n\"\n"
"\"if {$r==1} {return 0}\\n\"\n"
"\"if {$s1x==$ix && $s2x==$ix} {return 1}\\n\"\n"
"\"return 2\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Cut w {\\n\"\n"
"\"if {[console:canCut $w]==1} {\\n\"\n"
"\"console:Copy $w\\n\"\n"
"\"$w delete sel.first sel.last\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Paste w {\\n\"\n"
"\"if {[console:canCut $w]==1} {\\n\"\n"
"\"$w delete sel.first sel.last\\n\"\n"
"\"}\\n\"\n"
"\"if {[catch {selection get -displayof $w -selection CLIPBOARD} topaste]} {\\n\"\n"
"\"return\\n\"\n"
"\"}\\n\"\n"
"\"set prior 0\\n\"\n"
"\"foreach line [split $topaste \\\\n] {\\n\"\n"
"\"if {$prior} {\\n\"\n"
"\"console:Enter $w\\n\"\n"
"\"update\\n\"\n"
"\"}\\n\"\n"
"\"set prior 1\\n\"\n"
"\"$w insert insert $line\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:EnableEditMenu w {\\n\"\n"
"\"set m $w.mb.edit.m\\n\"\n"
"\"switch [console:canCut $w.t] {\\n\"\n"
"\"0 {\\n\"\n"
"\"$m entryconf Copy -state disabled\\n\"\n"
"\"$m entryconf Cut -state disabled\\n\"\n"
"\"}\\n\"\n"
"\"1 {\\n\"\n"
"\"$m entryconf Copy -state normal\\n\"\n"
"\"$m entryconf Cut -state normal\\n\"\n"
"\"}\\n\"\n"
"\"2 {\\n\"\n"
"\"$m entryconf Copy -state normal\\n\"\n"
"\"$m entryconf Cut -state disabled\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:SourceFile w {\\n\"\n"
"\"set types {\\n\"\n"
"\"{{TCL Scripts}  {.tcl}}\\n\"\n"
"\"{{All Files}    *}\\n\"\n"
"\"}\\n\"\n"
"\"set f [tk_getOpenFile -filetypes $types -title \\\"TCL Script To Source...\\\"]\\n\"\n"
"\"if {$f!=\\\"\\\"} {\\n\"\n"
"\"uplevel #0 source $f\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:SaveFile w {\\n\"\n"
"\"set types {\\n\"\n"
"\"{{Text Files}  {.txt}}\\n\"\n"
"\"{{All Files}    *}\\n\"\n"
"\"}\\n\"\n"
"\"set f [tk_getSaveFile -filetypes $types -title \\\"Write Screen To...\\\"]\\n\"\n"
"\"if {$f!=\\\"\\\"} {\\n\"\n"
"\"if {[catch {open $f w} fd]} {\\n\"\n"
"\"tk_messageBox -type ok -icon error -message $fd\\n\"\n"
"\"} else {\\n\"\n"
"\"puts $fd [string trimright [$w get 1.0 end] \\\\n]\\n\"\n"
"\"close $fd\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"}\\n\"\n"
"\"proc console:Clear w {\\n\"\n"
"\"$w delete 1.0 {insert linestart}\\n\"\n"
"\"}\\n\"\n"
;
char const zTail2[] = 
";  /* End of the console code */\n"
"#endif /* ET_ENABLE_TK */\n"
"\n"
"/*\n"
"** The \"printf\" code that follows dates from the 1980's.  It is in\n"
"** the public domain.  The original comments are included here for\n"
"** completeness.  They are slightly out-of-date.\n"
"**\n"
"** The following modules is an enhanced replacement for the \"printf\" programs\n"
"** found in the standard library.  The following enhancements are\n"
"** supported:\n"
"**\n"
"**      +  Additional functions.  The standard set of \"printf\" functions\n"
"**         includes printf, fprintf, sprintf, vprintf, vfprintf, and\n"
"**         vsprintf.  This module adds the following:\n"
"**\n"
"**           *  snprintf -- Works like sprintf, but has an extra argument\n"
"**                          which is the size of the buffer written to.\n"
"**\n"
"**           *  mprintf --  Similar to sprintf.  Writes output to memory\n"
"**                          obtained from malloc.\n"
"**\n"
"**           *  xprintf --  Calls a function to dispose of output.\n"
"**\n"
"**           *  nprintf --  No output, but returns the number of characters\n"
"**                          that would have been output by printf.\n"
"**\n"
"**           *  A v- version (ex: vsnprintf) of every function is also\n"
"**              supplied.\n"
"**\n"
"**      +  A few extensions to the formatting notation are supported:\n"
"**\n"
"**           *  The \"=\" flag (similar to \"-\") causes the output to be\n"
"**              be centered in the appropriately sized field.\n"
"**\n"
"**           *  The %b field outputs an integer in binary notation.\n"
"**\n"
"**           *  The %c field now accepts a precision.  The character output\n"
"**              is repeated by the number of times the precision specifies.\n"
"**\n"
"**           *  The %' field works like %c, but takes as its character the\n"
"**              next character of the format string, instead of the next\n"
"**              argument.  For example,  printf(\"%.78'-\")  prints 78 minus\n"
"**              signs, the same as  printf(\"%.78c\",'-').\n"
"**\n"
"**      +  When compiled using GCC on a SPARC, this version of printf is\n"
"**         faster than the library printf for SUN OS 4.1.\n"
"**\n"
"**      +  All functions are fully reentrant.\n"
"**\n"
"*/\n"
"/*\n"
"** Undefine COMPATIBILITY to make some slight changes in the way things\n"
"** work.  I think the changes are an improvement, but they are not\n"
"** backwards compatible.\n"
"*/\n"
"/* #define COMPATIBILITY       / * Compatible with SUN OS 4.1 */\n"
"\n"
"/*\n"
"** Characters that need to be escaped inside a TCL string.\n"
"*/\n"
"static char NeedEsc[] = {\n"
"  1,   1,   1,   1,   1,   1,   1,   1, 'b', 't', 'n',   1, 'f', 'r',   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  0,   0, '\"',   0, '$',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,\n"
"  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,\n"
"  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,\n"
"  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, '[','\\\\', ']',   0,   0,\n"
"  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,\n"
"  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   0,   1,   0,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,\n"
"};\n"
"\n"
"/*\n"
"** Conversion types fall into various categories as defined by the\n"
"** following enumeration.\n"
"*/\n"
"enum et_type {    /* The type of the format field */\n"
"   etRADIX,            /* Integer types.  %d, %x, %o, and so forth */\n"
"   etFLOAT,            /* Floating point.  %f */\n"
"   etEXP,              /* Exponentional notation. %e and %E */\n"
"   etGENERIC,          /* Floating or exponential, depending on exponent. %g */\n"
"   etSIZE,             /* Return number of characters processed so far. %n */\n"
"   etSTRING,           /* Strings. %s */\n"
"   etPERCENT,          /* Percent symbol. %% */\n"
"   etCHARX,            /* Characters. %c */\n"
"   etERROR,            /* Used to indicate no such conversion type */\n"
"/* The rest are extensions, not normally found in printf() */\n"
"   etCHARLIT,          /* Literal characters.  %' */\n"
"   etTCLESCAPE,        /* Strings with special characters escaped.  %q */\n"
"   etMEMSTRING,        /* A string which should be deleted after use. %z */\n"
"   etORDINAL           /* 1st, 2nd, 3rd and so forth */\n"
"};\n"
"\n"
"/*\n"
"** Each builtin conversion character (ex: the 'd' in \"%d\") is described\n"
"** by an instance of the following structure\n"
"*/\n"
"typedef struct et_info {   /* Information about each format field */\n"
"  int  fmttype;              /* The format field code letter */\n"
"  int  base;                 /* The base for radix conversion */\n"
"  char *charset;             /* The character set for conversion */\n"
"  int  flag_signed;          /* Is the quantity signed? */\n"
"  char *prefix;              /* Prefix on non-zero values in alt format */\n"
"  enum et_type type;          /* Conversion paradigm */\n"
"} et_info;\n"
"\n"
"/*\n"
"** The following table is searched linearly, so it is good to put the\n"
"** most frequently used conversion types first.\n"
"*/\n"
"static et_info fmtinfo[] = {\n"
"  { 'd',  10,  \"0123456789\",       1,    0, etRADIX,      },\n"
"  { 's',   0,  0,                  0,    0, etSTRING,     }, \n"
"  { 'q',   0,  0,                  0,    0, etTCLESCAPE,  },\n"
"  { 'z',   0,  0,                  0,    0, etMEMSTRING, },\n"
"  { 'c',   0,  0,                  0,    0, etCHARX,      },\n"
"  { 'o',   8,  \"01234567\",         0,  \"0\", etRADIX,      },\n"
"  { 'u',  10,  \"0123456789\",       0,    0, etRADIX,      },\n"
"  { 'x',  16,  \"0123456789abcdef\", 0, \"x0\", etRADIX,      },\n"
"  { 'X',  16,  \"0123456789ABCDEF\", 0, \"X0\", etRADIX,      },\n"
"  { 'r',  10,  \"0123456789\",       0,    0, etORDINAL,    },\n"
"  { 'f',   0,  0,                  1,    0, etFLOAT,      },\n"
"  { 'e',   0,  \"e\",                1,    0, etEXP,        },\n"
"  { 'E',   0,  \"E\",                1,    0, etEXP,        },\n"
"  { 'g',   0,  \"e\",                1,    0, etGENERIC,    },\n"
"  { 'G',   0,  \"E\",                1,    0, etGENERIC,    },\n"
"  { 'i',  10,  \"0123456789\",       1,    0, etRADIX,      },\n"
"  { 'n',   0,  0,                  0,    0, etSIZE,       },\n"
"  { '%',   0,  0,                  0,    0, etPERCENT,    },\n"
"  { 'b',   2,  \"01\",               0, \"b0\", etRADIX,      }, /* Binary */\n"
"  { 'p',  10,  \"0123456789\",       0,    0, etRADIX,      }, /* Pointers */\n"
"  { '\\'',  0,  0,                  0,    0, etCHARLIT,    }, /* Literal char */\n"
"};\n"
"#define etNINFO  (sizeof(fmtinfo)/sizeof(fmtinfo[0]))\n"
"\n"
"/*\n"
"** If NOFLOATINGPOINT is defined, then none of the floating point\n"
"** conversions will work.\n"
"*/\n"
"#ifndef etNOFLOATINGPOINT\n"
"/*\n"
"** \"*val\" is a double such that 0.1 <= *val < 10.0\n"
"** Return the ascii code for the leading digit of *val, then\n"
"** multiply \"*val\" by 10.0 to renormalize.\n"
"**\n"
"** Example:\n"
"**     input:     *val = 3.14159\n"
"**     output:    *val = 1.4159    function return = '3'\n"
"**\n"
"** The counter *cnt is incremented each time.  After counter exceeds\n"
"** 16 (the number of significant digits in a 64-bit float) '0' is\n"
"** always returned.\n"
"*/\n"
"static int et_getdigit(double *val, int *cnt){\n"
"  int digit;\n"
"  double d;\n"
"  if( (*cnt)++ >= 16 ) return '0';\n"
"  digit = (int)*val;\n"
"  d = digit;\n"
"  digit += '0';\n"
"  *val = (*val - d)*10.0;\n"
"  return digit;\n"
"}\n"
"#endif\n"
"\n"
"#define etBUFSIZE 1000  /* Size of the output buffer */\n"
"\n"
"/*\n"
"** The root program.  All variations call this core.\n"
"**\n"
"** INPUTS:\n"
"**   func   This is a pointer to a function taking three arguments\n"
"**            1. A pointer to anything.  Same as the \"arg\" parameter.\n"
"**            2. A pointer to the list of characters to be output\n"
"**               (Note, this list is NOT null terminated.)\n"
"**            3. An integer number of characters to be output.\n"
"**               (Note: This number might be zero.)\n"
"**\n"
"**   arg    This is the pointer to anything which will be passed as the\n"
"**          first argument to \"func\".  Use it for whatever you like.\n"
"**\n"
"**   fmt    This is the format string, as in the usual print.\n"
"**\n"
"**   ap     This is a pointer to a list of arguments.  Same as in\n"
"**          vfprint.\n"
"**\n"
"** OUTPUTS:\n"
"**          The return value is the total number of characters sent to\n"
"**          the function \"func\".  Returns -1 on a error.\n"
"**\n"
"** Note that the order in which automatic variables are declared below\n"
"** seems to make a big difference in determining how fast this beast\n"
"** will run.\n"
"*/\n"
"int vxprintf(\n"
"  void (*func)(void*,char*,int),\n"
"  void *arg,\n"
"  const char *format,\n"
"  va_list ap\n"
"){\n"
"  register const char *fmt; /* The format string. */\n"
"  register int c;           /* Next character in the format string */\n"
"  register char *bufpt;     /* Pointer to the conversion buffer */\n"
"  register int  precision;  /* Precision of the current field */\n"
"  register int  length;     /* Length of the field */\n"
"  register int  idx;        /* A general purpose loop counter */\n"
"  int count;                /* Total number of characters output */\n"
"  int width;                /* Width of the current field */\n"
"  int flag_leftjustify;     /* True if \"-\" flag is present */\n"
"  int flag_plussign;        /* True if \"+\" flag is present */\n"
"  int flag_blanksign;       /* True if \" \" flag is present */\n"
"  int flag_alternateform;   /* True if \"#\" flag is present */\n"
"  int flag_zeropad;         /* True if field width constant starts with zero */\n"
"  int flag_long;            /* True if \"l\" flag is present */\n"
"  int flag_center;          /* True if \"=\" flag is present */\n"
"  unsigned long longvalue;  /* Value for integer types */\n"
"  double realvalue;         /* Value for real types */\n"
"  et_info *infop;           /* Pointer to the appropriate info structure */\n"
"  char buf[etBUFSIZE];      /* Conversion buffer */\n"
"  char prefix;              /* Prefix character.  \"+\" or \"-\" or \" \" or '\\0'. */\n"
"  int  errorflag = 0;       /* True if an error is encountered */\n"
"  enum et_type xtype;       /* Conversion paradigm */\n"
"  char *zMem;               /* String to be freed */\n"
"  char *zExtra;             /* Extra memory used for etTCLESCAPE conversions */\n"
"  static char spaces[] = \"                                                  \"\n"
"     \"                                                                      \";\n"
"#define etSPACESIZE (sizeof(spaces)-1)\n"
"#ifndef etNOFLOATINGPOINT\n"
"  int  exp;                 /* exponent of real numbers */\n"
"  double rounder;           /* Used for rounding floating point values */\n"
"  int flag_dp;              /* True if decimal point should be shown */\n"
"  int flag_rtz;             /* True if trailing zeros should be removed */\n"
"  int flag_exp;             /* True to force display of the exponent */\n"
"  int nsd;                  /* Number of significant digits returned */\n"
"#endif\n"
"\n"
"  fmt = format;                     /* Put in a register for speed */\n"
"  count = length = 0;\n"
"  bufpt = 0;\n"
"  for(; (c=(*fmt))!=0; ++fmt){\n"
"    if( c!='%' ){\n"
"      register int amt;\n"
"      bufpt = (char *)fmt;\n"
"      amt = 1;\n"
"      while( (c=(*++fmt))!='%' && c!=0 ) amt++;\n"
"      (*func)(arg,bufpt,amt);\n"
"      count += amt;\n"
"      if( c==0 ) break;\n"
"    }\n"
"    if( (c=(*++fmt))==0 ){\n"
"      errorflag = 1;\n"
"      (*func)(arg,\"%\",1);\n"
"      count++;\n"
"      break;\n"
"    }\n"
"    /* Find out what flags are present */\n"
"    flag_leftjustify = flag_plussign = flag_blanksign = \n"
"     flag_alternateform = flag_zeropad = flag_center = 0;\n"
"    do{\n"
"      switch( c ){\n"
"        case '-':   flag_leftjustify = 1;     c = 0;   break;\n"
"        case '+':   flag_plussign = 1;        c = 0;   break;\n"
"        case ' ':   flag_blanksign = 1;       c = 0;   break;\n"
"        case '#':   flag_alternateform = 1;   c = 0;   break;\n"
"        case '0':   flag_zeropad = 1;         c = 0;   break;\n"
"        case '=':   flag_center = 1;          c = 0;   break;\n"
"        default:                                       break;\n"
"      }\n"
"    }while( c==0 && (c=(*++fmt))!=0 );\n"
"    if( flag_center ) flag_leftjustify = 0;\n"
"    /* Get the field width */\n"
"    width = 0;\n"
"    if( c=='*' ){\n"
"      width = va_arg(ap,int);\n"
"      if( width<0 ){\n"
"        flag_leftjustify = 1;\n"
"        width = -width;\n"
"      }\n"
"      c = *++fmt;\n"
"    }else{\n"
"      while( isdigit(c) ){\n"
"        width = width*10 + c - '0';\n"
"        c = *++fmt;\n"
"      }\n"
"    }\n"
"    if( width > etBUFSIZE-10 ){\n"
"      width = etBUFSIZE-10;\n"
"    }\n"
"    /* Get the precision */\n"
"    if( c=='.' ){\n"
"      precision = 0;\n"
"      c = *++fmt;\n"
"      if( c=='*' ){\n"
"        precision = va_arg(ap,int);\n"
"#ifndef etCOMPATIBILITY\n"
"        /* This is sensible, but SUN OS 4.1 doesn't do it. */\n"
"        if( precision<0 ) precision = -precision;\n"
"#endif\n"
"        c = *++fmt;\n"
"      }else{\n"
"        while( isdigit(c) ){\n"
"          precision = precision*10 + c - '0';\n"
"          c = *++fmt;\n"
"        }\n"
"      }\n"
"      /* Limit the precision to prevent overflowing buf[] during conversion */\n"
"      if( precision>etBUFSIZE-40 ) precision = etBUFSIZE-40;\n"
"    }else{\n"
"      precision = -1;\n"
"    }\n"
"    /* Get the conversion type modifier */\n"
"    if( c=='l' ){\n"
"      flag_long = 1;\n"
"      c = *++fmt;\n"
"    }else{\n"
"      flag_long = 0;\n"
"    }\n"
"    /* Fetch the info entry for the field */\n"
"    infop = 0;\n"
"    for(idx=0; idx<etNINFO; idx++){\n"
"      if( c==fmtinfo[idx].fmttype ){\n"
"        infop = &fmtinfo[idx];\n"
"        break;\n"
"      }\n"
"    }\n"
"    /* No info entry found.  It must be an error. */\n"
"    if( infop==0 ){\n"
"      xtype = etERROR;\n"
"    }else{\n"
"      xtype = infop->type;\n"
"    }\n"
"    zExtra = 0;\n"
"\n"
"    /*\n"
"    ** At this point, variables are initialized as follows:\n"
"    **\n"
"    **   flag_alternateform          TRUE if a '#' is present.\n"
"    **   flag_plussign               TRUE if a '+' is present.\n"
"    **   flag_leftjustify            TRUE if a '-' is present or if the\n"
"    **                               field width was negative.\n"
"    **   flag_zeropad                TRUE if the width began with 0.\n"
"    **   flag_long                   TRUE if the letter 'l' (ell) prefixed\n"
"    **                               the conversion character.\n"
"    **   flag_blanksign              TRUE if a ' ' is present.\n"
"    **   width                       The specified field width.  This is\n"
"    **                               always non-negative.  Zero is the default.\n"
"    **   precision                   The specified precision.  The default\n"
"    **                               is -1.\n"
"    **   xtype                       The class of the conversion.\n"
"    **   infop                       Pointer to the appropriate info struct.\n"
"    */\n"
"    switch( xtype ){\n"
"      case etORDINAL:\n"
"      case etRADIX:\n"
"        if( flag_long )  longvalue = va_arg(ap,long);\n"
"	else             longvalue = va_arg(ap,int);\n"
"#ifdef etCOMPATIBILITY\n"
"        /* For the format %#x, the value zero is printed \"0\" not \"0x0\".\n"
"        ** I think this is stupid. */\n"
"        if( longvalue==0 ) flag_alternateform = 0;\n"
"#else\n"
"        /* More sensible: turn off the prefix for octal (to prevent \"00\"),\n"
"        ** but leave the prefix for hex. */\n"
"        if( longvalue==0 && infop->base==8 ) flag_alternateform = 0;\n"
"#endif\n"
"        if( infop->flag_signed ){\n"
"          if( *(long*)&longvalue<0 ){\n"
"            longvalue = -*(long*)&longvalue;\n"
"            prefix = '-';\n"
"          }else if( flag_plussign )  prefix = '+';\n"
"          else if( flag_blanksign )  prefix = ' ';\n"
"          else                       prefix = 0;\n"
"        }else                        prefix = 0;\n"
"        if( flag_zeropad && precision<width-(prefix!=0) ){\n"
"          precision = width-(prefix!=0);\n"
"	}\n"
"        bufpt = &buf[etBUFSIZE];\n"
"        if( xtype==etORDINAL ){\n"
"          long a,b;\n"
"          a = longvalue%10;\n"
"          b = longvalue%100;\n"
"          bufpt -= 2;\n"
"          if( a==0 || a>3 || (b>10 && b<14) ){\n"
"            bufpt[0] = 't';\n"
"            bufpt[1] = 'h';\n"
"          }else if( a==1 ){\n"
"            bufpt[0] = 's';\n"
"            bufpt[1] = 't';\n"
"          }else if( a==2 ){\n"
"            bufpt[0] = 'n';\n"
"            bufpt[1] = 'd';\n"
"          }else if( a==3 ){\n"
"            bufpt[0] = 'r';\n"
"            bufpt[1] = 'd';\n"
"          }\n"
"        }\n"
"        {\n"
"          register char *cset;      /* Use registers for speed */\n"
"          register int base;\n"
"          cset = infop->charset;\n"
"          base = infop->base;\n"
"          do{                                           /* Convert to ascii */\n"
"            *(--bufpt) = cset[longvalue%base];\n"
"            longvalue = longvalue/base;\n"
"          }while( longvalue>0 );\n"
"	}\n"
"        length = (long)&buf[etBUFSIZE]-(long)bufpt;\n"
"        for(idx=precision-length; idx>0; idx--){\n"
"          *(--bufpt) = '0';                             /* Zero pad */\n"
"	}\n"
"        if( prefix ) *(--bufpt) = prefix;               /* Add sign */\n"
"        if( flag_alternateform && infop->prefix ){      /* Add \"0\" or \"0x\" */\n"
"          char *pre, x;\n"
"          pre = infop->prefix;\n"
"          if( *bufpt!=pre[0] ){\n"
"            for(pre=infop->prefix; (x=(*pre))!=0; pre++) *(--bufpt) = x;\n"
"	  }\n"
"        }\n"
"        length = (long)&buf[etBUFSIZE]-(long)bufpt;\n"
"        break;\n"
"      case etFLOAT:\n"
"      case etEXP:\n"
"      case etGENERIC:\n"
"        realvalue = va_arg(ap,double);\n"
"#ifndef etNOFLOATINGPOINT\n"
"        if( precision<0 ) precision = 6;         /* Set default precision */\n"
"        if( precision>etBUFSIZE-10 ) precision = etBUFSIZE-10;\n"
"        if( realvalue<0.0 ){\n"
"          realvalue = -realvalue;\n"
"          prefix = '-';\n"
"	}else{\n"
"          if( flag_plussign )          prefix = '+';\n"
"          else if( flag_blanksign )    prefix = ' ';\n"
"          else                         prefix = 0;\n"
"	}\n"
"        if( infop->type==etGENERIC && precision>0 ) precision--;\n"
"        rounder = 0.0;\n"
"#ifdef COMPATIBILITY\n"
"        /* Rounding works like BSD when the constant 0.4999 is used.  Wierd! */\n"
"        for(idx=precision, rounder=0.4999; idx>0; idx--, rounder*=0.1);\n"
"#else\n"
"        /* It makes more sense to use 0.5 */\n"
"        for(idx=precision, rounder=0.5; idx>0; idx--, rounder*=0.1);\n"
"#endif\n"
"        if( infop->type==etFLOAT ) realvalue += rounder;\n"
"        /* Normalize realvalue to within 10.0 > realvalue >= 1.0 */\n"
"        exp = 0;\n"
"        if( realvalue>0.0 ){\n"
"          int k = 0;\n"
"          while( realvalue>=1e8 && k++<100 ){ realvalue *= 1e-8; exp+=8; }\n"
"          while( realvalue>=10.0 && k++<100 ){ realvalue *= 0.1; exp++; }\n"
"          while( realvalue<1e-8 && k++<100 ){ realvalue *= 1e8; exp-=8; }\n"
"          while( realvalue<1.0 && k++<100 ){ realvalue *= 10.0; exp--; }\n"
"          if( k>=100 ){\n"
"            bufpt = \"NaN\";\n"
"            length = 3;\n"
"            break;\n"
"          }\n"
"	}\n"
"        bufpt = buf;\n"
"        /*\n"
"        ** If the field type is etGENERIC, then convert to either etEXP\n"
"        ** or etFLOAT, as appropriate.\n"
"        */\n"
"        flag_exp = xtype==etEXP;\n"
"        if( xtype!=etFLOAT ){\n"
"          realvalue += rounder;\n"
"          if( realvalue>=10.0 ){ realvalue *= 0.1; exp++; }\n"
"        }\n"
"        if( xtype==etGENERIC ){\n"
"          flag_rtz = !flag_alternateform;\n"
"          if( exp<-4 || exp>precision ){\n"
"            xtype = etEXP;\n"
"          }else{\n"
"            precision = precision - exp;\n"
"            xtype = etFLOAT;\n"
"          }\n"
"	}else{\n"
"          flag_rtz = 0;\n"
"	}\n"
"        /*\n"
"        ** The \"exp+precision\" test causes output to be of type etEXP if\n"
"        ** the precision is too large to fit in buf[].\n"
"        */\n"
"        nsd = 0;\n"
"        if( xtype==etFLOAT && exp+precision<etBUFSIZE-30 ){\n"
"          flag_dp = (precision>0 || flag_alternateform);\n"
"          if( prefix ) *(bufpt++) = prefix;         /* Sign */\n"
"          if( exp<0 )  *(bufpt++) = '0';            /* Digits before \".\" */\n"
"          else for(; exp>=0; exp--) *(bufpt++) = et_getdigit(&realvalue,&nsd);\n"
"          if( flag_dp ) *(bufpt++) = '.';           /* The decimal point */\n"
"          for(exp++; exp<0 && precision>0; precision--, exp++){\n"
"            *(bufpt++) = '0';\n"
"          }\n"
"          while( (precision--)>0 ) *(bufpt++) = et_getdigit(&realvalue,&nsd);\n"
"          *(bufpt--) = 0;                           /* Null terminate */\n"
"          if( flag_rtz && flag_dp ){     /* Remove trailing zeros and \".\" */\n"
"            while( bufpt>=buf && *bufpt=='0' ) *(bufpt--) = 0;\n"
"            if( bufpt>=buf && *bufpt=='.' ) *(bufpt--) = 0;\n"
"          }\n"
"          bufpt++;                            /* point to next free slot */\n"
"	}else{    /* etEXP or etGENERIC */\n"
"          flag_dp = (precision>0 || flag_alternateform);\n"
"          if( prefix ) *(bufpt++) = prefix;   /* Sign */\n"
"          *(bufpt++) = et_getdigit(&realvalue,&nsd);  /* First digit */\n"
"          if( flag_dp ) *(bufpt++) = '.';     /* Decimal point */\n"
"          while( (precision--)>0 ) *(bufpt++) = et_getdigit(&realvalue,&nsd);\n"
"          bufpt--;                            /* point to last digit */\n"
"          if( flag_rtz && flag_dp ){          /* Remove tail zeros */\n"
"            while( bufpt>=buf && *bufpt=='0' ) *(bufpt--) = 0;\n"
"            if( bufpt>=buf && *bufpt=='.' ) *(bufpt--) = 0;\n"
"          }\n"
"          bufpt++;                            /* point to next free slot */\n"
"          if( exp || flag_exp ){\n"
"            *(bufpt++) = infop->charset[0];\n"
"            if( exp<0 ){ *(bufpt++) = '-'; exp = -exp; } /* sign of exp */\n"
"            else       { *(bufpt++) = '+'; }\n"
"            if( exp>=100 ){\n"
"              *(bufpt++) = (exp/100)+'0';                /* 100's digit */\n"
"              exp %= 100;\n"
"  	    }\n"
"            *(bufpt++) = exp/10+'0';                     /* 10's digit */\n"
"            *(bufpt++) = exp%10+'0';                     /* 1's digit */\n"
"          }\n"
"	}\n"
"        /* The converted number is in buf[] and zero terminated. Output it.\n"
"        ** Note that the number is in the usual order, not reversed as with\n"
"        ** integer conversions. */\n"
"        length = (long)bufpt-(long)buf;\n"
"        bufpt = buf;\n"
"\n"
"        /* Special case:  Add leading zeros if the flag_zeropad flag is\n"
"        ** set and we are not left justified */\n"
"        if( flag_zeropad && !flag_leftjustify && length < width){\n"
"          int i;\n"
"          int nPad = width - length;\n"
"          for(i=width; i>=nPad; i--){\n"
"            bufpt[i] = bufpt[i-nPad];\n"
"          }\n"
"          i = prefix!=0;\n"
"          while( nPad-- ) bufpt[i++] = '0';\n"
"          length = width;\n"
"        }\n"
"#endif\n"
"        break;\n"
"      case etSIZE:\n"
"        *(va_arg(ap,int*)) = count;\n"
"        length = width = 0;\n"
"        break;\n"
"      case etPERCENT:\n"
"        buf[0] = '%';\n"
"        bufpt = buf;\n"
"        length = 1;\n"
"        break;\n"
"      case etCHARLIT:\n"
"      case etCHARX:\n"
"        c = buf[0] = (xtype==etCHARX ? va_arg(ap,int) : *++fmt);\n"
"        if( precision>=0 ){\n"
"          for(idx=1; idx<precision; idx++) buf[idx] = c;\n"
"          length = precision;\n"
"	}else{\n"
"          length =1;\n"
"	}\n"
"        bufpt = buf;\n"
"        break;\n"
"      case etSTRING:\n"
"      case etMEMSTRING:\n"
"        zMem = bufpt = va_arg(ap,char*);\n"
"        if( bufpt==0 ) bufpt = \"(null)\";\n"
"        length = strlen(bufpt);\n"
"        if( precision>=0 && precision<length ) length = precision;\n"
"        break;\n"
"      case etTCLESCAPE:\n"
"        {\n"
"          int i, j, n, c, k;\n"
"          char *arg = va_arg(ap,char*);\n"
"          if( arg==0 ) arg = \"(NULL)\";\n"
"          for(i=n=0; (c=arg[i])!=0; i++){\n"
"            k = NeedEsc[c&0xff];\n"
"            if( k==0 ){\n"
"              n++;\n"
"            }else if( k==1 ){\n"
"              n+=4;\n"
"            }else{\n"
"              n+=2;\n"
"            }\n"
"          }\n"
"          n++;\n"
"          if( n>etBUFSIZE ){\n"
"            bufpt = zExtra = Tcl_Alloc( n );\n"
"          }else{\n"
"            bufpt = buf;\n"
"          }\n"
"          for(i=j=0; (c=arg[i])!=0; i++){\n"
"            k = NeedEsc[c&0xff];\n"
"            if( k==0 ){\n"
"              bufpt[j++] = c;\n"
"            }else if( k==1 ){\n"
"              bufpt[j++] = '\\\\';\n"
"              bufpt[j++] = ((c>>6) & 3) + '0';\n"
"              bufpt[j++] = ((c>>3) & 7) + '0';\n"
"              bufpt[j++] = (c & 7) + '0';\n"
"            }else{\n"
"              bufpt[j++] = '\\\\';\n"
"              bufpt[j++] = k;\n"
"            }\n"
"          }\n"
"          bufpt[j] = 0;\n"
"          length = j;\n"
"          if( precision>=0 && precision<length ) length = precision;\n"
"        }\n"
"        break;\n"
"      case etERROR:\n"
"        buf[0] = '%';\n"
"        buf[1] = c;\n"
"        errorflag = 0;\n"
"        idx = 1+(c!=0);\n"
"        (*func)(arg,\"%\",idx);\n"
"        count += idx;\n"
"        if( c==0 ) fmt--;\n"
"        break;\n"
"    }/* End switch over the format type */\n"
"    /*\n"
"    ** The text of the conversion is pointed to by \"bufpt\" and is\n"
"    ** \"length\" characters long.  The field width is \"width\".  Do\n"
"    ** the output.\n"
"    */\n"
"    if( !flag_leftjustify ){\n"
"      register int nspace;\n"
"      nspace = width-length;\n"
"      if( nspace>0 ){\n"
"        if( flag_center ){\n"
"          nspace = nspace/2;\n"
"          width -= nspace;\n"
"          flag_leftjustify = 1;\n"
"	}\n"
"        count += nspace;\n"
"        while( nspace>=etSPACESIZE ){\n"
"          (*func)(arg,spaces,etSPACESIZE);\n"
"          nspace -= etSPACESIZE;\n"
"        }\n"
"        if( nspace>0 ) (*func)(arg,spaces,nspace);\n"
"      }\n"
"    }\n"
"    if( length>0 ){\n"
"      (*func)(arg,bufpt,length);\n"
"      count += length;\n"
"    }\n"
"    if( xtype==etMEMSTRING && zMem ){\n"
"      Tcl_Free(zMem);\n"
"    }\n"
"    if( flag_leftjustify ){\n"
"      register int nspace;\n"
"      nspace = width-length;\n"
"      if( nspace>0 ){\n"
"        count += nspace;\n"
"        while( nspace>=etSPACESIZE ){\n"
"          (*func)(arg,spaces,etSPACESIZE);\n"
"          nspace -= etSPACESIZE;\n"
"        }\n"
"        if( nspace>0 ) (*func)(arg,spaces,nspace);\n"
"      }\n"
"    }\n"
"    if( zExtra ){\n"
"      Tcl_Free(zExtra);\n"
"    }\n"
"  }/* End for loop over the format string */\n"
"  return errorflag ? -1 : count;\n"
"} /* End of function */\n"
"\n"
"/*\n"
"** The following section of code handles the mprintf routine, that\n"
"** writes to memory obtained from malloc().\n"
"*/\n"
"\n"
"/* This structure is used to store state information about the\n"
"** write to memory that is currently in progress.\n"
"*/\n"
"struct sgMprintf {\n"
"  char *zBase;     /* A base allocation */\n"
"  char *zText;     /* The string collected so far */\n"
"  int  nChar;      /* Length of the string so far */\n"
"  int  nAlloc;     /* Amount of space allocated in zText */\n"
"};\n"
"\n"
"/* \n"
"** The xprintf callback function. \n"
"**\n"
"** This routine add nNewChar characters of text in zNewText to\n"
"** the sgMprintf structure pointed to by \"arg\".\n"
"*/\n"
"static void mout(void *arg, char *zNewText, int nNewChar){\n"
"  struct sgMprintf *pM = (struct sgMprintf*)arg;\n"
"  if( pM->nChar + nNewChar + 1 > pM->nAlloc ){\n"
"    pM->nAlloc = pM->nChar + nNewChar*2 + 1;\n"
"    if( pM->zText==pM->zBase ){\n"
"      pM->zText = Tcl_Alloc(pM->nAlloc);\n"
"      if( pM->zText && pM->nChar ) memcpy(pM->zText,pM->zBase,pM->nChar);\n"
"    }else{\n"
"      pM->zText = Tcl_Realloc(pM->zText, pM->nAlloc);\n"
"    }\n"
"  }\n"
"  if( pM->zText ){\n"
"    memcpy(&pM->zText[pM->nChar], zNewText, nNewChar);\n"
"    pM->nChar += nNewChar;\n"
"    pM->zText[pM->nChar] = 0;\n"
"  }\n"
"}\n"
"\n"
"/*\n"
"** mprintf() works like printf(), but allocations memory to hold the\n"
"** resulting string and returns a pointer to the allocated memory.\n"
"*/\n"
"char *mprintf(const char *zFormat, ...){\n"
"  va_list ap;\n"
"  struct sgMprintf sMprintf;\n"
"  char *zNew;\n"
"  char zBuf[200];\n"
"\n"
"  sMprintf.nChar = 0;\n"
"  sMprintf.nAlloc = sizeof(zBuf);\n"
"  sMprintf.zText = zBuf;\n"
"  sMprintf.zBase = zBuf;\n"
"  va_start(ap,zFormat);\n"
"  vxprintf(mout,&sMprintf,zFormat,ap);\n"
"  va_end(ap);\n"
"  sMprintf.zText[sMprintf.nChar] = 0;\n"
"  if( sMprintf.zText==sMprintf.zBase ){\n"
"    zNew = Tcl_Alloc( sMprintf.nChar+1 );\n"
"    if( zNew ) strcpy(zNew,zBuf);\n"
"  }else{\n"
"    zNew = Tcl_Realloc(sMprintf.zText,sMprintf.nChar+1);\n"
"  }\n"
"  return zNew;\n"
"}\n"
"\n"
"/* This is the varargs version of mprintf.  \n"
"*/\n"
"char *vmprintf(const char *zFormat, va_list ap){\n"
"  struct sgMprintf sMprintf;\n"
"  char zBuf[200];\n"
"  sMprintf.nChar = 0;\n"
"  sMprintf.zText = zBuf;\n"
"  sMprintf.nAlloc = sizeof(zBuf);\n"
"  sMprintf.zBase = zBuf;\n"
"  vxprintf(mout,&sMprintf,zFormat,ap);\n"
"  sMprintf.zText[sMprintf.nChar] = 0;\n"
"  if( sMprintf.zText==sMprintf.zBase ){\n"
"    sMprintf.zText = Tcl_Alloc( strlen(zBuf)+1 );\n"
"    if( sMprintf.zText ) strcpy(sMprintf.zText,zBuf);\n"
"  }else{\n"
"    sMprintf.zText = Tcl_Realloc(sMprintf.zText,sMprintf.nChar+1);\n"
"  }\n"
"  return sMprintf.zText;\n"
"}\n"
"\n"
"/*\n"
"** Add text output to a Tcl_DString.\n"
"**\n"
"** This routine is called by vxprintf().  It's job is to add\n"
"** nNewChar characters of text from zNewText to the Tcl_DString\n"
"** that \"arg\" is pointing to.\n"
"*/\n"
"static void dstringout(void *arg, char *zNewText, int nNewChar){\n"
"  Tcl_DString *str = (Tcl_DString*)arg;\n"
"  Tcl_DStringAppend(str,zNewText,nNewChar);\n"
"}\n"
"\n"
"/*\n"
"** Append formatted output to a DString.\n"
"*/\n"
"char *Et_DStringAppendF(Tcl_DString *str, const char *zFormat, ...){\n"
"  va_list ap;\n"
"  va_start(ap,zFormat);\n"
"  vxprintf(dstringout,str,zFormat,ap);\n"
"  va_end(ap);\n"
"  return Tcl_DStringValue(str);\n"
"}\n"
"\n"
"/*\n"
"** Make this variable true to trace all calls to EvalF\n"
"*/\n"
"int Et_EvalTrace = 0;\n"
"\n"
"/*\n"
"** Eval the results of a string.\n"
"*/\n"
"int Et_EvalF(Tcl_Interp *interp, const char *zFormat, ...){\n"
"  char *zCmd;\n"
"  va_list ap;\n"
"  int result;\n"
"  va_start(ap,zFormat);\n"
"  zCmd = vmprintf(zFormat,ap);\n"
"  if( Et_EvalTrace ) printf(\"%s\\n\",zCmd);\n"
"  result = Tcl_Eval(interp,zCmd);\n"
"  if( Et_EvalTrace ) printf(\"%d %s\\n\",result,Tcl_GetStringResult(interp));\n"
"  Tcl_Free(zCmd);\n"
"  return result;\n"
"}\n"
"int Et_GlobalEvalF(Tcl_Interp *interp, const char *zFormat, ...){\n"
"  char *zCmd;\n"
"  va_list ap;\n"
"  int result;\n"
"  va_start(ap,zFormat);\n"
"  zCmd = vmprintf(zFormat,ap);\n"
"  if( Et_EvalTrace ) printf(\"%s\\n\",zCmd);\n"
"  result = Tcl_GlobalEval(interp,zCmd);\n"
"  if( Et_EvalTrace ) printf(\"%d %s\\n\",result,Tcl_GetStringResult(interp));\n"
"  Tcl_Free(zCmd);\n"
"  return result;\n"
"}\n"
"\n"
"/*\n"
"** Set the result of an interpreter using printf-like arguments.\n"
"*/\n"
"void Et_ResultF(Tcl_Interp *interp, const char *zFormat, ...){\n"
"  Tcl_DString str;\n"
"  va_list ap;\n"
"\n"
"  Tcl_DStringInit(&str);\n"
"  va_start(ap,zFormat);\n"
"  vxprintf(dstringout,&str,zFormat,ap);\n"
"  va_end(ap);\n"
"  Tcl_DStringResult(interp,&str);  \n"
"}\n"
"\n"
"#if ET_HAVE_OBJ\n"
"/*\n"
"** Append text to a string object.\n"
"*/\n"
"int Et_AppendObjF(Tcl_Obj *pObj, const char *zFormat, ...){\n"
"  va_list ap;\n"
"  int rc;\n"
"\n"
"  va_start(ap,zFormat);\n"
"  rc = vxprintf((void(*)(void*,char*,int))Tcl_AppendToObj, pObj, zFormat, ap);\n"
"  va_end(ap);\n"
"  return rc;\n"
"}\n"
"#endif\n"
"\n"
"\n"
"#if ET_WIN32\n"
"/*\n"
"** This array translates all characters into themselves.  Except\n"
"** for the \\ which gets translated into /.  And all upper-case\n"
"** characters are translated into lower case.  This is used for\n"
"** hashing and comparing filenames, to work around the Windows\n"
"** bug of ignoring filename case and using the wrong separator\n"
"** character for directories.\n"
"**\n"
"** The array is initialized by FilenameHashInit().\n"
"**\n"
"** We also define a macro ET_TRANS() that actually does\n"
"** the character translation.  ET_TRANS() is a no-op under\n"
"** unix.\n"
"*/\n"
"static char charTrans[256];\n"
"#define ET_TRANS(X) (charTrans[0xff&(int)(X)])\n"
"#else\n"
"#define ET_TRANS(X) (X)\n"
"#endif\n"
"\n"
"/*\n"
"** Hash a filename.  The value returned is appropriate for\n"
"** indexing into the Et_FileHashTable[] array.\n"
"*/\n"
"static int FilenameHash(char *zName){\n"
"  int h = 0;\n"
"  while( *zName ){\n"
"    h = h ^ (h<<5) ^ ET_TRANS(*(zName++));\n"
"  }\n"
"  if( h<0 ) h = -h;\n"
"  return h % (sizeof(Et_FileHashTable)/sizeof(Et_FileHashTable[0]));\n"
"}\n"
"\n"
"/*\n"
"** Compare two filenames.  Return 0 if they are the same and\n"
"** non-zero if they are different.\n"
"*/\n"
"static int FilenameCmp(char *z1, char *z2){\n"
"  int diff;\n"
"  while( (diff = ET_TRANS(*z1)-ET_TRANS(*z2))==0 && *z1!=0){\n"
"    z1++;\n"
"    z2++;\n"
"  }\n"
"  return diff;\n"
"}\n"
"\n"
"/*\n"
"** Initialize the file hash table\n"
"*/\n"
"static void FilenameHashInit(void){\n"
"  int i;\n"
"#if ET_WIN32\n"
"  for(i=0; i<sizeof(charTrans); i++){\n"
"    charTrans[i] = i;\n"
"  }\n"
"  for(i='A'; i<='Z'; i++){\n"
"    charTrans[i] = i + 'a' - 'A';\n"
"  }\n"
"  charTrans['\\\\'] = '/';\n"
"#endif\n"
"  for(i=0; i<sizeof(Et_FileSet)/sizeof(Et_FileSet[0]) - 1; i++){\n"
"    struct EtFile *p;\n"
"    int h;\n"
"    p = &Et_FileSet[i];\n"
"    h = FilenameHash(p->zName);\n"
"    p->pNext = Et_FileHashTable[h];\n"
"    Et_FileHashTable[h] = p;\n"
"  }\n"
"}\n"
"\n"
"/*\n"
"** Locate the text of a built-in file given its name.  \n"
"** Return 0 if not found.  Return this size of the file (not\n"
"** counting the null-terminator) in *pSize if pSize!=NULL.\n"
"**\n"
"** If deshroud==1 and the file is shrouded, then descramble\n"
"** the text.\n"
"*/\n"
"static char *FindBuiltinFile(char *zName, int deshroud, int *pSize){\n"
"  int h;\n"
"  struct EtFile *p;\n"
"\n"
"  h = FilenameHash(zName);\n"
"  p = Et_FileHashTable[h];\n"
"  while( p && FilenameCmp(p->zName,zName)!=0 ){ p = p->pNext; }\n"
"#if ET_SHROUD_KEY>0\n"
"  if( p && p->shrouded && deshroud ){\n"
"    char *z;\n"
"    int xor = ET_SHROUD_KEY;\n"
"    for(z=p->zData; *z; z++){\n"
"      if( *z>=0x20 ){ *z ^= xor; xor = (xor+1)&0x1f; }\n"
"    }\n"
"    p->shrouded = 0;\n"
"  }\n"
"#endif\n"
"  if( p && pSize ){\n"
"    *pSize = p->nData;\n"
"  }\n"
"  return p ? p->zData : 0;\n"
"}\n"
"\n"
"/*\n"
"** Add a new file to the list of built-in files.\n"
"**\n"
"** This routine makes a copy of zFilename.  But it does NOT make\n"
"** a copy of zData.  It just holds a pointer to zData and uses\n"
"** that for all file access.  So after calling this routine,\n"
"** you should never change zData!\n"
"*/\n"
"void Et_NewBuiltinFile(\n"
"  char *zFilename,  /* Name of the new file */\n"
"  char *zData,      /* Data for the new file */\n"
"  int nData         /* Number of bytes in the new file */\n"
"){\n"
"  int h;\n"
"  struct EtFile *p;\n"
"\n"
"  p = (struct EtFile*)Tcl_Alloc( sizeof(struct EtFile) + strlen(zFilename) + 1);\n"
"  if( p==0 ) return;\n"
"  p->zName = (char*)&p[1];\n"
"  strcpy(p->zName, zFilename);\n"
"  p->zData = zData;\n"
"  p->nData = nData;\n"
"  p->shrouded = 0;\n"
"  h = FilenameHash(zFilename);\n"
"  p->pNext = Et_FileHashTable[h];\n"
"  Et_FileHashTable[h] = p;\n"
"}\n"
"\n"
"/*\n"
"** A TCL interface to the Et_NewBuiltinFile function.  For Tcl8.0\n"
"** and later, we make this an Obj command so that it can deal with\n"
"** binary data.\n"
"*/\n"
"#if ET_HAVE_OBJ\n"
"static int Et_NewBuiltinFileCmd(ET_OBJARGS){\n"
"  char *zData, *zNew;\n"
"  int nData;\n"
"  if( objc!=3 ){\n"
"    Tcl_WrongNumArgs(interp, 1, objv, \"filename data\");\n"
"    return TCL_ERROR;\n"
"  }\n"
"  zData = (char*)Tcl_GetByteArrayFromObj(objv[2], &nData);\n"
"  zNew = Tcl_Alloc( nData + 1 );\n"
"  if( zNew ){\n"
"    memcpy(zNew, zData, nData);\n"
"    zNew[nData] = 0;\n"
"    Et_NewBuiltinFile(Tcl_GetStringFromObj(objv[1], 0), zNew, nData);\n"
"  }\n"
"  return TCL_OK;\n"
"}\n"
"#else\n"
"static int Et_NewBuiltinFileCmd(ET_TCLARGS){\n"
"  char *zData;\n"
"  int nData;\n"
"  if( argc!=3 ){\n"
"    Et_ResultF(interp,\"wrong # args: should be \\\"%s FILENAME DATA\\\"\", argv[0]);\n"
"    return TCL_ERROR;\n"
"  }\n"
"  nData = strlen(argv[2]) + 1;\n"
"  zData = Tcl_Alloc( nData );\n"
"  if( zData ){\n"
"    strcpy(zData, argv[2]);\n"
"    Et_NewBuiltinFile(argv[1], zData, nData);\n"
"  }\n"
"  return TCL_OK;\n"
"}\n"
"#endif\n"
"\n"
"/*\n"
"** The following section implements the InsertProc functionality.  The\n"
"** new InsertProc feature of Tcl8.0.3 and later allows us to overload\n"
"** the usual system call commands for file I/O and replace them with\n"
"** commands that operate on the built-in files.\n"
"*/\n"
"#ifdef ET_HAVE_INSERTPROC\n"
"\n"
"/* \n"
"** Each open channel to a built-in file is an instance of the\n"
"** following structure.\n"
"*/\n"
"typedef struct Et_FileStruct {\n"
"  char *zData;     /* All of the data */\n"
"  int nData;       /* Bytes of data, not counting the null terminator */\n"
"  int cursor;      /* How much of the data has been read so far */\n"
"} Et_FileStruct;\n"
"\n"
"/*\n"
"** Close a previously opened built-in file.\n"
"*/\n"
"static int Et_FileClose(ClientData instanceData, Tcl_Interp *interp){\n"
"  Et_FileStruct *p = (Et_FileStruct*)instanceData;\n"
"  Tcl_Free((char*)p);\n"
"  return 0;\n"
"}\n"
"\n"
"/*\n"
"** Read from a built-in file.\n"
"*/\n"
"static int Et_FileInput(\n"
"  ClientData instanceData,    /* The file structure */\n"
"  char *buf,                  /* Write the data read here */\n"
"  int bufSize,                /* Read this much data */\n"
"  int *pErrorCode             /* Write the error code here */\n"
"){\n"
"  Et_FileStruct *p = (Et_FileStruct*)instanceData;\n"
"  *pErrorCode = 0;\n"
"  if( p->cursor+bufSize>p->nData ){\n"
"    bufSize = p->nData - p->cursor;\n"
"  }\n"
"  memcpy(buf, &p->zData[p->cursor], bufSize);\n"
"  p->cursor += bufSize;\n"
"  return bufSize;\n"
"}\n"
"\n"
"/*\n"
"** Writes to a built-in file always return EOF.\n"
"*/\n"
"static int Et_FileOutput(\n"
"  ClientData instanceData,    /* The file structure */\n"
"  char *buf,                  /* Read the data from here */\n"
"  int toWrite,                /* Write this much data */\n"
"  int *pErrorCode             /* Write the error code here */\n"
"){\n"
"  *pErrorCode = 0;\n"
"  return 0;\n"
"}\n"
"\n"
"/*\n"
"** Move the cursor around within the built-in file.\n"
"*/\n"
"static int Et_FileSeek(\n"
"  ClientData instanceData,    /* The file structure */\n"
"  long offset,                /* Offset to seek to */\n"
"  int mode,                   /* One of SEEK_CUR, SEEK_SET or SEEK_END */\n"
"  int *pErrorCode             /* Write the error code here */\n"
"){\n"
"  Et_FileStruct *p = (Et_FileStruct*)instanceData;\n"
"  switch( mode ){\n"
"    case SEEK_CUR:     offset += p->cursor;   break;\n"
"    case SEEK_END:     offset += p->nData;    break;\n"
"    default:           break;\n"
"  }\n"
"  if( offset<0 ) offset = 0;\n"
"  if( offset>p->nData ) offset = p->nData;\n"
"  p->cursor = offset;\n"
"  return offset;\n"
"}\n"
"\n"
"/*\n"
"** The Watch method is a no-op\n"
"*/\n"
"static void Et_FileWatch(ClientData instanceData, int mask){\n"
"}\n"
"\n"
"/*\n"
"** The Handle method always returns an error.\n"
"*/\n"
"static int Et_FileHandle(ClientData notUsed, int dir, ClientData *handlePtr){\n"
"  return TCL_ERROR;\n"
"}\n"
"\n"
"/*\n"
"** This is the channel type that will access the built-in files.\n"
"*/\n"
"static Tcl_ChannelType builtinChannelType = {\n"
"    \"builtin\",			/* Type name. */\n"
"    NULL,			/* Always non-blocking.*/\n"
"    Et_FileClose,		/* Close proc. */\n"
"    Et_FileInput,		/* Input proc. */\n"
"    Et_FileOutput,		/* Output proc. */\n"
"    Et_FileSeek,		/* Seek proc. */\n"
"    NULL,			/* Set option proc. */\n"
"    NULL,			/* Get option proc. */\n"
"    Et_FileWatch,		/* Watch for events on console. */\n"
"    Et_FileHandle,		/* Get a handle from the device. */\n"
"};\n"
"\n"
"/*\n"
"** This routine attempts to do an open of a built-in file.\n"
"*/\n"
"static Tcl_Channel Et_FileOpen(\n"
"  Tcl_Interp *interp,     /* The TCL interpreter doing the open */\n"
"  char *zFilename,        /* Name of the file to open */\n"
"  char *modeString,       /* Mode string for the open (ignored) */\n"
"  int permissions         /* Permissions for a newly created file (ignored) */\n"
"){\n"
"  char *zData;\n"
"  Et_FileStruct *p;\n"
"  int nData;\n"
"  char zName[50];\n"
"  Tcl_Channel chan;\n"
"  static int count = 1;\n"
"\n"
"  zData = FindBuiltinFile(zFilename, 1, &nData);\n"
"  if( zData==0 ) return NULL;\n"
"  p = (Et_FileStruct*)Tcl_Alloc( sizeof(Et_FileStruct) );\n"
"  if( p==0 ) return NULL;\n"
"  p->zData = zData;\n"
"  p->nData = nData;\n"
"  p->cursor = 0;\n"
"  sprintf(zName,\"etbi_%x_%x\",((int)Et_FileOpen)>>12,count++);\n"
"  chan = Tcl_CreateChannel(&builtinChannelType, zName, \n"
"                           (ClientData)p, TCL_READABLE);\n"
"  return chan;\n"
"}\n"
"\n"
"/*\n"
"** This routine does a stat() system call for a built-in file.\n"
"*/\n"
"static int Et_FileStat(char *path, struct stat *buf){\n"
"  char *zData;\n"
"  int nData;\n"
"\n"
"  zData = FindBuiltinFile(path, 0, &nData);\n"
"  if( zData==0 ){\n"
"    return -1;\n"
"  }\n"
"  memset(buf, 0, sizeof(*buf));\n"
"  buf->st_mode = 0400;\n"
"  buf->st_size = nData;\n"
"  return 0;\n"
"}\n"
"\n"
"/*\n"
"** This routien does an access() system call for a built-in file.\n"
"*/\n"
"static int Et_FileAccess(char *path, int mode){\n"
"  char *zData;\n"
"\n"
"  if( mode & 3 ){\n"
"    return -1;\n"
"  }\n"
"  zData = FindBuiltinFile(path, 0, 0);\n"
"  if( zData==0 ){\n"
"    return -1;\n"
"  }\n"
"  return 0; \n"
"}\n"
"#endif  /* ET_HAVE_INSERTPROC */\n"
"\n"
"/*\n"
"** An overloaded version of \"source\".  First check for the file\n"
"** is one of the built-ins.  If it isn't a built-in, then check the\n"
"** disk.  But if ET_STANDALONE is set (which corresponds to the\n"
"** \"Strict\" option in the user interface) then never check the disk.\n"
"** This gives us a quick way to check for the common error of\n"
"** sourcing a file that exists on the development by mistake, \n"
"** and only discovering the mistake when you move the program\n"
"** to your customer's machine.\n"
"*/\n"
"static int Et_Source(ET_TCLARGS){\n"
"  char *z;\n"
"\n"
"  if( argc!=2 ){\n"
"    Et_ResultF(interp,\"wrong # args: should be \\\"%s FILENAME\\\"\", argv[0]);\n"
"    return TCL_ERROR;\n"
"  }\n"
"  z = FindBuiltinFile(argv[1], 1, 0);\n"
"  if( z ){\n"
"    int rc;\n"
"    rc = Tcl_Eval(interp,z);\n"
"    if (rc == TCL_ERROR) {\n"
"      char msg[200];\n"
"      sprintf(msg, \"\\n    (file \\\"%.150s\\\" line %d)\", argv[1],\n"
"        interp->errorLine);\n"
"      Tcl_AddErrorInfo(interp, msg);\n"
"    } else {\n"
"      rc = TCL_OK;\n"
"    }\n"
"    return rc;\n"
"  }\n"
"#if ET_STANDALONE\n"
"  Et_ResultF(interp,\"no such file: \\\"%s\\\"\", argv[1]);\n"
"  return TCL_ERROR;\n"
"#else\n"
"  return Tcl_EvalFile(interp,argv[1]);\n"
"#endif\n"
"}\n"
"\n"
"#ifndef ET_HAVE_INSERTPROC\n"
"/*\n"
"** An overloaded version of \"file exists\".  First check for the file\n"
"** in the file table, then go to disk.\n"
"**\n"
"** We only overload \"file exists\" if we don't have InsertProc() \n"
"** procedures.  If we do have InsertProc() procedures, they will\n"
"** handle this more efficiently.\n"
"*/\n"
"static int Et_FileExists(ET_TCLARGS){\n"
"  int i, rc;\n"
"  Tcl_DString str;\n"
"  if( argc==3 && strncmp(argv[1],\"exis\",4)==0 ){\n"
"    if( FindBuiltinFile(argv[2], 0, 0)!=0 ){\n"
"      Tcl_SetResult(interp, \"1\", TCL_STATIC);\n"
"      return TCL_OK;\n"
"    }\n"
"  }\n"
"  Tcl_DStringInit(&str);\n"
"  Tcl_DStringAppendElement(&str,\"Et_FileCmd\");\n"
"  for(i=1; i<argc; i++){\n"
"    Tcl_DStringAppendElement(&str, argv[i]);\n"
"  }\n"
"  rc = Tcl_Eval(interp, Tcl_DStringValue(&str));\n"
"  Tcl_DStringFree(&str);\n"
"  return rc;\n"
"}\n"
"#endif\n"
"\n"
"/*\n"
"** This is the main Tcl interpreter.  It's a global variable so it\n"
"** can be accessed easily from C code.\n"
"*/\n"
"Tcl_Interp *Et_Interp = 0;\n"
"\n"
"\n"
"#if ET_WIN32\n"
"/*\n"
"** Implement the Et_MessageBox command on Windows platforms.  We\n"
"** use the MessageBox() function from the Win32 API so that the\n"
"** error message will be displayed as a dialog box.  Writing to\n"
"** standard error doesn't do anything on windows.\n"
"*/\n"
"int Et_MessageBox(ET_TCLARGS){\n"
"  char *zMsg = \"(Empty Message)\";\n"
"  char *zTitle = \"Message...\";\n"
"\n"
"  if( argc>1 ){\n"
"    zTitle = argv[1];\n"
"  }\n"
"  if( argc>2 ){\n"
"    zMsg = argv[2];\n"
"  }\n"
"  MessageBox(0, zMsg, zTitle, MB_ICONSTOP | MB_OK);\n"
"  return TCL_OK;\n"
"}\n"
"#endif\n"
"\n"
"/*\n"
"** A default implementation for \"bgerror\"\n"
"*/\n"
"static char zBgerror[] = \n"
"  \"proc Et_Bgerror err {\\n\"\n"
"  \"  global errorInfo tk_library\\n\"\n"
"  \"  if {[info exists errorInfo]} {\\n\"\n"
"  \"    set ei $errorInfo\\n\"\n"
"  \"  } else {\\n\"\n"
"  \"    set ei {}\\n\"\n"
"  \"  }\\n\"\n"
"  \"  if {[catch {bgerror $err}]==0} return\\n\"\n"
"  \"  if {[string length $ei]>0} {\\n\"\n"
"  \"    set err $ei\\n\"\n"
"  \"  }\\n\"\n"
"  \"  if {[catch {Et_MessageBox {Error} $err}]} {\\n\"\n"
"  \"    puts stderr $err\\n\"\n"
"  \"  }\\n\"\n"
"  \"  exit\\n\"\n"
"  \"}\\n\"\n"
";\n"
"\n"
"/*\n"
"** Do the initialization.\n"
"**\n"
"** This routine is called after the interpreter is created, but\n"
"** before Et_PreInit() or Et_AppInit() have been run.\n"
"*/\n"
"static int Et_DoInit(Tcl_Interp *interp){\n"
"  int i;\n"
"  extern int Et_PreInit(Tcl_Interp*);\n"
"  extern int Et_AppInit(Tcl_Interp*);\n"
"\n"
"  /* Insert our alternative stat(), access() and open() procedures\n"
"  ** so that any attempt to work with a file will check our built-in\n"
"  ** scripts first.\n"
"  */\n"
"#ifdef ET_HAVE_INSERTPROC\n"
"  TclStatInsertProc(Et_FileStat);\n"
"  TclAccessInsertProc(Et_FileAccess);\n"
"  TclOpenFileChannelInsertProc(Et_FileOpen);\n"
"#endif\n"
"\n"
"  /* Initialize the hash-table for built-in scripts\n"
"  */\n"
"  FilenameHashInit();\n"
"\n"
"  /* The Et_NewBuiltFile command is inserted for use by FreeWrap\n"
"  ** and similar tools.\n"
"  */\n"
"#if ET_HAVE_OBJ\n"
"  Tcl_CreateObjCommand(interp,\"Et_NewBuiltinFile\",Et_NewBuiltinFileCmd,0,0);\n"
"#else\n"
"  Tcl_CreateCommand(interp,\"Et_NewBuiltinFile\",Et_NewBuiltinFileCmd,0,0);\n"
"#endif\n"
"\n"
"  /* Overload the \"file\" and \"source\" commands\n"
"  */\n"
"#ifndef ET_HAVE_INSERTPROC\n"
"  {\n"
"    static char zRename[] = \"rename file Et_FileCmd\";\n"
"    Tcl_Eval(interp,zRename);\n"
"    Tcl_CreateCommand(interp,\"file\",Et_FileExists,0,0);\n"
"  }\n"
"#endif\n"
"  Tcl_CreateCommand(interp,\"source\",Et_Source,0,0);\n"
"\n"
"  Et_Interp = interp;\n"
"#ifdef ET_TCL_LIBRARY\n"
"  Tcl_SetVar(interp,\"tcl_library\",ET_TCL_LIBRARY,TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar(interp,\"tcl_libPath\",ET_TCL_LIBRARY,TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar2(interp,\"env\",\"TCL_LIBRARY\",ET_TCL_LIBRARY,TCL_GLOBAL_ONLY);\n"
"#endif\n"
"#ifdef ET_TK_LIBRARY\n"
"  Tcl_SetVar(interp,\"tk_library\",ET_TK_LIBRARY,TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar2(interp,\"env\",\"TK_LIBRARY\",ET_TK_LIBRARY,TCL_GLOBAL_ONLY);\n"
"#endif\n"
"#if ET_WIN32\n"
"  Tcl_CreateCommand(interp,\"Et_MessageBox\",Et_MessageBox, 0, 0);\n"
"#endif  \n"
"  Tcl_Eval(interp,zBgerror);\n"
"#if ET_HAVE_PREINIT\n"
"  if( Et_PreInit(interp) == TCL_ERROR ){\n"
"    goto initerr;\n"
"  }\n"
"#endif\n"
"  if( Tcl_Init(interp) == TCL_ERROR ){\n"
"    goto initerr;\n"
"  }\n"
"  Et_GlobalEvalF(interp,\"set dir $tcl_library;source $dir/tclIndex;unset dir\");\n"
"#if ET_ENABLE_TK\n"
"  if( Tk_Init(interp) == TCL_ERROR ){\n"
"    goto initerr;\n"
"  }\n"
"  Tcl_StaticPackage(interp,\"Tk\", Tk_Init, 0);\n"
"  Et_GlobalEvalF(interp,\"set dir $tk_library;source $dir/tclIndex;unset dir\");\n"
"#endif\n"
"  /* Tcl_SetVar(interp, \"tcl_rcFileName\", \"~/.wishrc\", TCL_GLOBAL_ONLY); */\n"
"  for(i=0; i<sizeof(Et_CmdSet)/sizeof(Et_CmdSet[0]) - 1; i++){\n"
"    Tcl_CreateCommand(interp, Et_CmdSet[i].zName, Et_CmdSet[i].xProc, 0, 0);\n"
"  }\n"
"#if ET_ENABLE_OBJ\n"
"  for(i=0; i<sizeof(Et_ObjSet)/sizeof(Et_ObjSet[0]) - 1; i++){\n"
"    Tcl_CreateObjCommand(interp, Et_ObjSet[i].zName, Et_ObjSet[i].xProc, 0, 0);\n"
"  }\n"
"#endif\n"
"  Tcl_LinkVar(interp,\"Et_EvalTrace\",(char*)&Et_EvalTrace,TCL_LINK_BOOLEAN);\n"
"  Tcl_SetVar(interp,\"et_version\",ET_VERSION,TCL_GLOBAL_ONLY);\n"
"#if ET_HAVE_APPINIT\n"
"  if( Et_AppInit(interp) == TCL_ERROR ){\n"
"    goto initerr;\n"
"  }\n"
"#endif\n"
"#if ET_ENABLE_TK && !ET_EXTENSION\n"
"  Et_NewBuiltinFile(\"builtin:/console.tcl\", zEtConsole, sizeof(zEtConsole));\n"
"#if ET_CONSOLE\n"
"  Tcl_Eval(interp,\n"
"    \"source builtin:/console.tcl\\n\"\n"
"    \"console:create {.@console} {% } {Tcl/Tk Console}\\n\"\n"
"  );\n"
"#endif\n"
"#endif\n"
"#ifdef ET_MAIN_SCRIPT\n"
"  if( Et_EvalF(interp,\"source \\\"%q\\\"\", ET_MAIN_SCRIPT)!=TCL_OK ){\n"
"    goto initerr;\n"
"  }\n"
"#endif\n"
"  return TCL_OK;\n"
"\n"
"initerr:\n"
"  Et_EvalF(interp,\"Et_Bgerror \\\"%q\\\"\", Tcl_GetStringResult(interp));\n"
"  return TCL_ERROR;\n"
"}\n"
"\n"
"#if ET_READ_STDIN==0 || ET_AUTO_FORK!=0\n"
"/*\n"
"** Initialize everything.\n"
"*/\n"
"static int Et_Local_Init(int argc, char **argv){\n"
"  Tcl_Interp *interp;\n"
"  char *args;\n"
"  char buf[100];\n"
"#if !ET_HAVE_CUSTOM_MAINLOOP\n"
"  static char zWaitForever[] = \n"
"#if ET_ENABLE_TK\n"
"    \"bind . <Destroy> {+if {\\\"%W\\\"==\\\".\\\"} exit}\\n\"\n"
"#endif\n"
"    \"while 1 {vwait forever}\";\n"
"#endif\n"
"\n"
"  Tcl_FindExecutable(argv[0]);\n"
"  interp = Tcl_CreateInterp();\n"
"  args = Tcl_Merge(argc-1, argv+1);\n"
"  Tcl_SetVar(interp, \"argv\", args, TCL_GLOBAL_ONLY);\n"
"  ckfree(args);\n"
"  sprintf(buf, \"%d\", argc-1);\n"
"  Tcl_SetVar(interp, \"argc\", buf, TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar(interp, \"argv0\", argv[0], TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar(interp, \"tcl_interactive\", \"0\", TCL_GLOBAL_ONLY);\n"
"  Et_DoInit(interp);\n"
"#if ET_HAVE_CUSTOM_MAINLOOP\n"
"  Et_CustomMainLoop(interp);\n"
"#else\n"
"  Tcl_Eval(interp,zWaitForever);\n"
"#endif\n"
"  return 0;\n"
"}\n"
"#endif\n"
"\n"
"/*\n"
"** This routine is called to do the complete initialization.\n"
"*/\n"
"int Et_Init(int argc, char **argv){\n"
"#ifdef ET_TCL_LIBRARY\n"
"  putenv(\"TCL_LIBRARY=\" ET_TCL_LIBRARY);\n"
"#endif\n"
"#ifdef ET_TK_LIBRARY\n"
"  putenv(\"TK_LIBRARY=\" ET_TK_LIBRARY);\n"
"#endif\n"
"#if ET_CONSOLE || !ET_READ_STDIN\n"
"  Et_Local_Init(argc, argv);\n"
"#else\n"
"# if ET_ENABLE_TK\n"
"  Tk_Main(argc,argv,Et_DoInit);\n"
"# else\n"
"  Tcl_Main(argc, argv, Et_DoInit);\n"
"# endif\n"
"#endif\n"
"  return 0;\n"
"}\n"
"\n"
"#if !ET_HAVE_MAIN && !ET_EXTENSION\n"
"/*\n"
"** Main routine for UNIX programs.  If the user has supplied\n"
"** their own main() routine in a C module, then the ET_HAVE_MAIN\n"
"** macro will be set to 1 and this code will be skipped.\n"
"*/\n"
"int main(int argc, char **argv){\n"
"#if ET_AUTO_FORK\n"
"  int rc = fork();\n"
"  if( rc<0 ){\n"
"    perror(\"can't fork\");\n"
"    exit(1);\n"
"  }\n"
"  if( rc>0 ) return 0;\n"
"  close(0);\n"
"  open(\"/dev/null\",O_RDONLY);\n"
"  close(1);\n"
"  open(\"/dev/null\",O_WRONLY);\n"
"#endif\n"
"  return Et_Init(argc,argv)!=TCL_OK;\n"
"}\n"
"#endif\n"
"\n"
"#if ET_EXTENSION\n"
"/*\n"
"** If the -extension flag is used, then generate code that will be\n"
"** turned into a loadable shared library or DLL, not a standalone\n"
"** executable.\n"
"*/\n"
"int ET_EXTENSION_NAME(Tcl_Interp *interp){\n"
"  int i;\n"
"#ifndef ET_HAVE_INSERTPROC\n"
"  Tcl_AppendResult(interp,\n"
"       \"mktclapp can only generate extensions for Tcl/Tk version \"\n"
"       \"8.0.3 and later. This is version \"\n"
"       TCL_MAJOR_VERSION \".\" TCL_MINOR_VERSION \".\" TCL_RELEASE_SERIAL, 0);\n"
"  return TCL_ERROR;\n"
"#endif\n"
"#ifdef ET_HAVE_INSERTPROC\n"
"#ifdef USE_TCL_STUBS\n"
"  if( Tcl_InitStubs(interp,\"8.0\",0)==0 ){\n"
"    return TCL_ERROR;\n"
"  }\n"
"  if( Tk_InitStubs(interp,\"8.0\",0)==0 ){\n"
"    return TCL_ERROR;\n"
"  }\n"
"#endif\n"
"  Et_Interp = interp;\n"
"  TclStatInsertProc(Et_FileStat);\n"
"  TclAccessInsertProc(Et_FileAccess);\n"
"  TclOpenFileChannelInsertProc(Et_FileOpen);\n"
"  FilenameHashInit();\n"
"  for(i=0; i<sizeof(Et_CmdSet)/sizeof(Et_CmdSet[0]) - 1; i++){\n"
"    Tcl_CreateCommand(interp, Et_CmdSet[i].zName, Et_CmdSet[i].xProc, 0, 0);\n"
"  }\n"
"#if ET_ENABLE_OBJ\n"
"  for(i=0; i<sizeof(Et_ObjSet)/sizeof(Et_ObjSet[0]) - 1; i++){\n"
"    Tcl_CreateObjCommand(interp, Et_ObjSet[i].zName, Et_ObjSet[i].xProc, 0, 0);\n"
"  }\n"
"#endif\n"
"  Tcl_LinkVar(interp,\"Et_EvalTrace\",(char*)&Et_EvalTrace,TCL_LINK_BOOLEAN);\n"
"  Tcl_SetVar(interp,\"et_version\",ET_VERSION,TCL_GLOBAL_ONLY);\n"
"#if ET_HAVE_APPINIT\n"
"  if( Et_AppInit(interp) == TCL_ERROR ){\n"
"    return TCL_ERROR;\n"
"  }\n"
"#endif\n"
"#ifdef ET_MAIN_SCRIPT\n"
"  if( Et_EvalF(interp,\"source \\\"%q\\\"\", ET_MAIN_SCRIPT)!=TCL_OK ){\n"
"    return TCL_ERROR;\n"
"  }\n"
"#endif\n"
"  return TCL_OK;\n"
"#endif  /* ET_HAVE_INSERTPROC */\n"
"}\n"
"int ET_SAFE_EXTENSION_NAME(Tcl_Interp *interp){\n"
"  return ET_EXTENSION_NAME(interp);\n"
"}\n"
"#endif\n"
;
