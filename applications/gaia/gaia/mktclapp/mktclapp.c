/*
** Copyright (c) 1998 D. Richard Hipp
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
#include <unistd.h>
#include <time.h>
#include <assert.h>

/*
** Version information for this program
*/
static char zVersion[] = "mktclapp version 2.1.  May 1, 1999";

/*
** Each new TCL commands discovered while scanning the source code is
** stored in an instance of the following structure.
*/
typedef struct EtCmd EtCmd;
struct EtCmd {
  char *zIf;         /* Surrounding #if statement */
  char *zName;       /* Name of the command */
  int isObj;         /* True if this is an object command */
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
** All text contained with "#if ET_TCL_CODE" thru "#endif" from all
** C and C++ modules that are scanned.
*/
static char *zTclCode = 0;
static int nTclCode = 0;

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
** This routine is called when "#ifdef ET_TCL_CODE" is seen inside
** a C or C++ file.  It scans ahead to find the matching "#endif" and
** appends all the intervening source text on the zTclCode global
** variable.
**
** z[*pI] is the first character past the "#ifdef ET_TCL_CODE".  Before
** returning, update *pI to point past the "#endif"
*/
static void CollectTclCode(const char *fileName, const char *z, int *pI){
  int i, j;
  int start, end;
  int amt = 0;
  int go = 1;
  int atLineStart;
  char *zIf;

  start = *pI;
  while( z[start] && isspace(z[start]) ){ start++; }
  i = start;
  if( z[i]==0 ) return;
  while( go && z[i] ){
    switch( z[i] ){
      case '\n':
        amt += 5;
        break;

      case '\t':
      case '\r':
      case '\\':
      case '\"':
        amt += 2;
        break;

      case '#':
        if( (i==start || z[i-1]=='\n') && strncmp(&z[i],"#endif",6)==0 ){
          go = 0;
        }else{
          amt++;
        }
        break;

      default:
        if( isprint(z[i]) ) amt++;
        else amt += 4;
        break;
    }
    i++;
  }
  if( z[i]==0 ) return;
  end = --i;
  *pI = end+6;
  if( end==start ) return;
  if( nTclCode==0 ) nTclCode = 1;
  j = nTclCode - 1;
  zIf = IfString(0);
  if( zIf ){ amt += strlen(zIf) + 12; }
  nTclCode += amt;
  zTclCode = SafeRealloc( zTclCode, nTclCode  );
  if( zIf && (zIf[0]!='0' || zIf[1]!=0) ){
    sprintf(&zTclCode[j],"#if %s\n", zIf);
    j += strlen(&zTclCode[j]);
  }
  for(i=start; i<end; i++){
    if( atLineStart ){
      zTclCode[j++] = '"';
      atLineStart = 0;
    }
    switch( z[i] ){
      case '\n':
        zTclCode[j++] = '\\';
        zTclCode[j++] = 'n';
        zTclCode[j++] = '"';
        zTclCode[j++] = '\n';
        atLineStart = 1;
        break;

      case '\t':
        zTclCode[j++] = '\\';
        zTclCode[j++] = 't';
        break;

      case '\r':
        zTclCode[j++] = '\\';
        zTclCode[j++] = 'r';
        break;

      case '\\':
      case '\"':
        zTclCode[j++] = '\\';
        zTclCode[j++] = z[i];
        break;

      default:
        if( isprint(z[i]) ){
          zTclCode[j++] = z[i];
        }else{
          zTclCode[j++] = '\\';
          zTclCode[j++] = (z[i]>>6) & 3;
          zTclCode[j++] = (z[i]>>3) & 7;
          zTclCode[j++] = z[i] & 7;
        }
        break;
    }
  }
  if( zIf && (zIf[0]!='0' || zIf[1]!=0) ){
    sprintf(&zTclCode[j],"#endif\n");
    j += strlen(&zTclCode[j]);
  }
  assert( nTclCode-1==j );
  zTclCode[nTclCode-1] = 0;
}

/*
** Read the complete text of a file into memory.  Return 0 if there
** is any kind of error.
*/
char *ReadFileIntoMemory(const char *fileName){
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

  z = ReadFileIntoMemory(fileName);
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
          if( zArg && strcmp(zArg,"ET_TCL_CODE")==0 ){
            CollectTclCode(fileName, z, &i);
          }else{
            PushIf(zArg,start,0,1);
          }
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
        if( atLineStart ){
          c = 0;
        }
        break;
      case '#':
        if( atLineStart && !isalpha(z[i+1]) ){
          while( z[i] && z[i]!='\n' ){ i++; }
          c = 0;
          if( z[i]==0 ){ i--; }
        }else{
          atLineStart = 0;
        }
        break;
      case '\n':
        if( atLineStart ){
          c = 0;
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
  int xor;
  int atLineStart = 1;
  if( shroud>0 ){
    printf("\"y");
    xor = shroud;
    atLineStart = 0;
  }
  while( (c=*z)!=0 ){
    z++;
    if( c=='\r' && *z=='\n' ) continue;
    if( shroud>0 && c>=0x20 ){ c ^= xor; xor = (xor+1)&0x1f; }
    if( atLineStart ){
      putchar('"');
      atLineStart = 0;
    }
    switch( c ){
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
"ET_EXTERN int Et_EvalF(Tcl_Interp*,const char *,...);\n"
"ET_EXTERN int Et_GlobalEvalF(Tcl_Interp*,const char *,...);\n"
"ET_EXTERN int Et_DStringAppendF(Tcl_DString*,const char*,...);\n"
"ET_EXTERN int Et_ResultF(Tcl_Interp*,const char*,...);\n"
"ET_EXTERN Tcl_Interp *Et_Interp;\n"
"#define ET_TCLARGS ClientData clientData,Tcl_Interp*interp,int argc,char**argv\n"
"#define ET_OBJARGS ClientData clientData,Tcl_Interp*itnerp,int objc,Tcl_Obj *CONST objv[]\n"
"#endif\n"
;

/*
** Print a usage comment and die
*/
static void Usage(char *argv0){
  fprintf(stderr,"Usage: %s arguments...\n", argv0);
  fprintf(stderr,
     "  -header            print a header file and exit\n"
     "  -version           print the version number of mktclapp and exit\n"
     "  -notk              built a Tcl-only program.  No GUI\n"
     "  -autofork          automatically fork the program into the background\n"
     "  -strip-tcl         remove comments and extra white-space from\n"
     "                     subsequent TCL files\n"
     "  -dont-strip-tcl    stop stripping TCL files\n"
     "  -tcl-library       directory holding the TCL script library\n"
     "  -tk-library        directory holding the TK script library\n"
     "  -main-script FILE  run the script FILE after initialization\n"
     "  -read-stdin        read standard input\n"
     "  -shroud            hide compile-in TCL from view\n"
     "  -enable-obj        use TCL Obj commands where possible\n"
     "  -standalone        make the \"source\" TCL command only work\n"
     "                     for builtin scripts\n"
     "  -f FILE            read more command-line parameters from FILE\n"
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
      if( nNew + argc > nAlloc ){
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
  int doCompress = 1;     /* Current state of the compression flag */
  char *zTclLib = 0;      /* Name of the TCL library */
  char *zTkLib = 0;       /* Name of the TK library */
  char *zMainScript = 0;  /* Name of a script to run first */
  int shroud = 0;         /* True to encrypt the compiled-in TCL */
  int readStdin = 0;      /* True to read TCL commands from STDIN */
  int enableObj = 0;      /* Enable the use of object commands */
  int standalone = 0;     /* True to disable the "source" command */
  extern char zTail[];

  if( argc>=2 && strcmp(argv[1],"-header")==0 ){
    printf("%s",zHeader);
    return 0;
  }
  if( argc>=2 && strcmp(argv[1],"-version")==0 ){
    printf("%s\n",zVersion);
    return 0;
  }
  azTcl = SafeMalloc( sizeof(char*)*(argc + 1000) );
  aDoCompress = SafeMalloc( sizeof(int)*(argc + 1000) );
  for(i=1; i<argc; i++){
    if( argv[i][0]=='-' ){
      if( strcmp(argv[i],"-header")==0 ){
        printf("%s",zHeader);
        return 0;
      }else if( strcmp(argv[i],"-notk")==0 ){
        useTk = 0;
      }else if( strcmp(argv[i],"-autofork")==0 ){
        autoFork = 1;
      }else if( strcmp(argv[i],"-read-stdin")==0 ){
        readStdin = 1;
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
      }else if( i<argc-1 && strcmp(argv[i],"-main-script")==0 ){
        zMainScript = argv[++i];
      }else if( i<argc-1 && strcmp(argv[i],"-tcl-library")==0 ){
        zTclLib = argv[++i];
      }else if( i<argc-1 && strcmp(argv[i],"-tk-library")==0 ){
        zTkLib = argv[++i];
      }else if( strcmp(argv[i],"-f")==0 ){
        AddParameters(i, &argc, &argv);
      }else{
        Usage(argv[0]);
      }
    }else if( IsTclFile(argv[i]) ){
      if( access(argv[i],R_OK) ){
        fprintf(stderr,"%s: can't open \"%s\" for reading\n", Argv0, argv[i]);
        nError++;
      }else{
        azTcl[nTcl] = argv[i];
        aDoCompress[nTcl] = doCompress;
        nTcl++;
      }
    }else{
      ScanFile(argv[i]);
    }
  }
  if( nError>0 ) return nError;
  if( nObjCmd>0 ) enableObj = 1;
  printf(
    "/* This code is automatically generated by \"mktclapp\" */\n"
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
  SetMacro("ET_HAVE_APPINIT",seenEtAppInit);
  SetMacro("ET_HAVE_PREINIT",seenEtPreInit);
  SetMacro("ET_HAVE_MAIN",seenMain);
  if( zTclLib ){
    printf("#define ET_TCL_LIBRARY ");
    WriteAsString(zTclLib,0);
  }
  if( zTkLib ){
    printf("#define ET_TK_LIBRARY ");
    WriteAsString(zTkLib,0);
  }
  if( zMainScript ){
    printf("#define ET_MAIN_SCRIPT ");
    WriteAsString(zMainScript,0);
  }
  if( shroud>0 ){
    shroud = time(0) % 31 + 1;
  }
  printf("#define ET_SHROUD_KEY %d\n",shroud);
  printf("#define ET_READ_STDIN %d\n",readStdin);
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
  if( zTclCode ){
    printf("static char zEtTclCode[] = \n%s;\n", zTclCode);
  }else{
    printf("static char zEtTclCode[] = \"\";\n");
  }
  for(i=0; i<nTcl; i++){
    char *z;
    printf("static char Et_zFile%d[] = \n",i);
    z = ReadFileIntoMemory(azTcl[i]);
    if( z==0 ) continue;
    if( aDoCompress[i] ) CompressTcl(z);
    WriteAsString(z,shroud);
    printf(";\n");
    SafeFree(z);
  }
  printf(
    "struct EtFile {\n"
    "  char *zName;\n"
    "  char *zData;\n"
    "  struct EtFile *pNext;\n"
    "};\n"
    "static struct EtFile Et_FileSet[] = {\n"
  );
  for(i=0; i<nTcl; i++){
    printf("  { \"%s\", Et_zFile%d },\n", azTcl[i], i);
  }
  printf(
    "{0, 0}};\n"
    "static struct EtFile *Et_FileHashTable[%d];\n"
    "%s",
    nTcl*2 + 1,zTail
  );
  return nError;
}

char zTail[] =
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
"/*\n"
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
"#include <ctype.h>\n"
"#include <string.h>\n"
"#include <stdarg.h>\n"
"#include <stdio.h>\n"
"#if ET_ENABLE_TK\n"
"# include <tk.h>\n"
"#else\n"
"# include <tcl.h>\n"
"#endif\n"
"#include <stdlib.h>\n"
"#include <sys/types.h>\n"
"#include <sys/stat.h>\n"
"#include <fcntl.h>\n"
"#include <unistd.h>\n"
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
"enum e_type {    /* The type of the format field */\n"
"   RADIX,            /* Integer types.  %d, %x, %o, and so forth */\n"
"   FLOAT,            /* Floating point.  %f */\n"
"   EXP,              /* Exponentional notation. %e and %E */\n"
"   GENERIC,          /* Floating or exponential, depending on exponent. %g */\n"
"   SIZE,             /* Return number of characters processed so far. %n */\n"
"   STRING,           /* Strings. %s */\n"
"   PERCENT,          /* Percent symbol. %% */\n"
"   CHARX,            /* Characters. %c */\n"
"   ERROR,            /* Used to indicate no such conversion type */\n"
"/* The rest are extensions, not normally found in printf() */\n"
"   CHARLIT,          /* Literal characters.  %' */\n"
"   TCLESCAPE,        /* Strings with special characters escaped.  %q */\n"
"   MEM_STRING,       /* A string which should be deleted after use. %z */\n"
"   ORDINAL           /* 1st, 2nd, 3rd and so forth */\n"
"};\n"
"\n"
"/*\n"
"** Each builtin conversion character (ex: the 'd' in \"%d\") is described\n"
"** by an instance of the following structure\n"
"*/\n"
"typedef struct s_info {   /* Information about each format field */\n"
"  int  fmttype;              /* The format field code letter */\n"
"  int  base;                 /* The base for radix conversion */\n"
"  char *charset;             /* The character set for conversion */\n"
"  int  flag_signed;          /* Is the quantity signed? */\n"
"  char *prefix;              /* Prefix on non-zero values in alt format */\n"
"  enum e_type type;          /* Conversion paradigm */\n"
"} info;\n"
"\n"
"/*\n"
"** The following table is searched linearly, so it is good to put the\n"
"** most frequently used conversion types first.\n"
"*/\n"
"static info fmtinfo[] = {\n"
"  { 'd',  10,  \"0123456789\",       1,    0, RADIX,      },\n"
"  { 's',   0,  0,                  0,    0, STRING,     }, \n"
"  { 'q',   0,  0,                  0,    0, TCLESCAPE,  },\n"
"  { 'z',   0,  0,                  0,    0, MEM_STRING, },\n"
"  { 'c',   0,  0,                  0,    0, CHARX,      },\n"
"  { 'o',   8,  \"01234567\",         0,  \"0\", RADIX,      },\n"
"  { 'u',  10,  \"0123456789\",       0,    0, RADIX,      },\n"
"  { 'x',  16,  \"0123456789abcdef\", 0, \"x0\", RADIX,      },\n"
"  { 'X',  16,  \"0123456789ABCDEF\", 0, \"X0\", RADIX,      },\n"
"  { 'r',  10,  \"0123456789\",       0,    0, ORDINAL,    },\n"
"  { 'f',   0,  0,                  1,    0, FLOAT,      },\n"
"  { 'e',   0,  \"e\",                1,    0, EXP,        },\n"
"  { 'E',   0,  \"E\",                1,    0, EXP,        },\n"
"  { 'g',   0,  \"e\",                1,    0, GENERIC,    },\n"
"  { 'G',   0,  \"E\",                1,    0, GENERIC,    },\n"
"  { 'i',  10,  \"0123456789\",       1,    0, RADIX,      },\n"
"  { 'n',   0,  0,                  0,    0, SIZE,       },\n"
"  { '%',   0,  0,                  0,    0, PERCENT,    },\n"
"  { 'b',   2,  \"01\",               0, \"b0\", RADIX,      }, /* Binary notation */\n"
"  { 'p',  10,  \"0123456789\",       0,    0, RADIX,      }, /* Pointers */\n"
"  { '\\'',  0,  0,                  0,    0, CHARLIT,    }, /* Literal char */\n"
"};\n"
"#define NINFO  (sizeof(fmtinfo)/sizeof(info))  /* Size of the fmtinfo table */\n"
"\n"
"/*\n"
"** If NOFLOATINGPOINT is defined, then none of the floating point\n"
"** conversions will work.\n"
"*/\n"
"#ifndef NOFLOATINGPOINT\n"
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
"static int getdigit(double *val, int *cnt){\n"
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
"#define BUFSIZE 1000  /* Size of the output buffer */\n"
"\n"
"/*\n"
"** The root program.  All variations call this core.\n"
"**\n"
"** INPUTS:\n"
"**   func   This is a pointer to a function taking three arguments\n"
"**            1. A pointer to the list of characters to be output\n"
"**               (Note, this list is NOT null terminated.)\n"
"**            2. An integer number of characters to be output.\n"
"**               (Note: This number might be zero.)\n"
"**            3. A pointer to anything.  Same as the \"arg\" parameter.\n"
"**\n"
"**   arg    This is the pointer to anything which will be passed as the\n"
"**          third argument to \"func\".  Use it for whatever you like.\n"
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
"  void (*func)(char*,int,void*),\n"
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
"  info *infop;              /* Pointer to the appropriate info structure */\n"
"  char buf[BUFSIZE];        /* Conversion buffer */\n"
"  char prefix;              /* Prefix character.  \"+\" or \"-\" or \" \" or '\\0'. */\n"
"  int  errorflag = 0;       /* True if an error is encountered */\n"
"  enum e_type xtype;        /* Conversion paradigm */\n"
"  char *zMem;               /* String to be freed */\n"
"  char *zExtra;             /* Extra memory used for TCLESCAPE conversions */\n"
"  static char spaces[] = \"                                                  \"\n"
"     \"                                                                      \";\n"
"#define SPACESIZE (sizeof(spaces)-1)\n"
"#ifndef NOFLOATINGPOINT\n"
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
"      (*func)(bufpt,amt,arg);\n"
"      count += amt;\n"
"      if( c==0 ) break;\n"
"    }\n"
"    if( (c=(*++fmt))==0 ){\n"
"      errorflag = 1;\n"
"      (*func)(\"%\",1,arg);\n"
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
"    if( width > BUFSIZE-10 ){\n"
"      width = BUFSIZE-10;\n"
"    }\n"
"    /* Get the precision */\n"
"    if( c=='.' ){\n"
"      precision = 0;\n"
"      c = *++fmt;\n"
"      if( c=='*' ){\n"
"        precision = va_arg(ap,int);\n"
"#ifndef COMPATIBILITY\n"
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
"      if( precision>BUFSIZE-40 ) precision = BUFSIZE-40;\n"
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
"    for(idx=0; idx<NINFO; idx++){\n"
"      if( c==fmtinfo[idx].fmttype ){\n"
"        infop = &fmtinfo[idx];\n"
"        break;\n"
"      }\n"
"    }\n"
"    /* No info entry found.  It must be an error. */\n"
"    if( infop==0 ){\n"
"      xtype = ERROR;\n"
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
"      case ORDINAL:\n"
"      case RADIX:\n"
"        if( flag_long )  longvalue = va_arg(ap,long);\n"
"	else             longvalue = va_arg(ap,int);\n"
"#ifdef COMPATIBILITY\n"
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
"        bufpt = &buf[BUFSIZE];\n"
"        if( xtype==ORDINAL ){\n"
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
"        length = (long)&buf[BUFSIZE]-(long)bufpt;\n"
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
"        length = (long)&buf[BUFSIZE]-(long)bufpt;\n"
"        break;\n"
"      case FLOAT:\n"
"      case EXP:\n"
"      case GENERIC:\n"
"        realvalue = va_arg(ap,double);\n"
"#ifndef NOFLOATINGPOINT\n"
"        if( precision<0 ) precision = 6;         /* Set default precision */\n"
"        if( precision>BUFSIZE-10 ) precision = BUFSIZE-10;\n"
"        if( realvalue<0.0 ){\n"
"          realvalue = -realvalue;\n"
"          prefix = '-';\n"
"	}else{\n"
"          if( flag_plussign )          prefix = '+';\n"
"          else if( flag_blanksign )    prefix = ' ';\n"
"          else                         prefix = 0;\n"
"	}\n"
"        if( infop->type==GENERIC && precision>0 ) precision--;\n"
"        rounder = 0.0;\n"
"#ifdef COMPATIBILITY\n"
"        /* Rounding works like BSD when the constant 0.4999 is used.  Wierd! */\n"
"        for(idx=precision, rounder=0.4999; idx>0; idx--, rounder*=0.1);\n"
"#else\n"
"        /* It makes more sense to use 0.5 */\n"
"        for(idx=precision, rounder=0.5; idx>0; idx--, rounder*=0.1);\n"
"#endif\n"
"        if( infop->type==FLOAT ) realvalue += rounder;\n"
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
"        ** If the field type is GENERIC, then convert to either EXP\n"
"        ** or FLOAT, as appropriate.\n"
"        */\n"
"        flag_exp = xtype==EXP;\n"
"        if( xtype!=FLOAT ){\n"
"          realvalue += rounder;\n"
"          if( realvalue>=10.0 ){ realvalue *= 0.1; exp++; }\n"
"        }\n"
"        if( xtype==GENERIC ){\n"
"          flag_rtz = !flag_alternateform;\n"
"          if( exp<-4 || exp>precision ){\n"
"            xtype = EXP;\n"
"          }else{\n"
"            precision = precision - exp;\n"
"            xtype = FLOAT;\n"
"          }\n"
"	}else{\n"
"          flag_rtz = 0;\n"
"	}\n"
"        /*\n"
"        ** The \"exp+precision\" test causes output to be of type EXP if\n"
"        ** the precision is too large to fit in buf[].\n"
"        */\n"
"        nsd = 0;\n"
"        if( xtype==FLOAT && exp+precision<BUFSIZE-30 ){\n"
"          flag_dp = (precision>0 || flag_alternateform);\n"
"          if( prefix ) *(bufpt++) = prefix;         /* Sign */\n"
"          if( exp<0 )  *(bufpt++) = '0';            /* Digits before \".\" */\n"
"          else for(; exp>=0; exp--) *(bufpt++) = getdigit(&realvalue,&nsd);\n"
"          if( flag_dp ) *(bufpt++) = '.';           /* The decimal point */\n"
"          for(exp++; exp<0 && precision>0; precision--, exp++){\n"
"            *(bufpt++) = '0';\n"
"          }\n"
"          while( (precision--)>0 ) *(bufpt++) = getdigit(&realvalue,&nsd);\n"
"          *(bufpt--) = 0;                           /* Null terminate */\n"
"          if( flag_rtz && flag_dp ){     /* Remove trailing zeros and \".\" */\n"
"            while( bufpt>=buf && *bufpt=='0' ) *(bufpt--) = 0;\n"
"            if( bufpt>=buf && *bufpt=='.' ) *(bufpt--) = 0;\n"
"          }\n"
"          bufpt++;                            /* point to next free slot */\n"
"	}else{    /* EXP or GENERIC */\n"
"          flag_dp = (precision>0 || flag_alternateform);\n"
"          if( prefix ) *(bufpt++) = prefix;   /* Sign */\n"
"          *(bufpt++) = getdigit(&realvalue,&nsd);  /* First digit */\n"
"          if( flag_dp ) *(bufpt++) = '.';     /* Decimal point */\n"
"          while( (precision--)>0 ) *(bufpt++) = getdigit(&realvalue,&nsd);\n"
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
"      case SIZE:\n"
"        *(va_arg(ap,int*)) = count;\n"
"        length = width = 0;\n"
"        break;\n"
"      case PERCENT:\n"
"        buf[0] = '%';\n"
"        bufpt = buf;\n"
"        length = 1;\n"
"        break;\n"
"      case CHARLIT:\n"
"      case CHARX:\n"
"        c = buf[0] = (xtype==CHARX ? va_arg(ap,int) : *++fmt);\n"
"        if( precision>=0 ){\n"
"          for(idx=1; idx<precision; idx++) buf[idx] = c;\n"
"          length = precision;\n"
"	}else{\n"
"          length =1;\n"
"	}\n"
"        bufpt = buf;\n"
"        break;\n"
"      case STRING:\n"
"      case MEM_STRING:\n"
"        zMem = bufpt = va_arg(ap,char*);\n"
"        if( bufpt==0 ) bufpt = \"(null)\";\n"
"        length = strlen(bufpt);\n"
"        if( precision>=0 && precision<length ) length = precision;\n"
"        break;\n"
"      case TCLESCAPE:\n"
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
"          if( n>BUFSIZE ){\n"
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
"      case ERROR:\n"
"        buf[0] = '%';\n"
"        buf[1] = c;\n"
"        errorflag = 0;\n"
"        idx = 1+(c!=0);\n"
"        (*func)(\"%\",idx,arg);\n"
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
"        while( nspace>=SPACESIZE ){\n"
"          (*func)(spaces,SPACESIZE,arg);\n"
"          nspace -= SPACESIZE;\n"
"        }\n"
"        if( nspace>0 ) (*func)(spaces,nspace,arg);\n"
"      }\n"
"    }\n"
"    if( length>0 ){\n"
"      (*func)(bufpt,length,arg);\n"
"      count += length;\n"
"    }\n"
"    if( xtype==MEM_STRING && zMem ){\n"
"      Tcl_Free(zMem);\n"
"    }\n"
"    if( flag_leftjustify ){\n"
"      register int nspace;\n"
"      nspace = width-length;\n"
"      if( nspace>0 ){\n"
"        count += nspace;\n"
"        while( nspace>=SPACESIZE ){\n"
"          (*func)(spaces,SPACESIZE,arg);\n"
"          nspace -= SPACESIZE;\n"
"        }\n"
"        if( nspace>0 ) (*func)(spaces,nspace,arg);\n"
"      }\n"
"    }\n"
"    if( zExtra ){\n"
"      Tcl_Free(zExtra);\n"
"    }\n"
"  }/* End for loop over the format string */\n"
"  return errorflag ? -1 : count;\n"
"} /* End of function */\n"
"/*\n"
"** The following section of code handles the mprintf routine, that\n"
"** writes to memory obtained from malloc().\n"
"*/\n"
"\n"
"/* This structure is used to store state information about the\n"
"** write in progress\n"
"*/\n"
"struct sgMprintf {\n"
"  char *zBase;     /* A base allocation */\n"
"  char *zText;     /* The string collected so far */\n"
"  int  nChar;      /* Length of the string so far */\n"
"  int  nAlloc;     /* Amount of space allocated in zText */\n"
"};\n"
"\n"
"/* The xprintf callback function. */\n"
"static void mout(char *zNewText, int nNewChar, void *arg){\n"
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
"**\n"
"** We changed the name to TclMPrint() to conform with the Tcl private\n"
"** routine naming conventions.\n"
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
"**\n"
"** The name is changed to TclVMPrintf() to conform with Tcl naming\n"
"** conventions.\n"
"*/\n"
"char *vmprintf(\n"
"  const char *zFormat,\n"
"  va_list ap\n"
"){\n"
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
"*/\n"
"static void dstringout(char *zNewText, int nNewChar, void *arg){\n"
"  Tcl_DString *str = (Tcl_DString*)arg;\n"
"  Tcl_DStringAppend(str,zNewText,nNewChar);\n"
"}\n"
"\n"
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
"  if( Et_EvalTrace ) printf(\"%d %s\\n\",result,interp->result);\n"
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
"  if( Et_EvalTrace ) printf(\"%d %s\\n\",result,interp->result);\n"
"  Tcl_Free(zCmd);\n"
"  return result;\n"
"}\n"
"\n"
"/*\n"
"** Set the result of an interpreter\n"
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
"/*\n"
"** Hash a filename.  The value returned is appropriate for\n"
"** indexing into the Et_FileHashTable[] array.\n"
"*/\n"
"static int FilenameHash(char *zName){\n"
"  int h = 0;\n"
"  while( *zName ){\n"
"    h = h ^ (h<<5) ^ *(zName++);\n"
"  }\n"
"  if( h<0 ) h = -h;\n"
"  return h % (sizeof(Et_FileHashTable)/sizeof(Et_FileHashTable[0]));\n"
"}\n"
"\n"
"/*\n"
"** Initialize the file hash table\n"
"*/\n"
"static void FilenameHashInit(void){\n"
"  int i;\n"
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
"** Locate the text of a file given its name.  Return 0 if not found.\n"
"*/\n"
"static char *FindBuiltinFile(char *zName){\n"
"  int h;\n"
"  struct EtFile *p;\n"
"\n"
"  h = FilenameHash(zName);\n"
"  p = Et_FileHashTable[h];\n"
"  while( p && strcmp(p->zName,zName)!=0 ){ p = p->pNext; }\n"
"  return p ? p->zData : 0;\n"
"}\n"
"\n"
"/*\n"
"** An overloaded version of \"source\".  First check for the file\n"
"** in the file table, then go to disk.\n"
"*/\n"
"static int Et_Source(ET_TCLARGS){\n"
"  char *z;\n"
"\n"
"  if( argc!=2 ){\n"
"    Et_ResultF(interp,\"wrong # args: should be \\\"%s FILENAME\\\"\", argv[0]);\n"
"    return TCL_ERROR;\n"
"  }\n"
"  z = FindBuiltinFile(argv[1]);\n"
"  if( z ){\n"
"    int rc;\n"
"#if ET_SHROUD_KEY>0\n"
"    if( z[0]=='y' ){\n"
"      int xor = ET_SHROUD_KEY;\n"
"      char *z2 = z;\n"
"      z[0] = 'n';\n"
"      for(z2++; *z2; z2++){\n"
"        if( *z2>=0x20 ){ *z2 ^= xor; xor = (xor+1)&0x1f; }\n"
"      }\n"
"    }\n"
"    z++;\n"
"#endif\n"
"    rc = Tcl_Eval(interp,z);\n"
"    if (rc == TCL_ERROR) {\n"
"      char msg[200];\n"
"      sprintf(msg, \"\\n    (file \\\"%.150s\\\" line %d)\", argv[1],\n"
"        interp->errorLine);\n"
"      Tcl_AddErrorInfo(interp, msg);\n"
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
"/*\n"
"** An overloaded version of \"file exists\".  First check for the file\n"
"** in the file table, then go to disk.\n"
"*/\n"
"static int Et_FileExists(ET_TCLARGS){\n"
"  int i, rc;\n"
"  Tcl_DString str;\n"
"  if( argc==3 && strncmp(argv[1],\"exis\",4)==0 ){\n"
"    if( FindBuiltinFile(argv[2])!=0 ){\n"
"      interp->result = \"1\";\n"
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
"\n"
"/*\n"
"** This is the main Tcl interpreter.\n"
"*/\n"
"Tcl_Interp *Et_Interp = 0;\n"
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
"  \"    puts stderr $ei\\n\"\n"
"  \"  } else {\\n\"\n"
"  \"    puts stderr $err\\n\"\n"
"  \"  }\\n\"\n"
"  \"  exit\\n\"\n"
"  \"}\\n\"\n"
";\n"
"\n"
"/*\n"
"** Do the initialization.\n"
"**\n"
"** This routine is called after the interpreter is created, and\n"
"** after Et_PreInit() is run, but before Et_AppInit() is run.\n"
"*/\n"
"static int Et_DoInit(Tcl_Interp *interp){\n"
"  int i;\n"
"  extern int Et_PreInit(Tcl_Interp*);\n"
"  extern int Et_AppInit(Tcl_Interp*);\n"
"\n"
"  FilenameHashInit();\n"
"  if( sizeof(Et_FileSet)>1 ){\n"
"    Tcl_Eval(interp,\"rename file Et_FileCmd\");\n"
"    Tcl_CreateCommand(interp,\"source\",Et_Source,0,0);\n"
"    Tcl_CreateCommand(interp,\"file\",Et_FileExists,0,0);\n"
"  }\n"
"  Et_Interp = interp;\n"
"#ifdef ET_TCL_LIBRARY\n"
"  Tcl_SetVar(interp,\"tcl_library\",ET_TCL_LIBRARY,TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar2(interp,\"env\",\"TCL_LIBRARY\",ET_TCL_LIBRARY,TCL_GLOBAL_ONLY);\n"
"#endif\n"
"#ifdef ET_TK_LIBRARY\n"
"  Tcl_SetVar(interp,\"tk_library\",ET_TK_LIBRARY,TCL_GLOBAL_ONLY);\n"
"  Tcl_SetVar2(interp,\"env\",\"TK_LIBRARY\",ET_TK_LIBRARY,TCL_GLOBAL_ONLY);\n"
"#endif\n"
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
"#if ET_HAVE_APPINIT\n"
"  if( Et_AppInit(interp) == TCL_ERROR ){\n"
"    goto initerr;\n"
"  }\n"
"#endif\n"
"  if( zEtTclCode[0] && Tcl_Eval(interp,zEtTclCode)!=TCL_OK ){\n"
"    goto initerr;\n"
"  }\n"
"#ifdef ET_MAIN_SCRIPT\n"
"  if( Et_EvalF(interp,\"source \\\"%q\\\"\", ET_MAIN_SCRIPT)!=TCL_OK ){\n"
"    goto initerr;\n"
"  }\n"
"#endif\n"
"  return TCL_OK;\n"
"\n"
"initerr:\n"
"  Et_EvalF(interp,\"Et_Bgerror \\\"%q\\\"\", interp->result);\n"
"  return TCL_ERROR;\n"
"}\n"
"\n"
"\n"
"#if ET_READ_STDIN==0 || ET_AUTO_FORK!=0\n"
"/*\n"
"** Initialize everything.\n"
"*/\n"
"static int Et_Local_Init(int argc, char **argv){\n"
"  Tcl_Interp *interp;\n"
"  char *args;\n"
"  char buf[100];\n"
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
"  Tcl_Eval(interp,\"while 1 {vwait forever}\");\n"
"  /* Tcl_DeleteInterp(interp); */\n"
"  /* Tcl_Exit(0); */\n"
"  return 0;\n"
"}\n"
"#endif\n"
"\n"
"/*\n"
"** This routine is called to do the complete initialization.\n"
"*/\n"
"int Et_Init(int argc, char **argv){\n"
"#if ET_READ_STDIN==0 || ET_AUTO_FORK!=0\n"
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
"#if !ET_HAVE_MAIN\n"
"/*\n"
"** Main routine for UNIX programs\n"
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
;
