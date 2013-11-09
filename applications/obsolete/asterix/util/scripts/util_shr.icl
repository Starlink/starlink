define  AFL(ink)      $AST_BIN/util_mono
define  ARITH(metic)  $AST_BIN/util_mono 
define  ASHO(w)       $AST_BIN/util_mono
define  ASMOO(th)     $AST_BIN/util_mono
define  ASTC(onv)     $AST_BIN/util_mono
define  AXCEN(troid)  $AST_BIN/util_mono
define  AXCO(nv)      $AST_BIN/util_mono
define  AXF(lip)      $AST_BIN/util_mono
define  AXOR(der)     $AST_BIN/util_mono
define  AXSH(ow)      $AST_BIN/util_mono
define  BINGRP        $AST_BIN/util_mono
define  BINLIST       $AST_BIN/util_mono
define  BINMER(ge)    $AST_BIN/util_mono 
define  BINSUB(set)   $AST_BIN/util_mono 
define  BINSUM        $AST_BIN/util_mono 
define  COMP(are)     $AST_BIN/util_mono
define  CQUA(lity)    $AST_BIN/util_mono
define  ENMAP         $AST_BIN/util_mono 
define  EVBIN         $AST_BIN/util_mono 
define  EVLIST        $AST_BIN/util_mono 
define  EVMER(ge)     $AST_BIN/util_mono
define  EVPOL(ar)     $AST_BIN/util_mono
define  EVSOR(t)      $AST_BIN/util_mono
define  EVSUB(set)    $AST_BIN/util_mono 
define  EVCSUB(set)   $AST_BIN/util_mono 
define  EXPO(rt)      $AST_BIN/util_mono
define  FREQ(uency)   $AST_BIN/util_mono 
define  HDS2(text)    $AST_BIN/util_mono
define  HEAD(er)      $AST_BIN/util_mono
define  HIST          $AST_BIN/util_mono 
define  HISTMODE      $AST_BIN/util_mono 
define  IMPORT        $AST_BIN/util_mono 
define  INTER(p)      $AST_BIN/util_mono 
define  KSTAT         $AST_BIN/util_mono 
define  MAGIC         $AST_BIN/util_mono
define  MASK          $AST_BIN/util_mono
define  MEANDAT       $AST_BIN/util_mono
define  OPER(ate)     $AST_BIN/util_mono 
define  POLYF(it)     $AST_BIN/util_mono
define  PROJ(ect)     $AST_BIN/util_mono
define  QUAL(ity)     $AST_BIN/util_mono
define  RATIO         $AST_BIN/util_mono 
define  REBIN         $AST_BIN/util_mono 
define  REGIO(ns)     $AST_BIN/util_mono 
define  SCATT(ergram) $AST_BIN/util_mono 
define  SETRAN(ges)   $AST_BIN/util_mono 
define  SIGNIF        $AST_BIN/util_mono 
define  SLICE         $AST_BIN/util_mono 
define  SLOTMRG       $AST_BIN/util_mono 
define  SMOOTH        $AST_BIN/util_mono 
define  SPLIN(fit)    $AST_BIN/util_mono
define  STAT(istix)   $AST_BIN/util_mono 
define  SYSERR        $AST_BIN/util_mono 
define  TEXT2(hds)    $AST_BIN/util_mono

defstring CIGN(ore)     CQUALITY IGNORE=y OVER=y SET=n RESTORE=n
defstring CREST(ore)    CQUALITY RESTORE=y OVER=y SET=n IGNORE=n
defstring CSETQ(ual)    CQUALITY SET=y IGNORE=n RESTORE=n
defstring ARDQUAL       QUALITY IGNORE=y FSEL=y
defstring IGNORE        QUALITY IGNORE=y OVER=y SET=n RESTORE=n
defstring RESTORE       QUALITY RESTORE=y OVER=y SET=n IGNORE=n
defstring SETQ(ual)     QUALITY SET=y IGNORE=n RESTORE=n
defstring DTREND        POLYFIT DTREND=y
defstring SUBTRACT      ARITHMETIC OPER="-"
defstring ADD           ARITHMETIC OPER="+"
defstring MULTIPLY      ARITHMETIC OPER="*"
defstring DIVIDE        ARITHMETIC OPER="/"
defstring MKCOLIM       MEANDAT NFILES=3 WTMETH=U AVERAGE=NO~
                                  WEIGHT1=1 WEIGHT2=6 WEIGHT3=36

defstring HISTO(RY)     HIST

{define GLOBAL procedure to list globals}
defproc   glob(al)      $AST_BIN/global.icl
