package Override;

use Exporter ();
use Cwd;
@ISA = qw(Exporter);
@EXPORT_OK = qw($DEBUG getpwuid link setenv getenv symlink rename
                getcwd syswait find_executable absolute_path
                substitute_var_val $LATEX2HTMLDIR $TEMPDIR
		make_directory_absolute unlink $dd $envkey $image_pre);

$OS = $^O;                      # MSWin32, linux, OS/2 (or Warp? don't
                                # know)

if ($OS =~ os2) {
    $envkey = ';';
    $dd = '/';
    $image_pre = ''; # protect against long file-names
} elsif ($OS =~ dos) {
    $envkey = ';';
    $dd = '/';
    $image_pre = 'ps';
} elsif ($OS =~ macOS) {
    $envkey = '|';
    $dd = ':';
#} elsif ($OS =~ VMS) {
#    $envkey = '???';
#    $dd = '.';
} elsif ($OS =~ MSWin32) {
    require Win32;
    $envkey = ';';
    $dd = '\\';
} else {
    $envkey = ':';
    $dd = '/';
}

$TEMPDIR = &get_tmp_dir;

# Add your function here, add it to @EXPORT_OK and change the list in
# use Override in the scripts.

sub getpwuid {
  if ($OS =~ os2) {
    my $uid = shift;
    my ($username, $passwd, $gid, $realname, $home, $shell, $age, $comment);
    $username = $ENV{'USER'};
    $passwd = "XXXXX";
    $uid = 9999; # not used with os2
    $gid = 8888; # not used with os2
    $age = "not used";
    $comment = "no comment";
    $realname =  $ENV{'USERFULLNAME'};
    $home = $ENV{'HOME'};
    $shell = $ENV{'SHELL'};
    return ($username, $password, $uid, $gid, $age, $comment, $realname
	, $home, $shell);
  } elsif ($OS =~ MSWin32) {
    my $uid = shift;
    my ($username, $passwd, $gid, $realname, $home, $shell, $age, $comment);
    $username = Win32::LoginName();
    $uid = 9999; # not used with win32
    $gid = 8888; # not used with win32
    $passwd = "XXXXX";
    $age = "not used";
    $comment = "no comment";
    my $domain = Win32::DomainName();
    $domain =~ s/[<>]//g; # strip angle brackets
    $realname =  "$username\@$domain";
    $home = $ENV{'HOME'};
    $shell = $ENV{'comspec'};
    return ($username, $password, $uid, $gid, $age, $comment, $realname, $home, $shell);
  } elsif ($OS =~ dos) {
    my $uid = shift;
    my ($username, $passwd, $gid, $realname, $home, $shell, $age, $comment);
    $username = $ENV{'USER'};
    $uid = 9999; # not used with win32
    $gid = 8888; # not used with win32
    $passwd = "XXXXX";
    $age = "not used";
    $comment = "no comment";
    $realname =  $ENV{'USERFULLNAME'};
    $home = $ENV{'HOME'};
    $shell = $ENV{'comspec'};
    return ($username, $password, $uid, $gid, $age, $comment, $realname, $home, $shell);
  }  else {
    # must do this indirectly
    # else some systems return a generic root account
    local($tmp) = 'CORE::getpwuid';
    eval $tmp."(@_);";
  }
}

sub link {
    local ($from, $to) = @_;
    if ($OS =~ os2) {
	system("cp", $from, $to);
    } elsif ($OS =~MSWin32) {
        &main::cp($from, $to);
    } elsif ($OS =~ dos) {
	print "\nLinking $from -> $to\n";
	&cp ($from, $to);
    } else {
	CORE::link($from,$to) ;
    }
}

sub symlink {
    local ($to, $from) = @_;
    if ($OS =~ os2) {
	# No symlinks, so copy
	system("cp", $from, $to);
    } elsif ($OS =~MSWin32) {
        # No symlinks, so copy
        &main::cp($from, $to);
    } elsif ($OS =~ dos) {
        print "\nSym-Linking $from -> $to\n";
        &cp ($from, $to);
    } else {
	CORE::symlink($to,$from)
    }
}


# Is this really needed with DOS ?
# why not use  system("cp", $from, $to );  ?

sub cp {                       # Marcus Hennecke  6/3/96
    local($src, $dest) = @_;
    return unless (-f $src);
    if ( -d $dest ) {
	$src =~ /[^$dd]*$/;
	$dest .= $dd . $&;
	$dest =~ s|$dd$dd||g;
    }
    open(IN, $src) || return; binmode(IN);
    open(OUT, ">$dest") || return; binmode(OUT);
    local($/) = undef;
    print OUT <IN>;
    close(OUT);
    close(IN);
}


# Deprecated ?
sub getenv($) {
  my $envkey = shift;
  if ($OS =~ os2) {
      defined($ENV{$envkey}) ? split (";", $ENV{$envkey}) : undef;
  } else {
    defined($ENV{$envkey}) ? split (":", $ENV{$envkey}) : undef;
  }
}

# Deprecated ?
sub setenv($@) {
  my $envkey = shift;
  if ($OS =~ os2) {
      $ENV{$envkey} = join(";", @_);
  } else {
    $ENV{$envkey} = join(":", @_);
  }
}

# perl uses os2 rename which will complain about existing files, files
# in use and so forth, therefore we use 'cp' instead.
# Note that in many cases unlink fails. The old files must than be deleted
# manually after l2h finishes.
sub rename {
    my ($from,$to) = @_;
    if ($OS =~ /^(os2|MSWin32|dos)$/) {
        if (-e $to) {
	    CORE::unlink($to);
	}
	CORE::rename($from,$to);
    } else {
        CORE::rename($from,$to);
    }
}

sub unlink {
    my ($from) = @_;
#    if ($OS =~ os2) {
#	system("rm", $from);
#    } else {
	CORE::unlink($from) ;
#    }
}

# Given a directory name in either relative or absolute form, returns
# the absolute form.
# Note: The argument *must* be a directory name.
sub make_directory_absolute {
    local($path) = @_;
    local($orig_cwd);
    if ($OS =~ os2) {
	if (!($path =~ /[a-zA-Z]:\Q$dd/)) {   # if $path doesn't start  with 'x:'
	    $orig_cwd = getcwd();
	    chdir $path;
	    $path = getcwd();
	    chdir $orig_cwd;
	}
    } elsif ($OS =~ MSWin32) {
        $path =~ s/\\/${dd}/g;
        if ($path =~ /^\Q$dd\E/) {
            # add the current drive to this path
            Win32::GetCwd() =~ /^([A-Za-z]:)/;
            $path = $1 . $path;
        } elsif ($path =~ /^[A-Za-z]:\Q$dd/) {
        } else {
            $orig_cwd = Win32::GetCwd();
            Win32::SetCwd($path);
            $path = Win32::GetCwd();
            Win32::SetCwd($orig_cwd);
            $path =~ s/\\/${dd}/g;
        }
    } else {
	if (! ($path =~ /^\Q$dd/)) {   # if $path doesn't start with '/'
	    $orig_cwd = &getcwd();
	    chdir $path;
	    $path = &getcwd();
	    chdir $orig_cwd;
	}
    }
    $path;
}

sub getcwd {
    local($_);
    if ($OS =~ 'MSWin32') {
       $_ = Win32::GetCwd();
       $_ =~ s/\\/${dd}/g;
    } elsif ($OS =~ 'os2') {
       $_ = Cwd::getcwd();
    } else {
       $_ = Cwd::getcwd();
       if (! length) {
          $_  = `pwd`;
          die "'pwd' failed (out of memory?)\n"
              unless length;
          chomp;
       }
    }
    $_;
}

if (! defined &fork) {sub fork{return 0}};

sub syswait {
    local($_) = @_;
    print STDERR "\n$_\n" if ($DEBUG||($VERBOSITY > 2));
    if ($OS =~ 'MSWin32') {
# it seems that no command is using specific redirections ...
	system($_);
	return $?;
    } elsif ($OS =~ 'os2') {
	system("cmd.exe", "/c", $_);
    } elsif ($OS =~ dos) {
	local($cmd,$in,$out,$err);
	$cmd = $_[0];
	$in = $_[1];
	$out = $_[2];
	$err = $_[3];
	if (length($in)>0) {
	    open(SI, "<&STDIN");
	    open(STDIN, "<$in");
	    binmode(STDIN);
	}
	if (length($out)>0) {
	    open(SO, ">&STDOUT");
	    open(STDOUT, ">$out");
	    binmode(STDOUT);
	}
	if (length($err)>0) {
	    open(SE, ">&STDERR");
	    open(STDERR, ">$err");
	    binmode(STDERR);
	}
	$errcode = system($cmd);

	if (length($in)>0) {
	    close(STDIN);
	    open(STDIN, "<&SI");
	}
	if (length($out)>0) {
	    close(STDOUT);
	    open(STDOUT, ">&SO");
	}
	if (length($err)>0) {
	    close(STDERR);
	    open(STDERR, ">&SE");
	}
	return $errcode;

    } else {
       local($status);
       if ($child_pid = fork) {
           $status = waitpid($child_pid, 0);
           print "\n *** finished child process: \#$child_pid\n"
               if ($DEBUG||($VERBOSITY > 2));
           $child_pid = 0;
           return($?);
       } else {
           exec($_);
           print "$_[0]:  $!\n";
           exit($!);
       }
    }
}

sub absolute_path {
    local($_) = @_;
    if ($OS =~ MSWin32) {
       return $_ =~ /^([A-Za-z]:)?\Q$dd/;
    } else {
       return $_ =~ /^\Q$dd/;
    }
}

sub find_executable {
    local($fullname);
    local($name,@PATH) = @_;
    if ($OS =~ /os2|MSWin32|dos/) {
       if(&absolute_path($name)) {
           if(-e "$name") {
               $fullname = "$name";
               $fullname =~ s+\Q$dd+\\+;
               return $fullname;
           }
           else {
               return(undef);
           }
       }
       if($name =~ s+.*/++o) {
           warn "Warning: Stripping invalid path from $name\n";
       }
       foreach(split("${envkey}", $ENV{PATH} )) {
           if(-e "$_${dd}$name") {
               $fullname = "$_${dd}$name";
               $fullname =~ s+\Q$dd+\\+;
               return $fullname;
           }
           elsif (-e "$_${dd}$name.exe") {
               $fullname = "$_${dd}$name.exe";
               $fullname =~ s+\Q$dd\E+\\+;
               return $fullname;
           }
       }
       return(undef);
    } else {
       if($name =~ m+^\Q$dd+) {
           if(-e "$name" && -x _ ) {
               return "$name";
           }
       }
       if($name =~ s+.*(\Q$dd\E)++o) {
           warn "Warning: Stripping invalid path from $name\n";
       }
	# in case @PATH is not a list
	if ($#PATH == 0){
	    local($path) = shift @PATH;
	    @PATH = split(/$envkey/,$path);
	}
       foreach(@PATH) {
           if(-e "$_/$name") {
               if( -x _ ) {
                   return "$_/$name";
               } else {
                   warn "Warning: $_/$name exists, but is not executable.\n";
               }
           }
       }
       return(undef);
    }
}

# Changes lines of the form:
# $var=... to
# $var='$val' (or 'PNG', repsectively)
#
sub substitute_var_val {
    my($file, $var, $val) = @_;
    local($SUCCESS) = 0;

    if ( (-f "$LATEX2HTMLDIR${dd}$file") ||
       die "\nCannot find $LATEX2HTMLDIR${dd}$file\n"
	    . "Please check the value of \$LATEX2HTMLDIR in latex2html.config\n") {
       &rename("$LATEX2HTMLDIR${dd}$file","$LATEX2HTMLDIR${dd}$file.bak")
           || die "Cannot rename $LATEX2HTMLDIR${dd}$file\nPERL: $!";
       open(IN, "<$LATEX2HTMLDIR${dd}$file.bak")
           || die "Cannot open $LATEX2HTMLDIR${dd}$file.bak\nPERL: $!";
       open(OUT, ">$LATEX2HTMLDIR${dd}$file")
           || die "Cannot open $LATEX2HTMLDIR${dd}$file\nPERL: $!";
       chmod(0755, "$LATEX2HTMLDIR${dd}$file")
           if (-x "$LATEX2HTMLDIR${dd}$file.bak" && ! $OS =~ MSWin32);
#       $reg = "\\\$" . $var . "\\s*=.*\$";
       while (<IN>) {
           s/$var/do {
		   $SUCCESS = 1;
#		   "\$" . $var . "=" . eval('$val') . ";" .
		   eval('$val') . " \# Inserted by installation script"
		}/e;
           print OUT;
       }
       close IN;
       close OUT;
    }
    $SUCCESS;
}

sub get_tmp_dir {
   local ($TMP);
   if ($OS =~ '(MSWin32|os2)') {
      if (!($TMP)) {
          local (@l) = ("TMPDIR", "TEMP", "TMP", "c${dd}tmp");
          local ($var, $dir);
          foreach $var (@l) {
              $dir = $ENV{$var};
              if (-d $dir) {
                  $TMP = $dir;
                  last;
              }
          }
      }
      $TMP =~ s/\\/${dd}/g;

       if ($TMP =~ /\Q$dd\E$/) {
       } else {
          $TMP .= ${dd}
       }
   } else {
      $TMP = "${dd}tmp";
   }
   $TMP;
}

1;
