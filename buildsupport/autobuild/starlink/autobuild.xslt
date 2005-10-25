<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
debug = 0
checkout-source = 1
nice-level = 0
#control-file = build.sh
control-file = /home/vmwareshare/build.sh
# Force abortion of build when a module fails
# Not usually needed since builder is clever
# enough to skip a module if a dependency fails
abort-on-fail = 0

# Hostname, if different from that returned by 'hostname' command
# hostname = builder.example.com

tmp-dir = /home/vmwareshare/temp

lock = {
  file = /home/vmwareshare/rhel30linux_i386/.build.mutex
  use-flock = 1
}

build = {
  # This is where we check out the source to (for CVS anyway).
  home = /home/vmwareshare/rhel30linux_i386/build-home

  # ./configure --prefix=$AUTO_BUILD_ROOT/usr
  root = /home/vmwareshare/rhel30linux_i386/build-root
  
  # Cache packages which don't change
  cache-dir = /home/vmwareshare/rhel30linux_i386/build-cache
  cache = 1
  cache-timestamp = 1
}

# Module groups (for web status)
groups = {
  Applications = {
      label = Applications
  }
  Libraries = {
      label = Libraries
  }
  Buildsupport = {
      label = Buildsupport
  }
  Thirdparty = {
      label = Thirdparty
  }
  etc = {
      label = etc
  }
  Docs = {
      label = Docs
  }
  Java = {
      label = Java
  }
}

# Global environment variables
env = {
  USER = vmware
  STARCONF_DEFAULT_STARLINK = /home/vmwareshare/rhel30linux_i386/build-root
  STARCONF_DEFAULT_PREFIX = /home/vmwareshare/rhel30linux_i386/build-root
#  PATH = ${STARCONF_DEFAULT_PREFIX}/bin:${PATH}
#  PATH = ${STARCONF_DEFAULT_PREFIX}/buildsupport/bin:${PATH}
}

# Code repositories
repositories = {
  cvs = {
    label = Starlink Anonymous CVS Server
    module = Test::AutoBuild::Repository::CVS
    env = {
      CVSROOT = /home/vmwareshare/cvs
    }
  }
}

# The various package types to distribute
package-types = {
#  rpm = {
#    label = Linux RPMs
#    spool = /home/vmwareshare/rhel30linux_i386/packages/rpm
#    extension = .rpm
#  }
#  pkg = {
#    label = Solaris packages
#    spool = /home/vmwareshare/rhel30linux_i386/packages/pkg
#    extension = .pkg
#  }
#  zip = {
#    label = ZIP packages
#    spool = /home/vmwareshare/rhel30linux_i386/packages/zips
#    extension = .zip
#  }
  tgz = {
    label = Tar+GZip packages
    spool = /home/vmwareshare/rhel30linux_i386/packages/tars
    extension = .tar.gz
  }
#  deb = {
#    label = Debian Packages
#    spool = /home/vmwareshare/rhel30linux_i386/packages/debian
#    extension = .deb
#  }
}

publishers = {
  copy = {
    label = ussc@star.rl.ac.uk
    module = Test::AutoBuild::Publisher::Copy
  }
}

  modules = {
    buildsupport = {
      label = buildsupport
      paths = (
       buildsupport
      )
      repository = cvs
      group = Buildsupport
      links = (
       {
         href = http://www.starlink.ac.uk
         label = Starlink Homepage
       }
      )
     controlfile = /home/vmwareshare/buildsupport.sh
    }
    <xsl:for-each select="componentset/component">
    <xsl:if test="@id != 'starconf' and @id != 'automake'
                  and @id != 'autoconf' and @id != 'm4'
                  and @id != 'libtool' and @id != 'ssn78'
                  and @status != 'obsolete'">
      <xsl:value-of select="@id"/> = {
      label = <xsl:value-of select="@id"/>
      paths = (
       <xsl:value-of select="path"/>
      )
      repository = cvs
      <xsl:if test="starts-with(path,'applications')">
      group = Applications
      </xsl:if>
      <xsl:if test="starts-with(path,'etc')">
      group = etc
      </xsl:if>
      <xsl:if test="starts-with(path,'libraries')">
      group = Libraries
      </xsl:if> 
      <xsl:if test="starts-with(path,'docs')">
      group = Docs
      </xsl:if> 
      <xsl:if test="starts-with(path,'thirdparty')">
      group = Thirdparty
      </xsl:if>
      <xsl:if test="starts-with(path,'java')">
      group = Java
      </xsl:if>                        
      links = (
        {
         href = http://www.starlink.ac.uk
         label = Starlink Homepage
        }
      )
      depends = (
        <xsl:for-each select="dependencies/build">
          <xsl:sort order="ascending"/>
          <xsl:if test="not(following-sibling::build = .)">
          <xsl:text>&#10;        </xsl:text>
          <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
       <xsl:for-each select="dependencies/sourceset">
          <xsl:sort order="ascending"/>
          <xsl:if test="not(following-sibling::sourceset = .)">
          <xsl:text>&#10;        </xsl:text>
          <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
        buildsupport
      )
      controlfile = /home/vmwareshare/build.sh
    }
    </xsl:if>
    </xsl:for-each>
   
  }
  
# What to do when we finish a run
output = {
  # Send an email alert on failure
  email = {
    module = Test::AutoBuild::Output::EmailAlert
    label = Starlink Classic CVS Build
    options = {
      template-dir = /usr/etc/auto-build.d/templates
      url = http://dev.starlink.ac.uk/build
      addresses = stardev@jiscmail.ac.uk
      smtp_server = outbox.rl.ac.uk
      sender = ussc@star.rl.ac.uk
      send-on-success = true
    }
  }
  # Copy files to a ftp site
  ftp = {
    module = Test::AutoBuild::Output::PackageCopier
    label = FTP Site
    options = {
      directory = /home/vmwareshare/rhel30linux_i386/public_ftp
    }
  }
  # Copy files to a HTTP site
  http = {
    module = Test::AutoBuild::Output::PackageCopier
    label = Starlink Development Web
    options = {
      directory = /home/vmwareshare/rhel30linux_i386/public_html/dist
    }
  }
  # Copy logs to HTTP site
  log = {
    module = Test::AutoBuild::Output::LogCopier
    label = Build Log Files
    options = {
      directory = /home/vmwareshare/rhel30linux_i386/public_html/logs
    }
  }
  # Copy artifacts to HTTP site
#  artifacts = {
#    module = Test::AutoBuild::Output::ArtifactCopier
#    label = Build Artifacts
#    options = {
#      directory = /home/vmwareshare/rhel30linux_i386/public_html/artifacts/%m
#    }
#  }
  # Create an ISO image containing several modules
#  iso = {
#    module = Test::AutoBuild::Output::ISOBuilder
#    label = CD ISO image builder
#    options = {
#      variables = {
#        httppath = /~builder
#        defaultCSS = bluecurve.css
#        adminEmail = admin@builder.example.com
#        adminName = Build Administrator
#        title = Continuous Automatic Builder
#      }
#      scratch-dir = /var/tmp
#      iso-dest-dir = /home/vmwareshare/rhel30linux_i386/public_html/isos
#      template-dest-dir = /home/vmwareshare/rhel30linux_i386/public_html
#      template-src-dir = /usr/etc/auto-build.d/templates
#      files = (
#          index-iso.html
#      )
#      images = {
#        autobuild-unstable = {
#          name = autobuild-unstable.iso
#          label = Test-AutoBuild Unstable
#          package-types = (
#            rpm
#            zip
#          )
#          modules = (
#            autobuild-dev
#          )
#        }
#      }
#    }
#  }
  
  # Generate HTML status pages
  status = {
    module = Test::AutoBuild::Output::HTMLStatus
    label = Classic CVS Nightly Build Status
    options = {
      variables = {
        httppath = /classic
        defaultCSS = bluecurve.css
        adminEmail = ussc@star.rl.ac.uk
        adminName = Starlink Software
        title = Starlink Continuous Automatic Nightly Build
      }
      template-src-dir = /usr/etc/auto-build.d/templates
      template-dest-dir = /home/vmwareshare/rhel30linux_i386/public_html
# Placeholders in file names are:
#  %m -> module name
#  %g -> group
#  %r -> repository
#  %p -> package type
      files = (
         {
           src = index-group.html
           dst = index.html
         }
         index-module.html
         index-repository.html
         {
           src = module.html
           dst = module-%m.html
         }
         index.rss
         bluecurve.css
         redhat.css
      )
    }
  }
}

# End of file

</xsl:template>

</xsl:stylesheet>
