Instructions for releasing ERFA
===============================

* Clone the ERFA repository from github (if you haven't already done so),
  and change to the ERFA directory.

* Make sure you are on the "master" branch from the "liberfa" github 
  repository and have the latest version (if you have a fresh clone, this
  should already be the case).

* If a new version of SOFA exists, run `sofa_deriver.py` from the `erfa-fetch
  repository`_ in its own directory.  That will create a directory called `erfa`
  inside the `erfa-fetch` directory, and   you should copy its contents to the 
  `src` directory of `erfa`.  Use ``git diff`` in `erfa` to inspect the changes, 
  and then commit and push them to github.

* Update the version number in the `AC_INIT` macro of `configure.ac` to
  the version number you are about to release, and also update the version 
  mentioned in `README.rst`. Follow the instructions in 
  `Version numbering` below.

* Update the version info of the shared library in the `ERFA_LIB_VERSION_INFO` 
  macro of `configure.ac`. Follow the instructions in `Version numbering` below.

* Commit these changes using ``git commit``, with a commit message like 
  ``Preparing release v0.0.1``.

* Run `./bootstrap.sh`: you need `automake`, `autoconf` and `libtool` 
  installed.  If no errors appear, this will create a new `./configure`
  file.

* Run ``./configure``, which should create a `Makefile` in the top level 
  directory and in ./src

* Run ``make check``, which will build the library and run the tests -
  make sure they pass before proceeding.

* Run ``make distcheck``: this creates the distribution tarball, 
  unpackages it and runs the check inside the untarred directory.
  The resulting tarball will be named e.g., `erfa-0.0.1.tar.gz` and
  will be placed in the working directory.

* Tag the current commit with the version number.  A signed tag is preferred if you have an a signing key (e.g., do ``git tag -s v0.0.1``).  

* Push up your changes and the new tag to github: 
  ``git push --tags origin master``. (The command here assumes the git remote
  "origin" points to the "liberfa/erfa" repository.  If not, substitute the
  appropriate name.)

* Go to the "liberfa/erfa" repository for the github page, and click on the
  "releases" button, and then the release corresponding to the tag you just 
  made. 

* Click on the "Draft release notes or downloads" button (or it might be "Edit release").  Put the version number as
  the title (e.g., ``v0.0.1``)and for the description put 
  ``See `README.rst` for release notes.``

* Upload the tarball you created (e.g., `erfa-0.0.1.tar.gz`) by dropping it
  in the area that says "Attach binaries for this release  by dropping them 
  here." 

* Click the "Publish release" button.

* Update the release listing on Github Pages to include this release:
  Do ``git checkout gh-pages``, add a new ``<li>...</li>`` entry for the
  release in `index.html`, do ``git commit``, and then
  ``git push origin gh-pages``.

Version numbering
=================

ERFA needs to provide two different version numbers.  You need to update both.
The first is the 
**package version number** or **version number** proper. ERFA uses 
`semantic versioning <http://semver.org/>`_ to create this number.
For more on this choice, see 
`liberfa/erfa#6 <https://github.com/liberfa/erfa/issues/6>`_.

The second number is `shared library version info`. When a program has been 
linked with the ERFA shared library, the dynamic linker checks the version
info of the library requested by the program with those of the libraries 
present if the system. This version info is important to binary distributions
(such as Linux distributions). ERFA uses `libtool versioning <http://www.gnu.org/software/libtool/manual/html_node/Libtool-versioning.html>`_.


Package version number
----------------------

Semantic versioning dictates how to change the version number according to
changes to the API of the library. In the case of ERFA the API is:

  * The public C macros defined in erfam.h
  * The names, return types, number of arguments and types of the functions in erfa.h

To update the package version, the release manager has to check the relevant
information about the release, such as:

  * upstream SOFA documentation in http://www.iausofa.org/current_changes.html
  * relevant bug reports in the github project page

If the version is given in the form MAJOR.MINOR.PATCH, then

  * if there is a backwards incompatible change (function removed, types of
    arguments altered, macros renamed...) then increase MAJOR by one and set 
    the others to zero.
  * else if there is backwards compatible change (new function added or 
    new macro added) then do not change MAJOR, increase MINOR by one and 
    set PATCH to zero.
  * else
        you are either fixing a bug or making other improvements. Increase
        patch by one and do not change the others.

Change the version number in the `AC_INIT` macro and in `README.rst`

Shared library version info
---------------------------

For the shared  library version info, we are only interested in a subset of
the API, the **interfaces of the shared library**. As the C macros are 
interpolated away at compile time, the interfaces in the ERFA 
shared library are:

  * The names, return types, number of arguments and types of the functions 
  
Again, the release manager has to review the relevant information:

  * upstream SOFA documentation in http://www.iausofa.org/current_changes.html
  * relevant bug reports in the github project page

The shared library version info is stored in three numbers called *current*, *revision* and *age*. These numbers appear in the macro `ERFA_LIB_VERSION_INFO` in
the mentioned order.

If the version is given in the form CURRENT,REVISION,AGE then

  * if there is a backwards incompatible change (function removed, types of
    arguments altered...) then increase CURRENT by one and set 
    the others to zero (c,r,a -> c+1,0,0).
  * else if there is backwards compatible change (new function added)
    then increase both CURRENT and AGE by one, set REVISON to zero 
    (c,r,a -> c+1,0,a+1).
  * else if the library code has been modified at all
    then increase REVISION by one (c,r,a -> c,r+1,a)
  * else
       do not change the version info (c,r,a -> c,r,a)

Change the verion info in `ERFA_LIB_VERSION_INFO`

Examples
---------
We start with ERFA version 1.0.0 and library version info 0,0,0

* SOFA makes a new release. A function is added and two functions change their
  arguments. This is a backawars incompatible change, so the new package will
  have version 2.0.0 and the shared library version info will be 1,0,0

* We forgot to add README.rst to the release. We make a new one. The change
  is a bugfix (no API changes), the new release will be 2.0.1. The shared
  library version is not modified (no changes in the library source code).

* SOFA makes a new release. They just add a new function. The new package
  version will be 2.1.0. The shared library info will be 2,0,1 (both current
  and age are incremented).

* SOFA makes a new relase fixing some bugs in the code without changing the 
  API. New package version is 2.1.1. The shared library version is 2,1,1

* A contributor finds a bug in ERFA. The fix doesn't change the API. New
  package version is 2.1.2. The shared library version is 2,2,1

* SOFA makes a new release incorporating the bug fix and adding new functions.
  The new package version is 2.2.0. The shared library version is 3,0,2

* SOFA makes a new release removing functions. This is a backawars 
  incompatible change, so the new package will
  have version 3.0.0 and the shared library version info will be 4,0,0

.. _erfa-fetch repository: https://github.com/liberfa/erfa-fetch
