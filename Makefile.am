## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-vectors.texi
doc_mmck_vectors_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-vectors.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-vectors.html/
	@: > doc/mmck-vectors.html/$(am__dirstamp)

doc/mmck-vectors.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-vectors.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-vectors.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-vectors.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-vectors.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-vectors.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-vectors.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_VECTORS_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_VECTORS_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_VECTORS_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.vectors.import.$(MMCK_SOEXT)
#
MMCK_VECTORS_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the  shared library "(mmck vectors)".  We  need to build
# and install all of them.
#
MMCK_VECTORS_IMPORT_SHARED_LIBS	=

# This is the "(mmck vectors)" shared library file pathname.
#
MMCK_VECTORS_RUNTIME_SHARED_LIBS	= \
	lib/mmck.vectors.$(MMCK_SOEXT)

## --------------------------------------------------------------------

# This is  a list of types  specification files, one for  each module.
# They are  *not* installed.   They are  concatenated to  generate the
# globl types file.
#
MMCK_VECTORS_TYPES_FILES_MODULES	=

# This is  a list of the  types specification files.  To  be installed
# along with shared libraries.
#
MMCK_VECTORS_TYPES_FILES_SHARED_LIBS	= \
	lib/mmck.vectors.types

## --------------------------------------------------------------------

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_VECTORS_TEMP_FILES		=


#### library (mmck vectors): module version

MMCK_VECTORS_MODULES		+= lib/mmck.vectors.version.scm
MMCK_VECTORS_RUNTIME_OBJECTS	+= lib/mmck.vectors.version.$(OBJEXT)
MMCK_VECTORS_IMPORT_SOURCES	+= lib/mmck.vectors.version.import.scm
MMCK_VECTORS_IMPORT_DEPS		+= lib/mmck.vectors.version.import.$(MMCK_SOEXT)
MMCK_VECTORS_IMPORT_SHARED_LIBS	+= lib/mmck.vectors.version.import.$(MMCK_SOEXT)
MMCK_VECTORS_TYPES_FILES_MODULES	+= lib/mmck.vectors.version.types.scm
MMCK_VECTORS_TEMP_FILES		+= \
	lib/mmck.vectors.version.temp		\
	lib/mmck.vectors.version.lock		\
	lib/mmck.vectors.version.stamp

# A compiler  invocation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.vectors.version.stamp: lib/mmck.vectors.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.vectors.version.temp
	@touch lib/mmck.vectors.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.vectors.version.$(OBJEXT) $(<) -prologue lib/config.scm \
		-emit-types-file lib/mmck.vectors.version.types.scm
	@$(MV) mmck.vectors.version.import.scm lib/mmck.vectors.version.import.scm
	@mv -f lib/mmck.vectors.version.temp $@

lib/mmck.vectors.version.types.scm: lib/mmck.vectors.version.stamp

lib/mmck.vectors.version.import.scm lib/mmck.vectors.version.$(OBJEXT): lib/mmck.vectors.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.vectors.version.lock lib/mmck.vectors.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.vectors.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.vectors.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.vectors.version.stamp; \
	    $$dry rmdir lib/mmck.vectors.version.lock; \
	  else \
	    while test -d lib/mmck.vectors.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.vectors.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.vectors.version.import.$(MMCK_SOEXT): lib/mmck.vectors.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck vectors): module assertions

MMCK_VECTORS_MODULES		+= lib/mmck.vectors.assertions.scm
MMCK_VECTORS_RUNTIME_OBJECTS	+= lib/mmck.vectors.assertions.$(OBJEXT)
MMCK_VECTORS_IMPORT_SOURCES	+= lib/mmck.vectors.assertions.import.scm
MMCK_VECTORS_IMPORT_DEPS		+= lib/mmck.vectors.assertions.import.$(MMCK_SOEXT)
MMCK_VECTORS_IMPORT_SHARED_LIBS	+= lib/mmck.vectors.assertions.import.$(MMCK_SOEXT)
MMCK_VECTORS_TYPES_FILES_MODULES	+= lib/mmck.vectors.assertions.types.scm
MMCK_VECTORS_TEMP_FILES		+= \
	lib/mmck.vectors.assertions.temp	\
	lib/mmck.vectors.assertions.lock	\
	lib/mmck.vectors.assertions.stamp

MMCK_VECTORS_ASSERTIONS_DEPS = \
	$(MMCK_OBJECTS_DEPS)

lib/mmck.vectors.assertions.stamp: lib/mmck.vectors.assertions.scm $(MMCK_VECTORS_ASSERTIONS_DEPS)
	@rm -f lib/mmck.vectors.assertions.temp
	@touch lib/mmck.vectors.assertions.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.vectors.assertions.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.vectors.assertions.types.scm
	@$(MV) mmck.vectors.assertions.import.scm lib/mmck.vectors.assertions.import.scm
	@mv -f lib/mmck.vectors.assertions.temp $@

lib/mmck.vectors.assertions.types.scm: lib/mmck.vectors.assertions.stamp

lib/mmck.vectors.assertions.import.scm lib/mmck.vectors.assertions.$(OBJEXT): lib/mmck.vectors.assertions.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.vectors.assertions.lock lib/mmck.vectors.assertions.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.vectors.assertions.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.vectors.assertions.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.vectors.assertions.stamp; \
	    $$dry rmdir lib/mmck.vectors.assertions.lock; \
	  else \
	    while test -d lib/mmck.vectors.assertions.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.vectors.assertions.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.vectors.assertions.import.$(MMCK_SOEXT): lib/mmck.vectors.assertions.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck vectors): module core

MMCK_VECTORS_MODULES		+= lib/mmck.vectors.core.scm
MMCK_VECTORS_RUNTIME_OBJECTS	+= lib/mmck.vectors.core.$(OBJEXT)
MMCK_VECTORS_IMPORT_SOURCES	+= lib/mmck.vectors.core.import.scm
MMCK_VECTORS_IMPORT_DEPS		+= lib/mmck.vectors.core.import.$(MMCK_SOEXT)
MMCK_VECTORS_IMPORT_SHARED_LIBS	+= lib/mmck.vectors.core.import.$(MMCK_SOEXT)
MMCK_VECTORS_TYPES_FILES_MODULES	+= lib/mmck.vectors.core.types.scm
MMCK_VECTORS_TEMP_FILES		+= \
	lib/mmck.vectors.core.temp	\
	lib/mmck.vectors.core.lock	\
	lib/mmck.vectors.core.stamp

MMCK_VECTORS_CORE_DEPS = \
	lib/mmck.vectors.assertions.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.vectors.core.stamp: lib/mmck.vectors.core.scm $(MMCK_VECTORS_CORE_DEPS)
	@rm -f lib/mmck.vectors.core.temp
	@touch lib/mmck.vectors.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.vectors.core.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.vectors.core.types.scm
	@$(MV) mmck.vectors.core.import.scm lib/mmck.vectors.core.import.scm
	@mv -f lib/mmck.vectors.core.temp $@

lib/mmck.vectors.core.types.scm: lib/mmck.vectors.core.stamp

lib/mmck.vectors.core.import.scm lib/mmck.vectors.core.$(OBJEXT): lib/mmck.vectors.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.vectors.core.lock lib/mmck.vectors.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.vectors.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.vectors.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.vectors.core.stamp; \
	    $$dry rmdir lib/mmck.vectors.core.lock; \
	  else \
	    while test -d lib/mmck.vectors.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.vectors.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.vectors.core.import.$(MMCK_SOEXT): lib/mmck.vectors.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck vectors): main module

MMCK_VECTORS_MODULES		+= lib/mmck.vectors.scm
MMCK_VECTORS_RUNTIME_OBJECTS	+= lib/mmck.vectors.$(OBJEXT)
MMCK_VECTORS_IMPORT_SOURCES	+= lib/mmck.vectors.import.scm
MMCK_VECTORS_IMPORT_SHARED_LIBS	+= lib/mmck.vectors.import.$(MMCK_SOEXT)
MMCK_VECTORS_TEMP_FILES		+= \
	lib/mmck.vectors.temp			\
	lib/mmck.vectors.lock			\
	lib/mmck.vectors.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.vectors.stamp: lib/mmck.vectors.scm $(MMCK_VECTORS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.vectors.temp
	@touch lib/mmck.vectors.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.vectors.$(OBJEXT) $(<)
	@$(MV) mmck.vectors.import.scm lib/mmck.vectors.import.scm
	@mv -f lib/mmck.vectors.temp $@

lib/mmck.vectors.import.scm lib/mmck.vectors.$(OBJEXT): lib/mmck.vectors.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.vectors.lock lib/mmck.vectors.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.vectors.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.vectors.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.vectors.stamp; \
	    $$dry rmdir lib/mmck.vectors.lock; \
	  else \
	    while test -d lib/mmck.vectors.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.vectors.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.vectors.import.$(MMCK_SOEXT): lib/mmck.vectors.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck vectors): wrapping up

EXTRA_DIST	+= $(MMCK_VECTORS_MODULES)

CLEANFILES	+= \
	$(MMCK_VECTORS_RUNTIME_SHARED_LIBS)		\
	$(MMCK_VECTORS_RUNTIME_OBJECTS)		\
	$(MMCK_VECTORS_IMPORT_SHARED_LIBS)		\
	$(MMCK_VECTORS_IMPORT_SOURCES)			\
	$(MMCK_VECTORS_TYPES_FILES_MODULES)		\
	$(MMCK_VECTORS_TYPES_FILES_SHARED_LIBS)	\
	$(MMCK_VECTORS_TEMP_FILES)

lib/mmck.vectors.$(MMCK_SOEXT): $(MMCK_VECTORS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)

lib/mmck.vectors.types: $(MMCK_VECTORS_TYPES_FILES_MODULES)
	cat $(MMCK_VECTORS_TYPES_FILES_MODULES) >$@


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_VECTORS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_VECTORS_RUNTIME_SHARED_LIBS)

# All the types specification files that we want to install.
#
MMCK_ALL_TYPES_FILES	= \
	$(MMCK_VECTORS_TYPES_FILES_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= \
	$(MMCK_ALL_RUNTIME_SHARED_LIBS)		\
	$(MMCK_ALL_IMPORT_SHARED_LIBS)		\
	$(MMCK_ALL_TYPES_FILES)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-vectors-core.scm
MMCK_CHECK_OBJECTS	+= tests/test-vectors-core.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-vectors-core.exe

tests/test-vectors-core.$(OBJEXT): tests/test-vectors-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-vectors-core.exe: tests/test-vectors-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
