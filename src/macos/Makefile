# Builds the Mac OS X installer for Haskell Platform

# NOTE: This process depends on much of the scripts and makefiles that go into
# producing the generic source tarball for Haskell Platform. If that process
# changes, then this will likely need changing too. See ../generic

all: help

# location where the build and assembly of the image is performed
BUILD := dist
DIRS := $(BUILD)

SPEC_CABAL := ../../haskell-platform.cabal

GHC_VERSION := $(subst ==,,$(word 3,$(shell grep '^tested-with:' $(SPEC_CABAL))))
GHC_ARCH := i386
PLATFORM_VERSION := $(word 2,$(shell grep '^version:' $(SPEC_CABAL)))

# Locations are paths to things that are installed. There are three versions
# of every location:
#   TARGET_<foo>    - a path in the installation on the target system (always absolute)
#   IMAGE_<foo>     - a path in the image of the target system, on this build system
#   IMAGE_<foo>_ABS - absolute path to same

_REL_ROOT := 
TARGET_ROOT := /
IMAGE_ROOT := $(BUILD)/root
IMAGE_ROOT_ABS := $(abspath $(IMAGE_ROOT))
ROOT = $$(error Don't use ROOT as a variable. Use TARGET_ROOT, IMAGE_ROOT, or IMAGE_ROOT_ABS instead.)

DIRS += $(IMAGE_ROOT)

define location
	# $1 = location name, used as base for variables names,
	# $2 = location that this location is relative too, use ROOT for top
	# $3 = relative path
_REL_$1 := $$(_REL_$2)/$3
TARGET_$1 := $$(_REL_$1)
IMAGE_$1 := $(IMAGE_ROOT)$$(_REL_$1)
IMAGE_$1_ABS := $(abspath $$(IMAGE_$1))
$1 = $$(error Don't use $1 as a variable. Use TARGET_$1, IMAGE_$1, or IMAGE_$1_ABS instead.)
endef

# The two main components that will be installed
$(eval $(call location,FRAMEWORKS,ROOT,Library/Frameworks))
$(eval $(call location,HASKELL,ROOT,Library/Haskell))
DIRS += $(IMAGE_FRAMEWORKS) $(IMAGE_HASKELL)


###
### Fetch GHC distribution, and unpack it
###
GHC_PKG_URL := http://haskell.org/ghc/dist/$(GHC_VERSION)/GHC-$(GHC_VERSION)-$(GHC_ARCH).pkg
GHC_PKG := $(BUILD)/GHC-$(GHC_VERSION)-$(GHC_ARCH).pkg
GHC_PACKAGE := $(BUILD)/GHC-package
GHC_PAYLOAD := $(GHC_PACKAGE)/ghc.pkg/Payload

$(eval $(call location,GHC_FRAMEWORK,FRAMEWORKS,GHC.framework))
$(eval $(call location,GHC_BIN,GHC_FRAMEWORK,Versions/Current/usr/bin))
$(eval $(call location,GHC_EXEC,GHC_BIN,ghc))

$(GHC_PKG):
	curl $(GHC_PKG_URL) -o $@

$(GHC_PAYLOAD): | $(GHC_PKG)
	-rm -rf $(GHC_PACKAGE)
	pkgutil --expand $(GHC_PKG) $(GHC_PACKAGE)
	
$(IMAGE_GHC_FRAMEWORK): | $(GHC_PAYLOAD)
	-rm -rf $(IMAGE_GHC_FRAMEWORK)
	gunzip < $(GHC_PAYLOAD) | (cd $(IMAGE_FRAMEWORKS) && cpio -i)

ghc: dirs | $(IMAGE_GHC_FRAMEWORK)
clean-ghc:
	-rm -rf $(IMAGE_GHC_FRAMEWORK)
	-rm -rf $(GHC_PACKAGE)
clean: clean-ghc
.PHONY: ghc clean-ghc
.PRECIOUS: $(GHC_PKG)

###
### Rewrap GHC executables in versions of scripts that reference local dist.
### Rewrite GHC package database to one the references the local dist.
###
GHC_PROGS := ghc ghc-pkg haddock hsc2hs
$(eval $(call location,GHC_PACKAGE_DB,GHC_FRAMEWORK,Versions/Current/usr/lib/ghc-$(GHC_VERSION)/package.conf.d))

BUILD_PACKAGE_DB := $(BUILD)/package.conf.d
BUILD_BIN := $(BUILD)/bin
BUILD_PROGS :=
DIRS += $(BUILD_BIN) $(BUILD_PACKAGE_DB)

WITH_DB := GHC_PACKAGE_PATH=$(abspath $(BUILD_PACKAGE_DB))
WITH_PATH := PATH="$(abspath $(BUILD_BIN)):$$PATH"

define rewrap-prog # $1 = prog
$(BUILD_BIN)/$1: | $(IMAGE_GHC_FRAMEWORK)
	sed -e 's:$(TARGET_GHC_FRAMEWORK):$(IMAGE_GHC_FRAMEWORK_ABS):' $(IMAGE_GHC_BIN)/$1 > $$@
	chmod +x $$@
BUILD_PROGS += $(BUILD_BIN)/$1
endef
$(foreach prog,$(GHC_PROGS),\
  $(eval $(call rewrap-prog,$(prog))))

$(BUILD_PACKAGE_DB)/package.cache: | $(IMAGE_GHC_FRAMEWORK)
	for conf in $(IMAGE_GHC_PACKAGE_DB)/*.conf; do \
	  sed -e 's:$(TARGET_GHC_FRAMEWORK):$(IMAGE_GHC_FRAMEWORK_ABS):' $$conf > $(BUILD_PACKAGE_DB)/`basename $$conf`; \
	done
	$(BUILD_BIN)/ghc-pkg --package-conf $(BUILD_PACKAGE_DB) recache

rewrap: dirs ghc $(BUILD_PROGS) $(BUILD_PACKAGE_DB)/package.cache
clean-rewrap:
	-rm -rf $(BUILD_BIN)
	-rm -rf $(BUILD_PACKAGE_DB)
clean: clean-rewrap
.PHONEY: rewrap clean-rewrap

###
### Create a working copy of the base source image
###
GENERIC_SOURCE_TREE := ../generic/haskell-platform-$(PLATFORM_VERSION)
GENERIC_PACKAGE_LIST := $(GENERIC_SOURCE_TREE)/packages/platform.packages
BUILD_SOURCE_TREE := $(BUILD)/source
BUILD_PACKAGE_LIST := $(BUILD_SOURCE_TREE)/packages/platform.packages

$(GENERIC_PACKAGE_LIST):
	cd ../generic && $(WITH_DB) $(WITH_PATH) sh prepare.sh

$(BUILD_PACKAGE_LIST): $(GENERIC_PACKAGE_LIST)
	cp -r $(GENERIC_SOURCE_TREE)/ $(BUILD_SOURCE_TREE)

source: dirs $(BUILD_PACKAGE_LIST)
clean-source:
	-rm -rf $(BUILD_SOURCE_TREE)
clean: clean-source
.PHONY: source clean-source

###
### Build the platform libraries
###
$(eval $(call location,PLATFORM_BASE,HASKELL,ghc-$(GHC_VERSION)))
$(eval $(call location,PLATFORM_LIB,PLATFORM_BASE,lib))

CONFIG_STATUS := $(BUILD_SOURCE_TREE)/config.status
CONFIG_OPTIONS += --disable-user-install
CONFIG_OPTIONS += --prefix=$(TARGET_PLATFORM_LIB)
CD_AND := cd $(BUILD_SOURCE_TREE) &&
SRC_RUN := $(CD_AND) $(WITH_DB) $(WITH_PATH)

$(CONFIG_STATUS): $(BUILD_PACKAGE_LIST)
	$(SRC_RUN) ./configure $(CONFIG_OPTIONS)

config: rewrap source $(CONFIG_STATUS)
clean-config:
	-rm $(CONFIG_STATUS)
clean: clean-config
.PHONY: config clean-config


BUILD_STAMP := $(BUILD_SOURCE_TREE)/build.stamp
CABAL_OPTS := --prefix=$(TARGET_PLATFORM_LIB)/\$$pkgid
CABAL_OPTS += --libsubdir=
CABAL_OPTS += --datasubdir=
CABAL_OPTS += --docdir=\$$prefix/doc

$(BUILD_STAMP): $(CONFIG_STATUS)
	$(SRC_RUN) EXTRA_CONFIGURE_OPTS="${CABAL_OPTS}" scripts/build.sh
	@touch $@

build: config $(BUILD_STAMP)
clean-build:
	-rm $(BUILD_STAMP)
clean: clean-build
.PHONY: build clean-build


$(eval $(call location,PLATFORM_REGISTRATIONS,PLATFORM_LIB,registrations))

$(IMAGE_PLATFORM_REGISTRATIONS): $(BUILD_STAMP)
	-rm -rf $(IMAGE_PLATFORM_REGISTRATIONS)
	-mkdir -p $(IMAGE_PLATFORM_REGISTRATIONS)
	$(SRC_RUN) scripts/package.sh $(IMAGE_ROOT_ABS) $(IMAGE_PLATFORM_REGISTRATIONS_ABS)
	@touch $@

package: build $(IMAGE_PLATFORM_REGISTRATIONS)
clean-package:
	-rm -rf $(IMAGE_PLATFORM_REGISTRATIONS)
	
clean: clean-package
.PHONY: package clean-package


###
### Scripts
###
HP_POST_SCRIPT = $(BUILD)/hp-postinstall.sh

$(HP_POST_SCRIPT):
	echo "#!/bin/sh" > $@
	echo "for conf in $(TARGET_PLATFORM_REGISTRATIONS)/*" >> $@
	echo "do" >> $@
	echo "    /usr/bin/ghc-pkg register \$$conf" >> $@
	echo "done" >> $@
	echo "ln -sf $(TARGET_HASKELL)/bin/* /usr/bin" >> $@

scripts: $(HP_POST_SCRIPT)
clean-scripts:
	-rm -rf $(HP_POST_SCRIPT)
clean: clean-scripts
.PHONY: scripts clean-scripts


###
### Build master haddock
###
$(eval $(call location,DOC_BASE,PLATFORM_BASE,doc))
$(eval $(call location,INDEX_HTML,DOC_BASE,index.html))
$(eval $(call location,START_HTML,DOC_BASE,start.html))
$(eval $(call location,GHC_DOC_BASE,GHC_FRAMEWORK,Versions/${GHC_VERSION}-$(GHC_ARCH)/usr/share/doc/ghc/html))

define relpath # $1 from, $2 to
$(shell python -c "import os; print os.path.relpath('$1','$2')")
endef

PKGS_PLAT_REL := $(call relpath,$(IMAGE_PLATFORM_LIB),$(IMAGE_DOC_BASE))
PKGS_GHC_REL := $(call relpath,$(IMAGE_GHC_DOC_BASE),$(IMAGE_DOC_BASE))

DIRS += $(IMAGE_DOC_BASE)

$(IMAGE_INDEX_HTML): $(HP_POST_SCRIPT)
	read="" ; \
	for conf in $(IMAGE_PLATFORM_REGISTRATIONS)/*; do \
	  html=`grep haddock-html $$conf | cut -f 2 -d ' '` ; \
	  intf=`grep haddock-interfaces $$conf | cut -f 2 -d ' '` ; \
	  if [ -f ${IMAGE_ROOT}$$intf ] ; then \
	    read="$$read --read-interface=packages/$${html#${TARGET_PLATFORM_LIB}/},${IMAGE_ROOT}$$intf" ; \
	  fi ; \
	done ; \
	for pkg in `$(BUILD_BIN)/ghc-pkg list --package-conf=$(BUILD_PACKAGE_DB) --simple-output`; do \
	  html=`$(BUILD_BIN)/ghc-pkg field $$pkg haddock-html | cut -f 2 -d ' '` ; \
	  intf=`$(BUILD_BIN)/ghc-pkg field $$pkg haddock-interfaces | cut -f 2 -d ' '` ; \
	  if [ "$${pkg%-*}" \!= "ghc" -a -f ${IMAGE_ROOT}$$intf ] ; then \
	    read="$$read --read-interface=ghc-doc/$${html#${TARGET_GHC_DOC_BASE}/},${IMAGE_ROOT}$$intf" ; \
	  fi ; \
	done ; \
	./$(BUILD_BIN)/haddock --odir=${IMAGE_DOC_BASE} --gen-index --gen-contents \
	  $$read --title="Haskell Platform"
	# These can't be symlink'd in the image as they cause PackageMaker fits
	#	cd $(IMAGE_DOC_BASE) && ln -s $(PKGS_PLAT_REL) packages
	#	cd $(IMAGE_DOC_BASE) && ln -s $(PKGS_GHC_REL) ghc-doc
	# So symlink them in the post install script
	echo "ln -s $(PKGS_PLAT_REL) $(TARGET_DOC_BASE)/packages" >> $(HP_POST_SCRIPT)
	echo "ln -s $(PKGS_GHC_REL) $(TARGET_DOC_BASE)/ghc-doc" >> $(HP_POST_SCRIPT)

$(IMAGE_START_HTML): start.html
	cp $< $@
	
doc: ghc package scripts $(IMAGE_INDEX_HTML) $(IMAGE_START_HTML)
clean-doc:
	-rm -rf $(IMAGE_DOC_BASE)
clean: clean-doc
.PHONY: doc clean-doc


###
### Final polish of the tree
###
$(eval $(call location,PLATFORM_BIN,PLATFORM_BASE,bin))
	
polish: doc package
	-rm -rf $(IMAGE_PLATFORM_BIN)
	mkdir -p $(IMAGE_PLATFORM_BIN)
	cd $(IMAGE_PLATFORM_BIN) && ln -s ../lib/*/bin/* .
	-rm $(IMAGE_PLATFORM_BIN)/*-tests
	mv $(IMAGE_PLATFORM_BIN)/cabal $(IMAGE_PLATFORM_BIN)/cabal.real
	cp cabal.wrap $(IMAGE_PLATFORM_BIN)/cabal.wrap
	chmod +x $(IMAGE_PLATFORM_BIN)/cabal.wrap
	cd $(IMAGE_PLATFORM_BIN) && ln -s cabal.wrap cabal
	cd $(IMAGE_HASKELL) && ln -sf ghc-$(GHC_VERSION) current
	cd $(IMAGE_HASKELL) && ln -sf current/bin bin
	cd $(IMAGE_HASKELL) && ln -sf current/doc doc

.PHONY: polish


###
### Build the installer package
###
INSTALLER_PACKAGE = dist/HaskellPlatform.pkg

$(INSTALLER_PACKAGE): Platform.pmdoc ghc scripts polish
	#/Developer/usr/bin/packagemaker --doc Platform.pmdoc --verbose --out $@
	@echo "About to change permissions of root. You will need to authorize:"
	sudo chown -R -P root:admin $(IMAGE_ROOT)
	@echo "Build from PackageMaker please..."
	rm -f Platform.pmdoc/*-contents.xml
	open -a /Developer/Applications/Utilities/PackageMaker.app Platform.pmdoc
	
installer: $(INSTALLER_PACKAGE)
clean-installer:
	-rm $(INSTALLER_PACKAGE)
clean: clean-installer
.PHONY: installer clean-installer


###
### Misc
###

help:
	@echo "make one of the following targets:"
	@echo "   ghc     	 -- fetch and unpack ghc"
	@echo "   rewrap  	 -- create working relocated ghc"
	@echo "   source  	 -- create a working copy of the haskell platform source tree"
	@echo "   config     -- config the working copy to use the unpacked ghc"
	@echo "   build      -- build the packages in the working copy"
	@echo "   package    -- install the packages in the root tree"
	@echo "   scripts    -- create scripts needed for the installer"
	@echo "   doc        -- build the master doc index"
	@echo "   polish     -- various odds and ends to make the tree beautiful"
	@echo "   installler -- build the Mac installer package"
.PHONY: clean help

dirs: $(DIRS)
.PHONY: dirs
	
$(DIRS):
	-mkdir -p $@


