PACKAGE=rabbitmq_arguments_to_headers_exchange
RABBIT_VERSION=3.5.6
ARCH=$(PACKAGE)-$(RABBIT_VERSION)
DIST_DIR=ez
EBIN_DIR=ebin
INCLUDE_DIRS=include
DEPS_DIR=deps
DEPS ?=
DEPS_EZ=$(foreach DEP, $(DEPS), $(DEPS_DIR)/$(DEP).ez)
RABBITMQ_HOME ?= .

all:

clean:
	rm -rf $(DIST_DIR)
	rm -rf $(EBIN_DIR)

distclean:
	clean
	rm -rf $(DEPS_DIR)

package:
	rm -f $(DIST_DIR)/$(ARCH).ez
	mkdir -p $(DIST_DIR)/$(ARCH)
	cp -r $(EBIN_DIR) $(DIST_DIR)/$(ARCH)
	$(foreach EXTRA_DIR, $(INCLUDE_DIRS), cp -r $(EXTRA_DIR) $(DIST_DIR)/$(ARCH);)
	(cd $(DIST_DIR); zip -r $(ARCH).ez $(ARCH))
	rm -rf $(DIST_DIR)/$(ARCH)

