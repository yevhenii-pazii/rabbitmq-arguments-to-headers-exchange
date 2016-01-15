RABBIT_VERSION=3.5.6
APP_VERSION=1.0.3
PACKAGE=rabbitmq_arguments_to_headers_exchange

APP_FILE=src/rabbitmq_arguments_to_headers_exchange.app.src
ARCH=$(PACKAGE)-$(RABBIT_VERSION)-$(APP_VERSION)
DIST_DIR=ez
EBIN_DIR=ebin
INCLUDE_DIRS=include

version:
	sed -i -- 's/###/$(APP_VERSION)/g' $(APP_FILE)

clean:
	sed -i -- 's/$(APP_VERSION)/###/g' $(APP_FILE)
	rm -rf $(DIST_DIR)
	rm -rf $(EBIN_DIR)

package:
	rm -f $(DIST_DIR)/$(ARCH).ez
	mkdir -p $(DIST_DIR)/$(ARCH)
	cp -r $(EBIN_DIR) $(DIST_DIR)/$(ARCH)
	$(foreach EXTRA_DIR, $(INCLUDE_DIRS), cp -r $(EXTRA_DIR) $(DIST_DIR)/$(ARCH);)
	(cd $(DIST_DIR); zip -r $(ARCH).ez $(ARCH))
	rm -rf $(DIST_DIR)/$(ARCH)
	sed -i -- 's/$(APP_VERSION)/###/g' $(APP_FILE)

