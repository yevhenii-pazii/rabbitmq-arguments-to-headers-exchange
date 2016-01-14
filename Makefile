PROJECT = rabbitmq_arguments_to_headers_exchange

TEST_DEPS = amqp_client

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk


RABBIT_VERSION=3.6.0
APP_VERSION=1.0.2
APP_FILE=src/rabbitmq_arguments_to_headers_exchange.app.src
README=README.md
ARCH=$(PROJECT)-$(RABBIT_VERSION)
EZ_DIR=ez/
EBIN_DIR=ebin
INCLUDE_DIRS=include


current_rmq_ref=rabbitmq_v3_6_0
base_rmq_ref=rabbitmq_v3_6_0


# --------------------------------------------------------------------
# My
# --------------------------------------------------------------------

version:
	sed -i -- 's/###/$(APP_VERSION)/g' $(APP_FILE)

package:
	rm -rf $(EZ_DIR)
	mkdir -p $(EZ_DIR)/$(ARCH)
	cp -r $(EBIN_DIR) $(EZ_DIR)/$(ARCH)
	$(foreach EXTRA_DIR, $(INCLUDE_DIRS), cp -r $(EXTRA_DIR) $(EZ_DIR)/$(ARCH);)
	(cd $(EZ_DIR); zip -r $(ARCH).ez $(ARCH))
	rm -rf $(EZ_DIR)/$(ARCH)
	sed -i -- 's/$(APP_VERSION)/###/g' $(APP_FILE)

