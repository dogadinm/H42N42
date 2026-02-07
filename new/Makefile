PROJECT := h42n42
PORT    := 8080

OCSIGENSERVER := ocsigenserver

CONF_IN  := $(PROJECT).conf.in
CONF_OUT := local/$(PROJECT).conf

LOCAL_ROOT := local
WWW        := $(LOCAL_ROOT)/var/www/$(PROJECT)
LOG        := $(LOCAL_ROOT)/var/log/$(PROJECT)
DATA       := $(LOCAL_ROOT)/var/data/$(PROJECT)
RUN        := $(LOCAL_ROOT)/var/run

.PHONY: all build static conf run clean

all: run

build:
	dune build

prepare:
	mkdir -p $(WWW) $(LOG) $(DATA) $(RUN)

static: prepare
	cp -r static/* $(WWW)/

conf: prepare
	cp $(CONF_IN) $(CONF_OUT)

run: build static conf
	@echo "==> http://localhost:$(PORT)"
	$(OCSIGENSERVER) -c $(CONF_OUT)

clean:
	dune clean
	rm -rf local