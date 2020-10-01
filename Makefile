DIST_PATH := $(shell stack path | grep dist-dir | sed 's/[^ ]* //')
BINARY_NAME := barcode-backend
BINARY_PATH_RELATIVE := $(DIST_PATH)/build/${BINARY_NAME}/${BINARY_NAME}

all: build docker

build:
	stack build

docker: build
	sudo BINARY_PATH=$(BINARY_PATH_RELATIVE) docker build --build-arg BINARY_PATH -t ${BINARY_NAME} .
