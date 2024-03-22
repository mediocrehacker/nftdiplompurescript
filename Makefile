build:
	spago build	

.PHONY: build fmt
fmt:
	purs-tidy format-in-place src/*.purs

.PHONY: watch
watch: 
	@find ./src -name '*.purs'  | entr $(MAKE)
