
scripts := $(addprefix /usr/local/, $(basename $(wildcard bin/*)))

bin: $(scripts)

/usr/local/bin/%: bin/%*
	$(call print, Linking, $@ -> $<)
	@ln -s "$(abspath $<)" "$@"

# `make print-abc` to debug the value of variable abc
print-%: ; @echo $* is $($*)

# $(call print-rule, variable, extra)
#   Used to decorate the output before printing it to stdout.
define print
	@echo "[$(shell date +%H:%M:%S)] $(strip $1): $(strip $2)"
endef
