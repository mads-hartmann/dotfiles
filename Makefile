
illiterate := \
	$(patsubst illiterate/%,${HOME}/%,$(shell find illiterate -type d -maxdepth 1 -name ".*"))

# I strip the file extension.
scripts := \
	$(basename $(patsubst illiterate/bin/%,/usr/local/bin/%,$(shell find illiterate/bin -type f)))

link: $(scripts) $(illiterate)

${HOME}/%: illiterate/%
	$(call print, Linking, $@ -> $<)
	@ln -sf "$(abspath $<)" "$@"

/usr/local/bin/%: illiterate/bin/%*
	$(call print, Linking, $@ -> $<)
	@ln -sf "$(abspath $<)" "$@"

# `make print-abc` to debug the value of variable abc
print-%: ; @echo $* is $($*)

# $(call print-rule, variable, extra)
#   Used to decorate the output before printing it to stdout.
define print
	@echo "[$(shell date +%H:%M:%S)] $(strip $1): $(strip $2)"
endef
