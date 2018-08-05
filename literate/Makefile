
ORG_FILES := $(wildcard org/*.org)

build: build-website build-files

build-website: $(ORG_FILES)
	@emacs \
		--quick \
		--script etc/project.el \
		--eval "(org-publish-project \"org\")"

build-files: $(ORG_FILES)
	@emacs \
		--quick \
		--script etc/project.el \
		--eval "(org-babel-tangle-file \"$<\")"

clean:
	rm -rf output

# Debug.

print-%: ; @echo $* is $($*)
