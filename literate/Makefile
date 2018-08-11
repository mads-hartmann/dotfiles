
ORG_FILES := $(wildcard org/*.org)

build: build-website build-files

build-website: $(ORG_FILES)
	@emacs \
		--quick \
		--directory $(HOME)/.emacs.d/elpa/htmlize-20180412.1944 \
		--script etc/project.el \
		--eval "(org-publish-project \"everything\" t)"

build-files: $(ORG_FILES)
	@emacs \
		--quick \
		--script etc/project.el \
		--eval "(org-babel-tangle-file \"$<\")"

clean:
	rm -rf output

# Debug.

print-%: ; @echo $* is $($*)
