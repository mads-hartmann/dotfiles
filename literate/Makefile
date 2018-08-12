
ORG_FILES := $(wildcard org/*.org)

build: build-website build-files

build-website: $(ORG_FILES)
	@docker run \
		-ti \
		-v $(shell pwd):/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.1 \
			emacs \
			--script dotfiles/etc/project.el \
			--eval "(org-publish-project \"everything\" t)"

build-files: $(ORG_FILES)
	@emacs \
		--quick \
		--script etc/project.el \
		--eval "(org-babel-tangle-file \"$<\")"

shell:
	@docker run \
		-ti \
		-v $(shell pwd):/home/babel/dotfiles:rw \
		dotfiles-ci:latest bash

clean:
	rm -rf output

# Debug.

print-%: ; @echo $* is $($*)
