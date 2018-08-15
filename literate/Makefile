
ORG_FILES := $(wildcard org/**/*.org)

build: build-website build-files

# TODO: Improve this so it only generates the files that have changed. This should
# be possible through a simple %.html: %.org rule as there's a direct connection
# between the two.
build-website: $(ORG_FILES)
	@docker run \
		-ti \
		-v $(shell pwd):/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.1 \
			emacs \
			--script dotfiles/etc/project.el \
			--eval "(org-publish-project \"everything\" t)"

# There isn't a direct connection between the source files and the tangled output
# so it doesn't make sense to try to create rules for them.
tangle: $(ORG_FILES)
	@docker run \
		-ti \
		-v $(shell pwd):/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.1 \
			emacs \
			--script dotfiles/etc/project.el \
			--eval "(org-publish-project \"tangles\" t)"

shell:
	@docker run \
		-ti \
		-v $(shell pwd):/home/babel/dotfiles:rw \
		dotfiles-ci:latest bash

clean:
	rm -rf output

# Debug.

print-%: ; @echo $* is $($*)
