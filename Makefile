LN = /bin/ln

PREFIX = $(HOME)

dotfiles += .emacs
dotfiles += .erlang
dotfiles += .bashrc
dotfiles += .bash_profile
dotfiles += .tmux.conf
dotfiles += .dmrc

dotfile_links = $(addprefix $(PREFIX)/,$(dotfiles))

all: install

.PHONY: uninstall
uninstall:
	for path in $(dotfile_links); do \
		if [ -L $$path ]; then \
			$(RM) --verbose $$path || exit $$?; \
		fi \
	done

$(dotfile_links): $(addprefix $(PREFIX)/,%) : $(addprefix $(CURDIR)/,%)
	$(LN) --symbolic --no-target-directory $< $@

.PHONY: dotfiles
dotfiles: $(dotfile_links)

.PHONY: install
install: dotfiles

