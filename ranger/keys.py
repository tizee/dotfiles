#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Customized key bindings.

from ranger.api.keys import *

map = keymanager.get_context('browser')
@map("ef")
def edit_file_in_new_tmux_pane(arg):
    command = "shell tmux splitw -h 'vim " + arg.fm.env.cf.basename + "'"
    if 'TMUX' in os.environ.keys(): arg.fm.execute_console(command)
