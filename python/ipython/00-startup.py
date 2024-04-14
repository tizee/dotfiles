# -*- coding: utf-8 -*-

# see ipython_config.py

try:
    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd
    import requests
except ImportError as e :
    pass

# pretty printing
# duplicate with pprint in rich.pretty
# import pprint
try:
    from rich import print # better print
    from rich import inspect # inspect variables
    from rich import pretty # integrate into REPL
    from rich.pretty import pprint
    from rich.text import Text
    from rich.table import Table
    pretty.install()
    from IPython import get_ipython
    ipython = get_ipython()
    # ipython auto-reload
    ipython.run_line_magic("load_ext", "autoreload")
    ipython.run_line_magic("autoreload", "2")

    link_github = Text("github","link https://github.com")
except ImportError as e :
    pass



def _print_dict(d):
    """https://stackoverflow.com/questions/25118698/print-python-dictionary-with-utf8-values"""
    pprint('{%s}' % ',\n'.join("'%s': '%s'" % pair for pair in d.iteritems()))


def _print_json(dict_data, indent=4):
    """handle non-latin characters"""
    pprint(json.dumps(dict_data, indent=indent, ensure_ascii=False))


pp = pprint
print_dict = _print_dict
print_json = _print_json


def is_reload():
    """ export LENS_ADMIN_DEBUG=1 """
    flag = os.environ.get('RELOAD')
    if flag in ('1', 'True', 'true'):
        return True
    return False

# http://shawnleezx.github.io/blog/2015/08/03/some-notes-on-ipython-startup-script/
pp("(00-startup.py)")
