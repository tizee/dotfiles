# -*- coding: utf-8 -*-
import os
import json
import time
import datetime as dt
import collections
import sys

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
    # pretty.install()
    from IPython import get_ipython
    ipython = get_ipython()
    ipython.magic("load_ext rich")
except ImportError as e :
    pass


def _repr_dict(d):
    """https://stackoverflow.com/questions/25118698/print-python-dictionary-with-utf8-values"""
    print('{%s}' % ',\n'.join("'%s': '%s'" % pair for pair in d.iteritems()))


def _json_dumps(dict_data, indent=4):
    """handle non-latin characters"""
    print(json.dumps(dict_data, indent=indent, ensure_ascii=False))


repr_dict = _repr_dict
pp = pprint
json_dumps = _json_dumps

print("(imported datetime, os, pprint, re, sys, time, json)")
print("(00-startup.py)")

try:
    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd
    import requests
except ImportError as e :
    pass

def is_reload():
    """ export LENS_ADMIN_DEBUG=1 """
    flag = os.environ.get('RELOAD')
    if flag in ('1', 'True', 'true'):
        return True
    return False

# http://shawnleezx.github.io/blog/2015/08/03/some-notes-on-ipython-startup-script/
# _is_reload = is_reload()
# print("export RELOAD={}".format(_is_reload))
# if _is_reload:
#     # ipython auto-reload

#     ipython.magic("load_ext autoreload")
#     ipython.magic("autoreload 2")
