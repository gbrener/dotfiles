from __future__ import print_function, unicode_literals, division

import sys, os, shutil, glob, shlex
import re
import inspect
import numpy as np
import pandas as pd

from collections import namedtuple, defaultdict, OrderedDict, Counter, deque
from datetime import datetime, date, time, timedelta
from dateutil.parser import parse as parse_date

### IPython config
try:
    # Create IPython config directory if not exists
    ipython_dir = os.path.join(os.getenv('HOME'), 'Desktop', 'ipython')
    if not os.path.isdir(ipython_dir):
        os.mkdir(ipython_dir)
    c.BaseIPythonApplication.ipython_dir = ipython_dir

    c = get_config()
    c.TerminalIPythonApp.display_banner = False
    c.InteractiveShell.display_page = True # export PAGER=cat
    c.TerminalInteractiveShell.confirm_exit = False
    c.TerminalInteractiveShell.editor = os.getenv('VISUAL', os.getenv('EDITOR', 'nano'))
except NameError:
    this_file = re.split(r'#+ IPython', open(__file__).read(), 1)[0]
    print(this_file.rstrip()+'\n')
