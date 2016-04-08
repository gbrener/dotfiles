from __future__ import print_function, unicode_literals, division

import sys, os, shutil, glob, shlex
import re
import inspect
import datetime
import numpy as np
import pandas as pd
from collections import namedtuple, defaultdict, OrderedDict, Counter, deque
from dateutil.parser import parse as parse_date

#### IPython config
ipython_ver = None
try:
    c = get_config()
    c.InteractiveShellApp.exec_PYTHONSTARTUP = False
    c.TerminalIPythonApp.display_banner = False
    c.InteractiveShell.display_page = True # export PAGER=cat
    c.TerminalInteractiveShell.confirm_exit = False
    c.TerminalInteractiveShell.editor = os.getenv('VISUAL', os.getenv('EDITOR', 'nano'))

    import IPython
    ipython_ver = IPython.__version__
except NameError:
    pass

this_file = re.split(r'#+ IPython', open(__file__).read(), 1)[0]
ver = sys.version_info
print(this_file.rstrip()+'\n')
print('Python  v{0}.{1}.{2}'.format(ver.major, ver.minor, ver.micro))
if ipython_ver is not None:
    print('IPython v{0}'.format(ipython_ver))
