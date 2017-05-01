# Import libraries needed by this file
from __future__ import print_function, unicode_literals, division

import sys
import importlib
print('\nfrom __future__ import print_function, unicode_literals, division'
      '\nimport sys'
      '\nimport importlib')



class LazyImporter(object):
    def __init__(self, name, fromlib=None, alias=None):
        self.name = name
        self.fromlib = fromlib
        self.alias = alias


    def _import_module(self):
        # Assumes that globals() already contains import alias/name
        name = object.__getattribute__(self, 'name')
        fromlib = object.__getattribute__(self, 'fromlib')
        alias = object.__getattribute__(self, 'alias')

        print('Importing "{}"...'.format(name))

        if (alias is not None and alias in globals()) or name in globals():
            del globals()[(alias or name)]
        try:
            if fromlib is None:
                _module = importlib.import_module(name)
            else:
                _module = getattr(importlib.import_module(fromlib), name)
        except ModuleNotFoundError:
            globals()[(alias or name)] = self
            response = input('Module "{0}" not found. Install "{0}"? ([y]/n): '.format((fromlib or name)))
            if response in ('', 'y', 'Y', 'Yes', 'yes', 'YES'):
                import subprocess
                subprocess.call('conda install -y {}'.format(name), shell=True)
                return object.__getattribute__(self, '_import_module')()
            raise
        globals()[(alias or name)] = _module

        return _module


    def __getattribute__(self, attr):
        _module = object.__getattribute__(self, '_import_module')()
        return getattr(_module, attr)


    def __call__(self, *args, **kwargs):
        _callable = object.__getattribute__(self, '_import_module')()
        return _callable(*args, **kwargs)



__DEFERRED_IMPORTS = []
def defer_import(name, fromlib=None, alias=None):
    global __DEFERRED_IMPORTS
    globals()[(alias or name)] = LazyImporter(name, fromlib=fromlib, alias=alias)
    __DEFERRED_IMPORTS.append((name, fromlib, alias))


# Defer importing libraries, for fast startup time
defer_import('re')
defer_import('os')
defer_import('shutil')
defer_import('glob')
defer_import('shlex')
defer_import('inspect')
defer_import('datetime')
defer_import('namedtuple', fromlib='collections')
defer_import('defaultdict', fromlib='collections')
defer_import('OrderedDict', fromlib='collections')
defer_import('Counter', fromlib='collections')
defer_import('deque', fromlib='collections')
defer_import('numpy', alias='np')
defer_import('pandas', alias='pd')
defer_import('parse', fromlib='dateutil.parser', alias='parse_date')


# Display import lines
for _name, _fromlib, _alias in __DEFERRED_IMPORTS:
    _pretty_import = 'import {}'.format(_name)
    if _fromlib is not None:
        _pretty_import = 'from {} '.format(_fromlib) + _pretty_import
    if _alias is not None:
        _pretty_import += ' as {}'.format(_alias)
    print(_pretty_import)
print()


# Print python version and startup time
__ver = sys.version_info
print('Python  v{}.{}.{}'.format(__ver.major, __ver.minor, __ver.micro))
