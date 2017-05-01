# Import libraries needed by this file
from __future__ import print_function, unicode_literals, division

import time; start = time.time()
import sys
import importlib



class LazyImporter(object):
    def __init__(self, name, fromlib=None, alias=None):
        self._module = None
        self.name = name
        self.fromlib = fromlib
        self.alias = alias


    def _import_module(self):
        # Assumes that globals() already contains import alias/name
        if object.__getattribute__(self, '_module') is not None:
            return object.__getattribute__(self, '_module')

        name = object.__getattribute__(self, 'name')
        fromlib = object.__getattribute__(self, 'fromlib')
        alias = object.__getattribute__(self, 'alias')

        print('Importing "{}"...'.format(name))

        del globals()[(alias or name)]
        try:
            if fromlib is None:
                _module = importlib.import_module(name)
            else:
                _module = getattr(importlib.import_module(fromlib), name)
        except ModuleNotFoundError:
            globals()[(alias or name)] = self
            raise ModuleNotFoundError('No module named \'{}\''.format(name))
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
defer_import('ordereddict', fromlib='collections')
defer_import('counter', fromlib='collections')
defer_import('deque', fromlib='collections')
defer_import('numpy', alias='np')
defer_import('pandas', alias='pd')
defer_import('parse', fromlib='dateutil.parser', alias='parse_date')



# Display import lines
print()
for name, fromlib, alias in __DEFERRED_IMPORTS:
    pretty_import = 'import {}'.format(name)
    if fromlib is not None:
        pretty_import = 'from {} '.format(fromlib) + pretty_import
    if alias is not None:
        pretty_import += ' as {}'.format(alias)
    print(pretty_import)
print()


# Print python version
ver = sys.version_info
print('Python  v{}.{}.{}'.format(ver.major, ver.minor, ver.micro))
