import os
import capnp
import operator
import sys
from importlib.abc import Loader, MetaPathFinder
from importlib.util import spec_from_loader
from pathlib import Path
from functools import reduce

capnp.remove_import_hook()

my_path = Path(os.path.dirname(os.path.abspath(__file__)))

class _Importer(MetaPathFinder, Loader):
    @classmethod
    def find_spec(cls, fullname, path, target):
        if fullname.startswith(__name__):
            if fullname in sys.modules:
                return None
            parts = fullname.split(".")
            return spec_from_loader(fullname, cls)
        return None

    @staticmethod
    def create_module(spec):
        parts = spec.name.split(".")
        return capnp.load(
            str(reduce(operator.truediv, parts[1:], my_path)) + ".capnp",
            spec.name,
            sys.path,
        )

    @staticmethod
    def exec_module(module):
        pass

sys.meta_path.append(_Importer())
