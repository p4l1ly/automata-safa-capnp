#!/usr/bin/env python

import os
from distutils.core import setup
from pathlib import Path

setup(
    name='automata-safa-capnp',
    version='0.1',
    description='',
    author='Pavol Vargovčík',
    author_email='pavol.vargovcik@gmail.com',
    packages=['automata_safa_capnproto'],
    package_dir={'automata_safa_capnproto': 'schema'},
    package_data={
        'automata_safa_capnproto': [
            str(path.relative_to("schema")) for path in Path("schema").rglob("*.capnp")
        ]
    },
)
