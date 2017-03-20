#!/usr/bin/env python

from setuptools import setup
import io

import StationTester

def read(*filenames, **kwargs):
    encoding = kwargs.get('encoding', 'utf-8')
    sep = kwargs.get('sep', '\n')
    buf = []
    for filename in filenames:
        with io.open(filename, encoding=encoding) as f:
            buf.append(f.read())
    return sep.join(buf)

long_description = read('README.md') #, 'CHANGES.txt')

setup(name='StationTester',
    version=StationTester.__version__,
    description='IPOPP Station Testing Framework',
    long_description=long_description,
    author='Tylar Murray',
    author_email='tylarmurray@mail.usf.edu',
    url='https://github.com/USF-IMARS/station-tester',

    tests_require=['nose'],
    install_requires=[
        'networkx'  # iff you want to make graphs
    ],
    #cmdclass={'test': PyTest},

    packages=['StationTester']
)
