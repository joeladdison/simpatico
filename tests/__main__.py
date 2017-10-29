"""Unit tests

Usage: From project's root directory:
$ python tests

or

$ python -m unittest tests

"""
from __future__ import absolute_import
import unittest

from .naming import *
from .indents import *
from .length import *
from .braces import *
from .whitespace import *
from .comments import *
from .overall import *
from .general import *

unittest.main()
