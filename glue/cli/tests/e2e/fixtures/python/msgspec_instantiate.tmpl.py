# ruff: noqa

import sys
sys.path.insert(0, "__PARENT_DIR__")
from __MODULE__ import *

user = User(name="Alice", age=30)
assert user.name == "Alice"
assert user.age == 30
