import sys
sys.path.insert(0, "__PARENT_DIR__")

from __MODULE__ import *

try:
    error = Error(message="test error")
    assert error.message == "test error"
except Exception as e:
    print(f"ERROR: Error model failed: {e}")
    sys.exit(1)
