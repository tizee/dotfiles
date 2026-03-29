"""Pytest configuration and fixtures for rtpm tests."""

import argparse
import os
import sys
import importlib.util
import importlib.machinery

# Add the parent directory to sys.path to import the rtpm module
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import the main module (single file script without .py extension)
script_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "rtpm")

if not os.path.exists(script_path):
    raise FileNotFoundError(f"rtpm script not found at: {script_path}")

loader = importlib.machinery.SourceFileLoader("rtpm_mod", script_path)
spec = importlib.util.spec_from_loader("rtpm_mod", loader)

rtpm = importlib.util.module_from_spec(spec)
# Register in sys.modules BEFORE exec so dataclass can resolve cls.__module__
sys.modules["rtpm_mod"] = rtpm
spec.loader.exec_module(rtpm)
