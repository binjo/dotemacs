#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
get_putty_sessions.py

TODO
"""
__author__  = 'Binjo'
__version__ = '0.1'
__date__    = '2011-04-08 16:06:24'

import os, sys
import _winreg

def main():
    """TODO
    """
    reg = _winreg.ConnectRegistry( None, _winreg.HKEY_CURRENT_USER )

    try:
        rh = _winreg.OpenKey( reg, r'Software\SimonTatham\PuTTY\Sessions' )

        al = _winreg.QueryInfoKey(rh)[0]
        ks = [_winreg.EnumKey(rh, i) for i in xrange(al)]
    finally:
        _winreg.CloseKey(rh)
        reg.Close()

    if len(ks) != 0:
        for s in ks:
            print s
#-------------------------------------------------------------------------------
if __name__ == '__main__':
    main()
#-------------------------------------------------------------------------------
# EOF
