#! /usr/bin/env python

import re
import urllib
import sys

SPEC_URL = 'http://code.galois.com/darcs/haskell-platform/' \
    'haskell-platform.cabal'
CABAL_INSTALL_DEPS = ['zlib', 'mtl', 'parsec', 'HTTP', 'network']

def get_cabal_install_version(f):
    """File -> String"""
    r = re.compile(r'cabal-install\s*==\s*([0-9\.]+)\s*,')

    for l in f:
        if l.find('cabal-install') != -1:
            m = re.search(r, l)
            if m is not None:
                return m.group(1)
    return None

def get_package_versions(f):
    """File -> { String : String }"""
    d = {}
    r = re.compile(r'\s*([\w-]+)\s*==\s*([0-9.]+)\s*\,?\s*')

    for l in f:
        if l.find('library') == 0:
            break
    for l in f:
        m = re.match(r,l)
        if m:
            d[m.group(1)] = m.group(2)

    return d

def main(url):
    print("Fetching the haskell-platform.cabal file from '" + url + "'...\n")
    filename, headers = urllib.urlretrieve(url)

    with open(filename) as f:
        v = get_cabal_install_version(f)
        f.seek(0)
        d = get_package_versions(f)
        d['cabal-install'] = v

    for k,v in d.iteritems():
        print(k + " version is " + v)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(sys.argv[1])
    else:
        main(SPEC_URL)
