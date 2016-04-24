#!/usr/bin/env python


"""Install config files into their appropriate locations.

This script does different things depending on the OS, but
assumes it's on an operating system with the `find' command. For example,
it should work on Linux, Mac OS X, FreeBSD, Solaris, etc.
"""

from __future__ import print_function

import argparse
import platform
import subprocess
import os
import shutil
import sys


# The system name, e.g. "Linux", "Darwin", "posix", etc...
SYS_NAME = platform.system()

# This dict's keys represent the types of config files,
# and its values contain the following info:
#   1. Whether it's appropriate to copy these files,
#   2. The shell command to find the files of the config file-type, and
#   3. The top-level destination directory
DOTFILE_SPECS = {
    'home': (True,
             'find . -maxdepth 1 -type f -name ".*" -a -not -name ".git*"',
             '~'),

    'emacs': (True,
              'find .emacs.d -type f -name "*.el" -o -name "*.elc"',
              '~'),

    'systemd': (SYS_NAME == 'Linux',
                'find etc -type f -name "*.service"',
                '/'),

    'launchd': (SYS_NAME == 'Darwin',
                'find etc -type f -name "*.plist"',
                '/'),
}


def main(overwrite=False, verbose=False):
    """Copy the files to their appropriate locations.
    """
    print('Copying files to appropriate system locations:')
    sys.stdout.flush()

    for file_type, (condition, cmd, dest_dir) in DOTFILE_SPECS.items():
        # Skip processing this file_type if appropriate
        if not condition:
            continue

        # Retrieve the files to be copied
        file_paths_str = subprocess.check_output(cmd, shell=True)
        file_paths = filter(None, file_paths_str.strip().split('\n'))

        if file_paths:
            print('    Copying *{}* files...'.format(file_type))
            sys.stdout.flush()
        else:
            continue

        for file_path in map(os.path.normpath, file_paths):
            dest_file_path = os.path.expanduser(os.path.join(dest_dir, file_path))

            # Create the parent directory if it doesn't exist
            dest_dirname = os.path.dirname(dest_file_path)
            if dest_dirname and not os.path.isdir(dest_dirname):
                try:
                    os.makedirs(dest_dirname)
                    print('        Created directory {}...'.format(dest_dirname))
                    sys.stdout.flush()
                except OSError:
                    print('        Error creating {}: try running with "sudo"'.format(dest_dirname))
                    sys.stdout.flush()
                    continue

            # Copy the file to its destination. Handle clobbering, etc...
            if overwrite or not os.path.isfile(dest_file_path):
                try:
                    shutil.copyfile(file_path, dest_file_path)
                    if verbose:
                        print('        ...', dest_file_path)
                        sys.stdout.flush()
                except IOError:
                    print('        Error copying to {}: try running with "sudo"'.format(dest_file_path))
                    sys.stdout.flush()
                    continue


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Install config files into their appropriate locations.')
    parser.add_argument('-f', '--force', action='store_true', help='Overwrite existing files')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print each file path after copying')
    args = parser.parse_args()

    main(args.force, args.verbose)
