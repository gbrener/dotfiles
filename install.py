#!/usr/bin/env python


"""Install config files into their appropriate locations.

This script does different things depending on the OS, but
assumes it's on an operating system with the `find' command. For example,
it should work on Linux, Mac OS X, FreeBSD, Solaris, etc.
"""

from __future__ import print_function, unicode_literals

import argparse
import platform
import subprocess
import shutil
import os
import filecmp


# The system name, e.g. "Linux", "Darwin", "posix", etc...
SYS_NAME = platform.system()

DOTFILE_SPECS = {
    'home': (True,
             # Hidden files in the current directory.
             'find . -maxdepth 1 -type f -name ".*" -a -not -name ".git*"',
             '~'),

    'emacs': (True,
              # Emacs lisp and compiled emacs lisp files.
              'find .emacs.d -type f -name "*.el" -o -name "*.elc"',
              '~'),

    'systemd': (SYS_NAME == 'Linux',
                # Systemd .service files
                'find etc -type f -name "*.service"',
                '/'),

    'launchd': (SYS_NAME == 'Darwin',
                # Launchd .plist files
                'find etc -type f -name "*.plist"',
                '/'),
}


def main(verbose=False, dry_run=False):
    """Main function."""
    for _file_type, (condition, cmd, dest_dir) in DOTFILE_SPECS.items():
        # Skip copying anything if condition is not met.
        if not condition:
            continue

        # Find files to copy, and loop over each one.
        output = subprocess.check_output(cmd, shell=True)
        if not output:
            print('WARNING: no output for cmd `{}`'.format(cmd))
            continue

        for src_fpath in output.decode('utf-8').rstrip().split('\n'):
            # Compute destination path
            dst_fpath = os.path.normpath(os.path.expanduser(os.path.join(dest_dir, src_fpath)))

            # Skip file if it already exists and didn't change.
            # If it did change, show the diff before clobbering.
            if os.path.isfile(dst_fpath):
                files_are_eq = filecmp.cmp(src_fpath, dst_fpath)
                if files_are_eq:
                    continue
                elif verbose:
                    diff_cmd = 'diff {} {}'.format(src_fpath, dst_fpath)
                    print('\nDEBUG: '+diff_cmd)
                    subprocess.call(diff_cmd, shell=True)
                    print()

            # Create directories as needed.
            parent_dir = os.path.dirname(dst_fpath)
            if not os.path.isdir(parent_dir):
                print('DEBUG: mkdir -p {}'.format(parent_dir))
                if not dry_run:
                    os.makedirs(parent_dir)

            # Copy files
            print('DEBUG: cp {} {}'.format(src_fpath, dst_fpath))
            if not dry_run:
                shutil.copy(src_fpath, dst_fpath)


if __name__ == '__main__':
    # pylint: disable=invalid-name
    parser = argparse.ArgumentParser(description='Install config files to appropriate locations.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Show diff before clobbering')
    parser.add_argument('-d', '--dry-run', action='store_true', help='Don\'t actually do the work')
    args = parser.parse_args()

    if args.verbose:
        print('INFO: --verbose option detected. Diffs will be displayed.')

    if args.dry_run:
        print('INFO: --dry-run option detected. Commands will not be executed.')

    main(args.verbose, args.dry_run)
