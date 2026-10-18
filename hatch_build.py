"""
    hatch_build.py
    ~~~~~~~~~~~~~~

    Build hook that ships the lexers with their simple rules merged.

    When a wheel is built, ``scripts/optimize_lexers.py`` rewrites the lexer
    modules so that runs of consecutive simple rules are merged into one
    regex, which makes lexing considerably faster.  The sources in the
    repository stay as they are, and so do editable installs
    (``pip install -e .``), which is the way to run the unoptimized lexers
    while developing.  Set ``PYGMENTS_OPTIMIZE_LEXERS=0`` to build a wheel
    without the optimization.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import shutil
import subprocess
import sys
import tempfile

from hatchling.builders.hooks.plugin.interface import BuildHookInterface


class OptimizeLexersBuildHook(BuildHookInterface):
    PLUGIN_NAME = 'custom'

    def initialize(self, version, build_data):
        if self.target_name != 'wheel' or version != 'standard':
            return
        if os.environ.get('PYGMENTS_OPTIMIZE_LEXERS', '1') == '0':
            self.app.display_warning(
                'PYGMENTS_OPTIMIZE_LEXERS=0: shipping the lexers unoptimized')
            return

        self._tempdir = tempfile.mkdtemp(prefix='pygments-lexers-')
        script = os.path.join(self.root, 'scripts', 'optimize_lexers.py')
        subprocess.run([sys.executable, script, '--out', self._tempdir],
                       check=True, cwd=self.root)

        outdir = os.path.join(self._tempdir, 'pygments', 'lexers')
        names = sorted(os.listdir(outdir))
        for name in names:
            # force_include replaces the module from the source tree.
            build_data['force_include'][os.path.join(outdir, name)] = \
                f'pygments/lexers/{name}'
        self.app.display_info(
            f'Optimized the rules of {len(names)} lexer modules')

    def finalize(self, version, build_data, artifact_path):
        tempdir = getattr(self, '_tempdir', None)
        if tempdir:
            shutil.rmtree(tempdir, ignore_errors=True)
