"""
    pygments.lexers.boxdrawing
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for unicode box drawings characters.

    :copyright: Institut Pasteur (2025), author: Johann Dreo <johann.dreo@pasteur.fr>
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Comment, Text

__all__ = ['BoxDrawingLexer']


class BoxDrawingLexer(RegexLexer):
    """
    Highlight any unicode "box drawings" characters as Comment,
    remaining characters as Text or Text.Whitespace.

    Example:
    .. sourcecode:: boxdrawing

        ╭─────────╮            ╭─────────╮
        │My_source├──┤toward├──┤My_target│
        ╰┬────────╯     ┊      ╰─────┬───╯
         ┊              ┊            ┊
         ╰my_prop:this  ╳            ╽
    """

    name = 'boxdrawing'
    aliases = ['box', 'boxdrawing', 'boxdrawings', 'box-drawing', 'box-drawings', 'box_drawing', 'box_drawings']
    filenames = ['*.txt']
    url = 'https://en.wikipedia.org/wiki/Box-drawing_characters'
    version_added = '2.19.2'

    tokens = {
        'root': [
            # Unicode characters tagged as "BOX DRAWINGS .*"
            (r'[\u2500-\u257F]+', Comment),
            (r'\s+', Text.Whitespace),
            (r'[^\s\u2500-\u257F]+', Text),
        ]
    }
